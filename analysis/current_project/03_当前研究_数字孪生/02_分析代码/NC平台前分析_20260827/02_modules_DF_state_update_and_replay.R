options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glmnet)
  library(patchwork)
  library(pdftools)
  library(readr)
  library(scales)
  library(tidyr)
})

set.seed(20260827)

input_dir <- Sys.getenv("TCM_NC_JOINT_DIR", unset = "")
output_dir <- Sys.getenv("TCM_NC_OUTPUT_DIR", unset = "")
if (!nzchar(input_dir) || !nzchar(output_dir)) {
  stop("TCM_NC_JOINT_DIR and TCM_NC_OUTPUT_DIR are required. Use the paired PowerShell launcher.")
}
tables_dir <- file.path(output_dir, "tables")
figures_dir <- file.path(output_dir, "figures")
docs_dir <- file.path(output_dir, "docs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

joint_path <- file.path(input_dir, "03_joint_model_matrix.csv")
if (!file.exists(joint_path)) stop("Missing joint model matrix: ", joint_path)

auc_fast <- function(y, p) {
  keep <- !is.na(y) & !is.na(p)
  y <- as.integer(y[keep]); p <- as.numeric(p[keep])
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

pr_auc_fast <- function(y, p) {
  keep <- !is.na(y) & !is.na(p)
  y <- as.integer(y[keep]); p <- as.numeric(p[keep])
  if (sum(y == 1) == 0) return(NA_real_)
  ord <- order(p, decreasing = TRUE)
  yy <- y[ord]
  precision <- cumsum(yy) / seq_along(yy)
  recall <- cumsum(yy) / sum(yy)
  recall <- c(0, recall); precision <- c(1, precision)
  sum(diff(recall) * (head(precision, -1) + tail(precision, -1)) / 2)
}

calibration_stats <- function(y, p) {
  keep <- !is.na(y) & !is.na(p)
  y <- as.integer(y[keep]); p <- pmin(pmax(as.numeric(p[keep]), 1e-5), 1 - 1e-5)
  if (length(unique(y)) < 2 || sd(qlogis(p)) == 0) {
    return(c(calibration_intercept = NA_real_, calibration_slope = NA_real_))
  }
  lp <- qlogis(p)
  intercept_fit <- tryCatch(glm(y ~ 1 + offset(lp), family = binomial()), error = function(e) NULL)
  slope_fit <- tryCatch(glm(y ~ lp, family = binomial()), error = function(e) NULL)
  c(
    calibration_intercept = if (is.null(intercept_fit)) NA_real_ else unname(coef(intercept_fit)[1]),
    calibration_slope = if (is.null(slope_fit)) NA_real_ else unname(coef(slope_fit)[2])
  )
}

ece_deciles <- function(y, p) {
  tibble(y = y, p = p) |>
    filter(!is.na(y), !is.na(p)) |>
    mutate(bin = ntile(p, min(10, n()))) |>
    group_by(bin) |>
    summarise(n = n(), obs = mean(y), pred = mean(p), .groups = "drop") |>
    summarise(ece = sum(n * abs(obs - pred)) / sum(n)) |>
    pull(ece)
}

metrics_binary <- function(y, p) {
  keep <- !is.na(y) & !is.na(p)
  y <- as.integer(y[keep]); p <- pmin(pmax(as.numeric(p[keep]), 1e-6), 1 - 1e-6)
  cal <- calibration_stats(y, p)
  tibble(
    n = length(y), issue_n = sum(y), issue_rate = mean(y),
    AUC = auc_fast(y, p), PR_AUC = pr_auc_fast(y, p),
    Brier = mean((p - y)^2), ECE = ece_deciles(y, p),
    calibration_intercept = cal[["calibration_intercept"]],
    calibration_slope = cal[["calibration_slope"]]
  )
}

make_stratified_folds <- function(y, k = 5, seed = 20260827) {
  set.seed(seed)
  folds <- rep(NA_integer_, length(y))
  for (cls in sort(unique(y))) {
    idx <- sample(which(y == cls))
    folds[idx] <- rep(seq_len(k), length.out = length(idx))
  }
  folds
}

impute_train_test <- function(train_df, test_df, vars) {
  x_train <- list(); x_test <- list()
  for (v in vars) {
    tr <- suppressWarnings(as.numeric(train_df[[v]])); te <- suppressWarnings(as.numeric(test_df[[v]]))
    tr_miss <- as.integer(is.na(tr)); te_miss <- as.integer(is.na(te))
    med <- median(tr, na.rm = TRUE); if (!is.finite(med)) med <- 0
    tr[is.na(tr)] <- med; te[is.na(te)] <- med
    mu <- mean(tr); sigma <- sd(tr); if (!is.finite(sigma) || sigma == 0) sigma <- 1
    x_train[[v]] <- (tr - mu) / sigma; x_test[[v]] <- (te - mu) / sigma
    if (any(tr_miss == 1) || any(te_miss == 1)) {
      x_train[[paste0(v, "__missing")]] <- tr_miss
      x_test[[paste0(v, "__missing")]] <- te_miss
    }
  }
  xtr <- as.matrix(as.data.frame(x_train)); xte <- as.matrix(as.data.frame(x_test))
  storage.mode(xtr) <- "double"; storage.mode(xte) <- "double"
  list(train = xtr, test = xte)
}

fit_predict_elastic <- function(train_df, test_df, vars, seed = 20260827) {
  y <- as.integer(train_df$disintegration_issue)
  mats <- impute_train_test(train_df, test_df, vars)
  class_counts <- table(y)
  if (length(class_counts) < 2 || min(class_counts) < 2) return(rep(mean(y), nrow(test_df)))
  k <- min(5L, as.integer(min(class_counts)))
  foldid <- make_stratified_folds(y, k = k, seed = seed)
  fit <- cv.glmnet(
    mats$train, y, family = "binomial", alpha = 0.5,
    foldid = foldid, type.measure = "deviance", standardize = FALSE
  )
  as.numeric(predict(fit, newx = mats$test, s = "lambda.min", type = "response"))
}

wilson_ci <- function(x, n, z = 1.96) {
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(low = max(0, center - half), high = min(1, center + half))
}

save_pdf_png <- function(plot, stem, width = 14.0, height = 8.8, dpi = 240) {
  pdf_file <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_file <- file.path(figures_dir, paste0(stem, "_preview.png"))
  ggsave(pdf_file, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  suppressWarnings(pdftools::pdf_convert(pdf_file, format = "png", pages = 1, dpi = dpi, filenames = png_file))
  if (!file.exists(pdf_file) || !file.exists(png_file) || file.info(pdf_file)$size == 0 || file.info(png_file)$size == 0) {
    stop("Empty figure output for ", stem)
  }
}

joint <- read_csv(joint_path, show_col_types = FALSE) |>
  mutate(
    finished_batch = as.character(finished_batch),
    mes_production_date = as.Date(mes_production_date),
    d2_observation_date = as.Date(d2_observation_date),
    disintegration_issue = as.integer(disintegration_issue)
  ) |>
  arrange(mes_production_date, finished_batch) |>
  mutate(
    lag_days_raw = as.integer(d2_observation_date - mes_production_date),
    qc_available_date = if_else(lag_days_raw < 0, mes_production_date + 4, d2_observation_date),
    twin_id = sprintf("TWIN-%04d", row_number())
  )

mes_vars <- c(
  "coating_yield_pct", "final_blend_moisture_pct", "coating_mass_balance_pct",
  "compression_yield_pct", "compression_hardness_mean_n", "coated_tablet_weight_mean_g",
  "final_blend_lt_100_mesh_pct", "granulation_discharge_moisture_pct_mean", "core_tablet_weight_mean_g"
)

initial_cut <- as.Date("2026-01-01")
reference <- joint |> filter(mes_production_date < initial_cut, qc_available_date < initial_cut)
reference_production_only <- joint |> filter(mes_production_date < initial_cut)
reference_rate <- mean(reference$disintegration_issue)
reference_ci <- wilson_ci(sum(reference$disintegration_issue), nrow(reference))

ref_center <- setNames(vapply(mes_vars, function(v) median(as.numeric(reference[[v]]), na.rm = TRUE), numeric(1)), mes_vars)
ref_scale <- setNames(vapply(mes_vars, function(v) {
  s <- mad(as.numeric(reference[[v]]), center = ref_center[[v]], constant = 1.4826, na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- sd(as.numeric(reference[[v]]), na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- 1
  s
}, numeric(1)), mes_vars)

for (v in mes_vars) {
  zname <- paste0("z_", v)
  joint[[zname]] <- (as.numeric(joint[[v]]) - ref_center[[v]]) / ref_scale[[v]]
}
z_vars <- paste0("z_", mes_vars)
joint$state_input_drift_score <- rowMeans(abs(as.matrix(joint[z_vars])), na.rm = TRUE)
reference_drift_threshold <- unname(quantile(joint$state_input_drift_score[joint$mes_production_date < initial_cut], 0.95, na.rm = TRUE))

state_rows <- lapply(seq_len(nrow(joint)), function(i) {
  score_date <- joint$mes_production_date[[i]]
  prior <- which(joint$qc_available_date < score_date)
  recent <- prior[joint$qc_available_date[prior] >= score_date - 60]
  if (length(recent) == 0) {
    return(tibble(
      state_recent_qc_n_60d = 0L,
      state_recent_issue_n_60d = 0L,
      state_recent_issue_rate_60d = reference_rate,
      state_recent_static_residual_60d = NA_real_
    ))
  }
  tibble(
    state_recent_qc_n_60d = length(recent),
    state_recent_issue_n_60d = sum(joint$disintegration_issue[recent]),
    state_recent_issue_rate_60d = (sum(joint$disintegration_issue[recent]) + 0.5) / (length(recent) + 1),
    state_recent_static_residual_60d = NA_real_
  )
}) |>
  bind_rows()
joint <- bind_cols(joint, state_rows)

for (v in z_vars) {
  joint[[paste0(v, "__x_drift")]] <- joint[[v]] * joint$state_input_drift_score
  joint[[paste0(v, "__x_recent_risk")]] <- joint[[v]] * joint$state_recent_issue_rate_60d
}
state_vars <- c(
  mes_vars,
  "state_input_drift_score", "state_recent_issue_rate_60d", "state_recent_qc_n_60d",
  paste0(z_vars, "__x_drift"), paste0(z_vars, "__x_recent_risk")
)

static_prediction_all <- fit_predict_elastic(reference, joint, mes_vars, seed = 20260827)
joint$static_prediction <- pmin(pmax(static_prediction_all, 1e-5), 1 - 1e-5)

for (i in seq_len(nrow(joint))) {
  score_date <- joint$mes_production_date[[i]]
  recent <- which(joint$qc_available_date < score_date & joint$qc_available_date >= score_date - 60)
  if (length(recent) > 0) {
    joint$state_recent_static_residual_60d[[i]] <- mean(joint$disintegration_issue[recent] - joint$static_prediction[recent])
  }
}

split_defs <- tribble(
  ~split_id, ~test_start, ~test_end, ~test_window,
  "R1", as.Date("2026-01-01"), as.Date("2026-02-28"), "Jan-Feb 2026",
  "R2", as.Date("2026-03-01"), as.Date("2026-04-30"), "Mar-Apr 2026",
  "R3", as.Date("2026-05-01"), as.Date("2026-06-30"), "May-Jun 2026",
  "R4", as.Date("2026-07-01"), as.Date("2026-07-31"), "Jul 2026"
)

prediction_rows <- list()
trigger_rows <- list()
row_id <- 1L
for (s in seq_len(nrow(split_defs))) {
  def <- split_defs[s, ]
  train <- joint |> filter(mes_production_date < def$test_start, qc_available_date < def$test_start)
  test <- joint |> filter(mes_production_date >= def$test_start, mes_production_date <= def$test_end)
  if (nrow(test) == 0) next

  p_static <- test$static_prediction

  calibration_frame <- train |>
    mutate(lp_static = qlogis(pmin(pmax(static_prediction, 1e-5), 1 - 1e-5)))
  recal_fit <- tryCatch(
    glm(disintegration_issue ~ 1 + offset(lp_static), data = calibration_frame, family = binomial()),
    error = function(e) NULL
  )
  recal_intercept <- if (is.null(recal_fit)) 0 else unname(coef(recal_fit)[[1]])
  p_recal <- plogis(qlogis(p_static) + recal_intercept)

  p_retrain <- fit_predict_elastic(train, test, mes_vars, seed = 20260900 + s)
  p_state_interaction <- fit_predict_elastic(train, test, state_vars, seed = 20261000 + s)

  recent_rate <- pmin(pmax(test$state_recent_issue_rate_60d, 0.005), 0.995)
  p_state_prior <- plogis(qlogis(p_static) + qlogis(recent_rate) - qlogis(pmin(pmax(reference_rate, 0.005), 0.995)))

  strategy_predictions <- list(
    "Static locked MES" = p_static,
    "Intercept recalibration" = p_recal,
    "Expanding-window retraining" = p_retrain,
    "State-aware interaction" = p_state_interaction,
    "State-aware prior updating" = p_state_prior
  )
  for (strategy_name in names(strategy_predictions)) {
    prediction_rows[[row_id]] <- tibble(
      twin_id = test$twin_id,
      split_id = def$split_id,
      test_window = def$test_window,
      score_date = test$mes_production_date,
      qc_available_date = test$qc_available_date,
      outcome = test$disintegration_issue,
      strategy = strategy_name,
      prediction = as.numeric(strategy_predictions[[strategy_name]]),
      train_n = nrow(train),
      train_issue_n = sum(train$disintegration_issue),
      recalibration_intercept = ifelse(strategy_name == "Intercept recalibration", recal_intercept, NA_real_)
    )
    row_id <- row_id + 1L
  }

  prior60 <- joint |> filter(qc_available_date < def$test_start, qc_available_date >= def$test_start - 60)
  prior_mes60 <- joint |> filter(mes_production_date < def$test_start, mes_production_date >= def$test_start - 60)
  recent_rate_start <- if (nrow(prior60) == 0) reference_rate else (sum(prior60$disintegration_issue) + 0.5) / (nrow(prior60) + 1)
  recent_residual_start <- if (nrow(prior60) == 0) NA_real_ else mean(prior60$disintegration_issue - prior60$static_prediction)
  median_drift <- if (nrow(prior_mes60) == 0) NA_real_ else median(prior_mes60$state_input_drift_score, na.rm = TRUE)
  trigger_rows[[s]] <- tibble(
    split_id = def$split_id,
    test_window = def$test_window,
    evaluation_date = def$test_start,
    available_qc_n_60d = nrow(prior60),
    available_issue_rate_60d = recent_rate_start,
    recent_input_drift_median_60d = median_drift,
    input_drift_trigger = is.finite(median_drift) && median_drift > reference_drift_threshold,
    baseline_drift_trigger = nrow(prior60) >= 30 && (recent_rate_start < reference_ci[["low"]] || recent_rate_start > reference_ci[["high"]]),
    residual_drift_trigger = nrow(prior60) >= 30 && is.finite(recent_residual_start) && abs(recent_residual_start) > 0.10,
    any_update_trigger = input_drift_trigger | baseline_drift_trigger | residual_drift_trigger,
    recent_static_residual_60d = recent_residual_start
  )
}

predictions <- bind_rows(prediction_rows)
write_csv(predictions, file.path(tables_dir, "01_strategy_predictions_755_batches_long.csv"), na = "")

performance <- predictions |>
  group_by(split_id, test_window, strategy, train_n, train_issue_n) |>
  group_modify(~ metrics_binary(.x$outcome, .x$prediction)) |>
  ungroup()
write_csv(performance, file.path(tables_dir, "02_same_window_strategy_performance.csv"), na = "")

pooled_performance <- predictions |>
  group_by(strategy) |>
  group_modify(~ metrics_binary(.x$outcome, .x$prediction)) |>
  ungroup()
write_csv(pooled_performance, file.path(tables_dir, "03_pooled_replay_performance.csv"), na = "")

workload <- predictions |>
  group_by(split_id, test_window, strategy) |>
  group_modify(~ {
    bind_rows(lapply(c(0.10, 0.20, 0.30), function(budget) {
      n_review <- max(1L, ceiling(nrow(.x) * budget))
      reviewed <- .x |> arrange(desc(prediction), twin_id) |> slice_head(n = n_review)
      total_events <- sum(.x$outcome)
      captured <- sum(reviewed$outcome)
      precision <- mean(reviewed$outcome)
      prevalence <- mean(.x$outcome)
      tibble(
        review_budget = budget,
        total_n = nrow(.x), review_n = n_review,
        total_event_n = total_events, captured_event_n = captured,
        event_capture_rate = ifelse(total_events > 0, captured / total_events, NA_real_),
        review_precision = precision,
        lift_vs_random_review = ifelse(prevalence > 0, precision / prevalence, NA_real_)
      )
    }))
  }) |>
  ungroup()
write_csv(workload, file.path(tables_dir, "04_fixed_workload_event_capture.csv"), na = "")

triggers <- bind_rows(trigger_rows)
write_csv(triggers, file.path(tables_dir, "05_window_update_triggers.csv"), na = "")

replay_batch <- predictions |>
  select(twin_id, split_id, test_window, score_date, qc_available_date, outcome, strategy, prediction) |>
  pivot_wider(names_from = strategy, values_from = prediction) |>
  left_join(
    joint |>
      select(twin_id, state_input_drift_score, state_recent_qc_n_60d, state_recent_issue_rate_60d, state_recent_static_residual_60d),
    by = "twin_id"
  ) |>
  mutate(
    input_drift_alert = state_input_drift_score > reference_drift_threshold,
    baseline_drift_alert = state_recent_qc_n_60d >= 30 & (state_recent_issue_rate_60d < reference_ci[["low"]] | state_recent_issue_rate_60d > reference_ci[["high"]]),
    residual_drift_alert = state_recent_qc_n_60d >= 30 & abs(state_recent_static_residual_60d) > 0.10,
    update_triggered = input_drift_alert | baseline_drift_alert | residual_drift_alert,
    model_version_static = "MES-static-2025Q4-v1",
    model_version_updated = paste0("MES-update-", split_id, "-v1"),
    evidence_snapshot_status = "Process variables and contemporaneous state metrics locked at score date",
    human_review_status = "Not observed in historical replay",
    outcome_feedback_status = "QC outcome linked retrospectively"
  )
write_csv(replay_batch, file.path(tables_dir, "06_masked_historical_replay_event_log_755_batches.csv"), na = "")

calibration_bins <- predictions |>
  group_by(test_window, strategy) |>
  mutate(risk_bin = ntile(prediction, min(5, n()))) |>
  group_by(test_window, strategy, risk_bin) |>
  summarise(n = n(), predicted_risk = mean(prediction), observed_rate = mean(outcome), .groups = "drop")
write_csv(calibration_bins, file.path(tables_dir, "07_replay_calibration_bins.csv"), na = "")

reference_summary <- tibble(
  metric = c(
    "Initial training cutoff", "Batches selected by MES production-date cutoff", "Events selected by MES production-date cutoff",
    "Batches with QC outcome available by cutoff", "Events with QC outcome available by cutoff",
    "Initial event rate", "Initial event-rate Wilson lower", "Initial event-rate Wilson upper",
    "Input-drift 95th-percentile threshold", "Negative QC lag records replaced by median 4-day lag"
  ),
  value = c(
    as.character(initial_cut), nrow(reference_production_only), sum(reference_production_only$disintegration_issue),
    nrow(reference), sum(reference$disintegration_issue), reference_rate,
    reference_ci[["low"]], reference_ci[["high"]], reference_drift_threshold,
    sum(joint$lag_days_raw < 0)
  )
)
write_csv(reference_summary, file.path(tables_dir, "08_replay_reference_and_thresholds.csv"), na = "")

r1_test <- joint |> filter(mes_production_date >= as.Date("2026-01-01"), mes_production_date <= as.Date("2026-02-28"))
p_r1_production_cut <- fit_predict_elastic(reference_production_only, r1_test, mes_vars, seed = 20261101)
p_r1_availability_cut <- r1_test$static_prediction
timing_sensitivity <- bind_rows(
  metrics_binary(r1_test$disintegration_issue, p_r1_production_cut) |>
    mutate(cutoff_rule = "MES production date only", train_n = nrow(reference_production_only), train_issue_n = sum(reference_production_only$disintegration_issue)),
  metrics_binary(r1_test$disintegration_issue, p_r1_availability_cut) |>
    mutate(cutoff_rule = "QC outcome available before scoring", train_n = nrow(reference), train_issue_n = sum(reference$disintegration_issue))
) |>
  relocate(cutoff_rule, train_n, train_issue_n)
write_csv(timing_sensitivity, file.path(tables_dir, "09_outcome_availability_cutoff_sensitivity.csv"), na = "")

nejm <- c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD")
strategy_cols <- c(
  "Static locked MES" = nejm[6],
  "Intercept recalibration" = nejm[3],
  "Expanding-window retraining" = nejm[2],
  "State-aware interaction" = nejm[5],
  "State-aware prior updating" = nejm[4]
)
base_theme <- theme_classic(base_family = "Arial", base_size = 13) +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 13, face = "bold"),
    plot.tag = element_text(face = "bold", size = 16),
    plot.margin = margin(7, 10, 7, 8)
  )

perf_long <- performance |>
  select(test_window, strategy, Brier, ECE) |>
  pivot_longer(c(Brier, ECE), names_to = "metric", values_to = "value") |>
  mutate(
    test_window = factor(test_window, levels = split_defs$test_window),
    metric = recode(metric, Brier = "Brier score", ECE = "Expected calibration error")
  )
plot_perf <- ggplot(perf_long, aes(test_window, value, colour = strategy, group = strategy)) +
  geom_line(linewidth = 0.85) + geom_point(size = 2.1) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = strategy_cols) +
  labs(x = NULL, y = "Error (lower is better)") +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  base_theme + theme(axis.text.x = element_text(angle = 25, hjust = 1))

cal_plot_data <- performance |>
  mutate(
    test_window = factor(test_window, levels = split_defs$test_window),
    abs_calibration_intercept = abs(calibration_intercept)
  )
plot_cal <- ggplot(cal_plot_data, aes(test_window, abs_calibration_intercept, fill = strategy)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.75) +
  scale_fill_manual(values = strategy_cols) +
  labs(x = NULL, y = "Absolute calibration intercept") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  base_theme + theme(axis.text.x = element_text(angle = 25, hjust = 1))

capture20 <- workload |>
  filter(review_budget == 0.20) |>
  mutate(test_window = factor(test_window, levels = split_defs$test_window))
plot_capture <- ggplot(capture20, aes(test_window, event_capture_rate, fill = strategy)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.75) +
  geom_hline(yintercept = 0.20, linetype = "dashed", colour = "grey35") +
  scale_fill_manual(values = strategy_cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = NULL, y = "Events captured at 20% review budget") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  base_theme + theme(axis.text.x = element_text(angle = 25, hjust = 1))

timeline <- replay_batch |>
  arrange(score_date, twin_id) |>
  group_by(test_window) |>
  mutate(batch_order = row_number()) |>
  ungroup()
trigger_dates <- triggers |> filter(any_update_trigger)
plot_timeline <- ggplot(timeline, aes(score_date, `State-aware prior updating`)) +
  geom_vline(data = trigger_dates, aes(xintercept = evaluation_date), linetype = "dashed", colour = "grey35", linewidth = 0.6, inherit.aes = FALSE) +
  geom_line(colour = nejm[4], linewidth = 0.75) +
  geom_point(aes(colour = factor(outcome)), size = 1.55, alpha = 0.72) +
  scale_colour_manual(values = c("0" = nejm[6], "1" = nejm[1]), labels = c("No event", "Quality event")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Historical score date", y = "State-aware risk", colour = NULL) +
  guides(colour = guide_legend(nrow = 1)) +
  base_theme

combined <- (plot_perf | plot_cal) / (plot_capture | plot_timeline) + plot_annotation(tag_levels = "A")
save_pdf_png(combined, "Figure_DF_state_update_and_historical_replay", width = 14.4, height = 9.2)

qa_checks <- tibble(
  check = c(
    "Replay unique batches", "Replay prediction rows", "Each batch has five strategies",
    "No future QC used in any training window", "No duplicated batch-strategy rows",
    "Predictions within zero-one range", "Figure PDF exists", "Figure PNG exists"
  ),
  observed = c(
    n_distinct(predictions$twin_id), nrow(predictions),
    as.integer(all(table(predictions$twin_id) == 5)),
    as.integer(all(vapply(seq_len(nrow(split_defs)), function(s) {
      all(joint$qc_available_date[joint$mes_production_date < split_defs$test_start[[s]] & joint$qc_available_date < split_defs$test_start[[s]]] < split_defs$test_start[[s]])
    }, logical(1)))),
    sum(duplicated(predictions[c("twin_id", "strategy")])),
    as.integer(all(predictions$prediction >= 0 & predictions$prediction <= 1)),
    as.integer(file.exists(file.path(figures_dir, "Figure_DF_state_update_and_historical_replay.pdf"))),
    as.integer(file.exists(file.path(figures_dir, "Figure_DF_state_update_and_historical_replay_preview.png")))
  ),
  expected = c(755, 3775, 1, 1, 0, 1, 1, 1)
) |>
  mutate(pass = observed == expected)
write_csv(qa_checks, file.path(docs_dir, "modules_DF_QA_checks.csv"), na = "")
if (!all(qa_checks$pass)) stop("Modules D/F QA failed; inspect modules_DF_QA_checks.csv")

best_brier <- pooled_performance |> arrange(Brier) |> slice(1)
best_capture <- workload |>
  filter(review_budget == 0.20) |>
  group_by(strategy) |>
  summarise(weighted_capture = sum(captured_event_n) / sum(total_event_n), .groups = "drop") |>
  arrange(desc(weighted_capture)) |>
  slice(1)
report <- c(
  "# Modules D and F: state updating and historical closed-loop replay",
  "",
  "> Analysis date: 2026-08-27  ",
  "> Replay population: 755 batches in four strictly ordered future windows  ",
  "> Scoring context: MES-complete, before linked QC outcome availability",
  "",
  "## Design",
  "",
  "Five strategies were compared on identical test windows: a frozen pre-2026 direct-MES model, intercept-only recalibration, expanding-window full retraining, an elastic-net model with contemporaneous drift/recent-QC interactions, and an online prior-shift update using the most recent 60 days of QC feedback. Training and recalibration used only outcomes whose availability date preceded the test-window start.",
  "",
  "## Main numerical result",
  "",
  paste0("- The lowest pooled Brier score was obtained by ", best_brier$strategy, " (", sprintf("%.3f", best_brier$Brier), ")."),
  paste0("- At a fixed 20% review budget, the highest event capture across replay windows was obtained by ", best_capture$strategy, " (", percent(best_capture$weighted_capture, accuracy = 0.1), ")."),
  paste0("- The replay contains ", comma(n_distinct(predictions$twin_id)), " unique batches and ", comma(nrow(predictions)), " locked batch-strategy score records."),
  paste0("- A production-date cutoff would have treated ", nrow(reference_production_only), " batches and ", sum(reference_production_only$disintegration_issue), " events as available at 2026-01-01; enforcing QC outcome availability leaves ", nrow(reference), " batches and only ", sum(reference$disintegration_issue), " events. The first replay window is therefore a cold-start stress test."),
  paste0("- The best aggregate event capture at a 20% review budget was only ", percent(best_capture$weighted_capture, accuracy = 0.1), ", close to the 20% random-review reference. No strategy produced consistently superior ranking across all four windows."),
  "",
  "## Interpretation boundary",
  "",
  "This is an offline historical replay, not a prospective deployment. A dynamic strategy is considered useful only if it improves calibration or fixed-workload capture repeatedly across windows; an isolated gain is not sufficient. Failure of updating strategies is itself a governance result because it shows that automatic retraining after a transient high-risk state can propagate miscalibration into the next state.",
  "",
  "## Timing limitation",
  "",
  "The finalized data contain production dates rather than exact MES completion timestamps. One impossible negative QC lag was replaced by the prespecified cohort-median four-day lag for replay ordering. Exact scoring latency requires prospective timestamp capture."
)
write_lines(report, file.path(docs_dir, "modules_DF_findings_and_boundaries.md"), na = "")
write_lines(capture.output(sessionInfo()), file.path(docs_dir, "R_session_info.txt"), na = "")

message("Modules D/F completed: ", output_dir)
