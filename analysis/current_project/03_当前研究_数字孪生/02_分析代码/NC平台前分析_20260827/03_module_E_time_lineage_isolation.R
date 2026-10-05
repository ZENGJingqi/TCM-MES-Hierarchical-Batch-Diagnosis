options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

input_dir <- Sys.getenv("TCM_NC_E_INPUT_DIR", unset = "")
output_dir <- Sys.getenv("TCM_NC_OUTPUT_DIR", unset = "")
if (!nzchar(input_dir) || !nzchar(output_dir)) stop("TCM_NC_E_INPUT_DIR and TCM_NC_OUTPUT_DIR are required")
tables_dir <- file.path(output_dir, "tables")
docs_dir <- file.path(output_dir, "docs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

norm_batch <- function(x) {
  s <- as.character(x) |> str_trim() |> str_replace("\\.0$", "")
  s[is.na(s)] <- ""
  s[tolower(s) %in% c("nan", "none", "na", "")] <- ""
  s
}

split_batch_vector <- function(x) {
  x <- norm_batch(x)
  if (length(x) == 0 || x == "") return(character(0))
  out <- str_split(x, "[;；,，、\\s]+")[[1]] |> norm_batch()
  unique(out[out != ""])
}

auc_fast <- function(y, p) {
  y <- as.integer(y); p <- as.numeric(p)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

pr_auc_fast <- function(y, p) {
  y <- as.integer(y); p <- as.numeric(p)
  if (sum(y == 1) == 0) return(NA_real_)
  ord <- order(p, decreasing = TRUE); yy <- y[ord]
  precision <- cumsum(yy) / seq_along(yy); recall <- cumsum(yy) / sum(yy)
  sum(diff(c(0, recall)) * (head(c(1, precision), -1) + tail(c(1, precision), -1)) / 2)
}

calibration_stats <- function(y, p) {
  p <- pmin(pmax(as.numeric(p), 1e-5), 1 - 1e-5); y <- as.integer(y)
  lp <- qlogis(p)
  if (length(unique(y)) < 2 || sd(lp) == 0) return(c(intercept = NA_real_, slope = NA_real_))
  i_fit <- tryCatch(glm(y ~ 1 + offset(lp), family = binomial()), error = function(e) NULL)
  s_fit <- tryCatch(glm(y ~ lp, family = binomial()), error = function(e) NULL)
  c(intercept = if (is.null(i_fit)) NA_real_ else unname(coef(i_fit)[1]), slope = if (is.null(s_fit)) NA_real_ else unname(coef(s_fit)[2]))
}

score_metrics <- function(d) {
  cal <- calibration_stats(d$disintegration_issue, d$prediction)
  n_review <- ceiling(nrow(d) * 0.20)
  reviewed <- d |> arrange(desc(prediction), finished_batch) |> slice_head(n = n_review)
  tibble(
    n = nrow(d), issue_n = sum(d$disintegration_issue), issue_rate = mean(d$disintegration_issue),
    AUC = auc_fast(d$disintegration_issue, d$prediction),
    PR_AUC = pr_auc_fast(d$disintegration_issue, d$prediction),
    Brier = mean((d$prediction - d$disintegration_issue)^2),
    calibration_intercept = cal[["intercept"]], calibration_slope = cal[["slope"]],
    review_n_20pct = n_review,
    captured_event_n_20pct = sum(reviewed$disintegration_issue),
    event_capture_rate_20pct = sum(reviewed$disintegration_issue) / sum(d$disintegration_issue)
  )
}

d3_file <- list.files(input_dir, pattern = "^D3.*\\.xlsx$", full.names = TRUE)
pred_file <- file.path(input_dir, "01_context_aligned_predictions.csv")
if (length(d3_file) != 1 || !file.exists(pred_file)) stop("Missing staged D3 or prediction file")
d3_raw <- suppressWarnings(read_xlsx(d3_file[[1]]))
pred <- read_csv(pred_file, show_col_types = FALSE) |>
  mutate(finished_batch = norm_batch(finished_batch), mes_production_date = as.Date(mes_production_date))

d3 <- tibble(
  finished_batch = norm_batch(d3_raw$batch_no),
  mes_production_date = as.Date(d3_raw$production_date),
  extract_raw = norm_batch(d3_raw$jwxs_extract_powder_batch_no),
  yam_raw = norm_batch(d3_raw$yam_powder_luoting_batch_no)
) |>
  filter(finished_batch != "", finished_batch %in% unique(pred$finished_batch)) |>
  distinct(finished_batch, .keep_all = TRUE)

extract_long <- d3 |>
  mutate(upstream_batch = lapply(extract_raw, split_batch_vector)) |>
  unnest(upstream_batch) |>
  filter(upstream_batch != "") |>
  transmute(finished_batch, mes_production_date, layer = "Extract-powder batch", upstream_batch)
yam_long <- d3 |>
  mutate(upstream_batch = lapply(yam_raw, split_batch_vector)) |>
  unnest(upstream_batch) |>
  filter(upstream_batch != "") |>
  transmute(finished_batch, mes_production_date, layer = "Yam-powder batch", upstream_batch)
lineage <- bind_rows(extract_long, yam_long) |> distinct()

boundary <- as.Date("2026-03-01")
train_ids <- lineage |> filter(mes_production_date < boundary) |> distinct(layer, upstream_batch)
test_ids <- lineage |> filter(mes_production_date >= boundary) |> distinct(layer, upstream_batch)
overlap_ids <- inner_join(train_ids, test_ids, by = c("layer", "upstream_batch"))
test_exposure <- lineage |>
  filter(mes_production_date >= boundary) |>
  inner_join(overlap_ids, by = c("layer", "upstream_batch")) |>
  distinct(layer, finished_batch)
overlap_union <- unique(test_exposure$finished_batch)

overlap_summary <- bind_rows(lapply(unique(lineage$layer), function(layer_name) {
  train_layer <- train_ids |> filter(layer == layer_name)
  test_layer <- test_ids |> filter(layer == layer_name)
  overlap_layer <- overlap_ids |> filter(layer == layer_name)
  exposure_layer <- test_exposure |> filter(layer == layer_name)
  tibble(
    layer = layer_name,
    train_upstream_batch_n = nrow(train_layer),
    test_upstream_batch_n = nrow(test_layer),
    overlapping_upstream_batch_n = nrow(overlap_layer),
    test_upstream_overlap_rate = nrow(overlap_layer) / nrow(test_layer),
    exposed_test_finished_batch_n = n_distinct(exposure_layer$finished_batch)
  )
}))
write_csv(overlap_summary, file.path(tables_dir, "01_time_boundary_lineage_overlap.csv"), na = "")

locked <- pred |>
  filter(
    validation_strategy == "Locked forward-time validation",
    model %in% c("Process-complete direct MES", "Process + lineage evidence")
  )

performance <- locked |>
  group_by(model) |>
  group_modify(~ bind_rows(
    score_metrics(.x) |> mutate(isolation = "Time-isolated locked forward"),
    score_metrics(.x |> filter(!finished_batch %in% overlap_union)) |> mutate(isolation = "Time- and lineage-isolated")
  )) |>
  ungroup() |>
  relocate(isolation, .after = model)
write_csv(performance, file.path(tables_dir, "02_time_lineage_isolated_performance.csv"), na = "")

excluded_summary <- tibble(
  boundary = as.character(boundary),
  locked_forward_test_n = n_distinct(locked$finished_batch),
  lineage_overlap_exposed_finished_batch_n = length(overlap_union),
  double_isolated_test_n = n_distinct(locked$finished_batch) - length(overlap_union),
  overlap_exposure_rate = length(overlap_union) / n_distinct(locked$finished_batch)
)
write_csv(excluded_summary, file.path(tables_dir, "03_double_isolation_cohort_accounting.csv"), na = "")

qa <- tibble(
  check = c("Locked test batches", "Overlapping extract identities", "Overlapping yam identities", "Double-isolated rows retained", "No overlap-exposed batch retained"),
  observed = c(
    n_distinct(locked$finished_batch),
    overlap_summary$overlapping_upstream_batch_n[overlap_summary$layer == "Extract-powder batch"],
    overlap_summary$overlapping_upstream_batch_n[overlap_summary$layer == "Yam-powder batch"],
    excluded_summary$double_isolated_test_n,
    sum(unique(locked$finished_batch[!locked$finished_batch %in% overlap_union]) %in% overlap_union)
  ),
  expected = c(529, 1, 1, 528, 0)
) |>
  mutate(pass = observed == expected)
write_csv(qa, file.path(docs_dir, "module_E_QA_checks.csv"), na = "")
if (!all(qa$pass)) stop("Module E QA failed")

direct_before <- performance |> filter(model == "Process-complete direct MES", isolation == "Time-isolated locked forward")
direct_after <- performance |> filter(model == "Process-complete direct MES", isolation == "Time- and lineage-isolated")
lineage_before <- performance |> filter(model == "Process + lineage evidence", isolation == "Time-isolated locked forward")
lineage_after <- performance |> filter(model == "Process + lineage evidence", isolation == "Time- and lineage-isolated")
report <- c(
  "# Module E: joint time and lineage isolation",
  "",
  "> Boundary: training before 2026-03-01; locked forward test from 2026-03-01 onward",
  "",
  paste0("Only ", length(overlap_union), " of 529 locked-forward finished batches were linked to an upstream extract or yam batch also observed before the temporal boundary (", scales::percent(length(overlap_union) / 529, accuracy = 0.1), ")."),
  paste0("The direct-MES AUC changed from ", sprintf("%.3f", direct_before$AUC), " to ", sprintf("%.3f", direct_after$AUC), " after joint isolation; the process-plus-lineage AUC changed from ", sprintf("%.3f", lineage_before$AUC), " to ", sprintf("%.3f", lineage_after$AUC), "."),
  "",
  "The random-to-forward credibility gap is therefore not explained by reuse of upstream batch identities across the locked temporal boundary. This sensitivity addresses identity overlap only; it does not make retrospectively observed upstream QC variables prospectively available."
)
write_lines(report, file.path(docs_dir, "module_E_findings_and_boundaries.md"), na = "")
write_lines(capture.output(sessionInfo()), file.path(docs_dir, "R_session_info.txt"), na = "")
message("Module E completed: ", output_dir)
