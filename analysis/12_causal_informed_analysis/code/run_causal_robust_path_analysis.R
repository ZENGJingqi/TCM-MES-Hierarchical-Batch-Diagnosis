options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(officer)
  library(openxlsx)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

set.seed(20260427)

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
analysis_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
figures_dir <- file.path(analysis_dir, "figures")
tables_dir <- file.path(analysis_dir, "tables")
docs_dir <- file.path(analysis_dir, "docs")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

red <- "#A83B2B"
blue <- "#0A5C7A"
grey <- "#8C8C8C"

theme_set(theme_bw(base_family = "Arial", base_size = 18))
theme_update(
  plot.title = element_blank(),
  axis.title = element_text(size = 22, colour = "black"),
  axis.text = element_text(size = 18, colour = "black"),
  strip.text = element_text(size = 18, colour = "black", face = "bold"),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", linewidth = 0.8),
  plot.margin = margin(12, 18, 12, 18)
)

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330)
}

num_clean <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, "%", "")
  x <- str_replace_all(x, "％", "")
  x <- str_replace_all(x, ",", "")
  x <- str_extract(x, "-?\\d+\\.?\\d*")
  as.numeric(x)
}

std_vec <- function(x) {
  x <- num_clean(x)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 2.2e-16, "<2.2e-16", sprintf("%.3g", p)))
}

auc_rank <- function(y, pred) {
  ok <- is.finite(pred) & !is.na(y)
  y <- as.integer(y[ok])
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  r <- rank(pred, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

pr_auc_step <- function(y, pred) {
  ok <- is.finite(pred) & !is.na(y)
  y <- as.integer(y[ok])
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  ord <- order(pred, decreasing = TRUE)
  y <- y[ord]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  recall <- tp / sum(y == 1)
  precision <- tp / pmax(tp + fp, 1)
  recall <- c(0, recall)
  precision <- c(1, precision)
  sum(diff(recall) * precision[-1])
}

safe_glm <- function(formula, data, family = binomial()) {
  tryCatch(
    suppressWarnings(glm(formula, data = data, family = family)),
    error = function(e) NULL
  )
}

tidy_logit <- function(fit, model_name, layer = NA_character_) {
  if (is.null(fit)) return(tibble())
  sm <- summary(fit)$coefficients
  tibble(
    layer = layer,
    model = model_name,
    term = rownames(sm),
    estimate = sm[, "Estimate"],
    std_error = sm[, "Std. Error"],
    p_value = sm[, "Pr(>|z|)"],
    odds_ratio = exp(estimate),
    ci_low = exp(estimate - 1.96 * std_error),
    ci_high = exp(estimate + 1.96 * std_error),
    p_display = format_p(p_value)
  ) |>
    filter(term != "(Intercept)")
}

eval_binary_model <- function(fit, data, model_name) {
  if (is.null(fit)) {
    return(tibble(model = model_name, n = 0, issue_n = NA_integer_, AUC = NA_real_, PR_AUC = NA_real_, Brier = NA_real_))
  }
  pred <- as.numeric(predict(fit, newdata = data, type = "response"))
  tibble(
    model = model_name,
    n = nrow(data),
    issue_n = sum(data$disintegration_issue, na.rm = TRUE),
    AUC = auc_rank(data$disintegration_issue, pred),
    PR_AUC = pr_auc_step(data$disintegration_issue, pred),
    Brier = mean((data$disintegration_issue - pred)^2)
  )
}

bootstrap_batch_glm <- function(data, formula, terms, label_map, layer, n_boot = 1000) {
  full_fit <- safe_glm(formula, data)
  full_terms <- tidy_logit(full_fit, "Full-sample batch model", layer) |>
    filter(term %in% terms)

  boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = length(terms))
  colnames(boot_mat) <- terms
  n <- nrow(data)
  for (i in seq_len(n_boot)) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    boot_data <- data[idx, , drop = FALSE]
    fit <- safe_glm(formula, boot_data)
    if (is.null(fit)) next
    co <- coef(fit)
    matched <- intersect(terms, names(co))
    boot_mat[i, matched] <- co[matched]
  }

  boot_summary <- tibble(term = terms) |>
    rowwise() |>
    mutate(
      boot_valid_n = sum(is.finite(boot_mat[, term])),
      boot_estimate = median(boot_mat[, term], na.rm = TRUE),
      boot_ci_low = quantile(boot_mat[, term], 0.025, na.rm = TRUE, names = FALSE),
      boot_ci_high = quantile(boot_mat[, term], 0.975, na.rm = TRUE, names = FALSE)
    ) |>
    ungroup() |>
    mutate(
      layer = layer,
      boot_or = exp(boot_estimate),
      boot_or_low = exp(boot_ci_low),
      boot_or_high = exp(boot_ci_high)
    )

  full_terms |>
    select(layer, model, term, estimate, std_error, p_value, odds_ratio, ci_low, ci_high, p_display) |>
    right_join(boot_summary, by = c("layer", "term")) |>
    left_join(label_map, by = "term") |>
    mutate(label = if_else(is.na(label), term, label), n_boot = n_boot)
}

result_path <- file.path(tables_dir, "因果批次分解_第一版结果.xlsx")
if (!file.exists(result_path)) {
  stop("Run run_causal_batch_decomposition.R before this robust path analysis.")
}

causal_finished_matrix <- read_xlsx(result_path, sheet = "causal_finished_matrix")
extract_batch_summary <- read_xlsx(result_path, sheet = "extract_batch_summary")
chenpi_batch_summary <- read_xlsx(result_path, sheet = "chenpi_batch_summary")
yam_batch_summary <- read_xlsx(result_path, sheet = "yam_batch_summary")

label_map <- tibble::tribble(
  ~term, ~label,
  "abnormal_window_10pct", "Abnormal-window exposure, per 10%",
  "extract_moisture_z", "Extract-powder moisture, per SD",
  "extract_total_ash_z", "Extract-powder total ash, per SD",
  "extract_extract_z", "Extract-powder extract, per SD",
  "extract_hesperidin_z", "Extract-powder hesperidin, per SD",
  "chenpi_origin_zhejiang", "Chenpi source includes Zhejiang",
  "chenpi_moisture_z", "Chenpi moisture, per SD",
  "chenpi_hesperidin_z", "Chenpi hesperidin, per SD",
  "chenpi_impurities_z", "Chenpi impurities, per SD",
  "yam_rejected_rate_z", "Yam rejected-material rate, per SD",
  "yam_process_moisture_z", "Yam process moisture, per SD",
  "yam_120_mesh_z", "Yam through 120-mesh, per SD",
  "yam_yield_z", "Yam yield, per SD",
  "yam_mass_balance_z", "Yam mass balance, per SD",
  "any_zhejiang_chenpi", "Finished batch includes Zhejiang Chenpi",
  "coating_yield_z", "Coating yield, per SD",
  "final_blend_moisture_z", "Final-blend moisture, per SD",
  "coating_mass_balance_z", "Coating mass balance, per SD"
)

extract_model <- extract_batch_summary |>
  mutate(
    nonissue_n = finished_n - issue_n,
    abnormal_window_10pct = abnormal_window_frac * 10,
    extract_moisture_z = std_vec(extract_moisture_pct),
    extract_total_ash_z = std_vec(extract_total_ash_pct),
    extract_extract_z = std_vec(extract_extract_pct),
    extract_hesperidin_z = std_vec(extract_hesperidin_mg_g)
  )

chenpi_model <- chenpi_batch_summary |>
  mutate(
    nonissue_n = finished_n - issue_n,
    abnormal_window_10pct = abnormal_window_frac * 10,
    chenpi_origin_zhejiang = as.integer(chenpi_origin == "浙江"),
    chenpi_moisture_z = std_vec(chenpi_moisture_pct),
    chenpi_hesperidin_z = std_vec(chenpi_hesperidin_pct),
    chenpi_impurities_z = std_vec(chenpi_impurities_pct)
  )

yam_model <- yam_batch_summary |>
  mutate(
    nonissue_n = finished_n - issue_n,
    abnormal_window_10pct = abnormal_window_frac * 10,
    yam_rejected_rate_z = std_vec(yam_rejected_material_rate_pct),
    yam_process_moisture_z = std_vec(yam_process_moisture_pct),
    yam_120_mesh_z = std_vec(yam_through_120_mesh_pct),
    yam_yield_z = std_vec(yam_yield_pct),
    yam_mass_balance_z = std_vec(yam_mass_balance_pct)
  )

bootstrap_effects <- bind_rows(
  bootstrap_batch_glm(
    extract_model,
    cbind(issue_n, nonissue_n) ~ abnormal_window_10pct + extract_moisture_z + extract_total_ash_z + extract_extract_z + extract_hesperidin_z,
    c("abnormal_window_10pct", "extract_moisture_z", "extract_total_ash_z", "extract_extract_z", "extract_hesperidin_z"),
    label_map,
    "Extract-powder batch"
  ),
  bootstrap_batch_glm(
    chenpi_model,
    cbind(issue_n, nonissue_n) ~ abnormal_window_10pct + chenpi_origin_zhejiang + chenpi_moisture_z + chenpi_hesperidin_z + chenpi_impurities_z,
    c("abnormal_window_10pct", "chenpi_origin_zhejiang", "chenpi_moisture_z", "chenpi_hesperidin_z", "chenpi_impurities_z"),
    label_map,
    "Chenpi batch"
  ),
  bootstrap_batch_glm(
    yam_model,
    cbind(issue_n, nonissue_n) ~ abnormal_window_10pct + yam_rejected_rate_z + yam_process_moisture_z + yam_120_mesh_z + yam_yield_z + yam_mass_balance_z,
    c("abnormal_window_10pct", "yam_rejected_rate_z", "yam_process_moisture_z", "yam_120_mesh_z", "yam_yield_z", "yam_mass_balance_z"),
    label_map,
    "Chinese yam powder batch"
  )
) |>
  mutate(
    robust_direction = case_when(
      is.na(boot_or_low) | is.na(boot_or_high) ~ "Unstable",
      boot_or_low > 1 ~ "Positive",
      boot_or_high < 1 ~ "Negative",
      TRUE ~ "Not robust"
    )
  )

path_data <- causal_finished_matrix |>
  filter(!is.na(chenpi_origin_pattern)) |>
  mutate(
    disintegration_issue = as.integer(disintegration_issue),
    abnormal_window = as.integer(abnormal_window),
    any_zhejiang_chenpi = as.integer(any_zhejiang_chenpi),
    chenpi_moisture_z = std_vec(chenpi_moisture_pct_mean),
    chenpi_hesperidin_z = std_vec(chenpi_hesperidin_pct_mean),
    chenpi_impurities_z = std_vec(chenpi_impurities_pct_mean),
    extract_total_ash_z = std_vec(extract_total_ash_pct),
    extract_extract_z = std_vec(extract_extract_pct),
    extract_hesperidin_z = std_vec(extract_hesperidin_mg_g),
    yam_rejected_rate_z = std_vec(yam_rejected_material_rate_pct),
    yam_120_mesh_z = std_vec(yam_through_120_mesh_pct),
    yam_yield_z = std_vec(yam_yield_pct),
    coating_yield_z = std_vec(coating_yield_pct),
    final_blend_moisture_z = std_vec(final_blend_moisture_pct),
    coating_mass_balance_z = std_vec(coating_mass_balance_pct)
  )

path_vars <- c(
  "disintegration_issue", "abnormal_window", "any_zhejiang_chenpi",
  "chenpi_moisture_z", "chenpi_hesperidin_z", "chenpi_impurities_z",
  "extract_total_ash_z", "extract_extract_z", "extract_hesperidin_z",
  "yam_rejected_rate_z", "yam_120_mesh_z", "yam_yield_z",
  "coating_yield_z", "final_blend_moisture_z", "coating_mass_balance_z"
)

path_common <- path_data |>
  select(all_of(path_vars)) |>
  drop_na()

path_formulas <- list(
  "Time + Chenpi source" = disintegration_issue ~ abnormal_window + any_zhejiang_chenpi,
  "Add Chenpi quality" = disintegration_issue ~ abnormal_window + any_zhejiang_chenpi + chenpi_moisture_z + chenpi_hesperidin_z + chenpi_impurities_z,
  "Add extract-powder quality" = disintegration_issue ~ abnormal_window + any_zhejiang_chenpi + chenpi_moisture_z + chenpi_hesperidin_z + chenpi_impurities_z + extract_total_ash_z + extract_extract_z + extract_hesperidin_z,
  "Add yam-powder quality" = disintegration_issue ~ abnormal_window + any_zhejiang_chenpi + chenpi_moisture_z + chenpi_hesperidin_z + chenpi_impurities_z + extract_total_ash_z + extract_extract_z + extract_hesperidin_z + yam_rejected_rate_z + yam_120_mesh_z + yam_yield_z,
  "Add finished-product MES" = disintegration_issue ~ abnormal_window + any_zhejiang_chenpi + chenpi_moisture_z + chenpi_hesperidin_z + chenpi_impurities_z + extract_total_ash_z + extract_extract_z + extract_hesperidin_z + yam_rejected_rate_z + yam_120_mesh_z + yam_yield_z + coating_yield_z + final_blend_moisture_z + coating_mass_balance_z
)

path_fits <- lapply(names(path_formulas), function(nm) {
  fit <- safe_glm(path_formulas[[nm]], path_common)
  list(
    name = nm,
    fit = fit,
    effects = tidy_logit(fit, nm, "Finished-product path model"),
    performance = eval_binary_model(fit, path_common, nm)
  )
})

path_effects <- bind_rows(lapply(path_fits, `[[`, "effects")) |>
  left_join(label_map, by = "term") |>
  mutate(label = if_else(is.na(label), term, label))

path_performance <- bind_rows(lapply(path_fits, `[[`, "performance")) |>
  mutate(across(c(AUC, PR_AUC, Brier), ~round(.x, 3)))

source_path <- path_effects |>
  filter(term == "any_zhejiang_chenpi") |>
  arrange(factor(model, levels = names(path_formulas))) |>
  mutate(
    beta_reference = first(estimate),
    attenuation_pct = if_else(
      row_number() == 1,
      0,
      100 * (beta_reference - estimate) / abs(beta_reference)
    ),
    odds_ratio = exp(estimate),
    ci_low = exp(estimate - 1.96 * std_error),
    ci_high = exp(estimate + 1.96 * std_error)
  ) |>
  select(model, label, estimate, odds_ratio, ci_low, ci_high, p_value, p_display, attenuation_pct)

plot_or_upper <- 100
bootstrap_plot_df <- bootstrap_effects |>
  filter(term == "abnormal_window_10pct" | robust_direction != "Not robust") |>
  mutate(
    label = str_wrap(label, 28),
    layer = factor(layer, levels = c("Extract-powder batch", "Chenpi batch", "Chinese yam powder batch")),
    boot_or_plot = pmin(boot_or, plot_or_upper),
    boot_or_low_plot = pmax(boot_or_low, 0.01),
    boot_or_high_plot = pmin(boot_or_high, plot_or_upper),
    ci_truncated = is.finite(boot_or_high) & boot_or_high > plot_or_upper,
    ci_label = if_else(
      ci_truncated,
      paste0("95% CI >", plot_or_upper),
      sprintf("OR %.2f (%.2f-%.2f)", boot_or, boot_or_low, boot_or_high)
    )
  )

bootstrap_plot <- ggplot(bootstrap_plot_df, aes(x = boot_or_plot, y = reorder(label, boot_or_plot), colour = robust_direction)) +
  geom_vline(xintercept = 1, linewidth = 0.8, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(xmin = boot_or_low_plot, xmax = boot_or_high_plot), orientation = "y", width = 0.20, linewidth = 0.9) +
  geom_segment(
    data = bootstrap_plot_df |> filter(ci_truncated),
    aes(yend = reorder(label, boot_or_plot)),
    x = plot_or_upper / 1.35,
    xend = plot_or_upper,
    arrow = arrow(length = unit(0.16, "cm")),
    linewidth = 0.9,
    inherit.aes = TRUE
  ) +
  geom_point(size = 3.2) +
  geom_label(
    data = bootstrap_plot_df |> mutate(y_text = reorder(label, boot_or_plot)),
    mapping = aes(y = y_text, label = ci_label),
    x = 0.82,
    hjust = 1,
    size = 3.3,
    colour = "black",
    fill = "white",
    linewidth = 0.18,
    label.padding = unit(0.12, "lines"),
    family = "Arial",
    inherit.aes = FALSE
  ) +
  facet_wrap(~layer, scales = "free_y", ncol = 1) +
  scale_x_log10(limits = c(0.01, plot_or_upper), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) +
  scale_colour_manual(values = c("Positive" = red, "Negative" = blue, "Not robust" = grey, "Unstable" = grey), guide = "none") +
  labs(x = "Bootstrap odds ratio (plot truncated at 100)", y = NULL) +
  theme(axis.text.y = element_text(size = 15))
save_plot_dual(bootstrap_plot, "04_bootstrap_batch_level_effects", 10.8, 9.8)

source_path_plot <- source_path |>
  mutate(
    model = factor(model, levels = rev(names(path_formulas))),
    ci_low_plot = pmax(ci_low, 0.01),
    ci_high_plot = pmin(ci_high, 20),
    ci_truncated = ci_high > 20 | ci_low < 0.01,
    stat_label = sprintf("OR %.2f, P=%s", odds_ratio, p_display)
  )

attenuation_plot <- ggplot(source_path_plot, aes(x = odds_ratio, y = model)) +
  geom_vline(xintercept = 1, linewidth = 0.8, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(xmin = ci_low_plot, xmax = ci_high_plot), orientation = "y", width = 0.20, linewidth = 0.9, colour = blue) +
  geom_point(size = 3.0, colour = blue) +
  geom_label(
    data = source_path_plot,
    mapping = aes(y = model, label = stat_label),
    x = 18,
    hjust = 1,
    size = 4.0,
    colour = "black",
    fill = "white",
    linewidth = 0.18,
    label.padding = unit(0.12, "lines"),
    family = "Arial",
    inherit.aes = FALSE
  ) +
  scale_x_log10(limits = c(0.01, 20), breaks = c(0.01, 0.1, 1, 10), labels = c("0.01", "0.1", "1", "10")) +
  labs(x = "Adjusted odds ratio of Chenpi-source term", y = NULL) +
  theme(axis.text.y = element_text(size = 16))
save_plot_dual(attenuation_plot, "05_chenpi_source_path_attenuation", 11.5, 5.8)

path_perf_long <- path_performance |>
  select(model, AUC, PR_AUC, Brier) |>
  pivot_longer(cols = c(AUC, PR_AUC, Brier), names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(metric, levels = c("AUC", "PR_AUC", "Brier"), labels = c("AUC", "PR-AUC", "Brier score")),
    model = factor(str_wrap(model, 30), levels = str_wrap(path_performance$model, 30))
  )

path_perf_plot <- ggplot(path_perf_long, aes(x = value, y = model, fill = metric)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.3f", value)), hjust = -0.12, size = 4.0, family = "Arial") +
  facet_wrap(~metric, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("AUC" = red, "PR-AUC" = blue, "Brier score" = grey), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Model performance", y = NULL) +
  theme(axis.text.y = element_text(size = 15))
save_plot_dual(path_perf_plot, "06_path_attenuation_model_performance", 10.5, 9.2)

write.xlsx(
  list(
    bootstrap_batch_effects = bootstrap_effects,
    path_common_sample_summary = tibble(
      n = nrow(path_common),
      issue_n = sum(path_common$disintegration_issue),
      abnormal_window_n = sum(path_common$abnormal_window),
      zhejiang_exposed_n = sum(path_common$any_zhejiang_chenpi)
    ),
    path_effects = path_effects,
    chenpi_source_path_attenuation = source_path,
    path_model_performance = path_performance
  ),
  file.path(tables_dir, "因果稳健性与路径衰减分析.xlsx"),
  overwrite = TRUE
)

md_lines <- c(
  "# 因果稳健性与路径衰减分析",
  "",
  "## 分析目的",
  "",
  "本分析在第一版因果批次分解基础上，进一步补充批次级 bootstrap 稳健性和路径衰减分析。目标不是宣称随机因果，而是评估时间窗口、上游来源/质量、中间体质量和成品 MES 在崩解异常中的相对路径贡献。",
  "",
  "## 核心方法",
  "",
  "- 批次层面：以上游批次为单位，使用 grouped-binomial logistic model，并通过上游批次 bootstrap 估计 OR 的经验置信区间。",
  "- 路径层面：在共同完整样本中逐层加入 Chenpi source、Chenpi quality、extract-powder quality、yam-powder quality 和 finished-product MES，观察 Chenpi source 系数是否衰减。",
  "- 所有路径模型均控制 abnormal temporal window，以降低时间混杂。",
  "",
  "## 主要结果解释",
  "",
  "- Abnormal-window exposure 在各上游层级中均为最稳健信号，说明时间窗口仍是必须控制的核心因素。",
  "- Chenpi source 的路径效应需要谨慎解释；若加入质量和 MES 后系数衰减，说明其更可能通过质量/过程路径体现，而非独立来源效应。",
  "- 山药粉当前没有产地字段，本分析只支持 supplier/source batch 和过程质量层面的因果启发解释。",
  "",
  "## 输出图表",
  "",
  "- `04_bootstrap_batch_level_effects`: 批次层 bootstrap OR。",
  "- `05_chenpi_source_path_attenuation`: Chenpi source 逐层调整后的 OR 变化。",
  "- `06_path_attenuation_model_performance`: 路径模型性能变化。"
)
writeLines(md_lines, file.path(docs_dir, "因果稳健性与路径衰减分析说明.md"), useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "因果稳健性与路径衰减分析", style = "heading 1")
doc <- body_add_par(doc, "本分析用于补充批次级 bootstrap 稳健性和路径衰减分析，所有模型均将异常时间窗作为核心时间混杂因素控制。", style = "Normal")
doc <- body_add_par(doc, "共同完整样本", style = "heading 2")
doc <- body_add_flextable(doc, flextable(tibble(n = nrow(path_common), issue_n = sum(path_common$disintegration_issue), abnormal_window_n = sum(path_common$abnormal_window), zhejiang_exposed_n = sum(path_common$any_zhejiang_chenpi))) |> autofit())
doc <- body_add_par(doc, "批次层 Bootstrap 效应", style = "heading 2")
doc <- body_add_flextable(doc, flextable(bootstrap_effects |> select(layer, label, boot_or, boot_or_low, boot_or_high, robust_direction, boot_valid_n)) |> fontsize(size = 7, part = "all") |> autofit())
doc <- body_add_par(doc, "Chenpi source 逐层调整 OR", style = "heading 2")
doc <- body_add_flextable(doc, flextable(source_path) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "路径模型性能", style = "heading 2")
doc <- body_add_flextable(doc, flextable(path_performance) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "论文写法边界", style = "heading 2")
doc <- body_add_par(doc, "这部分应写作 causal-informed path decomposition，而不是 randomized causal inference。若来源效应在加入质量和 MES 后明显衰减，应表述为来源相关差异可能通过原料质量、中间体质量和成品过程路径传递。", style = "Normal")
print(doc, target = file.path(docs_dir, "因果稳健性与路径衰减分析说明.docx"))

message("Robust causal path analysis completed.")
