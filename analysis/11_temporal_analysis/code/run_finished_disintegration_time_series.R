options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(lubridate)
  library(officer)
  library(openxlsx)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
analysis_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "定稿数据_中文")
joint_dir <- file.path(project_dir, "联合建模_成品崩解问题驱动")

figures_dir <- file.path(analysis_dir, "figures")
tables_dir <- file.path(analysis_dir, "tables")
docs_dir <- file.path(analysis_dir, "docs")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

red <- "#A83B2B"
blue <- "#0A5C7A"
grey <- "#8C8C8C"
light_fill <- "#F7F4EF"
blue_fill <- "#EEF5F6"

theme_set(theme_bw(base_family = "Arial", base_size = 18))
theme_update(
  plot.title = element_blank(),
  axis.title = element_text(size = 23, colour = "black"),
  axis.text = element_text(size = 20, colour = "black"),
  strip.text = element_text(size = 20, colour = "black", face = "bold"),
  legend.title = element_text(size = 19, colour = "black"),
  legend.text = element_text(size = 18, colour = "black"),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", linewidth = 0.8),
  plot.margin = margin(12, 18, 12, 18)
)

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330)
}

season_label <- function(date) {
  m <- month(date)
  case_when(
    m %in% c(3, 4, 5) ~ "Spring",
    m %in% c(6, 7, 8) ~ "Summer",
    m %in% c(9, 10, 11) ~ "Autumn",
    m %in% c(12, 1, 2) ~ "Winter",
    TRUE ~ NA_character_
  )
}

wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- ifelse(n > 0, x / n, NA_real_)
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  tibble(rate = p, ci_low = pmax(0, centre - half), ci_high = pmin(1, centre + half))
}

add_wilson_ci <- function(df, x_col = "issue_n", n_col = "n", prefix = "issue_rate") {
  ci <- wilson_ci(df[[x_col]], df[[n_col]])
  bind_cols(df, ci) |>
    rename(
      "{prefix}" := rate,
      "{prefix}_ci_low" := ci_low,
      "{prefix}_ci_high" := ci_high
    )
}

corrected_or <- function(a, b, c, d) {
  # Haldane-Anscombe correction keeps OR finite when a month has zero issues.
  ((a + 0.5) * (d + 0.5)) / ((b + 0.5) * (c + 0.5))
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

fit_eval_glm <- function(data, formula, model_name) {
  vars <- all.vars(formula)
  model_data <- data |>
    select(all_of(vars)) |>
    drop_na()
  fit <- glm(formula, data = model_data, family = binomial())
  pred <- as.numeric(predict(fit, newdata = model_data, type = "response"))
  tibble(
    model = model_name,
    n = nrow(model_data),
    issue_n = sum(model_data$disintegration_issue),
    variables = length(attr(terms(formula), "term.labels")),
    AUC = auc_rank(model_data$disintegration_issue, pred),
    PR_AUC = pr_auc_step(model_data$disintegration_issue, pred),
    Brier = mean((model_data$disintegration_issue - pred)^2)
  )
}

read_final <- function(file) read_xlsx(file.path(data_dir, file))

D2 <- read_final("D2_成品理化数据_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    production_date = as.Date(生产日期),
    production_month = format(production_date, "%Y-%m"),
    season = factor(season_label(production_date), levels = c("Spring", "Summer", "Autumn", "Winter")),
    disintegration_time_min = `崩解时限(min)`,
    disintegration_issue = disintegration_time_min > 10,
    abnormal_window = production_month %in% c("2024-08", "2024-09", "2026-01", "2026-02")
  ) |>
  filter(规格 == "0.8g")

month_mid <- D2 |>
  group_by(production_month) |>
  summarise(month_date = as.Date(paste0(first(production_month), "-15")), .groups = "drop")

monthly_summary <- D2 |>
  group_by(production_month) |>
  summarise(
    month_date = as.Date(paste0(first(production_month), "-15")),
    n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE),
    disintegration_mean = mean(disintegration_time_min, na.rm = TRUE),
    disintegration_sd = sd(disintegration_time_min, na.rm = TRUE),
    disintegration_median = median(disintegration_time_min, na.rm = TRUE),
    disintegration_q1 = quantile(disintegration_time_min, 0.25, na.rm = TRUE),
    disintegration_q3 = quantile(disintegration_time_min, 0.75, na.rm = TRUE),
    abnormal_window = first(abnormal_window),
    .groups = "drop"
  ) |>
  add_wilson_ci()

overall_issue_n <- sum(D2$disintegration_issue, na.rm = TRUE)
overall_nonissue_n <- sum(!D2$disintegration_issue, na.rm = TRUE)

monthly_tests <- monthly_summary |>
  rowwise() |>
  mutate(
    nonissue_n = n - issue_n,
    other_issue_n = overall_issue_n - issue_n,
    other_nonissue_n = overall_nonissue_n - nonissue_n,
    odds_ratio = corrected_or(issue_n, nonissue_n, other_issue_n, other_nonissue_n),
    fisher_p = fisher.test(matrix(c(issue_n, nonissue_n, other_issue_n, other_nonissue_n), nrow = 2, byrow = TRUE))$p.value
  ) |>
  ungroup() |>
  mutate(
    fdr = p.adjust(fisher_p, method = "BH"),
    fisher_p_display = format_p(fisher_p),
    fdr_display = format_p(fdr),
    issue_rate_pct = issue_rate * 100,
    issue_rate_ci_low_pct = issue_rate_ci_low * 100,
    issue_rate_ci_high_pct = issue_rate_ci_high * 100
  ) |>
  arrange(production_month)

season_summary <- D2 |>
  group_by(season) |>
  summarise(
    n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE),
    disintegration_mean = mean(disintegration_time_min, na.rm = TRUE),
    disintegration_sd = sd(disintegration_time_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  add_wilson_ci() |>
  mutate(dataset = "All months")

season_summary_no_window <- D2 |>
  filter(!abnormal_window) |>
  group_by(season) |>
  summarise(
    n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE),
    disintegration_mean = mean(disintegration_time_min, na.rm = TRUE),
    disintegration_sd = sd(disintegration_time_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  add_wilson_ci() |>
  mutate(dataset = "Excluding abnormal windows")

season_sensitivity <- bind_rows(season_summary, season_summary_no_window) |>
  mutate(
    season = factor(as.character(season), levels = c("Spring", "Summer", "Autumn", "Winter")),
    dataset = factor(dataset, levels = c("All months", "Excluding abnormal windows")),
    issue_rate_pct = issue_rate * 100,
    issue_rate_ci_low_pct = issue_rate_ci_low * 100,
    issue_rate_ci_high_pct = issue_rate_ci_high * 100
  )

abnormal_window_summary <- D2 |>
  mutate(window_group = if_else(abnormal_window, "Abnormal temporal windows", "Other months")) |>
  group_by(window_group) |>
  summarise(
    n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE),
    disintegration_mean = mean(disintegration_time_min, na.rm = TRUE),
    disintegration_sd = sd(disintegration_time_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  add_wilson_ci() |>
  select(window_group, n, issue_n, issue_rate, issue_rate_ci_low, issue_rate_ci_high, disintegration_mean, disintegration_sd)

window_test <- {
  a <- abnormal_window_summary$issue_n[abnormal_window_summary$window_group == "Abnormal temporal windows"]
  b <- abnormal_window_summary$n[abnormal_window_summary$window_group == "Abnormal temporal windows"] - a
  c <- abnormal_window_summary$issue_n[abnormal_window_summary$window_group == "Other months"]
  d <- abnormal_window_summary$n[abnormal_window_summary$window_group == "Other months"] - c
  tibble(
    comparison = "Abnormal temporal windows vs other months",
    odds_ratio = corrected_or(a, b, c, d),
    fisher_p = fisher.test(matrix(c(a, b, c, d), nrow = 2, byrow = TRUE))$p.value
  ) |>
    mutate(fisher_p_display = format_p(fisher_p))
}

joint_matrix <- read_xlsx(file.path(joint_dir, "tables", "joint_disintegration_model_matrix.xlsx"), sheet = "joint_model_matrix") |>
  mutate(
    production_date = as.Date(production_date),
    production_month = format(production_date, "%Y-%m"),
    abnormal_window = production_month %in% c("2024-08", "2024-09", "2026-01", "2026-02"),
    disintegration_issue = as.integer(disintegration_issue)
  )

model_vars <- c(
  "disintegration_issue",
  "abnormal_window",
  "finished_coated_tablet_weight_g",
  "finished_active_content_mg_per_tablet",
  "coating_yield_pct",
  "final_blend_moisture_pct",
  "coating_mass_balance_pct",
  "compression_yield_pct",
  "compression_hardness_mean_n",
  "coated_tablet_weight_mean_g",
  "final_blend_lt_100_mesh_pct",
  "granulation_discharge_moisture_pct_mean",
  "core_tablet_weight_mean_g",
  "extract_total_ash_pct",
  "extract_extract_pct",
  "extract_hesperidin_mg_g",
  "chenpi_moisture_pct_mean",
  "chenpi_hesperidin_pct_mean",
  "chenpi_impurities_pct_mean",
  "yam_rejected_material_rate_pct",
  "yam_through_120_mesh_mean_pct",
  "yam_yield_pct"
)

model_df <- joint_matrix |>
  select(all_of(model_vars)) |>
  drop_na()

time_adjusted_model_performance <- bind_rows(
  fit_eval_glm(model_df, disintegration_issue ~ finished_coated_tablet_weight_g + finished_active_content_mg_per_tablet, "Finished-quality support only"),
  fit_eval_glm(model_df, disintegration_issue ~ abnormal_window, "Abnormal-window only"),
  fit_eval_glm(model_df, disintegration_issue ~ abnormal_window + coating_yield_pct + final_blend_moisture_pct + coating_mass_balance_pct + compression_yield_pct + compression_hardness_mean_n + coated_tablet_weight_mean_g + final_blend_lt_100_mesh_pct + granulation_discharge_moisture_pct_mean + core_tablet_weight_mean_g, "Abnormal-window + finished-product MES"),
  fit_eval_glm(model_df, disintegration_issue ~ abnormal_window + coating_yield_pct + final_blend_moisture_pct + coating_mass_balance_pct + compression_yield_pct + compression_hardness_mean_n + coated_tablet_weight_mean_g + final_blend_lt_100_mesh_pct + granulation_discharge_moisture_pct_mean + core_tablet_weight_mean_g + extract_total_ash_pct + extract_extract_pct + extract_hesperidin_mg_g + chenpi_moisture_pct_mean + chenpi_hesperidin_pct_mean + chenpi_impurities_pct_mean + yam_rejected_material_rate_pct + yam_through_120_mesh_mean_pct + yam_yield_pct, "Abnormal-window + full hierarchy")
) |>
  mutate(
    AUC = round(AUC, 3),
    PR_AUC = round(PR_AUC, 3),
    Brier = round(Brier, 3)
  )

assignment <- D2 |>
  transmute(
    finished_batch,
    production_date,
    production_month,
    season,
    specification = 规格,
    disintegration_time_min,
    disintegration_issue,
    abnormal_window
  )

time_plot <- ggplot() +
  geom_point(
    data = D2,
    aes(x = production_date, y = disintegration_time_min),
    colour = grey,
    size = 1.15
  ) +
  geom_hline(yintercept = 10, colour = red, linewidth = 1.3, linetype = "dashed") +
  geom_errorbar(
    data = monthly_summary,
    aes(x = month_date, ymin = pmax(disintegration_mean - disintegration_sd, 0), ymax = disintegration_mean + disintegration_sd),
    width = 8,
    linewidth = 0.9,
    colour = red
  ) +
  geom_line(
    data = monthly_summary,
    aes(x = month_date, y = disintegration_mean),
    colour = red,
    linewidth = 1.7
  ) +
  geom_point(
    data = monthly_summary,
    aes(x = month_date, y = disintegration_mean),
    colour = red,
    size = 3.4
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Production month", y = "Disintegration time (min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17))
save_plot_dual(time_plot, "01_finished_disintegration_time_series", 11.5, 6.2)

issue_rate_plot <- ggplot(monthly_tests, aes(x = month_date, y = issue_rate_pct)) +
  geom_col(aes(fill = abnormal_window), width = 24, colour = "black", linewidth = 0.25) +
  geom_errorbar(aes(ymin = issue_rate_ci_low_pct, ymax = issue_rate_ci_high_pct), width = 8, linewidth = 0.9, colour = "black") +
  geom_line(colour = red, linewidth = 1.5) +
  geom_point(colour = red, size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = red, `FALSE` = grey), guide = "none") +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = label_number(suffix = "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.04))) +
  labs(x = "Production month", y = ">10 min disintegration issue rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17))
save_plot_dual(issue_rate_plot, "02_monthly_disintegration_issue_rate", 11.5, 6.2)

season_plot <- ggplot(season_sensitivity, aes(x = season, y = issue_rate_pct, fill = dataset)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = issue_rate_ci_low_pct, ymax = issue_rate_ci_high_pct),
    position = position_dodge(width = 0.72),
    width = 0.18,
    linewidth = 0.85
  ) +
  scale_fill_manual(values = c("All months" = red, "Excluding abnormal windows" = blue), name = NULL) +
  scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.06))) +
  labs(x = "Season", y = ">10 min disintegration issue rate") +
  theme(legend.position = "top")
save_plot_dual(season_plot, "03_season_sensitivity_issue_rate", 9.2, 6.0)

model_perf_long <- time_adjusted_model_performance |>
  select(model, AUC, PR_AUC, Brier) |>
  pivot_longer(cols = c(AUC, PR_AUC, Brier), names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(metric, levels = c("AUC", "PR_AUC", "Brier"), labels = c("AUC", "PR-AUC", "Brier score")),
    model = factor(str_wrap(model, width = 34), levels = str_wrap(time_adjusted_model_performance$model, width = 34))
  )

model_plot <- ggplot(model_perf_long, aes(x = value, y = model, fill = metric)) +
  geom_col(width = 0.64, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.3f", value)), hjust = -0.12, size = 4.2, family = "Arial") +
  facet_wrap(~metric, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("AUC" = red, "PR-AUC" = blue, "Brier score" = grey), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Model performance", y = NULL) +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 16), strip.text = element_text(size = 20))
save_plot_dual(model_plot, "04_time_adjusted_model_performance", 10.5, 9.0)

write.xlsx(
  list(
    monthly_summary = monthly_summary,
    monthly_fisher_tests = monthly_tests,
    season_sensitivity = season_sensitivity,
    abnormal_window_summary = abnormal_window_summary,
    abnormal_window_test = window_test,
    time_adjusted_model_performance = time_adjusted_model_performance,
    abnormal_window_assignment = assignment
  ),
  file.path(tables_dir, "成品崩解时序分析结果.xlsx"),
  overwrite = TRUE
)

summary_lines <- c(
  "# 成品崩解时序分析结果",
  "",
  "## 分析定位",
  "",
  "本分析作为论文后续小节，用于判断成品崩解异常是否具有月份、季节或异常时间窗聚集特征，并为后续因果解析提供时间混杂控制变量。",
  "",
  "## 主要结果",
  "",
  sprintf("- 0.8 g 成品记录共 %s 条，其中 >10 min 崩解异常 %s 条，总体异常率 %.1f%%。", nrow(D2), sum(D2$disintegration_issue), mean(D2$disintegration_issue) * 100),
  sprintf("- 生产日期覆盖 %s 至 %s，共 %s 个生产月份。", min(D2$production_date), max(D2$production_date), n_distinct(D2$production_month)),
  "- 异常不是平稳季节性波动，而是集中在 2024-08/09 和 2026-01/02 两个异常时间窗。",
  sprintf("- 异常时间窗内异常率为 %.1f%%，其他月份异常率为 %.1f%%。", abnormal_window_summary$issue_rate[abnormal_window_summary$window_group == "Abnormal temporal windows"] * 100, abnormal_window_summary$issue_rate[abnormal_window_summary$window_group == "Other months"] * 100),
  sprintf("- 异常时间窗与其他月份比较的校正 OR 为 %.2f，Fisher P %s。", window_test$odds_ratio, window_test$fisher_p_display),
  "",
  "## 解释边界",
  "",
  "季节分析应作为敏感性分析，而不是主结论。当前数据首先支持 abnormal temporal windows，而不是稳定四季效应。后续因果分析必须控制 production month 或 abnormal_window，否则容易将时间混杂误判为原料或供应商效应。",
  "",
  "## 输出文件",
  "",
  "- `01_finished_disintegration_time_series`: 批次级崩解时限 + 月度均值 ± SD + 10 min 阈值。",
  "- `02_monthly_disintegration_issue_rate`: 月度 >10 min 异常率 + Wilson 95% CI。",
  "- `03_season_sensitivity_issue_rate`: 四季异常率敏感性分析，含去除异常时间窗后的结果。",
  "- `04_time_adjusted_model_performance`: 时间窗口与层级模型性能比较。"
)

writeLines(summary_lines, file.path(docs_dir, "成品崩解时序分析结果说明.md"), useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "成品崩解时序分析结果说明", style = "heading 1")
doc <- body_add_par(doc, "本分析用于判断成品崩解异常是否具有月份、季节或异常时间窗聚集特征，并为后续因果解析提供时间混杂控制变量。", style = "Normal")
doc <- body_add_par(doc, "月度统计", style = "heading 2")
doc <- body_add_flextable(doc, flextable(monthly_summary |> select(production_month, n, issue_n, issue_rate, disintegration_mean, disintegration_sd, abnormal_window)) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "异常时间窗", style = "heading 2")
doc <- body_add_flextable(doc, flextable(abnormal_window_summary) |> fontsize(size = 9, part = "all") |> autofit())
doc <- body_add_par(doc, "季节敏感性分析", style = "heading 2")
doc <- body_add_flextable(doc, flextable(season_sensitivity |> select(dataset, season, n, issue_n, issue_rate, disintegration_mean, disintegration_sd)) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "时间调整模型性能", style = "heading 2")
doc <- body_add_flextable(doc, flextable(time_adjusted_model_performance) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "结论", style = "heading 2")
doc <- body_add_par(doc, "成品崩解异常主要表现为异常时间窗聚集，而不是稳定四季效应。后续因果分析应优先控制 production month 或 abnormal_window，再评估原料、浸膏粉、山药粉和成品 MES 对终点崩解异常的贡献。", style = "Normal")
print(doc, target = file.path(docs_dir, "成品崩解时序分析结果说明.docx"))

message("Finished-product disintegration time-series analysis completed.")
