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

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
analysis_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "定稿数据_中文")
joint_dir <- file.path(project_dir, "联合建模_成品崩解问题驱动")
time_dir <- file.path(project_dir, "时序分析_成品崩解问题")

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
  strip.text = element_text(size = 19, colour = "black", face = "bold"),
  legend.title = element_text(size = 18, colour = "black"),
  legend.text = element_text(size = 17, colour = "black"),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", linewidth = 0.8),
  plot.margin = margin(12, 18, 12, 18)
)

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330)
}

read_final <- function(file) read_xlsx(file.path(data_dir, file))

split_batch_field <- function(data, batch_col, new_col) {
  data |>
    mutate("{new_col}" := as.character(.data[[batch_col]])) |>
    separate_rows(all_of(new_col), sep = "[;；,，、\\s]+") |>
    mutate("{new_col}" := str_squish(.data[[new_col]])) |>
    filter(!is.na(.data[[new_col]]), .data[[new_col]] != "", .data[[new_col]] != "NA")
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

fit_binary_eval <- function(data, formula, model_name) {
  vars <- all.vars(formula)
  model_data <- data |> select(all_of(vars)) |> drop_na()
  fit <- glm(formula, data = model_data, family = binomial())
  pred <- as.numeric(predict(fit, newdata = model_data, type = "response"))
  tibble(
    model = model_name,
    n = nrow(model_data),
    issue_n = sum(model_data$disintegration_issue),
    AUC = auc_rank(model_data$disintegration_issue, pred),
    PR_AUC = pr_auc_step(model_data$disintegration_issue, pred),
    Brier = mean((model_data$disintegration_issue - pred)^2)
  )
}

tidy_or <- function(fit, layer, model_name, variable_map = NULL) {
  sm <- summary(fit)$coefficients
  out <- tibble(
    layer = layer,
    model = model_name,
    term = rownames(sm),
    estimate = sm[, "Estimate"],
    std_error = sm[, "Std. Error"],
    p_value = sm[, "Pr(>|z|)"]
  ) |>
    filter(term != "(Intercept)") |>
    mutate(
      odds_ratio = exp(estimate),
      ci_low = exp(estimate - 1.96 * std_error),
      ci_high = exp(estimate + 1.96 * std_error),
      p_display = if_else(p_value < 2.2e-16, "<2.2e-16", sprintf("%.3g", p_value))
    )
  if (!is.null(variable_map)) {
    out <- out |>
      left_join(variable_map, by = "term") |>
      mutate(label = if_else(is.na(label), term, label))
  } else {
    out <- out |> mutate(label = term)
  }
  out
}

fit_binomial_or <- function(data, formula, layer, model_name, variable_map = NULL) {
  vars <- all.vars(formula)
  model_data <- data |> select(any_of(c(vars, "finished_n"))) |> drop_na(all_of(vars))
  if (nrow(model_data) == 0) {
    return(list(
      effects = tibble(
        layer = layer,
        model = model_name,
        term = NA_character_,
        estimate = NA_real_,
        std_error = NA_real_,
        p_value = NA_real_,
        odds_ratio = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_display = NA_character_,
        label = "No analyzable complete cases",
        n_upstream_batches = 0,
        downstream_finished_n = 0
      ),
      fit = NULL
    ))
  }
  fit <- glm(formula, data = model_data, family = binomial())
  list(
    effects = tidy_or(fit, layer, model_name, variable_map) |>
      mutate(n_upstream_batches = nrow(model_data), downstream_finished_n = sum(model_data$finished_n)),
    fit = fit
  )
}

D2 <- read_final("D2_成品理化数据_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    production_date = as.Date(生产日期),
    production_month = format(production_date, "%Y-%m"),
    disintegration_issue_raw = `崩解时限(min)` > 10
  ) |>
  filter(规格 == "0.8g") |>
  group_by(finished_batch) |>
  summarise(
    production_date = min(production_date, na.rm = TRUE),
    production_month = first(production_month),
    disintegration_time_min = mean(`崩解时限(min)`, na.rm = TRUE),
    disintegration_issue = any(disintegration_issue_raw, na.rm = TRUE),
    d2_record_n = n(),
    .groups = "drop"
  ) |>
  mutate(abnormal_window = production_month %in% c("2024-08", "2024-09", "2026-01", "2026-02"))

D3 <- read_final("D3_成品MES主表_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    extract_batch_field = as.character(`健胃消食片浸膏粉_批号`),
    yam_batch_field = as.character(`山药粉-罗亭_批号`)
  )

D4 <- read_final("D4_浸膏粉理化数据_定稿.xlsx") |>
  transmute(
    extract_batch = as.character(批号),
    extract_moisture_pct = num_clean(`水分%`),
    extract_total_ash_pct = num_clean(`总灰分%`),
    extract_extract_pct = num_clean(`浸出物%`),
    extract_hesperidin_mg_g = num_clean(`橙皮苷含量mg/g`)
  )

D5 <- read_final("D5_浸膏粉关联批次追溯_定稿.xlsx") |>
  transmute(
    extract_batch = as.character(浸膏粉批号),
    raw_type = 原料类型,
    raw_batch = as.character(原料批号)
  )

D6 <- read_final("D6_陈皮检测数据_定稿.xlsx") |>
  transmute(
    raw_batch = as.character(批号),
    chenpi_origin = 产地,
    chenpi_moisture_pct = num_clean(`水分%`),
    chenpi_hesperidin_pct = num_clean(`橙皮苷%`),
    chenpi_impurities_pct = num_clean(`杂质%`)
  )

D7_raw <- read_final("D7_山药粉MES主表_定稿.xlsx") |>
  mutate(yam_batch = as.character(批号))

supplier_map <- D7_raw |>
  count(供应商名称, sort = TRUE) |>
  mutate(
    supplier_group = case_when(
      is.na(供应商名称) ~ "Supplier unknown",
      row_number() == 1 ~ "Supplier A",
      row_number() == 2 ~ "Supplier B",
      TRUE ~ "Other suppliers"
    )
  )

D7 <- D7_raw |>
  left_join(supplier_map |> select(供应商名称, supplier_group), by = "供应商名称") |>
  transmute(
    yam_batch,
    yam_supplier_group = supplier_group,
    yam_rejected_material_weight_kg = num_clean(`挑选出不合格品数量E(kg)`),
    yam_rejected_material_rate_pct = num_clean(`占比(%)`),
    yam_process_moisture_pct = num_clean(`过程水分(%)`),
    yam_through_100_mesh_pct = num_clean(`细度100目(%)`),
    yam_through_120_mesh_pct = num_clean(`细度120目(%)`),
    yam_yield_pct = num_clean(`得率(%)`),
    yam_mass_balance_pct = num_clean(`物料平衡(%)`)
  )

finished_mes <- D2 |>
  inner_join(D3, by = "finished_batch")

finished_extract_long <- finished_mes |>
  split_batch_field("extract_batch_field", "extract_batch") |>
  select(finished_batch, production_month, abnormal_window, disintegration_time_min, disintegration_issue, extract_batch)

finished_yam_long <- finished_mes |>
  split_batch_field("yam_batch_field", "yam_batch") |>
  select(finished_batch, production_month, abnormal_window, disintegration_time_min, disintegration_issue, yam_batch)

finished_chenpi_long <- finished_extract_long |>
  inner_join(D5 |> select(extract_batch, raw_batch), by = "extract_batch", relationship = "many-to-many") |>
  inner_join(D6, by = "raw_batch", relationship = "many-to-many")

finished_chenpi_pattern <- finished_chenpi_long |>
  group_by(finished_batch) |>
  summarise(
    chenpi_origin_pattern = case_when(
      n_distinct(chenpi_origin) > 1 ~ "Mixed Chenpi origins",
      first(chenpi_origin) == "浙江" ~ "Zhejiang only",
      first(chenpi_origin) == "江西宜春" ~ "Jiangxi Yichun only",
      TRUE ~ "Unknown"
    ),
    any_zhejiang_chenpi = any(chenpi_origin == "浙江", na.rm = TRUE),
    chenpi_batch_n = n_distinct(raw_batch),
    chenpi_moisture_pct_mean = mean(chenpi_moisture_pct, na.rm = TRUE),
    chenpi_hesperidin_pct_mean = mean(chenpi_hesperidin_pct, na.rm = TRUE),
    chenpi_impurities_pct_mean = mean(chenpi_impurities_pct, na.rm = TRUE),
    .groups = "drop"
  )

finished_extract_quality <- finished_extract_long |>
  inner_join(D4, by = "extract_batch") |>
  group_by(finished_batch) |>
  summarise(
    extract_batch_n = n_distinct(extract_batch),
    extract_moisture_pct = mean(extract_moisture_pct, na.rm = TRUE),
    extract_total_ash_pct = mean(extract_total_ash_pct, na.rm = TRUE),
    extract_extract_pct = mean(extract_extract_pct, na.rm = TRUE),
    extract_hesperidin_mg_g = mean(extract_hesperidin_mg_g, na.rm = TRUE),
    .groups = "drop"
  )

finished_yam_quality <- finished_yam_long |>
  inner_join(D7, by = "yam_batch") |>
  group_by(finished_batch) |>
  summarise(
    yam_batch_n = n_distinct(yam_batch),
    yam_supplier_pattern = case_when(
      n_distinct(yam_supplier_group) > 1 ~ "Multiple suppliers",
      TRUE ~ first(yam_supplier_group)
    ),
    yam_rejected_material_rate_pct = mean(yam_rejected_material_rate_pct, na.rm = TRUE),
    yam_process_moisture_pct = mean(yam_process_moisture_pct, na.rm = TRUE),
    yam_through_120_mesh_pct = mean(yam_through_120_mesh_pct, na.rm = TRUE),
    yam_yield_pct = mean(yam_yield_pct, na.rm = TRUE),
    yam_mass_balance_pct = mean(yam_mass_balance_pct, na.rm = TRUE),
    .groups = "drop"
  )

joint_matrix <- read_xlsx(file.path(joint_dir, "tables", "joint_disintegration_model_matrix.xlsx"), sheet = "joint_model_matrix") |>
  mutate(
    finished_batch = as.character(finished_batch),
    abnormal_window = production_month %in% c("2024-08", "2024-09", "2026-01", "2026-02"),
    disintegration_issue = as.logical(disintegration_issue)
  )

causal_finished_matrix <- joint_matrix |>
  select(
    finished_batch, production_date, production_month, abnormal_window,
    disintegration_time_min, disintegration_issue,
    coating_yield_pct, final_blend_moisture_pct, coating_mass_balance_pct,
    compression_yield_pct, compression_hardness_mean_n, coated_tablet_weight_mean_g,
    final_blend_lt_100_mesh_pct, granulation_discharge_moisture_pct_mean,
    core_tablet_weight_mean_g
  ) |>
  left_join(finished_chenpi_pattern, by = "finished_batch") |>
  left_join(finished_extract_quality, by = "finished_batch") |>
  left_join(finished_yam_quality, by = "finished_batch") |>
  mutate(
    abnormal_window_10pct = as.numeric(abnormal_window) * 10,
    any_zhejiang_chenpi = if_else(is.na(any_zhejiang_chenpi), FALSE, any_zhejiang_chenpi)
  )

upstream_summary <- function(data, batch_col, layer_name) {
  data |>
    group_by(.data[[batch_col]]) |>
    summarise(
      upstream_batch = first(.data[[batch_col]]),
      layer = layer_name,
      finished_n = n_distinct(finished_batch),
      issue_n = n_distinct(finished_batch[disintegration_issue]),
      issue_rate = issue_n / finished_n,
      abnormal_window_n = n_distinct(finished_batch[abnormal_window]),
      abnormal_window_frac = abnormal_window_n / finished_n,
      disintegration_mean = mean(disintegration_time_min, na.rm = TRUE),
      .groups = "drop"
    ) |>
    select(layer, upstream_batch, finished_n, issue_n, issue_rate, abnormal_window_n, abnormal_window_frac, disintegration_mean)
}

extract_batch_summary <- finished_extract_long |>
  inner_join(D4, by = "extract_batch") |>
  upstream_summary("extract_batch", "Extract-powder batch") |>
  left_join(D4 |> rename(upstream_batch = extract_batch), by = "upstream_batch")

chenpi_batch_summary <- finished_chenpi_long |>
  upstream_summary("raw_batch", "Chenpi batch") |>
  left_join(D6 |> rename(upstream_batch = raw_batch), by = "upstream_batch") |>
  mutate(
    chenpi_origin_group = case_when(
      chenpi_origin == "浙江" ~ "Zhejiang",
      chenpi_origin == "江西宜春" ~ "Jiangxi Yichun",
      TRUE ~ "Other/unknown"
    )
  )

yam_batch_summary <- finished_yam_long |>
  inner_join(D7, by = "yam_batch") |>
  upstream_summary("yam_batch", "Chinese yam powder batch") |>
  left_join(D7 |> rename(upstream_batch = yam_batch), by = "upstream_batch")

batch_multiplicity_summary <- bind_rows(
  extract_batch_summary |> select(layer, upstream_batch, finished_n, issue_n, issue_rate, abnormal_window_frac),
  chenpi_batch_summary |> select(layer, upstream_batch, finished_n, issue_n, issue_rate, abnormal_window_frac),
  yam_batch_summary |> select(layer, upstream_batch, finished_n, issue_n, issue_rate, abnormal_window_frac)
) |>
  group_by(layer) |>
  summarise(
    upstream_batches = n(),
    downstream_finished_records = sum(finished_n),
    median_downstream_finished = median(finished_n),
    max_downstream_finished = max(finished_n),
    mixed_outcome_batches = sum(issue_n > 0 & issue_n < finished_n),
    all_issue_batches = sum(issue_n == finished_n & finished_n > 0),
    no_issue_batches = sum(issue_n == 0),
    median_abnormal_window_frac = median(abnormal_window_frac),
    .groups = "drop"
  )

origin_balance <- causal_finished_matrix |>
  filter(!is.na(chenpi_origin_pattern)) |>
  group_by(chenpi_origin_pattern) |>
  summarise(
    finished_n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE),
    issue_rate = mean(disintegration_issue, na.rm = TRUE),
    abnormal_window_n = sum(abnormal_window, na.rm = TRUE),
    abnormal_window_frac = mean(abnormal_window, na.rm = TRUE),
    extract_total_ash_mean = mean(extract_total_ash_pct, na.rm = TRUE),
    extract_extract_mean = mean(extract_extract_pct, na.rm = TRUE),
    chenpi_hesperidin_mean = mean(chenpi_hesperidin_pct_mean, na.rm = TRUE),
    chenpi_moisture_mean = mean(chenpi_moisture_pct_mean, na.rm = TRUE),
    .groups = "drop"
  )

source_balance_yam <- causal_finished_matrix |>
  filter(!is.na(yam_supplier_pattern)) |>
  group_by(yam_supplier_pattern) |>
  summarise(
    finished_n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE),
    issue_rate = mean(disintegration_issue, na.rm = TRUE),
    abnormal_window_n = sum(abnormal_window, na.rm = TRUE),
    abnormal_window_frac = mean(abnormal_window, na.rm = TRUE),
    yam_rejected_rate_mean = mean(yam_rejected_material_rate_pct, na.rm = TRUE),
    yam_120_mesh_mean = mean(yam_through_120_mesh_pct, na.rm = TRUE),
    .groups = "drop"
  )

extract_model_df <- extract_batch_summary |>
  mutate(
    nonissue_n = finished_n - issue_n,
    abnormal_window_10pct = abnormal_window_frac * 10,
    extract_moisture_z = std_vec(extract_moisture_pct),
    extract_total_ash_z = std_vec(extract_total_ash_pct),
    extract_extract_z = std_vec(extract_extract_pct),
    extract_hesperidin_z = std_vec(extract_hesperidin_mg_g)
  )

chenpi_model_df <- chenpi_batch_summary |>
  mutate(
    nonissue_n = finished_n - issue_n,
    abnormal_window_10pct = abnormal_window_frac * 10,
    chenpi_origin_group = factor(chenpi_origin_group, levels = c("Jiangxi Yichun", "Zhejiang", "Other/unknown")),
    chenpi_moisture_z = std_vec(chenpi_moisture_pct),
    chenpi_hesperidin_z = std_vec(chenpi_hesperidin_pct),
    chenpi_impurities_z = std_vec(chenpi_impurities_pct)
  )

yam_model_df <- yam_batch_summary |>
  mutate(
    nonissue_n = finished_n - issue_n,
    abnormal_window_10pct = abnormal_window_frac * 10,
    yam_rejected_rate_z = std_vec(yam_rejected_material_rate_pct),
    yam_process_moisture_z = std_vec(yam_process_moisture_pct),
    yam_120_mesh_z = std_vec(yam_through_120_mesh_pct),
    yam_yield_z = std_vec(yam_yield_pct),
    yam_mass_balance_z = std_vec(yam_mass_balance_pct)
  )

variable_map <- tibble::tribble(
  ~term, ~label,
  "abnormal_window_10pct", "Abnormal-window exposure, per 10%",
  "extract_moisture_z", "Extract-powder moisture, per SD",
  "extract_total_ash_z", "Extract-powder total ash, per SD",
  "extract_extract_z", "Extract-powder extract, per SD",
  "extract_hesperidin_z", "Extract-powder hesperidin, per SD",
  "chenpi_origin_groupZhejiang", "Chenpi origin: Zhejiang vs Jiangxi Yichun",
  "chenpi_origin_groupOther/unknown", "Chenpi origin: other/unknown vs Jiangxi Yichun",
  "chenpi_moisture_z", "Chenpi moisture, per SD",
  "chenpi_hesperidin_z", "Chenpi hesperidin, per SD",
  "chenpi_impurities_z", "Chenpi impurities, per SD",
  "yam_rejected_rate_z", "Yam rejected-material rate, per SD",
  "yam_process_moisture_z", "Yam process moisture, per SD",
  "yam_120_mesh_z", "Yam through 120-mesh, per SD",
  "yam_yield_z", "Yam yield, per SD",
  "yam_mass_balance_z", "Yam mass balance, per SD"
)

extract_or <- fit_binomial_or(
  extract_model_df,
  cbind(issue_n, nonissue_n) ~ abnormal_window_10pct + extract_moisture_z + extract_total_ash_z + extract_extract_z + extract_hesperidin_z,
  "Extract-powder batch",
  "Batch-level binomial model",
  variable_map
)$effects

chenpi_or <- fit_binomial_or(
  chenpi_model_df,
  cbind(issue_n, nonissue_n) ~ abnormal_window_10pct + chenpi_origin_group + chenpi_moisture_z + chenpi_hesperidin_z + chenpi_impurities_z,
  "Chenpi batch",
  "Batch-level binomial model",
  variable_map
)$effects

yam_or <- fit_binomial_or(
  yam_model_df,
  cbind(issue_n, nonissue_n) ~ abnormal_window_10pct + yam_rejected_rate_z + yam_process_moisture_z + yam_120_mesh_z + yam_yield_z + yam_mass_balance_z,
  "Chinese yam powder batch",
  "Batch-level binomial model",
  variable_map
)$effects

batch_level_effects <- bind_rows(extract_or, chenpi_or, yam_or) |>
  mutate(
    odds_ratio = if_else(is.infinite(odds_ratio), NA_real_, odds_ratio),
    ci_low = if_else(is.infinite(ci_low), NA_real_, ci_low),
    ci_high = if_else(is.infinite(ci_high), NA_real_, ci_high)
  )

path_model_df <- causal_finished_matrix |>
  mutate(
    disintegration_issue = as.integer(disintegration_issue),
    abnormal_window = as.integer(abnormal_window),
    any_zhejiang_chenpi = as.integer(any_zhejiang_chenpi),
    coating_yield_z = std_vec(coating_yield_pct),
    final_blend_moisture_z = std_vec(final_blend_moisture_pct),
    coating_mass_balance_z = std_vec(coating_mass_balance_pct),
    extract_total_ash_z = std_vec(extract_total_ash_pct),
    extract_extract_z = std_vec(extract_extract_pct),
    chenpi_moisture_z = std_vec(chenpi_moisture_pct_mean),
    chenpi_hesperidin_z = std_vec(chenpi_hesperidin_pct_mean),
    yam_rejected_rate_z = std_vec(yam_rejected_material_rate_pct),
    yam_120_mesh_z = std_vec(yam_through_120_mesh_pct)
  )

path_model_performance <- bind_rows(
  fit_binary_eval(path_model_df, disintegration_issue ~ abnormal_window, "Time-window only"),
  fit_binary_eval(path_model_df, disintegration_issue ~ abnormal_window + any_zhejiang_chenpi, "Time-window + Chenpi source"),
  fit_binary_eval(path_model_df, disintegration_issue ~ abnormal_window + any_zhejiang_chenpi + extract_total_ash_z + extract_extract_z + chenpi_moisture_z + chenpi_hesperidin_z + yam_rejected_rate_z + yam_120_mesh_z, "Time-window + upstream quality"),
  fit_binary_eval(path_model_df, disintegration_issue ~ abnormal_window + any_zhejiang_chenpi + extract_total_ash_z + extract_extract_z + chenpi_moisture_z + chenpi_hesperidin_z + yam_rejected_rate_z + yam_120_mesh_z + coating_yield_z + final_blend_moisture_z + coating_mass_balance_z, "Time-window + upstream + MES")
) |>
  mutate(across(c(AUC, PR_AUC, Brier), ~round(.x, 3)))

multiplicity_plot_df <- batch_multiplicity_summary |>
  mutate(layer = factor(layer, levels = c("Extract-powder batch", "Chenpi batch", "Chinese yam powder batch")))

multiplicity_plot <- ggplot(multiplicity_plot_df, aes(x = layer, y = median_downstream_finished, fill = layer)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.25) +
  geom_point(aes(y = max_downstream_finished), colour = red, size = 3.8) +
  geom_text(aes(label = paste0("median=", median_downstream_finished, "\nmax=", max_downstream_finished)), vjust = -0.25, size = 4.7, family = "Arial") +
  scale_fill_manual(values = c("Extract-powder batch" = grey, "Chenpi batch" = red, "Chinese yam powder batch" = blue), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Upstream batch layer", y = "Downstream finished-product batches") +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))
save_plot_dual(multiplicity_plot, "01_upstream_downstream_batch_multiplicity", 9.6, 6.2)

batch_issue_plot_df <- bind_rows(
  extract_batch_summary |> select(layer, upstream_batch, finished_n, issue_rate, abnormal_window_frac),
  chenpi_batch_summary |> select(layer, upstream_batch, finished_n, issue_rate, abnormal_window_frac),
  yam_batch_summary |> select(layer, upstream_batch, finished_n, issue_rate, abnormal_window_frac)
) |>
  mutate(
    layer = factor(layer, levels = c("Extract-powder batch", "Chenpi batch", "Chinese yam powder batch")),
    issue_rate_pct = issue_rate * 100,
    abnormal_window_pct = abnormal_window_frac * 100
  )

batch_issue_plot <- ggplot(batch_issue_plot_df, aes(x = abnormal_window_pct, y = issue_rate_pct)) +
  geom_point(aes(size = finished_n), colour = grey) +
  geom_smooth(method = "lm", se = FALSE, colour = red, linewidth = 1.3) +
  facet_wrap(~layer, nrow = 1) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  scale_size_continuous(name = "Downstream\nbatches", range = c(1.2, 5.5)) +
  labs(x = "Downstream batches in abnormal temporal windows", y = ">10 min issue rate") +
  theme(legend.position = "top")
save_plot_dual(batch_issue_plot, "02_upstream_issue_rate_vs_abnormal_window_exposure", 13.5, 5.8)

path_perf_plot_df <- path_model_performance |>
  select(model, AUC, PR_AUC, Brier) |>
  pivot_longer(cols = c(AUC, PR_AUC, Brier), names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(metric, levels = c("AUC", "PR_AUC", "Brier"), labels = c("AUC", "PR-AUC", "Brier score")),
    model = factor(str_wrap(model, 32), levels = str_wrap(path_model_performance$model, 32))
  )

path_perf_plot <- ggplot(path_perf_plot_df, aes(x = value, y = model, fill = metric)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.3f", value)), hjust = -0.12, size = 4.1, family = "Arial") +
  facet_wrap(~metric, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("AUC" = red, "PR-AUC" = blue, "Brier score" = grey), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Model performance", y = NULL) +
  theme(axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15))
save_plot_dual(path_perf_plot, "03_causal_path_model_performance", 10.4, 8.8)

write.xlsx(
  list(
    causal_finished_matrix = causal_finished_matrix,
    batch_multiplicity_summary = batch_multiplicity_summary,
    chenpi_origin_balance = origin_balance,
    yam_supplier_balance = source_balance_yam,
    extract_batch_summary = extract_batch_summary,
    chenpi_batch_summary = chenpi_batch_summary,
    yam_batch_summary = yam_batch_summary,
    batch_level_effects = batch_level_effects,
    path_model_performance = path_model_performance,
    supplier_anonymization_map = supplier_map
  ),
  file.path(tables_dir, "因果批次分解_第一版结果.xlsx"),
  overwrite = TRUE
)

md_lines <- c(
  "# 因果批次分解第一版结果",
  "",
  "## 阶段性质量检查",
  "",
  "前面已完成的联合建模和时序分析总体一致：成品 MES 是最大诊断增益层，异常时间窗是最重要的时间混杂因素。时序分析显示异常高度集中在 2024-08/09 和 2026-01/02，因此因果建模必须首先控制 abnormal_window 或 production_month。",
  "",
  "## 因果建模定位",
  "",
  "本阶段不是直接证明随机因果，而是进行 causal-informed hierarchical batch-effect decomposition。核心思想是利用一批上游物料对应多批下游成品的追溯结构，区分上游批次效应、时间窗口效应和成品 MES 近端过程效应。",
  "",
  "## 批次网络结构",
  "",
  paste(capture.output(print(batch_multiplicity_summary)), collapse = "\n"),
  "",
  "## 主要判断",
  "",
  "- 陈皮批次具有最强的一对多下游结构，适合做 source/origin 层级因果解析。",
  "- 山药粉当前没有产地字段，只能分析 supplier/source batch 和过程质量，不能写成 origin effect。",
  "- 上游批次对应的成品异常率与 abnormal-window exposure 强相关，说明时间混杂必须优先控制。",
  "- 第一版路径模型显示，在 time-window 基础上加入 upstream quality 和 MES 后性能继续改善，但这应解释为路径诊断增强，而不是前瞻预测。",
  "",
  "## 下一步",
  "",
  "1. 对 Chenpi origin/source 做更严格的聚类稳健或 bootstrap 效应估计。",
  "2. 对 extract-powder batch 和 yam-powder batch 做 within-upstream vs between-upstream 分解。",
  "3. 建立最终 DAG 图和路径衰减表，用于论文因果分析小节。"
)
writeLines(md_lines, file.path(docs_dir, "因果批次分解_第一版结果说明.md"), useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "因果批次分解第一版结果", style = "heading 1")
doc <- body_add_par(doc, "本阶段进行 causal-informed hierarchical batch-effect decomposition，重点利用一批上游物料对应多批下游成品的追溯结构，区分上游批次效应、时间窗口效应和成品 MES 近端过程效应。", style = "Normal")
doc <- body_add_par(doc, "批次网络结构", style = "heading 2")
doc <- body_add_flextable(doc, flextable(batch_multiplicity_summary) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "陈皮产地/来源平衡", style = "heading 2")
doc <- body_add_flextable(doc, flextable(origin_balance) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "山药粉供应商/来源平衡", style = "heading 2")
doc <- body_add_flextable(doc, flextable(source_balance_yam) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "批次层效应模型", style = "heading 2")
doc <- body_add_flextable(doc, flextable(batch_level_effects |> select(layer, label, odds_ratio, ci_low, ci_high, p_display, n_upstream_batches, downstream_finished_n)) |> fontsize(size = 7, part = "all") |> autofit())
doc <- body_add_par(doc, "路径模型性能", style = "heading 2")
doc <- body_add_flextable(doc, flextable(path_model_performance) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "解释边界", style = "heading 2")
doc <- body_add_par(doc, "本结果不能直接写成随机因果结论。当前最稳妥的论文表达是：在控制异常时间窗后，利用批次追溯网络进行因果启发式路径分解，识别上游批次、原料来源、中间体质量和成品 MES 近端过程对崩解异常的相对贡献。", style = "Normal")
print(doc, target = file.path(docs_dir, "因果批次分解_第一版结果说明.docx"))

message("Causal-informed batch decomposition completed.")
