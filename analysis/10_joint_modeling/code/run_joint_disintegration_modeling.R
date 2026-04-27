options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(glmnet)
  library(officer)
  library(openxlsx)
  library(pROC)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(xgboost)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
root_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/", mustWork = TRUE)
cn_dir <- file.path(root_dir, "定稿数据_中文")
docs_dir <- file.path(project_dir, "docs")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

choose_cn_file <- function(prefix) {
  files <- list.files(cn_dir, pattern = paste0("^", prefix, ".*\\.xlsx$"), full.names = TRUE)
  if (length(files) == 0) stop("No source file found for prefix: ", prefix)
  normalizePath(files[1], winslash = "/", mustWork = TRUE)
}

norm_batch <- function(x) {
  s <- as.character(x) |>
    str_trim() |>
    str_replace("\\.0$", "")
  s[is.na(s)] <- ""
  s[tolower(s) %in% c("nan", "none", "na", "")] <- ""
  s
}

parse_numeric_vector <- function(x) {
  if (is.na(x) || str_trim(as.character(x)) == "") return(numeric(0))
  values <- str_split(as.character(x), "[;；,，、\\s]+")[[1]]
  suppressWarnings(as.numeric(values[values != ""]))
}

parse_numeric_mean <- function(x) {
  values <- parse_numeric_vector(x)
  if (length(values) == 0 || all(is.na(values))) return(NA_real_)
  mean(values, na.rm = TRUE)
}

split_batch_vector <- function(x) {
  x <- norm_batch(x)
  if (length(x) == 0 || x == "") return(character(0))
  out <- str_split(x, "[;；,，、\\s]+")[[1]]
  out <- norm_batch(out)
  out[out != ""]
}

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_first <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ format(round(p, 3), nsmall = 3, trim = TRUE)
  )
}

save_plot_dual <- function(plot_obj, stem, width, height) {
  pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figures_dir, paste0(stem, ".png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, device = cairo_pdf)
  ggsave(png_path, plot_obj, width = width, height = height, dpi = 330)
}

set_plot_style <- function() {
  theme_set(theme_bw(base_family = "Arial", base_size = 24))
  theme_update(
    plot.title = element_blank(),
    axis.title = element_text(size = 28, colour = "black"),
    axis.text = element_text(size = 24, colour = "black"),
    strip.text = element_text(size = 24, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.8),
    plot.margin = margin(10, 18, 10, 12)
  )
}

set_plot_style()
point_colour <- "#8C8C8C"
trend_colour <- "#A83B2B"
bar_colour <- "#0A5C7A"

input_d2 <- choose_cn_file("D2")
input_d3 <- choose_cn_file("D3")
input_d4 <- choose_cn_file("D4")
input_d5 <- choose_cn_file("D5")
input_d6 <- choose_cn_file("D6")
input_d7 <- choose_cn_file("D7")

d2_raw <- read_xlsx(input_d2)
d3_raw <- read_xlsx(input_d3)
d4_raw <- read_xlsx(input_d4)
d5_raw <- read_xlsx(input_d5)
d6_raw <- read_xlsx(input_d6)
d7_raw <- read_xlsx(input_d7)

d2_batch <- tibble(
  finished_batch = norm_batch(d2_raw[[1]]),
  production_date = as.Date(d2_raw[[2]]),
  finished_coated_tablet_weight_g = as.numeric(d2_raw[[3]]),
  disintegration_time_min = as.numeric(d2_raw[[4]]),
  finished_active_content_mg_per_tablet = as.numeric(d2_raw[[5]]),
  dosage_strength = str_replace_all(as.character(d2_raw[[8]]), "\\s+", "")
) |>
  filter(dosage_strength == "0.8g", finished_batch != "") |>
  group_by(finished_batch) |>
  summarise(
    production_date = min(production_date, na.rm = TRUE),
    finished_coated_tablet_weight_g = safe_mean(finished_coated_tablet_weight_g),
    disintegration_time_min = safe_mean(disintegration_time_min),
    disintegration_issue = as.integer(any(disintegration_time_min > 10, na.rm = TRUE)),
    finished_active_content_mg_per_tablet = safe_mean(finished_active_content_mg_per_tablet),
    d2_record_n = n(),
    .groups = "drop"
  ) |>
  mutate(production_month = format(production_date, "%Y-%m"))

d3_feature_map <- tibble::tribble(
  ~feature, ~label, ~source_col,
  "coating_abraded_rate_permille", "Abraded-tablet rate during coating (‰)", "包衣_磨损_占比(‰)",
  "coating_abraded_weight_g", "Abraded-tablet weight during coating (g)", "包衣_磨损_重量(g)",
  "blend_powder_i_mass_balance_pct", "Blend powder I mass balance (%)", "混合粉I_物料平衡(%)",
  "blend_powder_i_yield_pct", "Blend powder I yield (%)", "混合粉I_得率(%)",
  "compression_broken_tablet_rate_permille", "Broken-tablet rate during compression (‰)", "压片_外观_占比(‰)",
  "compression_broken_tablet_weight_g", "Broken-tablet weight during compression (g)", "压片_外观_残缺片量(g)",
  "coated_tablet_hardness_mean_n", "Coated tablet hardness (N)", "包衣_包衣片_平均硬度(N)",
  "coated_tablet_weight_mean_g", "Coated tablet weight (g)", "包衣_包衣片_平均片重(g)",
  "coating_mass_balance_pct", "Coating mass balance (%)", "包衣_物料平衡(%)",
  "coating_yield_pct", "Coating yield (%)", "包衣_得率(%)",
  "compression_hardness_mean_n", "Compression hardness (N)", "压片_硬度_平均值(N)",
  "compression_mass_balance_pct", "Compression mass balance (%)", "压片_物料平衡(%)",
  "compression_tablet_weight_mean_g", "Compression tablet weight (g)", "压片_片重_平均值(g)",
  "compression_yield_pct", "Compression yield (%)", "压片_得率(%)",
  "core_tablet_weight_mean_g", "Core tablet weight (g)", "包衣_素片_平均片重(g)",
  "final_blend_lt_100_mesh_pct", "Final blend <100 mesh (%)", "总混_粒度Y(<100目%)",
  "final_blend_mass_balance_pct", "Final-blend mass balance (%)", "总混_物料平衡(%)",
  "final_blend_moisture_pct", "Final-blend moisture (%)", "总混_水分(%)",
  "final_blend_yield_pct", "Final-blend yield (%)", "总混_得率(%)",
  "final_product_mass_balance_pct", "Final-product mass balance (%)", "成品_物料平衡(%)",
  "final_product_yield_pct", "Final-product yield (%)", "成品_得率(%)",
  "granulation_discharge_moisture_pct_mean", "Granulation moisture, mean (%)", "制粒_出锅水分(%)",
  "milling_mass_balance_pct", "Milling mass balance (%)", "粉碎_物料平衡(%)",
  "coating_missing_tablet_rate_permille", "Missing-tablet rate during coating (‰)", "包衣_缺片_占比(‰)",
  "coating_missing_tablet_weight_g", "Missing-tablet weight during coating (g)", "包衣_缺片_重量(g)"
)

d3_base <- tibble(
  finished_batch = norm_batch(d3_raw[[1]]),
  mes_production_date = as.Date(d3_raw[[2]]),
  extract_batch_raw = norm_batch(d3_raw[[3]]),
  yam_batch_raw = norm_batch(d3_raw[[6]])
)

for (i in seq_len(nrow(d3_feature_map))) {
  feature <- d3_feature_map$feature[i]
  col <- d3_feature_map$source_col[i]
  if (col == "制粒_出锅水分(%)") {
    d3_base[[feature]] <- vapply(d3_raw[[col]], parse_numeric_mean, numeric(1))
  } else {
    d3_base[[feature]] <- suppressWarnings(as.numeric(d3_raw[[col]]))
  }
}

d3_base <- d3_base |>
  filter(finished_batch != "") |>
  distinct(finished_batch, .keep_all = TRUE)

d3_extract_long <- d3_base |>
  select(finished_batch, extract_batch_raw) |>
  mutate(extract_batch = lapply(extract_batch_raw, split_batch_vector)) |>
  unnest(extract_batch) |>
  filter(extract_batch != "") |>
  distinct(finished_batch, extract_batch)

d3_yam_long <- d3_base |>
  select(finished_batch, yam_batch_raw) |>
  mutate(yam_batch = lapply(yam_batch_raw, split_batch_vector)) |>
  unnest(yam_batch) |>
  filter(yam_batch != "") |>
  distinct(finished_batch, yam_batch)

d4_batch <- tibble(
  extract_batch = norm_batch(d4_raw[[1]]),
  extract_moisture_pct = as.numeric(d4_raw[[2]]),
  extract_total_ash_pct = as.numeric(d4_raw[[3]]),
  extract_extract_pct = as.numeric(d4_raw[[4]]),
  extract_hesperidin_mg_g = as.numeric(d4_raw[[5]])
) |>
  filter(extract_batch != "") |>
  group_by(extract_batch) |>
  summarise(across(where(is.numeric), safe_mean), .groups = "drop")

extract_by_finished <- d3_extract_long |>
  left_join(d4_batch, by = "extract_batch") |>
  group_by(finished_batch) |>
  summarise(
    extract_batch_n = n_distinct(extract_batch),
    extract_matched_d4_batch_n = n_distinct(extract_batch[!is.na(extract_total_ash_pct)]),
    extract_moisture_pct = safe_mean(extract_moisture_pct),
    extract_total_ash_pct = safe_mean(extract_total_ash_pct),
    extract_extract_pct = safe_mean(extract_extract_pct),
    extract_hesperidin_mg_g = safe_mean(extract_hesperidin_mg_g),
    .groups = "drop"
  )

d5_chenpi <- tibble(
  extract_batch = norm_batch(d5_raw[[1]]),
  material_type = as.character(d5_raw[[2]]) |> str_squish(),
  raw_batch = as.character(d5_raw[[3]]) |> str_squish(),
  raw_order = suppressWarnings(as.numeric(d5_raw[[4]]))
) |>
  filter(material_type == "陈皮")

extract_chenpi_batches <- function(x) {
  text <- as.character(x) |> str_squish()
  if (is.na(text) || text == "") return(character(0))
  values <- str_extract_all(text, "\\d{7}(?:-\\d+)?")[[1]]
  norm_batch(values)
}

trace_long <- d5_chenpi |>
  mutate(chenpi_batch = lapply(raw_batch, extract_chenpi_batches)) |>
  unnest(chenpi_batch) |>
  mutate(
    extract_batch = norm_batch(extract_batch),
    chenpi_batch = norm_batch(chenpi_batch)
  ) |>
  filter(extract_batch != "", chenpi_batch != "") |>
  distinct(extract_batch, chenpi_batch, raw_batch, raw_order)

d6_batch <- tibble(
  chenpi_batch = norm_batch(d6_raw[[3]]),
  chenpi_origin = as.character(d6_raw[[4]]) |> str_squish(),
  chenpi_moisture_pct = as.numeric(d6_raw[[5]]),
  chenpi_hesperidin_pct = as.numeric(d6_raw[[6]]),
  chenpi_impurities_pct = as.numeric(d6_raw[[7]])
) |>
  filter(chenpi_batch != "") |>
  group_by(chenpi_batch) |>
  summarise(
    chenpi_origin = safe_first(chenpi_origin),
    chenpi_moisture_pct = safe_mean(chenpi_moisture_pct),
    chenpi_hesperidin_pct = safe_mean(chenpi_hesperidin_pct),
    chenpi_impurities_pct = safe_mean(chenpi_impurities_pct),
    .groups = "drop"
  )

chenpi_by_extract <- trace_long |>
  left_join(d6_batch, by = "chenpi_batch") |>
  group_by(extract_batch) |>
  summarise(
    chenpi_batch_n = n_distinct(chenpi_batch),
    chenpi_test_batch_n = n_distinct(chenpi_batch[!is.na(chenpi_hesperidin_pct)]),
    chenpi_moisture_pct_mean = safe_mean(chenpi_moisture_pct),
    chenpi_hesperidin_pct_mean = safe_mean(chenpi_hesperidin_pct),
    chenpi_impurities_pct_mean = safe_mean(chenpi_impurities_pct),
    chenpi_origin_pattern = paste(sort(unique(chenpi_origin[!is.na(chenpi_origin) & chenpi_origin != ""])), collapse = " + "),
    .groups = "drop"
  ) |>
  mutate(chenpi_origin_pattern = ifelse(chenpi_origin_pattern == "", NA_character_, chenpi_origin_pattern))

chenpi_by_finished <- d3_extract_long |>
  left_join(chenpi_by_extract, by = "extract_batch") |>
  group_by(finished_batch) |>
  summarise(
    chenpi_linked_extract_batch_n = n_distinct(extract_batch[!is.na(chenpi_hesperidin_pct_mean)]),
    chenpi_batch_n = sum(chenpi_batch_n, na.rm = TRUE),
    chenpi_test_batch_n = sum(chenpi_test_batch_n, na.rm = TRUE),
    chenpi_moisture_pct_mean = safe_mean(chenpi_moisture_pct_mean),
    chenpi_hesperidin_pct_mean = safe_mean(chenpi_hesperidin_pct_mean),
    chenpi_impurities_pct_mean = safe_mean(chenpi_impurities_pct_mean),
    chenpi_origin_pattern = paste(sort(unique(chenpi_origin_pattern[!is.na(chenpi_origin_pattern)])), collapse = " + "),
    .groups = "drop"
  ) |>
  mutate(chenpi_origin_pattern = ifelse(chenpi_origin_pattern == "", NA_character_, chenpi_origin_pattern))

d7_batch <- tibble(
  yam_batch = norm_batch(d7_raw[[1]]),
  yam_supplier = as.character(d7_raw[[3]]) |> str_squish(),
  yam_input_kg = as.numeric(d7_raw[[5]]),
  yam_rejected_material_weight_kg = as.numeric(d7_raw[[6]]),
  yam_rejected_material_rate_pct = as.numeric(d7_raw[[7]]),
  yam_process_moisture_mean_pct = vapply(d7_raw[[8]], parse_numeric_mean, numeric(1)),
  yam_through_100_mesh_mean_pct = vapply(d7_raw[[11]], parse_numeric_mean, numeric(1)),
  yam_through_120_mesh_mean_pct = vapply(d7_raw[[12]], parse_numeric_mean, numeric(1)),
  yam_yield_pct = as.numeric(d7_raw[[13]]),
  yam_mass_balance_pct = as.numeric(d7_raw[[14]])
) |>
  filter(yam_batch != "") |>
  group_by(yam_batch) |>
  summarise(
    yam_supplier = safe_first(yam_supplier),
    across(starts_with("yam_") & where(is.numeric), safe_mean),
    .groups = "drop"
  )

yam_by_finished <- d3_yam_long |>
  left_join(d7_batch, by = "yam_batch") |>
  group_by(finished_batch) |>
  summarise(
    yam_batch_n = n_distinct(yam_batch),
    yam_matched_d7_batch_n = n_distinct(yam_batch[!is.na(yam_rejected_material_rate_pct)]),
    yam_rejected_material_weight_kg = safe_mean(yam_rejected_material_weight_kg),
    yam_rejected_material_rate_pct = safe_mean(yam_rejected_material_rate_pct),
    yam_process_moisture_mean_pct = safe_mean(yam_process_moisture_mean_pct),
    yam_through_100_mesh_mean_pct = safe_mean(yam_through_100_mesh_mean_pct),
    yam_through_120_mesh_mean_pct = safe_mean(yam_through_120_mesh_mean_pct),
    yam_yield_pct = safe_mean(yam_yield_pct),
    yam_mass_balance_pct = safe_mean(yam_mass_balance_pct),
    yam_supplier_pattern = paste(sort(unique(yam_supplier[!is.na(yam_supplier) & yam_supplier != ""])), collapse = " + "),
    .groups = "drop"
  ) |>
  mutate(yam_supplier_pattern = ifelse(yam_supplier_pattern == "", NA_character_, yam_supplier_pattern))

joint <- d2_batch |>
  inner_join(d3_base |> select(-extract_batch_raw, -yam_batch_raw, -mes_production_date), by = "finished_batch") |>
  left_join(extract_by_finished, by = "finished_batch") |>
  left_join(chenpi_by_finished, by = "finished_batch") |>
  left_join(yam_by_finished, by = "finished_batch") |>
  arrange(production_date, finished_batch)

baseline_vars <- c("finished_coated_tablet_weight_g", "finished_active_content_mg_per_tablet")
d3_all <- d3_feature_map$feature
d3_core <- c(
  "coating_yield_pct", "final_blend_moisture_pct", "coating_mass_balance_pct",
  "compression_yield_pct", "compression_hardness_mean_n", "coated_tablet_weight_mean_g",
  "final_blend_lt_100_mesh_pct", "granulation_discharge_moisture_pct_mean",
  "core_tablet_weight_mean_g"
)
d4_all <- c("extract_moisture_pct", "extract_total_ash_pct", "extract_extract_pct", "extract_hesperidin_mg_g")
d4_core <- c("extract_total_ash_pct", "extract_extract_pct")
d6_all <- c("chenpi_moisture_pct_mean", "chenpi_hesperidin_pct_mean", "chenpi_impurities_pct_mean")
d6_core <- c("chenpi_hesperidin_pct_mean", "chenpi_moisture_pct_mean")
d7_all <- c(
  "yam_rejected_material_weight_kg", "yam_rejected_material_rate_pct",
  "yam_process_moisture_mean_pct", "yam_through_120_mesh_mean_pct",
  "yam_yield_pct", "yam_mass_balance_pct"
)
d7_core <- c("yam_rejected_material_rate_pct", "yam_through_120_mesh_mean_pct")

all_candidate_vars <- unique(c(baseline_vars, d3_all, d4_all, d6_all, d7_all))
core_vars <- unique(c(baseline_vars, d3_core, d4_core, d6_core, d7_core))

label_map <- bind_rows(
  tibble(variable = baseline_vars, label = c("Finished coated tablet weight (g)", "Finished active content (mg/tablet)"), layer = "Finished-product quality"),
  d3_feature_map |> transmute(variable = feature, label, layer = "Finished-product MES"),
  tibble(variable = d4_all, label = c("Extract-powder moisture (%)", "Extract-powder total ash (%)", "Extract-powder extract (%)", "Extract-powder hesperidin content (mg/g)"), layer = "Extract-powder quality"),
  tibble(variable = d6_all, label = c("Chenpi moisture (%)", "Chenpi hesperidin (%)", "Chenpi impurities (%)"), layer = "Chenpi quality"),
  tibble(variable = d7_all, label = c("Rejected material weight (kg)", "Rejected material rate (%)", "Process moisture mean (%)", "Through 120-mesh mean (%)", "Yield (%)", "Mass balance (%)"), layer = "Chinese yam powder MES")
) |>
  distinct(variable, .keep_all = TRUE)

missing_summary <- lapply(all_candidate_vars, function(v) {
  values <- joint[[v]]
  tibble(
    variable = v,
    n = length(values),
    available_n = sum(!is.na(values)),
    missing_n = sum(is.na(values)),
    missing_pct = mean(is.na(values)) * 100,
    unique_n = n_distinct(values, na.rm = TRUE)
  )
}) |>
  bind_rows() |>
  left_join(label_map, by = "variable") |>
  relocate(layer, label, .after = variable) |>
  arrange(layer, desc(missing_pct), variable)

variable_audit <- missing_summary |>
  mutate(
    primary_role = case_when(
      variable %in% core_vars ~ "Core model",
      variable %in% all_candidate_vars ~ "Extended candidate model",
      TRUE ~ "Not used"
    ),
    model_status = case_when(
      unique_n < 2 ~ "Excluded from modeling: <2 unique non-missing values",
      missing_pct >= 99 ~ "Excluded from modeling: almost completely missing",
      variable %in% core_vars ~ "Included in core model",
      TRUE ~ "Included in extended model"
    )
  )

modelable_all_vars <- variable_audit |>
  filter(model_status %in% c("Included in core model", "Included in extended model")) |>
  pull(variable)
modelable_core_vars <- intersect(core_vars, modelable_all_vars)

linkage_summary <- tibble::tribble(
  ~item, ~value,
  "0.8g finished-product batches in D2", n_distinct(d2_batch$finished_batch),
  "0.8g finished-product batches linked to D3 MES", nrow(joint),
  "Linked batches with disintegration >10 min", sum(joint$disintegration_issue == 1, na.rm = TRUE),
  "Linked batches with disintegration <=10 min", sum(joint$disintegration_issue == 0, na.rm = TRUE),
  "Batches linked to extract-powder quality", sum(!is.na(joint$extract_total_ash_pct)),
  "Batches linked to Chenpi quality", sum(!is.na(joint$chenpi_hesperidin_pct_mean)),
  "Batches linked to Chinese yam powder MES", sum(!is.na(joint$yam_rejected_material_rate_pct)),
  "Core model candidate variables", length(modelable_core_vars),
  "Extended model candidate variables", length(modelable_all_vars)
)

impute_train_test <- function(train_df, test_df, vars, month_levels, include_month = TRUE) {
  x_train <- list()
  x_test <- list()
  feature_names <- character()

  for (v in vars) {
    train_values <- suppressWarnings(as.numeric(train_df[[v]]))
    test_values <- suppressWarnings(as.numeric(test_df[[v]]))
    miss_train <- as.integer(is.na(train_values))
    miss_test <- as.integer(is.na(test_values))
    med <- median(train_values, na.rm = TRUE)
    if (is.na(med)) med <- 0
    train_values[is.na(train_values)] <- med
    test_values[is.na(test_values)] <- med
    mu <- mean(train_values, na.rm = TRUE)
    sigma <- sd(train_values, na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) sigma <- 1
    x_train[[v]] <- (train_values - mu) / sigma
    x_test[[v]] <- (test_values - mu) / sigma
    feature_names <- c(feature_names, v)
    if (any(miss_train == 1) || any(miss_test == 1)) {
      miss_name <- paste0(v, "__missing")
      x_train[[miss_name]] <- miss_train
      x_test[[miss_name]] <- miss_test
      feature_names <- c(feature_names, miss_name)
    }
  }

  if (include_month) {
    month_train <- model.matrix(
      ~ production_month - 1,
      data = data.frame(production_month = factor(train_df$production_month, levels = month_levels))
    )
    month_test <- model.matrix(
      ~ production_month - 1,
      data = data.frame(production_month = factor(test_df$production_month, levels = month_levels))
    )
    x_train_mat <- cbind(as.data.frame(x_train), as.data.frame(month_train)) |> as.matrix()
    x_test_mat <- cbind(as.data.frame(x_test), as.data.frame(month_test)) |> as.matrix()
  } else {
    x_train_mat <- as.data.frame(x_train) |> as.matrix()
    x_test_mat <- as.data.frame(x_test) |> as.matrix()
  }
  storage.mode(x_train_mat) <- "double"
  storage.mode(x_test_mat) <- "double"
  list(x_train = x_train_mat, x_test = x_test_mat)
}

metrics_binary <- function(y, p) {
  y <- as.integer(y)
  keep <- !is.na(y) & !is.na(p)
  y <- y[keep]
  p <- p[keep]
  if (length(unique(y)) < 2) {
    return(tibble(AUC = NA_real_, PR_AUC = NA_real_, Brier = mean((p - y)^2)))
  }
  roc_auc <- as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE)))
  ord <- order(p, decreasing = TRUE)
  y_ord <- y[ord]
  precision <- cumsum(y_ord) / seq_along(y_ord)
  recall <- cumsum(y_ord) / sum(y_ord)
  recall <- c(0, recall)
  precision <- c(1, precision)
  pr_auc <- sum(diff(recall) * (head(precision, -1) + tail(precision, -1)) / 2)
  tibble(AUC = roc_auc, PR_AUC = pr_auc, Brier = mean((p - y)^2))
}

make_stratified_folds <- function(y, k = 5, seed = 20260426) {
  set.seed(seed)
  folds <- rep(NA_integer_, length(y))
  for (cls in sort(unique(y))) {
    idx <- which(y == cls)
    idx <- sample(idx)
    folds[idx] <- rep(seq_len(k), length.out = length(idx))
  }
  folds
}

fit_cv_glmnet_safe <- function(x, y, alpha = 0.5) {
  class_counts <- table(y)
  if (length(class_counts) < 2 || min(class_counts) < 2) {
    return(NULL)
  }
  inner_k <- min(5, as.integer(min(class_counts)))
  foldid <- make_stratified_folds(y, k = inner_k, seed = 20260428)
  cv.glmnet(
    x,
    y,
    family = "binomial",
    alpha = alpha,
    foldid = foldid,
    type.measure = "deviance",
    standardize = FALSE
  )
}

cv_glmnet_predict <- function(data, vars, alpha = 0.5, k = 5, include_month = TRUE) {
  y <- data$disintegration_issue
  folds <- make_stratified_folds(y, k = k)
  month_levels <- sort(unique(data$production_month))
  pred <- rep(NA_real_, nrow(data))
  for (fold in seq_len(k)) {
    train_df <- data[folds != fold, , drop = FALSE]
    test_df <- data[folds == fold, , drop = FALSE]
    matrices <- impute_train_test(train_df, test_df, vars, month_levels, include_month = include_month)
    fit <- fit_cv_glmnet_safe(matrices$x_train, train_df$disintegration_issue, alpha = alpha)
    if (is.null(fit)) {
      pred[folds == fold] <- mean(train_df$disintegration_issue, na.rm = TRUE)
    } else {
      pred[folds == fold] <- as.numeric(predict(fit, newx = matrices$x_test, s = "lambda.min", type = "response"))
    }
  }
  pred
}

fit_glmnet_full <- function(data, vars, alpha = 0.5, include_month = TRUE) {
  month_levels <- sort(unique(data$production_month))
  matrices <- impute_train_test(data, data, vars, month_levels, include_month = include_month)
  fit <- fit_cv_glmnet_safe(matrices$x_train, data$disintegration_issue, alpha = alpha)
  if (is.null(fit)) {
    return(tibble(term = character(), coefficient = numeric(), abs_coefficient = numeric()))
  }
  co <- as.matrix(coef(fit, s = "lambda.min"))
  tibble(
    term = rownames(co),
    coefficient = as.numeric(co[, 1])
  ) |>
    filter(term != "(Intercept)", coefficient != 0) |>
    mutate(abs_coefficient = abs(coefficient)) |>
    arrange(desc(abs_coefficient))
}

model_sets <- list(
  "Baseline" = baseline_vars,
  "Baseline + MES" = unique(c(baseline_vars, d3_core)),
  "Baseline + MES + extract-powder" = unique(c(baseline_vars, d3_core, d4_core)),
  "Baseline + MES + extract-powder + Chenpi" = unique(c(baseline_vars, d3_core, d4_core, d6_core)),
  "Full core hierarchy" = modelable_core_vars,
  "Extended candidates" = modelable_all_vars
)

run_model_set <- function(include_month) {
  lapply(names(model_sets), function(name) {
  vars <- intersect(model_sets[[name]], modelable_all_vars)
  pred <- cv_glmnet_predict(joint, vars, alpha = 0.5, k = 5, include_month = include_month)
  metrics <- metrics_binary(joint$disintegration_issue, pred)
  tibble(
    adjustment = ifelse(include_month, "Month-adjusted", "No month adjustment"),
    model = name,
    n_variables = length(vars),
    prediction = list(pred)
  ) |>
    bind_cols(metrics)
  }) |>
    bind_rows()
}

model_predictions <- bind_rows(
  run_model_set(include_month = FALSE),
  run_model_set(include_month = TRUE)
)

prediction_export <- tibble(
  finished_batch = joint$finished_batch,
  disintegration_issue = joint$disintegration_issue,
  disintegration_time_min = joint$disintegration_time_min,
  production_month = joint$production_month
)
for (i in seq_len(nrow(model_predictions))) {
  prediction_export[[paste0("pred_", make.names(model_predictions$adjustment[i]), "_", make.names(model_predictions$model[i]))]] <- model_predictions$prediction[[i]]
}

model_performance <- model_predictions |>
  select(adjustment, model, n_variables, AUC, PR_AUC, Brier)

elastic_net_coefficients <- fit_glmnet_full(joint, modelable_core_vars, alpha = 0.5, include_month = FALSE) |>
  mutate(
    base_variable = str_remove(term, "__missing$"),
    term_type = ifelse(str_detect(term, "__missing$"), "Missingness indicator", "Value"),
    label = coalesce(label_map$label[match(base_variable, label_map$variable)], term),
    layer = coalesce(label_map$layer[match(base_variable, label_map$variable)], "Time / other")
  )

make_full_matrix <- function(data, vars, include_month = TRUE) {
  month_levels <- sort(unique(data$production_month))
  impute_train_test(data, data, vars, month_levels, include_month = include_month)$x_train
}

xgb_vars <- modelable_all_vars
xgb_x <- make_full_matrix(joint, xgb_vars, include_month = FALSE)
xgb_y <- joint$disintegration_issue
folds <- make_stratified_folds(xgb_y, k = 5, seed = 20260427)
xgb_pred <- rep(NA_real_, length(xgb_y))
for (fold in seq_len(5)) {
  train_idx <- which(folds != fold)
  test_idx <- which(folds == fold)
  dtrain <- xgb.DMatrix(xgb_x[train_idx, , drop = FALSE], label = xgb_y[train_idx])
  dtest <- xgb.DMatrix(xgb_x[test_idx, , drop = FALSE])
  fit <- xgb.train(
    data = dtrain,
    nrounds = 120,
    params = list(
      max_depth = 2,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      objective = "binary:logistic",
      eval_metric = "auc"
    ),
    verbose = 0
  )
  xgb_pred[test_idx] <- predict(fit, dtest)
}
xgb_performance <- metrics_binary(xgb_y, xgb_pred) |>
  mutate(adjustment = "No month adjustment", model = "XGBoost extended candidates", n_variables = length(xgb_vars)) |>
  select(adjustment, model, n_variables, AUC, PR_AUC, Brier)

xgb_full <- xgb.train(
  data = xgb.DMatrix(xgb_x, label = xgb_y),
  nrounds = 120,
  params = list(
    max_depth = 2,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = "binary:logistic",
    eval_metric = "auc"
  ),
  verbose = 0
)
shap <- predict(xgb_full, xgb.DMatrix(xgb_x), predcontrib = TRUE)
shap_df <- as.data.frame(shap)
shap_importance <- tibble(
  term = names(shap_df),
  mean_abs_shap = vapply(shap_df, function(x) mean(abs(x), na.rm = TRUE), numeric(1))
) |>
  filter(!term %in% c("BIAS", "(Intercept)")) |>
  mutate(
    base_variable = str_remove(term, "__missing$"),
    base_variable = str_replace(base_variable, "^production_month", "production_month"),
    label = case_when(
      base_variable == "production_month" ~ "Production month",
      TRUE ~ coalesce(label_map$label[match(base_variable, label_map$variable)], term)
    ),
    layer = case_when(
      base_variable == "production_month" ~ "Time",
      TRUE ~ coalesce(label_map$layer[match(base_variable, label_map$variable)], "Other")
    )
  ) |>
  group_by(base_variable, label, layer) |>
  summarise(mean_abs_shap = sum(mean_abs_shap), .groups = "drop") |>
  arrange(desc(mean_abs_shap))

complete_case_vars <- unique(c(d3_core, d4_core, d6_core, d7_core))
complete_case_data <- joint |>
  filter(if_all(all_of(complete_case_vars), ~ !is.na(.x)))
complete_case_summary <- tibble(
  item = c("Complete cases for core cross-layer variables", "Complete-case >10 min batches", "Complete-case <=10 min batches"),
  value = c(nrow(complete_case_data), sum(complete_case_data$disintegration_issue == 1), sum(complete_case_data$disintegration_issue == 0))
)

time_cutoff <- "2026-01"
time_train <- joint |> filter(production_month < time_cutoff)
time_test <- joint |> filter(production_month >= time_cutoff)
if (nrow(time_train) > 50 && nrow(time_test) > 20 && length(unique(time_test$disintegration_issue)) == 2) {
  matrices <- impute_train_test(time_train, time_test, modelable_core_vars, sort(unique(joint$production_month)), include_month = FALSE)
  time_fit <- fit_cv_glmnet_safe(matrices$x_train, time_train$disintegration_issue, alpha = 0.5)
  if (is.null(time_fit)) {
    time_split_performance <- tibble(
      train_period = paste(min(time_train$production_month), "to", max(time_train$production_month)),
      test_period = paste(min(time_test$production_month), "to", max(time_test$production_month)),
      train_n = nrow(time_train), test_n = nrow(time_test), test_issue_n = sum(time_test$disintegration_issue == 1),
      AUC = NA_real_, PR_AUC = NA_real_, Brier = NA_real_
    )
  } else {
    time_pred <- as.numeric(predict(time_fit, newx = matrices$x_test, s = "lambda.min", type = "response"))
    time_split_performance <- metrics_binary(time_test$disintegration_issue, time_pred) |>
      mutate(
        train_period = paste(min(time_train$production_month), "to", max(time_train$production_month)),
        test_period = paste(min(time_test$production_month), "to", max(time_test$production_month)),
        train_n = nrow(time_train),
        test_n = nrow(time_test),
        test_issue_n = sum(time_test$disintegration_issue == 1)
      )
  }
} else {
  time_split_performance <- tibble(
    train_period = NA_character_, test_period = NA_character_,
    train_n = nrow(time_train), test_n = nrow(time_test), test_issue_n = sum(time_test$disintegration_issue == 1),
    AUC = NA_real_, PR_AUC = NA_real_, Brier = NA_real_
  )
}

perf_long <- bind_rows(model_performance, xgb_performance) |>
  pivot_longer(cols = c(AUC, PR_AUC, Brier), names_to = "metric", values_to = "value") |>
  mutate(
    model = factor(model, levels = unique(model)),
    adjustment = factor(adjustment, levels = c("No month adjustment", "Month-adjusted"))
  )

performance_plot <- ggplot(perf_long, aes(x = model, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64, colour = "black", linewidth = 0.25) +
  scale_fill_manual(values = c(AUC = trend_colour, PR_AUC = bar_colour, Brier = point_colour)) +
  facet_wrap(~adjustment, ncol = 1, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = "Cross-validated performance", fill = NULL) +
  theme(
    axis.text.y = element_text(size = 19),
    legend.position = "top",
    legend.text = element_text(size = 22)
  )
save_plot_dual(performance_plot, "01_layered_model_performance", 13.5, 10.8)

coef_plot_df <- elastic_net_coefficients |>
  filter(!str_detect(term, "^production_month")) |>
  slice_max(abs_coefficient, n = 20) |>
  mutate(
    plot_label = ifelse(term_type == "Missingness indicator", paste0(label, " missing"), label),
    plot_label = make.unique(plot_label, sep = " "),
    plot_label = factor(plot_label, levels = rev(plot_label))
  )

coef_plot <- ggplot(coef_plot_df, aes(x = coefficient, y = plot_label, fill = coefficient > 0)) +
  geom_col(width = 0.66, colour = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, linewidth = 0.8, colour = "black") +
  scale_fill_manual(values = c(`TRUE` = trend_colour, `FALSE` = bar_colour), guide = "none") +
  labs(x = "Elastic-net coefficient", y = NULL) +
  theme(axis.text.y = element_text(size = 20))
save_plot_dual(coef_plot, "02_elastic_net_core_coefficients", 12.5, 7.6)

shap_plot_df <- shap_importance |>
  filter(base_variable != "production_month") |>
  slice_max(mean_abs_shap, n = 20) |>
  mutate(label = factor(label, levels = rev(label)))

shap_plot <- ggplot(shap_plot_df, aes(x = mean_abs_shap, y = label)) +
  geom_col(width = 0.66, fill = bar_colour, colour = "black", linewidth = 0.25) +
  labs(x = "Mean absolute SHAP value", y = NULL) +
  theme(axis.text.y = element_text(size = 20))
save_plot_dual(shap_plot, "03_xgboost_shap_importance", 12.5, 7.6)

missing_plot_df <- missing_summary |>
  filter(variable %in% modelable_all_vars) |>
  group_by(layer) |>
  summarise(
    variables = n(),
    median_missing_pct = median(missing_pct),
    max_missing_pct = max(missing_pct),
    .groups = "drop"
  )

missing_plot <- ggplot(missing_plot_df, aes(x = reorder(layer, median_missing_pct), y = median_missing_pct)) +
  geom_col(width = 0.64, fill = bar_colour, colour = "black", linewidth = 0.25) +
  geom_text(
    aes(label = paste0("n = ", variables)),
    vjust = -0.45,
    family = "Arial",
    size = 6.4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Median missingness among model variables (%)") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 20))
save_plot_dual(missing_plot, "04_model_variable_missingness_by_layer", 10.8, 6.4)

write.xlsx(
  list(
    linkage_summary = linkage_summary,
    variable_audit = variable_audit,
    missing_summary = missing_summary,
    joint_model_matrix = joint,
    complete_case_summary = complete_case_summary
  ),
  file.path(tables_dir, "joint_disintegration_model_matrix.xlsx"),
  overwrite = TRUE
)

write.xlsx(
  list(
    model_performance = bind_rows(model_performance, xgb_performance),
    time_split_performance = time_split_performance,
    elastic_net_coefficients = elastic_net_coefficients,
    xgboost_shap_importance = shap_importance,
    predictions = prediction_export
  ),
  file.path(tables_dir, "joint_disintegration_model_results.xlsx"),
  overwrite = TRUE
)

markdown_table <- function(df) {
  df <- as.data.frame(df)
  df[] <- lapply(df, as.character)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- apply(df, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

performance_text <- bind_rows(model_performance, xgb_performance) |>
  mutate(
    AUC = sprintf("%.3f", AUC),
    PR_AUC = sprintf("%.3f", PR_AUC),
    Brier = sprintf("%.3f", Brier)
  ) |>
  rename(
    Adjustment = adjustment,
    Model = model,
    `Variables, n` = n_variables
  )

summary_lines <- c(
  "# 成品崩解问题驱动联合建模结果摘要",
  "",
  "## 一、联合矩阵",
  sprintf("本次联合建模以 0.8 g 成品批次为分析单元，共纳入 %d 个 D2-D3 可匹配成品批次，其中崩解时限 >10 min 批次 %d 个，≤10 min 批次 %d 个。", nrow(joint), sum(joint$disintegration_issue == 1), sum(joint$disintegration_issue == 0)),
  sprintf("其中 %d 个批次可连接到浸膏粉理化数据，%d 个批次可连接到陈皮检测数据，%d 个批次可连接到山药粉 MES 数据。", sum(!is.na(joint$extract_total_ash_pct)), sum(!is.na(joint$chenpi_hesperidin_pct_mean)), sum(!is.na(joint$yam_rejected_material_rate_pct))),
  "",
  "## 二、模型策略",
  "主模型采用 Elastic-net Logistic 回归，用于处理多变量共线和中等样本量下的变量收缩。模型分为不含月份的风险信号模型和含生产月份的时间校正模型。分层模型用于比较不同数据层级对崩解异常诊断能力的增益，XGBoost + SHAP 用于非线性验证和变量重要性排序。",
  "",
  "## 三、主要结果",
  markdown_table(performance_text),
  "",
  sprintf("时间切分验证显示，训练集为 %s，测试集为 %s。由于测试集中 %d/%d 批次为 >10 min，而训练集中异常批次极少，该结果更适合提示时间窗口漂移明显，不应被解释为稳定的前瞻预测性能。", time_split_performance$train_period[1], time_split_performance$test_period[1], time_split_performance$test_issue_n[1], time_split_performance$test_n[1]),
  "",
  "Elastic-net 主模型中，非零系数变量见 `joint_disintegration_model_results.xlsx`。XGBoost 验证模型的 SHAP 排名见同一结果表和 `03_xgboost_shap_importance.pdf`。",
  "",
  "## 四、解释边界",
  "当前联合模型仍属于真实世界批次级关联诊断。模型结果可用于支持“成品崩解异常可沿 MES 批次链路追溯到上游物料质量和过程风险信号”，但不能直接写成单一变量的因果效应。后续可继续开展图模型、时间切分验证和因果敏感性分析。"
)

summary_md <- file.path(docs_dir, "联合建模结果摘要.md")
writeLines(summary_lines, summary_md, useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "成品崩解问题驱动联合建模结果摘要", style = "heading 1")
doc <- body_add_par(doc, sprintf("本次联合建模以 0.8 g 成品批次为分析单元，共纳入 %d 个 D2-D3 可匹配成品批次，其中崩解时限 >10 min 批次 %d 个，≤10 min 批次 %d 个。", nrow(joint), sum(joint$disintegration_issue == 1), sum(joint$disintegration_issue == 0)), style = "Normal")
doc <- body_add_par(doc, sprintf("其中 %d 个批次可连接到浸膏粉理化数据，%d 个批次可连接到陈皮检测数据，%d 个批次可连接到山药粉 MES 数据。", sum(!is.na(joint$extract_total_ash_pct)), sum(!is.na(joint$chenpi_hesperidin_pct_mean)), sum(!is.na(joint$yam_rejected_material_rate_pct))), style = "Normal")
doc <- body_add_par(doc, "模型性能", style = "heading 2")
doc <- body_add_flextable(doc, flextable(bind_rows(model_performance, xgb_performance) |> mutate(across(where(is.numeric), ~ round(.x, 4)))))
doc <- body_add_par(doc, "时间切分验证", style = "heading 2")
doc <- body_add_par(doc, sprintf("训练集为 %s，测试集为 %s。测试集中 %d/%d 批次为 >10 min，而训练集中异常批次极少，因此该结果主要提示时间窗口漂移明显，不宜解释为稳定前瞻预测性能。", time_split_performance$train_period[1], time_split_performance$test_period[1], time_split_performance$test_issue_n[1], time_split_performance$test_n[1]), style = "Normal")
doc <- body_add_par(doc, "解释边界", style = "heading 2")
doc <- body_add_par(doc, "当前联合模型属于真实世界批次级关联诊断，可支持分层追溯和风险信号识别，但不能直接写成单一变量的因果效应。后续应继续开展图模型、时间切分验证和因果敏感性分析。", style = "Normal")
print(doc, target = file.path(docs_dir, "联合建模结果摘要.docx"))

message("Saved joint matrix: ", file.path(tables_dir, "joint_disintegration_model_matrix.xlsx"))
message("Saved model results: ", file.path(tables_dir, "joint_disintegration_model_results.xlsx"))
message("Saved figures: ", figures_dir)
