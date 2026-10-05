options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glmnet)
  library(lubridate)
  library(pdftools)
  library(pROC)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
  library(xgboost)
})

set.seed(20260819)

data_dir <- Sys.getenv("TCM_DATA_DIR", unset = "")
project_dir <- Sys.getenv("TCM_OUTPUT_DIR", unset = "")
if (data_dir == "" || project_dir == "") {
  stop("TCM_DATA_DIR and TCM_OUTPUT_DIR must point to ASCII-only staging paths.")
}
data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
docs_dir <- file.path(project_dir, "docs")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

choose_file <- function(prefix) {
  files <- list.files(data_dir, pattern = paste0("^", prefix, ".*\\.xlsx$"), full.names = TRUE)
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
  png_path <- file.path(figures_dir, paste0(stem, "_preview.png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")
  suppressWarnings(pdftools::pdf_convert(pdf_path, format = "png", pages = 1, dpi = 220, filenames = png_path))
  if (!file.exists(pdf_path) || !file.exists(png_path) || file.info(pdf_path)$size == 0 || file.info(png_path)$size == 0) {
    stop("Empty figure output: ", stem)
  }
}

set_plot_style <- function() {
  theme_set(theme_classic(base_family = "Arial", base_size = 16))
  theme_update(
    plot.title = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    legend.position = "top",
    legend.title = element_text(size = 16, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 18, 10, 12)
  )
}

set_plot_style()
point_colour <- "#6F6F6F"
trend_colour <- "#BC3C29"
bar_colour <- "#0072B5"
orange_colour <- "#E18727"
green_colour <- "#20854E"

input_d2 <- choose_file("D2")
input_d3 <- choose_file("D3")
input_d4 <- choose_file("D4")
input_d5 <- choose_file("D5")
input_d6 <- choose_file("D6")
input_d7 <- choose_file("D7")

d2_raw <- read_xlsx(input_d2)
d3_raw <- read_xlsx(input_d3)
d4_raw <- read_xlsx(input_d4)
d5_raw <- read_xlsx(input_d5)
d6_raw <- read_xlsx(input_d6)
d7_raw <- read_xlsx(input_d7)

d3_pct_columns <- names(d3_raw)[str_detect(names(d3_raw), "_pct$")]
d3_invalid_pct <- sum(vapply(d3_raw[d3_pct_columns], function(x) {
  values <- suppressWarnings(as.numeric(x))
  sum(!is.na(values) & (values < 0 | values > 110))
}, numeric(1)))
if (d3_invalid_pct > 0) {
  stop("D3 contains percentage values outside the prespecified 0-110% analytical range; clean the finalized data before modeling.")
}

d2_batch <- tibble(
  finished_batch = norm_batch(d2_raw$batch_no),
  d2_record_date = as.Date(d2_raw$production_date),
  finished_coated_tablet_weight_g = as.numeric(d2_raw$coated_tablet_weight_g),
  disintegration_time_min = as.numeric(d2_raw$disintegration_time_min),
  finished_active_content_mg_per_tablet = as.numeric(d2_raw$active_content_mg_per_tablet),
  dosage_strength = str_replace_all(as.character(d2_raw$dosage_strength), "\\s+", "")
) |>
  filter(dosage_strength == "0.8g", finished_batch != "") |>
  group_by(finished_batch) |>
  summarise(
    latest_record_issue = as.integer(disintegration_time_min[which.max(d2_record_date)] > 10),
    disintegration_issue = as.integer(any(disintegration_time_min > 10, na.rm = TRUE)),
    d2_observation_date = max(d2_record_date, na.rm = TRUE),
    finished_coated_tablet_weight_g = safe_mean(finished_coated_tablet_weight_g),
    disintegration_time_min = safe_mean(disintegration_time_min),
    finished_active_content_mg_per_tablet = safe_mean(finished_active_content_mg_per_tablet),
    d2_record_n = n(),
    .groups = "drop"
  )

d3_feature_map <- tibble::tribble(
  ~feature, ~label, ~source_col,
  "coating_abraded_rate_permille", "Abraded-tablet rate during coating (‰)", "coating_abraded_rate_permille",
  "coating_abraded_weight_g", "Abraded-tablet weight during coating (g)", "coating_abraded_weight_g",
  "blend_powder_i_mass_balance_pct", "Blend powder I mass balance (%)", "blend_powder_i_mass_balance_pct",
  "blend_powder_i_yield_pct", "Blend powder I yield (%)", "blend_powder_i_yield_pct",
  "compression_broken_tablet_rate_permille", "Broken-tablet rate during compression (‰)", "compression_broken_tablet_rate_permille",
  "compression_broken_tablet_weight_g", "Broken-tablet weight during compression (g)", "compression_broken_tablet_weight_g",
  "coated_tablet_hardness_mean_n", "Coated tablet hardness (N)", "coated_tablet_hardness_mean_n",
  "coated_tablet_weight_mean_g", "Coated tablet weight (g)", "coated_tablet_weight_mean_g",
  "coating_mass_balance_pct", "Coating mass balance (%)", "coating_mass_balance_pct",
  "coating_yield_pct", "Coating yield (%)", "coating_yield_pct",
  "compression_hardness_mean_n", "Compression hardness (N)", "compression_hardness_mean_n",
  "compression_mass_balance_pct", "Compression mass balance (%)", "compression_mass_balance_pct",
  "compression_tablet_weight_mean_g", "Compression tablet weight (g)", "compression_tablet_weight_mean_g",
  "compression_yield_pct", "Compression yield (%)", "compression_yield_pct",
  "core_tablet_weight_mean_g", "Core tablet weight (g)", "core_tablet_weight_mean_g",
  "final_blend_lt_100_mesh_pct", "Final blend <100 mesh (%)", "final_blend_lt_100_mesh_pct",
  "final_blend_mass_balance_pct", "Final-blend mass balance (%)", "final_blend_mass_balance_pct",
  "final_blend_moisture_pct", "Final-blend moisture (%)", "final_blend_moisture_pct",
  "final_blend_yield_pct", "Final-blend yield (%)", "final_blend_yield_pct",
  "final_product_mass_balance_pct", "Final-product mass balance (%)", "final_product_mass_balance_pct",
  "final_product_yield_pct", "Final-product yield (%)", "final_product_yield_pct",
  "granulation_discharge_moisture_pct_mean", "Granulation moisture, mean (%)", "granulation_discharge_moisture_pct",
  "milling_mass_balance_pct", "Milling mass balance (%)", "milling_mass_balance_pct",
  "coating_missing_tablet_rate_permille", "Missing-tablet rate during coating (‰)", "coating_missing_tablet_rate_permille",
  "coating_missing_tablet_weight_g", "Missing-tablet weight during coating (g)", "coating_missing_tablet_weight_g"
)

d3_base <- tibble(
  finished_batch = norm_batch(d3_raw$batch_no),
  mes_production_date = as.Date(d3_raw$production_date),
  extract_batch_raw = norm_batch(d3_raw$jwxs_extract_powder_batch_no),
  yam_batch_raw = norm_batch(d3_raw$yam_powder_luoting_batch_no)
)

for (i in seq_len(nrow(d3_feature_map))) {
  feature <- d3_feature_map$feature[i]
  col <- d3_feature_map$source_col[i]
  if (col == "granulation_discharge_moisture_pct") {
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
  extract_batch = norm_batch(d4_raw$batch_no),
  extract_moisture_pct = as.numeric(d4_raw$moisture_pct),
  extract_total_ash_pct = as.numeric(d4_raw$total_ash_pct),
  extract_extract_pct = as.numeric(d4_raw$extractives_pct),
  extract_hesperidin_mg_g = as.numeric(d4_raw$hesperidin_content_mg_per_g)
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
  extract_batch = norm_batch(d5_raw$extract_powder_batch_no),
  material_type = as.character(d5_raw$material_type) |> str_squish(),
  raw_batch = as.character(d5_raw$material_batch_no) |> str_squish(),
  raw_order = suppressWarnings(as.numeric(d5_raw$material_seq))
) |>
  filter(material_type == "\u9648\u76ae" | str_to_lower(material_type) %in% c("chenpi", "aged tangerine peel"))

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
  chenpi_batch = norm_batch(d6_raw$batch_no),
  chenpi_origin = as.character(d6_raw$origin) |> str_squish(),
  chenpi_moisture_pct = as.numeric(d6_raw$moisture_pct),
  chenpi_hesperidin_pct = as.numeric(d6_raw$hesperidin_pct),
  chenpi_impurities_pct = as.numeric(d6_raw$impurities_pct)
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
  yam_batch = norm_batch(d7_raw$batch_no),
  yam_supplier = as.character(d7_raw$supplier_name) |> str_squish(),
  yam_input_kg = as.numeric(d7_raw$input_kg),
  yam_rejected_material_weight_kg = as.numeric(d7_raw$rejected_material_weight_kg),
  yam_rejected_material_rate_pct = as.numeric(d7_raw$rejected_material_rate_pct),
  yam_process_moisture_mean_pct = vapply(d7_raw$process_moisture_values_pct, parse_numeric_mean, numeric(1)),
  yam_through_100_mesh_mean_pct = vapply(d7_raw$through_100_mesh_pct, parse_numeric_mean, numeric(1)),
  yam_through_120_mesh_mean_pct = vapply(d7_raw$through_120_mesh_pct, parse_numeric_mean, numeric(1)),
  yam_yield_pct = as.numeric(d7_raw$yield_pct),
  yam_mass_balance_pct = as.numeric(d7_raw$mass_balance_pct)
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
  inner_join(d3_base |> select(-extract_batch_raw, -yam_batch_raw), by = "finished_batch") |>
  left_join(extract_by_finished, by = "finished_batch") |>
  left_join(chenpi_by_finished, by = "finished_batch") |>
  left_join(yam_by_finished, by = "finished_batch") |>
  mutate(
    production_date = mes_production_date,
    production_month = format(production_date, "%Y-%m"),
    old_abnormal_window = production_month %in% c("2024-08", "2024-09", "2026-01", "2026-02")
  ) |>
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

time_cutoff <- "2026-03"
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

performance_plot_df <- perf_long |>
  filter(metric %in% c("AUC", "PR_AUC"), model != "XGBoost extended candidates")
performance_plot <- ggplot(performance_plot_df, aes(x = value, y = model, colour = adjustment, shape = adjustment)) +
  geom_point(size = 4, stroke = 1.1) +
  facet_wrap(~metric, nrow = 1, labeller = as_labeller(c(AUC = "AUC", PR_AUC = "PR-AUC"))) +
  scale_colour_manual(values = c("No month adjustment" = bar_colour, "Month-adjusted" = trend_colour)) +
  scale_shape_manual(values = c("No month adjustment" = 16, "Month-adjusted" = 1)) +
  scale_x_continuous(limits = c(0.30, 1.00), breaks = seq(0.4, 1.0, 0.2)) +
  labs(x = "Five-fold cross-validated performance", y = NULL, colour = NULL, shape = NULL) +
  theme(
    axis.text.y = element_text(size = 14, colour = "black"),
    legend.position = "top",
    legend.text = element_text(size = 15, colour = "black"),
    legend.key.width = grid::unit(1.15, "cm"),
    legend.key.height = grid::unit(0.55, "cm"),
    legend.box.margin = margin(0, 0, 8, 0),
    panel.spacing = grid::unit(1.2, "cm")
  )
save_plot_dual(performance_plot, "01_layered_model_performance", 13.5, 7.6)

coef_plot_df <- elastic_net_coefficients |>
  filter(!str_detect(term, "^production_month"), term_type == "Value") |>
  slice_max(abs_coefficient, n = 15) |>
  mutate(
    plot_label = ifelse(term_type == "Missingness indicator", paste0(label, " missing"), label),
    plot_label = make.unique(plot_label, sep = " "),
    plot_label = factor(plot_label, levels = rev(plot_label))
  )

coef_plot <- ggplot(coef_plot_df, aes(x = coefficient, y = plot_label, fill = coefficient > 0)) +
  geom_col(width = 0.66, colour = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, linewidth = 0.8, colour = "black") +
  scale_fill_manual(values = c(`TRUE` = trend_colour, `FALSE` = bar_colour), guide = "none") +
  labs(x = "Elastic-net coefficient", y = "Model variable") +
  theme(
    axis.text.y = element_text(size = 14, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black")
  )
save_plot_dual(coef_plot, "02_elastic_net_core_coefficients", 12.5, 7.6)

shap_plot_df <- shap_importance |>
  filter(base_variable != "production_month") |>
  slice_max(mean_abs_shap, n = 20) |>
  mutate(label = factor(label, levels = rev(label)))

shap_plot <- ggplot(shap_plot_df, aes(x = mean_abs_shap, y = label)) +
  geom_col(width = 0.66, fill = bar_colour, colour = "black", linewidth = 0.25) +
  labs(x = "Mean absolute SHAP value", y = "Model variable") +
  theme(
    axis.text.y = element_text(size = 14, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black")
  )
save_plot_dual(shap_plot, "03_xgboost_shap_importance", 12.5, 7.6)

missing_plot_df <- missing_summary |>
  filter(variable %in% modelable_all_vars) |>
  group_by(layer) |>
  summarise(
    variables = n(),
    median_missing_pct = median(missing_pct),
    max_missing_pct = max(missing_pct),
    .groups = "drop"
  ) |>
  mutate(
    layer_label = case_when(
      layer == "Finished-product MES" ~ "Finished-product MES",
      layer == "Finished-product quality" ~ "Finished-product quality",
      layer == "Chinese yam powder MES" ~ "Chinese yam powder MES",
      layer == "Chenpi quality" ~ "Chenpi quality",
      layer == "Extract-powder quality" ~ "Extract-powder quality",
      TRUE ~ layer
    )
  )

missing_plot <- ggplot(missing_plot_df, aes(x = median_missing_pct, y = reorder(layer_label, median_missing_pct))) +
  geom_col(width = 0.64, fill = bar_colour, colour = "black", linewidth = 0.25) +
  geom_text(
    aes(label = paste0("n = ", variables)),
    hjust = -0.18,
    family = "Arial",
    size = 7.2
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(x = "Median missingness (%)", y = "Data layer") +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 16, margin = margin(t = 8), color = "black"),
    axis.title.y = element_text(size = 16, margin = margin(r = 8), color = "black"),
    plot.margin = margin(8, 34, 10, 12)
  )
save_plot_dual(missing_plot, "04_model_variable_missingness_by_layer", 11.8, 4.8)

wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(low = max(0, centre - half), high = min(1, centre + half))
}

d2_monthly_summary <- d2_batch |>
  mutate(
    production_month = format(d2_observation_date, "%Y-%m"),
    month_date = as.Date(paste0(production_month, "-01"))
  ) |>
  group_by(production_month, month_date) |>
  summarise(n = n(), issue_n = sum(disintegration_issue), issue_rate = mean(disintegration_issue), .groups = "drop") |>
  rowwise() |>
  mutate(ci_low = wilson_ci(issue_n, n)[1], ci_high = wilson_ci(issue_n, n)[2]) |>
  ungroup()

linked_monthly_summary <- joint |>
  group_by(production_month) |>
  summarise(n = n(), issue_n = sum(disintegration_issue), issue_rate = mean(disintegration_issue), .groups = "drop") |>
  rowwise() |>
  mutate(ci_low = wilson_ci(issue_n, n)[1], ci_high = wilson_ci(issue_n, n)[2]) |>
  ungroup() |>
  mutate(month_date = as.Date(paste0(production_month, "-01")))

d2_window_summary <- d2_batch |>
  mutate(
    production_month = format(d2_observation_date, "%Y-%m"),
    window = case_when(
      production_month %in% c("2024-08", "2024-09") ~ "Aug-Sep 2024",
      production_month %in% c("2026-01", "2026-02") ~ "Jan-Feb 2026",
      TRUE ~ "Other months"
    ),
    window = factor(window, levels = c("Aug-Sep 2024", "Jan-Feb 2026", "Other months"))
  ) |>
  group_by(window) |>
  summarise(n = n(), issue_n = sum(disintegration_issue), issue_rate = mean(disintegration_issue), .groups = "drop") |>
  rowwise() |>
  mutate(ci_low = wilson_ci(issue_n, n)[1], ci_high = wilson_ci(issue_n, n)[2]) |>
  ungroup()

screen_one <- function(v) {
  x <- suppressWarnings(as.numeric(joint[[v]]))
  y <- joint$disintegration_time_min
  g <- joint$disintegration_issue
  ok <- is.finite(x) & is.finite(y) & !is.na(g)
  if (sum(ok) < 20 || length(unique(x[ok])) < 3 || length(unique(g[ok])) < 2) {
    return(tibble(variable = v, available_n = sum(ok), spearman_rho = NA_real_, spearman_p = NA_real_,
                  median_issue = NA_real_, median_nonissue = NA_real_, wilcoxon_p = NA_real_))
  }
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  wt <- suppressWarnings(wilcox.test(x[ok] ~ g[ok], exact = FALSE))
  tibble(
    variable = v,
    available_n = sum(ok),
    spearman_rho = unname(ct$estimate),
    spearman_p = ct$p.value,
    median_issue = median(x[ok & g == 1], na.rm = TRUE),
    median_nonissue = median(x[ok & g == 0], na.rm = TRUE),
    wilcoxon_p = wt$p.value
  )
}

univariate_screening <- bind_rows(lapply(modelable_all_vars, screen_one)) |>
  mutate(
    spearman_fdr = p.adjust(spearman_p, method = "BH"),
    wilcoxon_fdr = p.adjust(wilcoxon_p, method = "BH"),
    median_difference = median_issue - median_nonissue
  ) |>
  left_join(label_map, by = "variable") |>
  arrange(wilcoxon_fdr, spearman_fdr)

endpoint_sensitivity <- tibble(
  definition = c("Any repeated QC record >10 min", "Latest QC record >10 min"),
  issue_n = c(sum(joint$disintegration_issue), sum(joint$latest_record_issue)),
  total_n = nrow(joint)
) |>
  mutate(issue_rate = issue_n / total_n)

run_threshold_sensitivity <- function(threshold) {
  dat <- joint
  dat$disintegration_issue <- as.integer(dat$disintegration_time_min > threshold)
  random_pred <- cv_glmnet_predict(dat, modelable_core_vars, alpha = 0.5, k = 5, include_month = FALSE)
  random_metrics <- metrics_binary(dat$disintegration_issue, random_pred) |>
    mutate(
      threshold_min = threshold,
      validation = "Random five-fold CV",
      train_n = nrow(dat),
      test_n = nrow(dat),
      test_issue_n = sum(dat$disintegration_issue)
    )

  train_dat <- dat |> filter(production_month < time_cutoff)
  test_dat <- dat |> filter(production_month >= time_cutoff)
  if (length(unique(train_dat$disintegration_issue)) == 2 && length(unique(test_dat$disintegration_issue)) == 2) {
    mats <- impute_train_test(train_dat, test_dat, modelable_core_vars, sort(unique(dat$production_month)), include_month = FALSE)
    fit <- fit_cv_glmnet_safe(mats$x_train, train_dat$disintegration_issue, alpha = 0.5)
    if (!is.null(fit)) {
      p <- as.numeric(predict(fit, newx = mats$x_test, s = "lambda.min", type = "response"))
      time_metrics <- metrics_binary(test_dat$disintegration_issue, p) |>
        mutate(
          threshold_min = threshold,
          validation = "Prospective Mar-Jul 2026",
          train_n = nrow(train_dat),
          test_n = nrow(test_dat),
          test_issue_n = sum(test_dat$disintegration_issue)
        )
    } else {
      time_metrics <- tibble(AUC = NA_real_, PR_AUC = NA_real_, Brier = NA_real_, threshold_min = threshold,
                             validation = "Prospective Mar-Jul 2026", train_n = nrow(train_dat), test_n = nrow(test_dat),
                             test_issue_n = sum(test_dat$disintegration_issue))
    }
  } else {
    time_metrics <- tibble(AUC = NA_real_, PR_AUC = NA_real_, Brier = NA_real_, threshold_min = threshold,
                           validation = "Prospective Mar-Jul 2026", train_n = nrow(train_dat), test_n = nrow(test_dat),
                           test_issue_n = sum(test_dat$disintegration_issue))
  }
  bind_rows(random_metrics, time_metrics)
}

threshold_sensitivity <- bind_rows(lapply(c(9, 10, 11), run_threshold_sensitivity)) |>
  select(threshold_min, validation, train_n, test_n, test_issue_n, AUC, PR_AUC, Brier)

finished_outcome <- joint |>
  select(finished_batch, disintegration_issue, disintegration_time_min) |>
  distinct()

extract_batch_outcomes <- d3_extract_long |>
  inner_join(finished_outcome, by = "finished_batch") |>
  inner_join(d4_batch, by = "extract_batch") |>
  distinct(extract_batch, finished_batch, .keep_all = TRUE) |>
  group_by(extract_batch) |>
  summarise(
    descendant_finished_n = n_distinct(finished_batch),
    descendant_issue_n = sum(disintegration_issue),
    descendant_issue_rate = mean(disintegration_issue),
    descendant_disintegration_mean = mean(disintegration_time_min),
    across(c(extract_moisture_pct, extract_total_ash_pct, extract_extract_pct, extract_hesperidin_mg_g), safe_mean),
    .groups = "drop"
  )

chenpi_batch_outcomes <- trace_long |>
  inner_join(d3_extract_long, by = "extract_batch", relationship = "many-to-many") |>
  inner_join(finished_outcome, by = "finished_batch") |>
  inner_join(d6_batch, by = "chenpi_batch") |>
  distinct(chenpi_batch, finished_batch, .keep_all = TRUE) |>
  group_by(chenpi_batch) |>
  summarise(
    descendant_finished_n = n_distinct(finished_batch),
    descendant_issue_n = sum(disintegration_issue),
    descendant_issue_rate = mean(disintegration_issue),
    descendant_disintegration_mean = mean(disintegration_time_min),
    across(c(chenpi_moisture_pct, chenpi_hesperidin_pct, chenpi_impurities_pct), safe_mean),
    .groups = "drop"
  )

yam_batch_outcomes <- d3_yam_long |>
  inner_join(finished_outcome, by = "finished_batch") |>
  inner_join(d7_batch, by = "yam_batch") |>
  distinct(yam_batch, finished_batch, .keep_all = TRUE) |>
  group_by(yam_batch) |>
  summarise(
    descendant_finished_n = n_distinct(finished_batch),
    descendant_issue_n = sum(disintegration_issue),
    descendant_issue_rate = mean(disintegration_issue),
    descendant_disintegration_mean = mean(disintegration_time_min),
    across(c(yam_rejected_material_weight_kg, yam_rejected_material_rate_pct, yam_process_moisture_mean_pct,
             yam_through_120_mesh_mean_pct, yam_yield_pct, yam_mass_balance_pct), safe_mean),
    .groups = "drop"
  )

screen_upstream_batch <- function(df, vars, layer_name, label_values) {
  out <- lapply(vars, function(v) {
    x <- as.numeric(df[[v]])
    y <- as.numeric(df$descendant_issue_rate)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 10 || length(unique(x[ok])) < 3) {
      return(tibble(variable = v, batch_n = sum(ok), rho = NA_real_, p_value = NA_real_))
    }
    ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
    tibble(variable = v, batch_n = sum(ok), rho = unname(ct$estimate), p_value = ct$p.value)
  }) |>
    bind_rows() |>
    mutate(layer = layer_name, label = unname(label_values[variable]))
  out
}

extract_labels <- c(
  extract_moisture_pct = "Extract-powder moisture (%)", extract_total_ash_pct = "Extract-powder total ash (%)",
  extract_extract_pct = "Extract-powder extract (%)", extract_hesperidin_mg_g = "Extract-powder hesperidin (mg/g)"
)
chenpi_labels <- c(
  chenpi_moisture_pct = "Chenpi moisture (%)", chenpi_hesperidin_pct = "Chenpi hesperidin (%)",
  chenpi_impurities_pct = "Chenpi impurities (%)"
)
yam_labels <- c(
  yam_rejected_material_weight_kg = "Rejected material weight (kg)", yam_rejected_material_rate_pct = "Rejected material rate (%)",
  yam_process_moisture_mean_pct = "Process moisture mean (%)", yam_through_120_mesh_mean_pct = "Through 120-mesh mean (%)",
  yam_yield_pct = "Yam powder yield (%)", yam_mass_balance_pct = "Yam powder mass balance (%)"
)

upstream_batch_screening <- bind_rows(
  screen_upstream_batch(extract_batch_outcomes, names(extract_labels), "Extract-powder batch", extract_labels),
  screen_upstream_batch(chenpi_batch_outcomes, names(chenpi_labels), "Chenpi batch", chenpi_labels),
  screen_upstream_batch(yam_batch_outcomes, names(yam_labels), "Yam powder batch", yam_labels)
) |>
  mutate(fdr = p.adjust(p_value, method = "BH")) |>
  arrange(fdr)

window_rects <- tibble(
  xmin = as.Date(c("2024-08-01", "2026-01-01")),
  xmax = as.Date(c("2024-09-30", "2026-02-28")),
  xmid = as.Date(c("2024-09-01", "2026-01-31")),
  label = c("Window 1", "Window 2")
)

monthly_plot <- ggplot(d2_monthly_summary, aes(month_date, issue_rate)) +
  geom_rect(
    data = window_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = trend_colour, alpha = 0.08
  ) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#EAF2F8", colour = NA) +
  geom_line(colour = bar_colour, linewidth = 1.1) +
  geom_point(aes(shape = issue_n > 0), fill = "white", colour = bar_colour, size = 3, stroke = 1) +
  geom_text(
    data = window_rects,
    aes(x = xmid, y = 0.98, label = label),
    inherit.aes = FALSE, family = "Arial", size = 5, colour = trend_colour
  ) +
  geom_hline(yintercept = 0.10, linetype = "dashed", colour = point_colour, linewidth = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 1), guide = "none") +
  labs(x = "Finished-product quality-observation month (D2)", y = "Batches with disintegration time >10 min (%)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot_dual(monthly_plot, "05_monthly_disintegration_issue_rate", 12.8, 7.2)

linked_monthly_plot <- ggplot(linked_monthly_summary, aes(month_date, issue_rate)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#EAF2F8", colour = NA) +
  geom_line(colour = bar_colour, linewidth = 1.1) +
  geom_point(aes(shape = issue_n > 0), fill = "white", colour = bar_colour, size = 3, stroke = 1) +
  geom_hline(yintercept = 0.10, linetype = "dashed", colour = point_colour, linewidth = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 1), guide = "none") +
  labs(x = "MES production month (D3-linked subset)", y = "Batches with disintegration time >10 min (%)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot_dual(linked_monthly_plot, "05b_D3_linked_monthly_issue_rate", 12.8, 7.2)

window_plot <- ggplot(d2_window_summary, aes(x = window, y = issue_rate)) +
  geom_col(width = 0.62, fill = bar_colour, colour = "black", linewidth = 0.25) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.14, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%d/%d (%.1f%%)", issue_n, n, 100 * issue_rate)), vjust = -0.6, size = 5, family = "Arial") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Batches with disintegration time >10 min (%)") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_plot_dual(window_plot, "06_original_window_concentration", 8.5, 6.4)

screen_plot_df <- univariate_screening |>
  filter(layer == "Finished-product MES", is.finite(wilcoxon_fdr)) |>
  arrange(wilcoxon_fdr, desc(abs(spearman_rho))) |>
  slice_head(n = 15) |>
  mutate(score = pmin(-log10(pmax(wilcoxon_fdr, 1e-12)), 12), label = factor(label, levels = rev(label)))
screen_plot <- ggplot(screen_plot_df, aes(score, label)) +
  geom_col(fill = orange_colour, colour = "black", linewidth = 0.25, width = 0.66) +
  geom_vline(xintercept = -log10(0.05), linetype = "dashed", colour = point_colour) +
  labs(x = expression(-log[10](FDR)), y = "Finished-product MES variable") +
  theme(axis.text.y = element_text(size = 13))
save_plot_dual(screen_plot, "07_mes_univariate_screening", 11.5, 7.6)

threshold_plot_df <- threshold_sensitivity |>
  pivot_longer(c(AUC, PR_AUC), names_to = "metric", values_to = "value") |>
  mutate(threshold_label = factor(paste0(">", threshold_min, " min"), levels = c(">9 min", ">10 min", ">11 min")))
threshold_plot <- ggplot(threshold_plot_df, aes(x = threshold_label, y = value, colour = validation, shape = validation)) +
  geom_point(position = position_dodge(width = 0.35), size = 4, stroke = 1.1) +
  facet_wrap(~metric, nrow = 1, labeller = as_labeller(c(AUC = "AUC", PR_AUC = "PR-AUC"))) +
  scale_colour_manual(values = c("Random five-fold CV" = bar_colour, "Prospective Mar-Jul 2026" = trend_colour)) +
  scale_shape_manual(values = c("Random five-fold CV" = 16, "Prospective Mar-Jul 2026" = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Disintegration issue definition", y = "Model performance", colour = NULL, shape = NULL) +
  theme(legend.position = "top")
save_plot_dual(threshold_plot, "08_threshold_and_time_validation_sensitivity", 10.8, 6.4)

upstream_plot_df <- upstream_batch_screening |>
  filter(is.finite(rho)) |>
  arrange(rho) |>
  mutate(plot_label = factor(paste0(label, "  (n=", batch_n, ")"), levels = paste0(label, "  (n=", batch_n, ")")),
         significant = fdr < 0.05)
upstream_plot <- ggplot(upstream_plot_df, aes(x = rho, y = plot_label, fill = significant)) +
  geom_col(width = 0.66, colour = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.7) +
  scale_fill_manual(
    values = c(`TRUE` = trend_colour, `FALSE` = "#D9E7F2"),
    breaks = c(TRUE, FALSE),
    labels = c("Yes", "No")
  ) +
  labs(x = "Spearman correlation with descendant-batch issue rate", y = NULL, fill = "FDR < 0.05") +
  theme(axis.text.y = element_text(size = 13), legend.position = "top")
save_plot_dual(upstream_plot, "09_upstream_batch_level_associations", 12.8, 7.8)

write_csv <- function(x, filename) {
  write.csv(as.data.frame(x), file.path(tables_dir, filename), row.names = FALSE, fileEncoding = "UTF-8")
}
write_csv(linkage_summary, "01_linkage_summary.csv")
write_csv(variable_audit, "02_variable_audit.csv")
write_csv(joint, "03_joint_model_matrix.csv")
write_csv(bind_rows(model_performance, xgb_performance), "04_model_performance.csv")
write_csv(time_split_performance, "05_time_split_performance.csv")
write_csv(elastic_net_coefficients, "06_elastic_net_coefficients.csv")
write_csv(shap_importance, "07_xgboost_shap_importance.csv")
write_csv(univariate_screening, "08_univariate_screening.csv")
write_csv(d2_monthly_summary, "09_monthly_issue_rate.csv")
write_csv(linked_monthly_summary, "09b_D3_linked_monthly_issue_rate.csv")
write_csv(d2_window_summary, "10_original_window_comparison.csv")
write_csv(endpoint_sensitivity, "11_endpoint_sensitivity.csv")
write_csv(prediction_export, "12_cross_validated_predictions.csv")
write_csv(complete_case_summary, "13_complete_case_summary.csv")
write_csv(threshold_sensitivity, "14_threshold_sensitivity.csv")
write_csv(upstream_batch_screening, "15_upstream_batch_level_screening.csv")
write_csv(extract_batch_outcomes, "16_extract_batch_descendant_outcomes.csv")
write_csv(chenpi_batch_outcomes, "17_chenpi_batch_descendant_outcomes.csv")
write_csv(yam_batch_outcomes, "18_yam_batch_descendant_outcomes.csv")

writeLines(capture.output(sessionInfo()), file.path(docs_dir, "R_session_info.txt"), useBytes = TRUE)
message("Saved reanalysis tables: ", tables_dir)
message("Saved publication figures: ", figures_dir)
