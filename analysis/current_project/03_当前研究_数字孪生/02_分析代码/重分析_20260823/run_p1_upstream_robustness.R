options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(pdftools)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

set.seed(20260823)

data_dir <- Sys.getenv("TCM_P1_DATA_DIR", unset = "")
joint_path <- Sys.getenv("TCM_P1_JOINT", unset = "")
base_dir <- Sys.getenv("TCM_P1_BASE_DIR", unset = "")
p0_dir <- Sys.getenv("TCM_P1_P0_DIR", unset = "")
output_dir <- Sys.getenv("TCM_P1_OUTPUT", unset = "")
if (any(c(data_dir, joint_path, base_dir, p0_dir, output_dir) == "")) {
  stop("TCM_P1_DATA_DIR, TCM_P1_JOINT, TCM_P1_BASE_DIR, TCM_P1_P0_DIR, and TCM_P1_OUTPUT are required.")
}

tables_dir <- file.path(output_dir, "tables")
figures_dir <- file.path(output_dir, "figures")
docs_dir <- file.path(output_dir, "docs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

nejm <- c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD")
theme_set(theme_classic(base_family = "Arial", base_size = 15))
theme_update(
  axis.title = element_text(size = 17, colour = "black"),
  axis.text = element_text(size = 14, colour = "black"),
  strip.text = element_text(size = 15, colour = "black", face = "bold"),
  legend.position = "top",
  legend.title = element_text(size = 14, colour = "black"),
  legend.text = element_text(size = 14, colour = "black"),
  plot.title = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 16, 10, 12)
)

save_plot_dual <- function(plot_obj, stem, width, height) {
  pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figures_dir, paste0(stem, "_preview.png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")
  suppressWarnings(pdftools::pdf_convert(pdf_path, format = "png", pages = 1, dpi = 220, filenames = png_path))
  if (!file.exists(pdf_path) || !file.exists(png_path) || file.info(pdf_path)$size == 0 || file.info(png_path)$size == 0) {
    stop("Empty figure output: ", stem)
  }
}

choose_file <- function(prefix) {
  files <- list.files(data_dir, pattern = paste0("^", prefix, ".*\\.xlsx$"), full.names = TRUE)
  if (length(files) == 0) stop("Missing staged input for ", prefix)
  files[1]
}

norm_batch <- function(x) {
  s <- as.character(x) |> str_trim() |> str_replace("\\.0$", "")
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
  out <- str_split(x, "[;；,，、\\s]+")[[1]] |> norm_batch()
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

wilson_ci <- function(x, n) {
  if (n == 0) return(c(low = NA_real_, high = NA_real_))
  z <- qnorm(0.975)
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(low = max(0, center - half), high = min(1, center + half))
}

bootstrap_spearman <- function(x, y, b = 2000, seed = 1) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 10 || length(unique(x)) < 3 || length(unique(y)) < 3) {
    return(c(rho = NA_real_, low = NA_real_, high = NA_real_, p = NA_real_))
  }
  test <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  set.seed(seed)
  boots <- replicate(b, {
    idx <- sample.int(length(x), length(x), replace = TRUE)
    suppressWarnings(cor(x[idx], y[idx], method = "spearman"))
  })
  c(
    rho = unname(test$estimate),
    low = unname(quantile(boots, 0.025, na.rm = TRUE, names = FALSE)),
    high = unname(quantile(boots, 0.975, na.rm = TRUE, names = FALSE)),
    p = test$p.value
  )
}

partial_spearman_once <- function(x, y, z) {
  ok <- is.finite(x) & is.finite(y) & is.finite(z)
  x <- x[ok]
  y <- y[ok]
  z <- z[ok]
  if (length(x) < 10 || length(unique(x)) < 3 || length(unique(y)) < 3 || length(unique(z)) < 2) return(NA_real_)
  rx <- residuals(lm(rank(x, ties.method = "average") ~ rank(z, ties.method = "average")))
  ry <- residuals(lm(rank(y, ties.method = "average") ~ rank(z, ties.method = "average")))
  suppressWarnings(cor(rx, ry))
}

bootstrap_partial_spearman <- function(x, y, z, b = 2000, seed = 1) {
  ok <- is.finite(x) & is.finite(y) & is.finite(z)
  x <- x[ok]
  y <- y[ok]
  z <- z[ok]
  n <- length(x)
  rho <- partial_spearman_once(x, y, z)
  if (n < 10 || !is.finite(rho)) return(c(rho = NA_real_, low = NA_real_, high = NA_real_, p = NA_real_))
  t_stat <- rho * sqrt((n - 3) / max(1e-12, 1 - rho^2))
  p_value <- 2 * pt(abs(t_stat), df = n - 3, lower.tail = FALSE)
  set.seed(seed)
  boots <- replicate(b, {
    idx <- sample.int(n, n, replace = TRUE)
    partial_spearman_once(x[idx], y[idx], z[idx])
  })
  c(
    rho = rho,
    low = unname(quantile(boots, 0.025, na.rm = TRUE, names = FALSE)),
    high = unname(quantile(boots, 0.975, na.rm = TRUE, names = FALSE)),
    p = p_value
  )
}

d2_raw <- suppressWarnings(read_xlsx(choose_file("D2")))
d3_raw <- suppressWarnings(read_xlsx(choose_file("D3")))
d4_raw <- suppressWarnings(read_xlsx(choose_file("D4")))
d5_raw <- suppressWarnings(read_xlsx(choose_file("D5")))
d6_raw <- suppressWarnings(read_xlsx(choose_file("D6")))
d7_raw <- suppressWarnings(read_xlsx(choose_file("D7")))

joint <- read.csv(joint_path, check.names = FALSE, stringsAsFactors = FALSE) |>
  transmute(
    finished_batch = norm_batch(finished_batch),
    disintegration_issue = as.integer(disintegration_issue),
    disintegration_time_min = as.numeric(disintegration_time_min),
    mes_production_date = as.Date(mes_production_date),
    production_month = as.character(production_month)
  ) |>
  distinct(finished_batch, .keep_all = TRUE)

month_stats <- joint |>
  group_by(production_month) |>
  summarise(month_n = n(), month_issue_n = sum(disintegration_issue), month_issue_rate = mean(disintegration_issue), .groups = "drop")

finished_outcome <- joint |>
  left_join(month_stats, by = "production_month") |>
  mutate(
    expected_month_rate_loo = ifelse(month_n > 1, (month_issue_n - disintegration_issue) / (month_n - 1), month_issue_rate),
    month_residual = disintegration_issue - expected_month_rate_loo
  )

d3_base <- tibble(
  finished_batch = norm_batch(d3_raw$batch_no),
  extract_batch_raw = norm_batch(d3_raw$jwxs_extract_powder_batch_no),
  yam_batch_raw = norm_batch(d3_raw$yam_powder_luoting_batch_no)
) |>
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

d5_chenpi <- tibble(
  extract_batch = norm_batch(d5_raw$extract_powder_batch_no),
  material_type = as.character(d5_raw$material_type) |> str_squish(),
  raw_batch = as.character(d5_raw$material_batch_no) |> str_squish(),
  raw_order = suppressWarnings(as.numeric(d5_raw$material_seq))
)

extract_chenpi_batches <- function(x) {
  text <- as.character(x) |> str_squish()
  if (is.na(text) || text == "") return(character(0))
  str_extract_all(text, "\\d{7}(?:-\\d+)?")[[1]] |> norm_batch()
}

trace_long <- d5_chenpi |>
  mutate(chenpi_batch = lapply(raw_batch, extract_chenpi_batches)) |>
  unnest(chenpi_batch) |>
  mutate(extract_batch = norm_batch(extract_batch), chenpi_batch = norm_batch(chenpi_batch)) |>
  filter(extract_batch != "", chenpi_batch != "") |>
  distinct(material_type, extract_batch, chenpi_batch, raw_batch, raw_order)

# The Windows R locale on this host may render Chinese material-type labels as
# mojibake. We therefore identify the Chenpi material-type code as the D5 label
# having the largest exact batch-number overlap with D6, rather than comparing
# the rendered Chinese text. The locked-result reconciliation below verifies
# that this locale-safe route reproduces the original 32 linked batches.

d6_batch <- tibble(
  chenpi_batch = norm_batch(d6_raw$batch_no),
  chenpi_moisture_pct = as.numeric(d6_raw$moisture_pct),
  chenpi_hesperidin_pct = as.numeric(d6_raw$hesperidin_pct),
  chenpi_impurities_pct = as.numeric(d6_raw$impurities_pct)
) |>
  filter(chenpi_batch != "") |>
  group_by(chenpi_batch) |>
  summarise(across(where(is.numeric), safe_mean), .groups = "drop")

chenpi_material_type <- trace_long |>
  inner_join(d6_batch |> select(chenpi_batch), by = "chenpi_batch") |>
  count(material_type, name = "matched_rows") |>
  arrange(desc(matched_rows)) |>
  slice(1) |>
  pull(material_type)
trace_long <- trace_long |>
  filter(material_type == chenpi_material_type)

d7_batch <- tibble(
  yam_batch = norm_batch(d7_raw$batch_no),
  yam_rejected_material_weight_kg = as.numeric(d7_raw$rejected_material_weight_kg),
  yam_rejected_material_rate_pct = as.numeric(d7_raw$rejected_material_rate_pct),
  yam_process_moisture_mean_pct = vapply(d7_raw$process_moisture_values_pct, parse_numeric_mean, numeric(1)),
  yam_through_120_mesh_mean_pct = vapply(d7_raw$through_120_mesh_pct, parse_numeric_mean, numeric(1)),
  yam_yield_pct = as.numeric(d7_raw$yield_pct),
  yam_mass_balance_pct = as.numeric(d7_raw$mass_balance_pct)
) |>
  filter(yam_batch != "") |>
  group_by(yam_batch) |>
  summarise(across(where(is.numeric), safe_mean), .groups = "drop")

extract_long <- d3_extract_long |>
  inner_join(finished_outcome, by = "finished_batch") |>
  inner_join(d4_batch, by = "extract_batch") |>
  distinct(extract_batch, finished_batch, .keep_all = TRUE)

chenpi_long <- trace_long |>
  inner_join(d3_extract_long, by = "extract_batch", relationship = "many-to-many") |>
  inner_join(finished_outcome, by = "finished_batch") |>
  inner_join(d6_batch, by = "chenpi_batch") |>
  distinct(chenpi_batch, finished_batch, .keep_all = TRUE)

yam_long <- d3_yam_long |>
  inner_join(finished_outcome, by = "finished_batch") |>
  inner_join(d7_batch, by = "yam_batch") |>
  distinct(yam_batch, finished_batch, .keep_all = TRUE)

summarise_upstream <- function(df, id_col, vars, layer) {
  df |>
    group_by(.data[[id_col]]) |>
    summarise(
      descendant_finished_n = n_distinct(finished_batch),
      descendant_issue_n = sum(disintegration_issue),
      descendant_issue_rate = mean(disintegration_issue),
      descendant_month_residual_mean = mean(month_residual),
      descendant_expected_month_rate = mean(expected_month_rate_loo),
      descendant_month_n = n_distinct(production_month),
      across(all_of(vars), safe_mean),
      .groups = "drop"
    ) |>
    mutate(layer = layer)
}

extract_vars <- c("extract_moisture_pct", "extract_total_ash_pct", "extract_extract_pct", "extract_hesperidin_mg_g")
chenpi_vars <- c("chenpi_moisture_pct", "chenpi_hesperidin_pct", "chenpi_impurities_pct")
yam_vars <- c("yam_rejected_material_weight_kg", "yam_rejected_material_rate_pct", "yam_process_moisture_mean_pct", "yam_through_120_mesh_mean_pct", "yam_yield_pct", "yam_mass_balance_pct")

extract_batch_summary <- summarise_upstream(extract_long, "extract_batch", extract_vars, "Extract-powder batch")
chenpi_batch_summary <- summarise_upstream(chenpi_long, "chenpi_batch", chenpi_vars, "Chenpi batch")
yam_batch_summary <- summarise_upstream(yam_long, "yam_batch", yam_vars, "Yam-powder batch")

labels <- c(
  extract_moisture_pct = "Extract-powder moisture (%)",
  extract_total_ash_pct = "Extract-powder total ash (%)",
  extract_extract_pct = "Extract-powder extract (%)",
  extract_hesperidin_mg_g = "Extract-powder hesperidin (mg/g)",
  chenpi_moisture_pct = "Chenpi moisture (%)",
  chenpi_hesperidin_pct = "Chenpi hesperidin (%)",
  chenpi_impurities_pct = "Chenpi impurities (%)",
  yam_rejected_material_weight_kg = "Rejected material weight (kg)",
  yam_rejected_material_rate_pct = "Rejected material rate (%)",
  yam_process_moisture_mean_pct = "Process moisture mean (%)",
  yam_through_120_mesh_mean_pct = "Through 120-mesh mean (%)",
  yam_yield_pct = "Yam-powder yield (%)",
  yam_mass_balance_pct = "Yam-powder mass balance (%)"
)

screen_layer <- function(df, vars, layer, min_descendants, seed_offset) {
  dd <- df |> filter(descendant_finished_n >= min_descendants)
  rows <- list()
  rid <- 1L
  for (v in vars) {
    x <- as.numeric(dd[[v]])
    for (outcome_name in c("Unadjusted descendant issue rate", "Month-adjusted descendant residual", "Partial Spearman adjusted for month composition")) {
      if (outcome_name == "Unadjusted descendant issue rate") {
        y <- dd$descendant_issue_rate
        stats <- bootstrap_spearman(x, y, b = 2000, seed = 20260823 + seed_offset + rid)
        analysis_n <- sum(is.finite(x) & is.finite(y))
      } else if (outcome_name == "Month-adjusted descendant residual") {
        y <- dd$descendant_month_residual_mean
        stats <- bootstrap_spearman(x, y, b = 2000, seed = 20260823 + seed_offset + rid)
        analysis_n <- sum(is.finite(x) & is.finite(y))
      } else {
        y <- dd$descendant_issue_rate
        z <- dd$descendant_expected_month_rate
        stats <- bootstrap_partial_spearman(x, y, z, b = 2000, seed = 20260823 + seed_offset + rid)
        analysis_n <- sum(is.finite(x) & is.finite(y) & is.finite(z))
      }
      rows[[rid]] <- tibble(
        layer = layer,
        variable = v,
        label = unname(labels[v]),
        sensitivity_set = ifelse(min_descendants == 1, "All linked upstream batches", "At least 2 descendant finished-product batches"),
        min_descendants = min_descendants,
        batch_n = analysis_n,
        outcome = outcome_name,
        rho = stats[["rho"]],
        ci_low = stats[["low"]],
        ci_high = stats[["high"]],
        p_value = stats[["p"]]
      )
      rid <- rid + 1L
    }
  }
  bind_rows(rows)
}

screening_long <- bind_rows(
  screen_layer(extract_batch_summary, extract_vars, "Extract-powder batch", 1, 100),
  screen_layer(chenpi_batch_summary, chenpi_vars, "Chenpi batch", 1, 200),
  screen_layer(yam_batch_summary, yam_vars, "Yam-powder batch", 1, 300),
  screen_layer(extract_batch_summary, extract_vars, "Extract-powder batch", 2, 400),
  screen_layer(chenpi_batch_summary, chenpi_vars, "Chenpi batch", 2, 500),
  screen_layer(yam_batch_summary, yam_vars, "Yam-powder batch", 2, 600)
) |>
  group_by(sensitivity_set, outcome) |>
  mutate(fdr = p.adjust(p_value, method = "BH")) |>
  ungroup()

primary_wide <- screening_long |>
  filter(sensitivity_set == "All linked upstream batches") |>
  select(layer, variable, label, batch_n, outcome, rho, ci_low, ci_high, p_value, fdr) |>
  pivot_wider(
    names_from = outcome,
    values_from = c(batch_n, rho, ci_low, ci_high, p_value, fdr),
    names_glue = "{.value}_{outcome}"
  ) |>
  rename_with(~ str_replace_all(.x, "Unadjusted descendant issue rate", "unadjusted")) |>
  rename_with(~ str_replace_all(.x, "Month-adjusted descendant residual", "residual_adjusted")) |>
  rename_with(~ str_replace_all(.x, "Partial Spearman adjusted for month composition", "time_adjusted")) |>
  mutate(
    direction_retained = sign(rho_unadjusted) == sign(rho_time_adjusted),
    adjusted_ci_excludes_zero = ci_low_time_adjusted > 0 | ci_high_time_adjusted < 0,
    stability_class = case_when(
      layer == "Chenpi batch" & fdr_time_adjusted < 0.05 & adjusted_ci_excludes_zero ~ "Time-adjusted association retained; exploratory small-n layer",
      layer == "Chenpi batch" ~ "Not robust after time adjustment; exploratory small-n layer",
      fdr_time_adjusted < 0.05 & adjusted_ci_excludes_zero & direction_retained ~ "Time-adjusted association retained",
      direction_retained & abs(rho_time_adjusted) < 0.5 * abs(rho_unadjusted) ~ "Direction retained but materially attenuated",
      direction_retained ~ "Direction retained; uncertainty includes null",
      TRUE ~ "Direction changed after time adjustment"
    )
  ) |>
  arrange(fdr_time_adjusted)

base_upstream <- read.csv(file.path(base_dir, "15_upstream_batch_level_screening.csv"), stringsAsFactors = FALSE) |>
  select(variable, base_batch_n = batch_n, base_rho = rho)
reconciliation <- primary_wide |>
  left_join(base_upstream, by = "variable") |>
  transmute(
    variable, layer,
    batch_n_rebuilt = batch_n_unadjusted,
    batch_n_base = base_batch_n,
    batch_n_match = batch_n_unadjusted == base_batch_n,
    rho_rebuilt = rho_unadjusted,
    rho_base = base_rho,
    rho_abs_difference = abs(rho_unadjusted - base_rho),
    rho_match = rho_abs_difference < 1e-10
  )
write.csv(reconciliation, file.path(tables_dir, "05_upstream_reconciliation_with_locked_analysis.csv"), row.names = FALSE, na = "")
if (!all(reconciliation$batch_n_match) || !all(reconciliation$rho_match)) {
  print(reconciliation |> filter(!batch_n_match | !rho_match))
  stop("Rebuilt upstream screening does not reconcile with the locked clean-data analysis.")
}

descendant_summary <- bind_rows(
  extract_batch_summary |> transmute(layer, descendant_finished_n),
  chenpi_batch_summary |> transmute(layer, descendant_finished_n),
  yam_batch_summary |> transmute(layer, descendant_finished_n)
) |>
  group_by(layer) |>
  summarise(
    upstream_batch_n = n(),
    batches_with_2plus_descendants = sum(descendant_finished_n >= 2),
    descendant_n_min = min(descendant_finished_n),
    descendant_n_median = median(descendant_finished_n),
    descendant_n_mean = mean(descendant_finished_n),
    descendant_n_max = max(descendant_finished_n),
    .groups = "drop"
  )

write.csv(month_stats, file.path(tables_dir, "01_D3_month_issue_baseline.csv"), row.names = FALSE, na = "")
write.csv(descendant_summary, file.path(tables_dir, "02_upstream_descendant_count_summary.csv"), row.names = FALSE, na = "")
write.csv(screening_long, file.path(tables_dir, "03_upstream_bootstrap_screening_long.csv"), row.names = FALSE, na = "")
write.csv(primary_wide, file.path(tables_dir, "04_upstream_time_adjusted_robustness.csv"), row.names = FALSE, na = "")

# Figure 5 candidate: unadjusted versus time-adjusted upstream-batch associations.
plot_order <- primary_wide |>
  arrange(layer, rho_time_adjusted) |>
  mutate(plot_label = paste0(label, "  (n=", batch_n_time_adjusted, ")")) |>
  pull(plot_label)
plot_df <- screening_long |>
  filter(
    sensitivity_set == "All linked upstream batches",
    outcome %in% c("Unadjusted descendant issue rate", "Partial Spearman adjusted for month composition")
  ) |>
  mutate(
    plot_label = factor(paste0(label, "  (n=", batch_n, ")"), levels = plot_order),
    layer_display = recode(layer, `Chenpi batch` = "Chenpi", `Extract-powder batch` = "Extract powder", `Yam-powder batch` = "Yam powder"),
    significant = fdr < 0.05,
    outcome = recode(outcome, `Partial Spearman adjusted for month composition` = "Month-adjusted partial Spearman"),
    outcome = factor(outcome, levels = c("Unadjusted descendant issue rate", "Month-adjusted partial Spearman"))
  )
p_upstream <- ggplot(plot_df, aes(rho, plot_label, colour = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#666666", linewidth = 0.7) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.16, linewidth = 0.8, position = position_dodge(width = 0.48)) +
  geom_point(aes(shape = significant), size = 3.3, stroke = 1.0, position = position_dodge(width = 0.48)) +
  facet_grid(layer_display ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c("Unadjusted descendant issue rate" = nejm[1], "Month-adjusted partial Spearman" = nejm[2]), name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "FDR < 0.05", `FALSE` = "FDR >= 0.05"), name = NULL) +
  scale_x_continuous(limits = c(-0.75, 0.75), breaks = seq(-0.6, 0.6, 0.2), labels = number_format(accuracy = 0.1)) +
  labs(x = "Spearman correlation at the true upstream-batch level", y = NULL) +
  guides(shape = guide_legend(order = 1, nrow = 1), colour = guide_legend(order = 2, nrow = 1)) +
  theme(axis.text.y = element_text(size = 12.5), legend.position = "top", legend.box = "vertical", panel.spacing = unit(0.65, "lines"))
save_plot_dual(p_upstream, "Figure_5_upstream_time_adjusted_associations", 13.2, 9.0)

# MES-linkage coverage and issue-rate comparison by D2 quality-observation month.
d2_batch <- tibble(
  finished_batch = norm_batch(d2_raw$batch_no),
  observation_date = as.Date(d2_raw$production_date),
  disintegration_time_min = as.numeric(d2_raw$disintegration_time_min),
  dosage_strength = str_replace_all(as.character(d2_raw$dosage_strength), "\\s+", "")
) |>
  filter(dosage_strength == "0.8g", finished_batch != "") |>
  group_by(finished_batch) |>
  summarise(
    observation_date = max(observation_date, na.rm = TRUE),
    disintegration_issue = as.integer(any(disintegration_time_min > 10, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    linked_to_mes = finished_batch %in% joint$finished_batch,
    observation_month = format(observation_date, "%Y-%m"),
    month_date = as.Date(paste0(observation_month, "-01"))
  )

coverage_monthly <- d2_batch |>
  filter(observation_date >= as.Date("2025-04-01"), observation_date <= as.Date("2026-07-31")) |>
  group_by(observation_month, month_date) |>
  summarise(total_n = n(), linked_n = sum(linked_to_mes), coverage_rate = mean(linked_to_mes), .groups = "drop") |>
  mutate(
    ci_low = mapply(function(x, n) wilson_ci(x, n)[1], linked_n, total_n),
    ci_high = mapply(function(x, n) wilson_ci(x, n)[2], linked_n, total_n)
  )

issue_by_linkage <- d2_batch |>
  filter(observation_date >= as.Date("2025-04-01"), observation_date <= as.Date("2026-07-31")) |>
  group_by(observation_month, month_date, linked_to_mes) |>
  summarise(n = n(), issue_n = sum(disintegration_issue), issue_rate = mean(disintegration_issue), .groups = "drop") |>
  mutate(
    ci_low = mapply(function(x, nn) wilson_ci(x, nn)[1], issue_n, n),
    ci_high = mapply(function(x, nn) wilson_ci(x, nn)[2], issue_n, n)
  ) |>
  mutate(linkage_group = ifelse(linked_to_mes, "MES-linked", "Not MES-linked"))

write.csv(coverage_monthly, file.path(tables_dir, "06_monthly_MES_linkage_coverage.csv"), row.names = FALSE, na = "")
write.csv(issue_by_linkage, file.path(tables_dir, "07_monthly_issue_rate_by_MES_linkage.csv"), row.names = FALSE, na = "")

coverage_plot_df <- bind_rows(
  coverage_monthly |> transmute(month_date, panel = "MES linkage coverage", group = "All D2 target batches", value = coverage_rate, ci_low, ci_high),
  issue_by_linkage |> transmute(month_date, panel = "Issue rate by linkage status", group = linkage_group, value = issue_rate, ci_low, ci_high)
) |>
  mutate(panel = factor(panel, levels = c("MES linkage coverage", "Issue rate by linkage status")))

p_coverage <- ggplot(coverage_plot_df, aes(month_date, value, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  facet_grid(panel ~ ., scales = "free_y", switch = "y") +
  scale_colour_manual(values = c("All D2 target batches" = nejm[5], "MES-linked" = nejm[1], "Not MES-linked" = nejm[2]), name = NULL) +
  scale_fill_manual(values = c("All D2 target batches" = nejm[5], "MES-linked" = nejm[1], "Not MES-linked" = nejm[2]), name = NULL) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Finished-product quality-observation month (D2)", y = NULL) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "top",
    panel.spacing = unit(0.7, "lines")
  )
save_plot_dual(p_coverage, "Supplementary_Figure_MES_linkage_coverage", 12.8, 8.2)

# Update the evidence hierarchy table using time-adjusted upstream estimates.
p0_evidence <- read.csv(file.path(p0_dir, "06_main_table_evidence_hierarchy.csv"), stringsAsFactors = FALSE)
upstream_keep <- c("extract_hesperidin_mg_g", "yam_rejected_material_rate_pct", "extract_total_ash_pct", "extract_extract_pct", "yam_through_120_mesh_mean_pct", "chenpi_hesperidin_pct")
adjusted_evidence <- primary_wide |>
  filter(variable %in% upstream_keep) |>
  transmute(
    evidence_layer = "Trace-supported upstream association",
    signal = label,
    analysis_unit_n = batch_n_time_adjusted,
    effect_measure = "Partial Spearman rho adjusted for descendant month composition",
    estimate = rho_time_adjusted,
    uncertainty = paste0("95% batch-bootstrap CI ", formatC(ci_low_time_adjusted, digits = 3, format = "f"), " to ", formatC(ci_high_time_adjusted, digits = 3, format = "f"), "; FDR=", formatC(fdr_time_adjusted, format = "g", digits = 3)),
    stability = ifelse(
      variable == "extract_hesperidin_mg_g",
      "Retained in partial-Spearman and >=2-descendant sensitivity analyses; not retained by residual-based adjustment",
      stability_class
    ),
    interpretation_boundary = "Month-composition-adjusted investigation hypothesis; not a causal root cause"
  )
final_evidence <- bind_rows(
  p0_evidence |> filter(evidence_layer != "Trace-supported upstream association"),
  adjusted_evidence
)
write.csv(final_evidence, file.path(tables_dir, "08_final_evidence_hierarchy_with_P1.csv"), row.names = FALSE, na = "")

writeLines(capture.output(sessionInfo()), file.path(docs_dir, "R_session_info.txt"), useBytes = TRUE)
writeLines(c(
  "# P1 upstream robustness analysis notes",
  "",
  "- Analysis date: 2026-08-23.",
  "- Primary analytical unit: true upstream batch; descendant finished-product records were not treated as independent upstream samples.",
  "- Primary calendar adjustment: partial Spearman correlation at the upstream-batch level, controlling each batch's expected issue rate from its descendant-month composition.",
  "- Sensitivity calendar adjustment: each descendant outcome was residualized against its leave-one-out D3 production-month issue rate; residuals were averaged within upstream batch.",
  "- Uncertainty: 2,000 percentile bootstrap resamples of upstream batches.",
  "- Multiple testing: Benjamini-Hochberg FDR within each outcome definition and sensitivity set.",
  "- Sensitivity: all linked upstream batches versus batches with at least two descendant finished-product batches.",
  "- Reconciliation: rebuilt unadjusted batch counts and Spearman estimates must exactly match the locked clean-data analysis.",
  "- Result boundary: extract-powder hesperidin was the only association retained after the primary month-composition adjustment and FDR control; the residual-based adjustment did not retain this signal, so it is treated as a method-dependent investigation candidate rather than a confirmed material driver.",
  "- Interpretation: all upstream estimates remain associations and investigation hypotheses, not causal root causes."
), file.path(docs_dir, "P1_analysis_notes.md"), useBytes = TRUE)

message("P1 upstream robustness analysis completed: ", output_dir)
