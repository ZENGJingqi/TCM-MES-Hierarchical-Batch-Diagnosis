options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(pdftools)
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

set.seed(20260827)

data_dir <- Sys.getenv("TCM_NC_DATA_DIR", unset = "")
output_dir <- Sys.getenv("TCM_NC_OUTPUT_DIR", unset = "")
if (!nzchar(data_dir) || !nzchar(output_dir)) {
  stop("TCM_NC_DATA_DIR and TCM_NC_OUTPUT_DIR are required. Use the paired PowerShell launcher.")
}
tables_dir <- file.path(output_dir, "tables")
figures_dir <- file.path(output_dir, "figures")
docs_dir <- file.path(output_dir, "docs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

choose_file <- function(prefix) {
  files <- list.files(data_dir, pattern = paste0("^", prefix, ".*\\.xlsx$"), full.names = TRUE)
  if (length(files) != 1) stop("Expected exactly one finalized file for ", prefix, "; found ", length(files))
  files[[1]]
}

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

parse_numeric_mean <- function(x) {
  if (is.na(x) || str_trim(as.character(x)) == "") return(NA_real_)
  values <- str_split(as.character(x), "[;；,，、\\s]+")[[1]]
  values <- suppressWarnings(as.numeric(values[values != ""]))
  if (length(values) == 0 || all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
}

safe_rate <- function(x) if (length(x) == 0) NA_real_ else mean(x, na.rm = TRUE)

fit_linkage_model <- function(data, flag, cohort_label) {
  contingency <- table(data[[flag]], data$disintegration_issue)
  sparse_or_separated <- length(unique(data[[flag]])) < 2 || length(unique(data$disintegration_issue)) < 2 ||
    any(rowSums(contingency) < 20) || any(contingency == 0)
  if (sparse_or_separated) {
    return(tibble(cohort = cohort_label, linkage_flag = flag, model = c("Unadjusted", "Month-adjusted"),
                  n = nrow(data), odds_ratio = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_,
                  estimability = "Not estimable: invariant, sparse, or separated groups"))
  }
  fits <- list(
    Unadjusted = glm(reformulate(flag, response = "disintegration_issue"), data = data, family = binomial()),
    `Month-adjusted` = glm(reformulate(c(flag, "factor(production_month)"), response = "disintegration_issue"), data = data, family = binomial())
  )
  bind_rows(lapply(names(fits), function(nm) {
    fit <- fits[[nm]]
    beta <- coef(fit)[[flag]]
    se <- sqrt(vcov(fit)[flag, flag])
    tibble(
      cohort = cohort_label,
      linkage_flag = flag,
      model = nm,
      n = nobs(fit),
      odds_ratio = exp(beta),
      ci_low = exp(beta - 1.96 * se),
      ci_high = exp(beta + 1.96 * se),
      p_value = summary(fit)$coefficients[flag, "Pr(>|z|)"],
      estimability = "Estimable"
    )
  }))
}

save_pdf_png <- function(plot, stem, width = 13.2, height = 8.2, dpi = 240) {
  pdf_file <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_file <- file.path(figures_dir, paste0(stem, "_preview.png"))
  ggsave(pdf_file, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  suppressWarnings(pdftools::pdf_convert(pdf_file, format = "png", pages = 1, dpi = dpi, filenames = png_file))
  if (!file.exists(pdf_file) || !file.exists(png_file) || file.info(pdf_file)$size == 0 || file.info(png_file)$size == 0) {
    stop("Empty figure output for ", stem)
  }
}

d2_raw <- suppressWarnings(read_xlsx(choose_file("D2")))
d3_raw <- suppressWarnings(read_xlsx(choose_file("D3")))
d4_raw <- suppressWarnings(read_xlsx(choose_file("D4")))
d5_raw <- suppressWarnings(read_xlsx(choose_file("D5")))
d6_raw <- suppressWarnings(read_xlsx(choose_file("D6")))
d7_raw <- suppressWarnings(read_xlsx(choose_file("D7")))

d2 <- tibble(
  finished_batch = norm_batch(d2_raw$batch_no),
  observation_date = as.Date(d2_raw$production_date),
  dosage_strength = str_replace_all(as.character(d2_raw$dosage_strength), "\\s+", ""),
  disintegration_time_min = suppressWarnings(as.numeric(d2_raw$disintegration_time_min))
) |>
  filter(finished_batch != "", dosage_strength == "0.8g") |>
  group_by(finished_batch) |>
  summarise(
    observation_date = max(observation_date, na.rm = TRUE),
    record_n = n(),
    disintegration_time_min = mean(disintegration_time_min, na.rm = TRUE),
    disintegration_issue = as.integer(any(disintegration_time_min > 10, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(production_month = format(observation_date, "%Y-%m"))

d3 <- tibble(
  finished_batch = norm_batch(d3_raw$batch_no),
  mes_production_date = as.Date(d3_raw$production_date),
  extract_batch_raw = norm_batch(d3_raw$jwxs_extract_powder_batch_no),
  yam_batch_raw = norm_batch(d3_raw$yam_powder_luoting_batch_no),
  coating_yield_pct = suppressWarnings(as.numeric(d3_raw$coating_yield_pct)),
  final_blend_moisture_pct = suppressWarnings(as.numeric(d3_raw$final_blend_moisture_pct)),
  coating_mass_balance_pct = suppressWarnings(as.numeric(d3_raw$coating_mass_balance_pct)),
  compression_yield_pct = suppressWarnings(as.numeric(d3_raw$compression_yield_pct)),
  compression_hardness_mean_n = suppressWarnings(as.numeric(d3_raw$compression_hardness_mean_n)),
  coated_tablet_weight_mean_g = suppressWarnings(as.numeric(d3_raw$coated_tablet_weight_mean_g)),
  final_blend_lt_100_mesh_pct = suppressWarnings(as.numeric(d3_raw$final_blend_lt_100_mesh_pct)),
  granulation_discharge_moisture_pct_mean = vapply(d3_raw$granulation_discharge_moisture_pct, parse_numeric_mean, numeric(1)),
  core_tablet_weight_mean_g = suppressWarnings(as.numeric(d3_raw$core_tablet_weight_mean_g))
) |>
  filter(finished_batch != "") |>
  distinct(finished_batch, .keep_all = TRUE)

core_mes_vars <- c(
  "coating_yield_pct", "final_blend_moisture_pct", "coating_mass_balance_pct",
  "compression_yield_pct", "compression_hardness_mean_n", "coated_tablet_weight_mean_g",
  "final_blend_lt_100_mesh_pct", "granulation_discharge_moisture_pct_mean", "core_tablet_weight_mean_g"
)

d3_extract_long <- d3 |>
  select(finished_batch, extract_batch_raw) |>
  mutate(extract_batch = lapply(extract_batch_raw, split_batch_vector)) |>
  unnest(extract_batch) |>
  filter(extract_batch != "") |>
  distinct(finished_batch, extract_batch)

d3_yam_long <- d3 |>
  select(finished_batch, yam_batch_raw) |>
  mutate(yam_batch = lapply(yam_batch_raw, split_batch_vector)) |>
  unnest(yam_batch) |>
  filter(yam_batch != "") |>
  distinct(finished_batch, yam_batch)

d4_batches <- norm_batch(d4_raw$batch_no) |> unique()
d4_batches <- d4_batches[d4_batches != ""]
d7_batches <- norm_batch(d7_raw$batch_no) |> unique()
d7_batches <- d7_batches[d7_batches != ""]
d5_extract_batches <- norm_batch(d5_raw$extract_powder_batch_no) |> unique()
d5_extract_batches <- d5_extract_batches[d5_extract_batches != ""]

extract_raw_batches <- function(x) {
  text <- as.character(x) |> str_squish()
  if (is.na(text) || text == "") return(character(0))
  str_extract_all(text, "\\d{7}(?:-\\d+)?")[[1]] |> norm_batch()
}

trace_long <- tibble(
  extract_batch = norm_batch(d5_raw$extract_powder_batch_no),
  material_type = as.character(d5_raw$material_type) |> str_squish(),
  raw_batch_text = as.character(d5_raw$material_batch_no) |> str_squish()
) |>
  mutate(raw_batch = lapply(raw_batch_text, extract_raw_batches)) |>
  unnest(raw_batch) |>
  filter(extract_batch != "", raw_batch != "") |>
  distinct(material_type, extract_batch, raw_batch)

d6_batches <- norm_batch(d6_raw$batch_no) |> unique()
d6_batches <- d6_batches[d6_batches != ""]
chenpi_material_type <- trace_long |>
  group_by(material_type) |>
  summarise(d6_exact_overlap_n = n_distinct(raw_batch[raw_batch %in% d6_batches]), .groups = "drop") |>
  arrange(desc(d6_exact_overlap_n)) |>
  slice(1) |>
  pull(material_type)
chenpi_trace <- trace_long |> filter(material_type == chenpi_material_type)

linked <- d2 |>
  inner_join(d3, by = "finished_batch") |>
  mutate(
    production_month = format(mes_production_date, "%Y-%m"),
    direct_mes_core_complete = as.integer(if_all(all_of(core_mes_vars), ~ !is.na(.x))),
    has_extract_reference = as.integer(extract_batch_raw != ""),
    has_yam_reference = as.integer(yam_batch_raw != "")
  )

extract_flags <- d3_extract_long |>
  filter(finished_batch %in% linked$finished_batch) |>
  group_by(finished_batch) |>
  summarise(
    extract_parent_n = n_distinct(extract_batch),
    matched_D4 = as.integer(any(extract_batch %in% d4_batches)),
    traced_D5 = as.integer(any(extract_batch %in% d5_extract_batches)),
    matched_D6_chenpi = as.integer(any(extract_batch %in% chenpi_trace$extract_batch[chenpi_trace$raw_batch %in% d6_batches])),
    .groups = "drop"
  )

yam_flags <- d3_yam_long |>
  filter(finished_batch %in% linked$finished_batch) |>
  group_by(finished_batch) |>
  summarise(
    yam_parent_n = n_distinct(yam_batch),
    matched_D7 = as.integer(any(yam_batch %in% d7_batches)),
    .groups = "drop"
  )

linked <- linked |>
  left_join(extract_flags, by = "finished_batch") |>
  left_join(yam_flags, by = "finished_batch") |>
  mutate(
    across(c(extract_parent_n, yam_parent_n, matched_D4, traced_D5, matched_D6_chenpi, matched_D7), ~ replace_na(.x, 0)),
    both_direct_material_refs = as.integer(has_extract_reference == 1 & has_yam_reference == 1),
    both_matched_material_layers = as.integer(matched_D4 == 1 & matched_D7 == 1),
    thread_component_n = direct_mes_core_complete + has_extract_reference + matched_D4 + traced_D5 + matched_D6_chenpi + has_yam_reference + matched_D7,
    twin_id = sprintf("TWIN-%04d", row_number())
  ) |>
  arrange(mes_production_date, finished_batch) |>
  mutate(twin_id = sprintf("TWIN-%04d", row_number()))

entity_inventory <- tibble(
  dataset = c("D2", "D3", "D4", "D5", "D6", "D7"),
  entity = c("Finished-product QC records", "Finished-product MES batches", "Extract-powder QC batches", "Extract-to-raw-material traceability edges", "Chenpi QC batches", "Yam-powder MES batches"),
  record_n = c(nrow(d2_raw), nrow(d3_raw), nrow(d4_raw), nrow(d5_raw), nrow(d6_raw), nrow(d7_raw)),
  unique_entity_n = c(n_distinct(norm_batch(d2_raw$batch_no)), n_distinct(norm_batch(d3_raw$batch_no)), n_distinct(norm_batch(d4_raw$batch_no)), nrow(d5_raw), n_distinct(norm_batch(d6_raw$batch_no)), n_distinct(norm_batch(d7_raw$batch_no))),
  intended_grain = c("QC record", "Finished batch", "Extract batch", "One extract-material relation", "Chenpi batch", "Yam-powder batch")
)
write_csv(entity_inventory, file.path(tables_dir, "01_entity_inventory.csv"), na = "")

coverage_definitions <- tribble(
  ~coverage_item, ~numerator,
  "D2 0.8-g finished-product cohort", nrow(d2),
  "D2 batches linked to D3 MES", nrow(linked),
  "Nine direct MES features complete", sum(linked$direct_mes_core_complete),
  "Extract-powder batch reference present", sum(linked$has_extract_reference),
  "Extract-powder reference matched to D4", sum(linked$matched_D4),
  "Extract-powder reference traceable to D5 raw-material layer", sum(linked$traced_D5),
  "D5 Chenpi relation matched to D6 QC", sum(linked$matched_D6_chenpi),
  "Yam-powder batch reference present", sum(linked$has_yam_reference),
  "Yam-powder reference matched to D7", sum(linked$matched_D7),
  "Both D4 and D7 material layers matched", sum(linked$both_matched_material_layers)
) |>
  mutate(
    denominator = c(nrow(d2), nrow(d2), rep(nrow(linked), n() - 2)),
    coverage_rate = numerator / denominator
  )
write_csv(coverage_definitions, file.path(tables_dir, "02_twin_linkage_coverage.csv"), na = "")

d3_start <- min(d3$mes_production_date, na.rm = TRUE)
d3_end <- max(d3$mes_production_date, na.rm = TRUE)
d2_overlap <- d2 |>
  filter(observation_date >= d3_start, observation_date <= d3_end) |>
  mutate(
    linked_to_D3 = as.integer(finished_batch %in% linked$finished_batch),
    production_month = format(observation_date, "%Y-%m")
  )

monthly_selection <- d2_overlap |>
  group_by(production_month) |>
  summarise(
    d2_batch_n = n(),
    d3_linked_n = sum(linked_to_D3),
    d3_linkage_rate = mean(linked_to_D3),
    issue_rate_all = mean(disintegration_issue),
    issue_rate_linked = ifelse(sum(linked_to_D3) > 0, mean(disintegration_issue[linked_to_D3 == 1]), NA_real_),
    issue_rate_unlinked = ifelse(sum(linked_to_D3 == 0) > 0, mean(disintegration_issue[linked_to_D3 == 0]), NA_real_),
    .groups = "drop"
  )

monthly_linked <- linked |>
  group_by(production_month) |>
  summarise(
    linked_batch_n = n(),
    issue_n = sum(disintegration_issue),
    issue_rate = mean(disintegration_issue),
    direct_mes_core_complete_rate = mean(direct_mes_core_complete),
    extract_reference_rate = mean(has_extract_reference),
    D4_match_rate = mean(matched_D4),
    D5_trace_rate = mean(traced_D5),
    D6_chenpi_match_rate = mean(matched_D6_chenpi),
    yam_reference_rate = mean(has_yam_reference),
    D7_match_rate = mean(matched_D7),
    .groups = "drop"
  )

monthly_coverage <- full_join(monthly_selection, monthly_linked, by = "production_month") |>
  arrange(production_month)
write_csv(monthly_coverage, file.path(tables_dir, "03_monthly_linkage_and_selection.csv"), na = "")

selection_models <- fit_linkage_model(d2_overlap, "linked_to_D3", "D2 batches in D3-overlap period")
within_flags <- c("direct_mes_core_complete", "has_extract_reference", "matched_D4", "traced_D5", "matched_D6_chenpi", "has_yam_reference", "matched_D7")
within_models <- bind_rows(lapply(within_flags, function(flag) fit_linkage_model(linked, flag, "D2-D3 linked twins")))
linkage_models <- bind_rows(selection_models, within_models)
write_csv(linkage_models, file.path(tables_dir, "04_linkage_outcome_associations.csv"), na = "")

extract_desc <- d3_extract_long |>
  filter(finished_batch %in% linked$finished_batch) |>
  count(extract_batch, name = "descendant_finished_batch_n") |>
  transmute(layer = "Extract-powder batch", upstream_batch = extract_batch, descendant_finished_batch_n)
yam_desc <- d3_yam_long |>
  filter(finished_batch %in% linked$finished_batch) |>
  count(yam_batch, name = "descendant_finished_batch_n") |>
  transmute(layer = "Yam-powder batch", upstream_batch = yam_batch, descendant_finished_batch_n)
raw_desc <- trace_long |>
  inner_join(d3_extract_long |> filter(finished_batch %in% linked$finished_batch), by = "extract_batch", relationship = "many-to-many") |>
  group_by(raw_batch) |>
  summarise(descendant_finished_batch_n = n_distinct(finished_batch), .groups = "drop") |>
  transmute(layer = "D5 raw-material batch", upstream_batch = raw_batch, descendant_finished_batch_n)
lineage_descendants <- bind_rows(extract_desc, yam_desc, raw_desc)
write_csv(lineage_descendants, file.path(tables_dir, "05_lineage_descendant_counts.csv"), na = "")

lineage_summary <- lineage_descendants |>
  group_by(layer) |>
  summarise(
    upstream_batch_n = n(),
    reused_batch_n = sum(descendant_finished_batch_n >= 2),
    reused_batch_rate = mean(descendant_finished_batch_n >= 2),
    descendant_min = min(descendant_finished_batch_n),
    descendant_median = median(descendant_finished_batch_n),
    descendant_mean = mean(descendant_finished_batch_n),
    descendant_p90 = quantile(descendant_finished_batch_n, 0.90, names = FALSE),
    descendant_max = max(descendant_finished_batch_n),
    .groups = "drop"
  )
write_csv(lineage_summary, file.path(tables_dir, "06_lineage_multiplicity_summary.csv"), na = "")

twin_audit <- linked |>
  transmute(
    twin_id, mes_production_date, production_month, disintegration_issue,
    direct_mes_core_complete, has_extract_reference, matched_D4, traced_D5,
    matched_D6_chenpi, has_yam_reference, matched_D7,
    extract_parent_n, yam_parent_n, thread_component_n
  )
write_csv(twin_audit, file.path(tables_dir, "07_masked_batch_twin_completeness.csv"), na = "")

pattern_summary <- twin_audit |>
  unite("coverage_pattern", c(direct_mes_core_complete, has_extract_reference, matched_D4, traced_D5, matched_D6_chenpi, has_yam_reference, matched_D7), sep = "-") |>
  count(coverage_pattern, sort = TRUE, name = "batch_n") |>
  mutate(batch_rate = batch_n / sum(batch_n))
write_csv(pattern_summary, file.path(tables_dir, "08_twin_coverage_patterns.csv"), na = "")

nejm <- c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD")
base_theme <- theme_classic(base_family = "Arial", base_size = 13) +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    strip.text = element_text(size = 13, face = "bold", colour = "black"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.tag = element_text(face = "bold", size = 16),
    plot.margin = margin(7, 10, 7, 8)
  )

plot_cov <- coverage_definitions |>
  filter(coverage_item != "D2 0.8-g finished-product cohort") |>
  mutate(
    label = case_when(
      coverage_item == "D2 batches linked to D3 MES" ~ "D2-D3 MES linkage",
      coverage_item == "Nine direct MES features complete" ~ "Nine direct MES variables",
      coverage_item == "Extract-powder batch reference present" ~ "Extract batch reference",
      coverage_item == "Extract-powder reference matched to D4" ~ "Extract QC (D4)",
      coverage_item == "Extract-powder reference traceable to D5 raw-material layer" ~ "Raw-material trace (D5)",
      coverage_item == "D5 Chenpi relation matched to D6 QC" ~ "Chenpi QC (D6)",
      coverage_item == "Yam-powder batch reference present" ~ "Yam batch reference",
      coverage_item == "Yam-powder reference matched to D7" ~ "Yam MES (D7)",
      coverage_item == "Both D4 and D7 material layers matched" ~ "Both material layers",
      TRUE ~ coverage_item
    ),
    label = factor(label, levels = rev(label))
  ) |>
  ggplot(aes(x = coverage_rate, y = label)) +
  geom_col(fill = nejm[2], width = 0.68) +
  geom_text(aes(label = paste0(comma(numerator), " (", percent(coverage_rate, accuracy = 0.1), ")")), hjust = -0.04, size = 3.6, family = "Arial") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.27), breaks = seq(0, 1, 0.25)) +
  labs(x = "Coverage among eligible batches", y = NULL) + base_theme

monthly_long <- monthly_coverage |>
  select(production_month, d3_linkage_rate, D4_match_rate, D5_trace_rate, D7_match_rate) |>
  pivot_longer(-production_month, names_to = "series", values_to = "rate") |>
  mutate(
    series = recode(series,
                    d3_linkage_rate = "D2-D3 linkage",
                    D4_match_rate = "Extract QC (D4)",
                    D5_trace_rate = "Raw-material trace (D5)",
                    D7_match_rate = "Yam MES (D7)"),
    production_month = as.Date(paste0(production_month, "-01"))
  )
plot_month <- ggplot(monthly_long, aes(production_month, rate, colour = series, group = series)) +
  geom_line(linewidth = 0.85) + geom_point(size = 1.8) +
  scale_colour_manual(values = nejm[c(1, 2, 4, 5)]) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.02)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  labs(x = "Production month", y = "Monthly coverage", colour = NULL) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  base_theme + theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top")

plot_desc <- lineage_descendants |>
  mutate(layer = factor(layer, levels = c("Extract-powder batch", "Yam-powder batch", "D5 raw-material batch"))) |>
  ggplot(aes(layer, descendant_finished_batch_n, fill = layer)) +
  geom_boxplot(width = 0.58, outlier.alpha = 0.45, outlier.size = 1.2) +
  scale_fill_manual(values = nejm[c(2, 4, 3)], guide = "none") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200), labels = comma) +
  labs(x = NULL, y = "Descendant finished batches (log scale)") +
  base_theme + theme(axis.text.x = element_text(angle = 20, hjust = 1))

plot_score <- linked |>
  count(thread_component_n, disintegration_issue, name = "batch_n") |>
  group_by(thread_component_n) |>
  mutate(batch_rate = batch_n / sum(batch_n)) |>
  ungroup() |>
  mutate(issue_label = ifelse(disintegration_issue == 1, "Quality event", "No event")) |>
  ggplot(aes(factor(thread_component_n), batch_n, fill = issue_label)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Quality event" = nejm[1], "No event" = nejm[6])) +
  labs(x = "Observed data-thread components (of 7)", y = "Finished batches", fill = NULL) +
  base_theme

combined <- (plot_cov | plot_month) / (plot_desc | plot_score) +
  plot_layout(widths = c(1.12, 1)) +
  plot_annotation(tag_levels = "A")
save_pdf_png(combined, "Figure_A_digital_thread_coverage_and_genealogy", width = 14.0, height = 9.0)

qa_checks <- tibble(
  check = c(
    "D2 0.8-g unique batches",
    "D2-D3 linked batches",
    "Linked quality events",
    "No duplicated finished-batch twin IDs",
    "All component flags binary",
    "No current D3 percentage values outside 0-110%",
    "Figure PDF exists",
    "Figure PNG exists"
  ),
  observed = c(
    nrow(d2), nrow(linked), sum(linked$disintegration_issue),
    sum(duplicated(linked$finished_batch)),
    sum(!unlist(linked[c("direct_mes_core_complete", "has_extract_reference", "matched_D4", "traced_D5", "matched_D6_chenpi", "has_yam_reference", "matched_D7")]) %in% c(0, 1)),
    sum(vapply(d3_raw[names(d3_raw)[str_detect(names(d3_raw), "_pct$")]], function(x) {
      z <- suppressWarnings(as.numeric(x)); sum(!is.na(z) & (z < 0 | z > 110))
    }, numeric(1))),
    as.integer(file.exists(file.path(figures_dir, "Figure_A_digital_thread_coverage_and_genealogy.pdf"))),
    as.integer(file.exists(file.path(figures_dir, "Figure_A_digital_thread_coverage_and_genealogy_preview.png")))
  ),
  expected = c(4296, 1477, 380, 0, 0, 0, 1, 1)
) |>
  mutate(pass = observed == expected)
write_csv(qa_checks, file.path(docs_dir, "module_A_QA_checks.csv"), na = "")
if (!all(qa_checks$pass)) stop("Module A QA failed; inspect module_A_QA_checks.csv")

coverage_lookup <- setNames(coverage_definitions$coverage_rate, coverage_definitions$coverage_item)
count_lookup <- setNames(coverage_definitions$numerator, coverage_definitions$coverage_item)
report <- c(
  "# Module A: digital-thread and physical-virtual mapping audit",
  "",
  "> Analysis date: 2026-08-27  ",
  "> Source: finalized English D2-D7 workbooks only  ",
  "> Grain: one 0.8-g finished-product batch per digital-twin record",
  "",
  "## Locked findings",
  "",
  paste0("- The complete D2 cohort contains ", comma(nrow(d2)), " unique 0.8-g finished-product batches; ", comma(nrow(linked)), " (", percent(nrow(linked) / nrow(d2), accuracy = 0.1), ") link to D3 MES and form the primary batch-twin cohort."),
  paste0("- Within the ", comma(nrow(linked)), " twins, extract-powder references are present for ", comma(count_lookup[["Extract-powder batch reference present"]]), " batches (", percent(coverage_lookup[["Extract-powder batch reference present"]], accuracy = 0.1), "), and yam-powder references are present for ", comma(count_lookup[["Yam-powder batch reference present"]]), " batches (", percent(coverage_lookup[["Yam-powder batch reference present"]], accuracy = 0.1), ")."),
  paste0("- D5 raw-material traceability is available for only ", comma(count_lookup[["Extract-powder reference traceable to D5 raw-material layer"]]), " finished batches (", percent(coverage_lookup[["Extract-powder reference traceable to D5 raw-material layer"]], accuracy = 0.1), "). This is partial upstream traceability, not complete genealogy coverage."),
  "- The previously documented value of 214 D5-traceable finished batches was an identifier-normalization artifact: D3 batch references imported as strings retained a trailing .0 and failed to match integer-formatted D5 identifiers. The corrected standardized-batch result is 1,025 batches.",
  paste0("- Upstream batches are repeatedly reused: median descendant counts are ", paste0(lineage_summary$layer, "=", lineage_summary$descendant_median, collapse = "; "), ". Finished-batch rows are therefore not independent replicates of upstream material evidence."),
  "",
  "## Interpretation boundary",
  "",
  "The data support an auditable but incomplete batch digital thread. Direct MES and immediate extract/yam references have high coverage, whereas raw-material traceability is partial. The manuscript must distinguish 'batch-resolved digital thread' from 'complete end-to-end genealogy'. Linkage and missingness are reported by month and outcome so that downstream analyses do not treat the connected cohort as an unselected sample.",
  "",
  "## Reproducibility",
  "",
  "All batch-level public-facing audit tables use deterministic masked twin identifiers. The source workbooks are not modified. PDF was generated first and the PNG was rendered from that PDF."
)
write_lines(report, file.path(docs_dir, "module_A_findings_and_boundaries.md"), na = "")
write_lines(capture.output(sessionInfo()), file.path(docs_dir, "R_session_info.txt"), na = "")

message("Module A completed: ", output_dir)
