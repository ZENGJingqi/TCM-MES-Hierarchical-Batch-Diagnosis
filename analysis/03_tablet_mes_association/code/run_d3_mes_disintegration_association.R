options(warn = 1)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(openxlsx)
  library(patchwork)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

project_dir <- normalizePath(file.path(getwd(), "D3_成品MES与成品崩解关联分析"), winslash = "/", mustWork = FALSE)
figures_dir <- file.path(project_dir, "figures")
tables_dir <- file.path(project_dir, "tables")
docs_dir <- file.path(project_dir, "docs")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

d2_file <- file.path(getwd(), "定稿数据_英文", "D2_finished_product_physchem_final.xlsx")
d3_file <- file.path(getwd(), "定稿数据_英文", "D3_finished_product_mes_main_final.xlsx")

normalize_batch <- function(x) {
  if (inherits(x, "numeric")) {
    out <- ifelse(is.na(x), NA_character_, sprintf("%.0f", x))
  } else {
    out <- as.character(x)
  }
  out <- str_trim(out)
  out <- str_replace(out, "\\.0$", "")
  out[out == ""] <- NA_character_
  out
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

parse_multi_stats <- function(x) {
  x <- as.character(x)
  values <- strsplit(x, ";", fixed = TRUE)
  row_stat <- function(v, stat) {
    nums <- safe_num(str_trim(v))
    nums <- nums[is.finite(nums)]
    if (length(nums) == 0) {
      return(NA_real_)
    }
    switch(
      stat,
      n = length(nums),
      mean = mean(nums),
      sd = ifelse(length(nums) > 1, sd(nums), NA_real_),
      min = min(nums),
      max = max(nums)
    )
  }
  tibble(
    n = vapply(values, row_stat, numeric(1), stat = "n"),
    mean = vapply(values, row_stat, numeric(1), stat = "mean"),
    sd = vapply(values, row_stat, numeric(1), stat = "sd"),
    min = vapply(values, row_stat, numeric(1), stat = "min"),
    max = vapply(values, row_stat, numeric(1), stat = "max")
  ) |>
    mutate(
      range = max - min,
      cv_pct = if_else(is.finite(mean) & mean != 0, sd / abs(mean) * 100, NA_real_),
      across(everything(), ~if_else(is.infinite(.x) | is.nan(.x), NA_real_, .x))
    )
}

format_p <- function(p_value) {
  if (is.na(p_value)) {
    return("P = NA")
  }
  if (p_value < 0.001) {
    return("P < 0.001")
  }
  paste0("P = ", formatC(p_value, format = "f", digits = 3))
}

format_p_stars <- function(p_value, method = "Wilcoxon") {
  stars <- case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
  paste0(method, " ", format_p(p_value), " (", stars, ")")
}

clean_label <- function(x) {
  label_dictionary <- c(
    blend_powder_i_through_80_mesh_pct = "Blend powder I through 80 mesh (%)",
    blend_powder_ii_through_60_mesh_pct = "Blend powder II through 60 mesh (%)",
    blend_powder_ii_retained_on_60_mesh_g = "Blend powder II retained on 60 mesh (g)",
    blend_powder_i_yield_pct = "Blend powder I yield (%)",
    blend_powder_i_mass_balance_pct = "Blend powder I mass balance (%)",
    milling_yield_pct = "Milling yield (%)",
    milling_mass_balance_pct = "Milling mass balance (%)",
    final_blend_yield_pct = "Final-blend yield (%)",
    final_blend_mass_balance_pct = "Final-blend mass balance (%)",
    final_blend_gt_30_mesh_pct = "Final blend >30 mesh (%)",
    final_blend_lt_100_mesh_pct = "Final blend <100 mesh (%)",
    coating_yield_pct = "Coating yield (%)",
    final_blend_moisture_pct = "Final-blend moisture (%)",
    coating_mass_balance_pct = "Coating mass balance (%)",
    granulation_moisture_meter_temp_c = "Granulation meter temperature (℃)",
    final_blend_moisture_meter_temp_c = "Final-blend meter temperature (℃)",
    granulation_moisture_meter_rh_pct = "Granulation meter RH (%)",
    final_blend_moisture_meter_rh_pct = "Final-blend meter RH (%)",
    compression_yield_pct = "Compression yield (%)",
    compression_mass_balance_pct = "Compression mass balance (%)",
    compression_broken_tablet_weight_g = "Broken-tablet weight during compression (g)",
    compression_broken_tablet_rate_permille = "Broken-tablet rate during compression (‰)",
    compression_tablet_weight_mean_g = "Compression tablet weight (g)",
    compression_hardness_mean_n = "Compression hardness (N)",
    granulation_discharge_moisture_pct_min = "Granulation moisture, min (%)",
    granulation_discharge_moisture_pct_mean = "Granulation moisture, mean (%)",
    granulation_discharge_moisture_pct_max = "Granulation moisture, max (%)",
    core_tablet_weight_mean_g = "Core tablet weight (g)",
    coated_tablet_weight_mean_g = "Coated tablet weight (g)",
    coated_tablet_hardness_mean_n = "Coated tablet hardness (N)",
    coating_missing_tablet_weight_g = "Missing-tablet weight during coating (g)",
    coating_missing_tablet_rate_permille = "Missing-tablet rate during coating (‰)",
    coating_abraded_weight_g = "Abraded-tablet weight during coating (g)",
    coating_abraded_rate_permille = "Abraded-tablet rate during coating (‰)",
    final_product_yield_pct = "Final-product yield (%)",
    final_product_mass_balance_pct = "Final-product mass balance (%)",
    jwxs_extract_powder_input_kg = "Extract-powder input (kg)"
  )
  vapply(x, function(value) {
    if (value %in% names(label_dictionary)) {
      return(unname(label_dictionary[[value]]))
    }
    stat_dictionary <- c(
      cv_pct = "CV",
      range = "range",
      mean = "mean",
      sd = "SD",
      min = "min",
      max = "max"
    )
    unit_dictionary <- c(
      pct = "%",
      kg = "kg",
      g = "g",
      n = "N",
      c = "°C",
      permille = "per mille"
    )
    stat_label <- NA_character_
    base_value <- value
    for (stat_name in names(stat_dictionary)) {
      suffix <- paste0("_", stat_name)
      if (str_ends(base_value, fixed(suffix))) {
        stat_label <- unname(stat_dictionary[[stat_name]])
        base_value <- str_remove(base_value, paste0(suffix, "$"))
        break
      }
    }
    unit_label <- NA_character_
    for (unit_name in names(unit_dictionary)) {
      suffix <- paste0("_", unit_name)
      if (str_ends(base_value, fixed(suffix))) {
        unit_label <- unname(unit_dictionary[[unit_name]])
        base_value <- str_remove(base_value, paste0(suffix, "$"))
        break
      }
    }
    base_label <- base_value |>
      str_replace_all("_", " ") |>
      str_to_sentence()
    if (!is.na(stat_label) && !is.na(unit_label)) {
      return(paste0(base_label, ", ", stat_label, " (", unit_label, ")"))
    }
    if (!is.na(stat_label)) {
      return(paste0(base_label, ", ", stat_label))
    }
    if (!is.na(unit_label)) {
      return(paste0(base_label, " (", unit_label, ")"))
    }
    value |>
      str_replace_all("_rh_pct$", " RH (%)") |>
      str_replace_all("_pct$", " (%)") |>
      str_replace_all("_kg$", " (kg)") |>
      str_replace_all("_g$", " (g)") |>
      str_replace_all("_n$", " (N)") |>
      str_replace_all("_c$", " (℃)") |>
      str_replace_all("_permille$", " (per mille)") |>
      str_replace_all("_", " ") |>
      str_to_sentence()
  }, character(1))
}

save_dual <- function(plot_obj, stem, width = 7.2, height = 5.8) {
  pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figures_dir, paste0(stem, ".png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, units = "in", device = cairo_pdf, dpi = 330, bg = "white")
  ggsave(png_path, plot_obj, width = width, height = height, units = "in", dpi = 330, bg = "white")
  invisible(c(pdf = pdf_path, png = png_path))
}

point_colour <- "#8C8C8C"
line_colour <- "#A83B2B"
box_colour <- "#0A5C7A"
box_fill <- "#EAF2F5"
text_colour <- "#464F5F"
grid_colour <- "#DADDE2"

theme_set(theme_bw(base_family = "Arial", base_size = 17))
theme_update(
  plot.title = element_blank(),
  axis.title = element_text(size = 20, colour = "black"),
  axis.text = element_text(size = 17, colour = "black"),
  strip.text = element_text(size = 16, colour = "black"),
  panel.grid.major = element_line(colour = grid_colour, linewidth = 0.45),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.75),
  axis.line = element_line(colour = "black", linewidth = 0.75),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 15),
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.margin = margin(12, 16, 12, 14)
)

d2 <- read_xlsx(d2_file) |>
  mutate(
    batch_no = normalize_batch(batch_no),
    production_date = as.Date(production_date),
    production_month = format(production_date, "%Y-%m"),
    dosage_strength = str_replace_all(as.character(dosage_strength), "\\s+", "")
  ) |>
  filter(dosage_strength == "0.8g", !is.na(batch_no), !is.na(disintegration_time_min)) |>
  mutate(
    disintegration_issue = disintegration_time_min > 10,
    disintegration_group = if_else(
      disintegration_issue,
      "Disintegration > 10 min",
      "Disintegration <= 10 min"
    )
  )

d3_raw <- read_xlsx(d3_file) |>
  mutate(batch_no = normalize_batch(batch_no))

multi_cols <- names(d3_raw)[vapply(d3_raw, function(x) {
  is.character(x) && any(str_detect(x, fixed(";")), na.rm = TRUE)
}, logical(1))]
multi_cols <- setdiff(multi_cols, c("batch_no"))

d3_features <- d3_raw
for (col in multi_cols) {
  stats <- parse_multi_stats(d3_raw[[col]])
  stat_names <- c("mean")
  for (stat_name in stat_names) {
    d3_features[[paste0(col, "_", stat_name)]] <- stats[[stat_name]]
  }
}

id_pattern <- "(^|_)batch_no($|_)|supplier($|_)|production_date$|_values_|material_batch_no($|_)"
all_numeric_feature_cols <- names(d3_features)[vapply(d3_features, is.numeric, logical(1))]
all_numeric_feature_cols <- all_numeric_feature_cols[!str_detect(all_numeric_feature_cols, id_pattern)]
count_only_cols <- c(
  "compression_tablet_weight_n",
  "compression_hardness_n",
  "core_tablet_weight_n",
  "coated_tablet_weight_n",
  "coated_tablet_hardness_n"
)
input_amount_cols <- all_numeric_feature_cols[str_detect(all_numeric_feature_cols, "_input_kg($|_)")]
instrument_environment_cols <- c(
  "granulation_moisture_meter_temp_c",
  "final_blend_moisture_meter_temp_c",
  "granulation_moisture_meter_rh_pct",
  "final_blend_moisture_meter_rh_pct"
)
feature_exclusion_reason <- tibble(
  feature = all_numeric_feature_cols,
  prescreen_status = case_when(
    feature %in% count_only_cols ~ "Excluded before screening: observation-count field",
    feature %in% input_amount_cols ~ "Excluded before screening: material input amount",
    feature %in% instrument_environment_cols ~ "Excluded before screening: moisture-meter temperature/RH",
    TRUE ~ "Eligible process or in-process quality parameter"
  )
)
feature_cols <- feature_exclusion_reason |>
  filter(prescreen_status == "Eligible process or in-process quality parameter") |>
  pull(feature)

analysis_df <- d2 |>
  inner_join(d3_features, by = "batch_no", suffix = c("_quality", "_mes"))

candidate_feature_audit <- lapply(all_numeric_feature_cols, function(feature) {
  x <- analysis_df[[feature]]
  ok <- is.finite(x) & !is.na(analysis_df$disintegration_time_min) & !is.na(analysis_df$disintegration_issue)
  reference_n <- sum(ok & !analysis_df$disintegration_issue)
  issue_n <- sum(ok & analysis_df$disintegration_issue)
  unique_n <- length(unique(x[ok]))
  prescreen_status <- feature_exclusion_reason$prescreen_status[match(feature, feature_exclusion_reason$feature)]
  tibble(
    feature = feature,
    feature_label = clean_label(feature),
    valid_n = sum(ok),
    unique_n = unique_n,
    reference_n = reference_n,
    issue_n = issue_n,
    analysis_status = case_when(
      prescreen_status != "Eligible process or in-process quality parameter" ~ prescreen_status,
      sum(ok) < 30 ~ "Excluded: valid n < 30",
      unique_n < 5 ~ "Excluded: <5 unique values",
      reference_n < 5 | issue_n < 5 ~ "Excluded: group n < 5",
      TRUE ~ "Included in FDR screening"
    )
  )
}) |>
  bind_rows() |>
  arrange(desc(analysis_status == "Included in FDR screening"), feature_label)

feature_screen <- lapply(feature_cols, function(feature) {
  x <- analysis_df[[feature]]
  ok <- is.finite(x) & !is.na(analysis_df$disintegration_time_min) & !is.na(analysis_df$disintegration_issue)
  if (sum(ok) < 30 || length(unique(x[ok])) < 5) {
    return(NULL)
  }
  group_counts <- table(analysis_df$disintegration_issue[ok])
  if (length(group_counts) < 2 || any(group_counts < 5)) {
    return(NULL)
  }

  spearman <- suppressWarnings(cor.test(x[ok], analysis_df$disintegration_time_min[ok], method = "spearman", exact = FALSE))
  wilcox <- suppressWarnings(wilcox.test(x[ok] ~ analysis_df$disintegration_issue[ok], exact = FALSE))
  glm_df <- tibble(
    issue = analysis_df$disintegration_issue[ok],
    z = as.numeric(scale(x[ok])),
    production_month = factor(analysis_df$production_month[ok])
  ) |>
    filter(is.finite(z))
  fit <- tryCatch(glm(issue ~ z, data = glm_df, family = binomial()), error = function(e) NULL)
  fit_adjusted <- tryCatch(glm(issue ~ z + production_month, data = glm_df, family = binomial()), error = function(e) NULL)
  if (is.null(fit)) {
    glm_p <- NA_real_
    glm_or <- NA_real_
    glm_low <- NA_real_
    glm_high <- NA_real_
  } else {
    coef_table <- summary(fit)$coefficients
    glm_p <- coef_table["z", "Pr(>|z|)"]
    glm_or <- exp(coef(fit)[["z"]])
    ci <- suppressWarnings(exp(confint.default(fit, parm = "z")))
    glm_low <- ci[1]
    glm_high <- ci[2]
  }
  if (is.null(fit_adjusted)) {
    adjusted_glm_p <- NA_real_
    adjusted_glm_or <- NA_real_
    adjusted_glm_low <- NA_real_
    adjusted_glm_high <- NA_real_
  } else {
    adjusted_coef_table <- summary(fit_adjusted)$coefficients
    adjusted_glm_p <- adjusted_coef_table["z", "Pr(>|z|)"]
    adjusted_glm_or <- exp(coef(fit_adjusted)[["z"]])
    adjusted_ci <- suppressWarnings(exp(confint.default(fit_adjusted, parm = "z")))
    adjusted_glm_low <- adjusted_ci[1]
    adjusted_glm_high <- adjusted_ci[2]
  }

  ref_values <- x[ok & !analysis_df$disintegration_issue]
  issue_values <- x[ok & analysis_df$disintegration_issue]
  tibble(
    feature = feature,
    feature_label = clean_label(feature),
    n = sum(ok),
    reference_n = length(ref_values),
    issue_n = length(issue_values),
    spearman_rho = unname(spearman$estimate),
    spearman_p = spearman$p.value,
    wilcoxon_p = wilcox$p.value,
    reference_median = median(ref_values, na.rm = TRUE),
    issue_median = median(issue_values, na.rm = TRUE),
    median_difference_issue_minus_reference = issue_median - reference_median,
    logistic_or_per_sd = glm_or,
    logistic_ci_low = glm_low,
    logistic_ci_high = glm_high,
    logistic_p = glm_p,
    month_adjusted_or_per_sd = adjusted_glm_or,
    month_adjusted_ci_low = adjusted_glm_low,
    month_adjusted_ci_high = adjusted_glm_high,
    month_adjusted_p = adjusted_glm_p
  )
}) |>
  bind_rows() |>
  mutate(
    spearman_fdr = p.adjust(spearman_p, method = "BH"),
    wilcoxon_fdr = p.adjust(wilcoxon_p, method = "BH"),
    logistic_fdr = p.adjust(logistic_p, method = "BH"),
    month_adjusted_fdr = p.adjust(month_adjusted_p, method = "BH"),
    min_fdr = pmin(spearman_fdr, wilcoxon_fdr, logistic_fdr, na.rm = TRUE),
    screening_score = -log10(pmax(min_fdr, .Machine$double.xmin)),
    screening_judgment = case_when(
      min_fdr < 0.001 ~ "FDR < 0.001",
      min_fdr < 0.01 ~ "FDR < 0.01",
      min_fdr < 0.05 ~ "FDR < 0.05",
      TRUE ~ "Not significant"
    ),
    direction = if_else(median_difference_issue_minus_reference >= 0, "Higher in >10 min", "Lower in >10 min")
  ) |>
  arrange(min_fdr, desc(abs(spearman_rho)))

top_features <- feature_screen |>
  filter(!is.na(min_fdr)) |>
  arrange(min_fdr, desc(abs(spearman_rho))) |>
  slice_head(n = 8)

plot_long <- analysis_df |>
  select(batch_no, disintegration_time_min, disintegration_group, all_of(top_features$feature)) |>
  pivot_longer(
    cols = all_of(top_features$feature),
    names_to = "feature",
    values_to = "value"
  ) |>
  filter(is.finite(value)) |>
  left_join(top_features |> select(feature, feature_label, wilcoxon_p), by = "feature") |>
  mutate(
    feature_label = factor(feature_label, levels = top_features$feature_label),
    disintegration_group = factor(disintegration_group, levels = c("Disintegration <= 10 min", "Disintegration > 10 min"))
  )

p_rank <- feature_screen |>
  mutate(
    feature_label = factor(feature_label, levels = rev(feature_label)),
    direction = factor(direction, levels = c("Higher in >10 min", "Lower in >10 min"))
  ) |>
  ggplot(aes(x = screening_score, y = feature_label, fill = direction)) +
  geom_col(width = 0.68, colour = NA) +
  geom_text(
    aes(label = screening_judgment),
    hjust = -0.08,
    family = "Arial",
    size = 5.8,
    colour = "black"
  ) +
  scale_fill_manual(values = c(
    "Higher in >10 min" = line_colour,
    "Lower in >10 min" = box_colour
  ), labels = c(
    "Higher in >10 min" = "Higher in >10 min group",
    "Lower in >10 min" = "Lower in >10 min group"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  coord_cartesian(clip = "off") +
  labs(x = "-log10(BH-adjusted screening P)", y = NULL, fill = NULL) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 18),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.title.x = element_text(size = 20),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(14, 58, 18, 14)
  )
rank_plot_height <- max(7.6, 0.42 * nrow(feature_screen) + 2.2)
save_dual(p_rank, "01_d3_mes_feature_screening_rank", width = 11.2, height = rank_plot_height)

plot_single_group <- function(feature, stem) {
  meta <- feature_screen |> filter(feature == !!feature) |> slice(1)
  plot_df <- analysis_df |>
    transmute(
      disintegration_group = factor(
        disintegration_group,
        levels = c("Disintegration <= 10 min", "Disintegration > 10 min")
      ),
      value = .data[[feature]]
    ) |>
    filter(is.finite(value))
  group_n <- plot_df |>
    count(disintegration_group, name = "n") |>
    mutate(
      label = if_else(
        disintegration_group == "Disintegration <= 10 min",
        paste0("<= 10 min\n(n = ", comma(n), ")"),
        paste0("> 10 min\n(n = ", comma(n), ")")
      )
    )
  label_map <- setNames(group_n$label, as.character(group_n$disintegration_group))
  y_range <- range(plot_df$value, na.rm = TRUE)
  y_pad <- diff(y_range) * 0.15
  if (!is.finite(y_pad) || y_pad == 0) {
    y_pad <- max(abs(y_range), na.rm = TRUE) * 0.05
  }
  stat_label <- format_p_stars(meta$wilcoxon_p, "Wilcoxon")
  p <- ggplot(plot_df, aes(x = disintegration_group, y = value)) +
    geom_boxplot(outlier.shape = NA, width = 0.55, fill = box_fill, colour = box_colour, linewidth = 0.95) +
    geom_jitter(width = 0.14, size = 1.7, colour = point_colour) +
    annotate("segment", x = 1, xend = 2, y = y_range[2] + y_pad * 0.35, yend = y_range[2] + y_pad * 0.35, colour = text_colour, linewidth = 0.75) +
    annotate("text", x = 1.5, y = y_range[2] + y_pad * 0.78, label = stat_label, family = "Arial", size = 5.4, colour = text_colour, lineheight = 0.95) +
    scale_x_discrete(labels = label_map) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.16))) +
    labs(x = "Finished-product disintegration group", y = meta$feature_label) +
    theme(
      axis.text.x = element_text(size = 18, lineheight = 0.95),
      axis.title.x = element_text(size = 20, margin = margin(t = 8)),
      axis.title.y = element_text(size = 20, margin = margin(r = 8))
    )
  save_dual(p, stem, width = 7.2, height = 5.4)
}

unlink(
  file.path(figures_dir, c(
    "02_d3_coating_yield_by_disintegration_group.pdf",
    "02_d3_coating_yield_by_disintegration_group.png",
    "03_d3_final_blend_moisture_by_disintegration_group.pdf",
    "03_d3_final_blend_moisture_by_disintegration_group.png",
    "04_d3_coating_mass_balance_by_disintegration_group.pdf",
    "04_d3_coating_mass_balance_by_disintegration_group.png"
  )),
  force = TRUE
)
unlink(
  file.path(figures_dir, sprintf("02_d3_significant_boxplot_%02d_*.pdf", 1:99)),
  force = TRUE
)
unlink(
  file.path(figures_dir, sprintf("02_d3_significant_boxplot_%02d_*.png", 1:99)),
  force = TRUE
)

significant_features <- feature_screen |>
  filter(!is.na(min_fdr), min_fdr < 0.05) |>
  arrange(min_fdr, desc(abs(spearman_rho)))

sanitize_stem <- function(x) {
  x |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("^_|_$", "") |>
    str_to_lower()
}

if (nrow(significant_features) > 0) {
  for (i in seq_len(nrow(significant_features))) {
    plot_single_group(
      significant_features$feature[i],
      sprintf("02_d3_significant_boxplot_%02d_%s", i, sanitize_stem(significant_features$feature[i]))
    )
  }
}

scatter_features <- significant_features
scatter_long <- analysis_df |>
  select(batch_no, disintegration_time_min, disintegration_issue, all_of(scatter_features$feature)) |>
  pivot_longer(cols = all_of(scatter_features$feature), names_to = "feature", values_to = "value") |>
  filter(is.finite(value)) |>
  left_join(scatter_features |> select(feature, feature_label, spearman_rho, spearman_p, min_fdr, n), by = "feature") |>
  mutate(feature_label = factor(feature_label, levels = scatter_features$feature_label))

scatter_annotation <- scatter_features |>
  rowwise() |>
  transmute(
    feature_label = factor(feature_label, levels = scatter_features$feature_label),
    label = paste0(
      "Spearman rho = ", sprintf("%.3f", spearman_rho),
      "\n", format_p(spearman_p),
      "\nFDR ", str_remove(format_p(min_fdr), "^P "),
      "\nn = ", n
    )
  ) |>
  ungroup()

p_scatter <- ggplot(scatter_long, aes(x = value, y = disintegration_time_min)) +
  geom_point(colour = point_colour, size = 1.15) +
  geom_smooth(method = "lm", se = FALSE, colour = line_colour, linewidth = 1.45) +
  geom_hline(yintercept = 10, linetype = "dashed", colour = "#6A6A6A", linewidth = 0.8) +
  geom_text(
    data = scatter_annotation,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.04,
    vjust = 1.08,
    family = "Arial",
    size = 4.2,
    lineheight = 0.92,
    colour = "black"
  ) +
  facet_wrap(~feature_label, scales = "free_x", ncol = 3) +
  labs(x = "MES feature value", y = "Disintegration time (min)") +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.18))) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 15),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 19),
    panel.spacing = unit(1.0, "lines")
  )
save_dual(p_scatter, "05_d3_key_mes_features_vs_disintegration_time", width = 12.8, height = 10.8)

unlink(
  file.path(figures_dir, c(
    "02_d3_mes_top_features_group_comparison.pdf",
    "02_d3_mes_top_features_group_comparison.png",
    "03_d3_mes_top_features_disintegration_scatter.pdf",
    "03_d3_mes_top_features_disintegration_scatter.png"
  )),
  force = TRUE
)

linkage_summary <- tibble(
  item = c(
    "0.8g finished-product quality records",
    "0.8g records linked to finished-product MES",
    "Unique 0.8g finished-product batches linked to MES",
    "Linked records with disintegration > 10 min",
    "Linked records with disintegration <= 10 min",
    "Candidate MES numeric features screened",
    "MES features passing screening filters"
  ),
  value = c(
    nrow(d2),
    nrow(analysis_df),
    n_distinct(analysis_df$batch_no),
    sum(analysis_df$disintegration_issue, na.rm = TRUE),
    sum(!analysis_df$disintegration_issue, na.rm = TRUE),
    length(feature_cols),
    nrow(feature_screen)
  )
)

workbook_path <- file.path(tables_dir, "D3_MES_finished_disintegration_association_results.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "linkage_summary")
writeData(wb, "linkage_summary", linkage_summary)
addWorksheet(wb, "candidate_feature_audit")
writeData(wb, "candidate_feature_audit", candidate_feature_audit)
addWorksheet(wb, "feature_screening")
writeData(wb, "feature_screening", feature_screen)
addWorksheet(wb, "top_features")
writeData(wb, "top_features", top_features)
saveWorkbook(wb, workbook_path, overwrite = TRUE)

top_text <- top_features |>
  rowwise() |>
  transmute(
    line = paste0(
      "- ", feature_label,
      ": Spearman rho = ", sprintf("%.3f", spearman_rho),
      ", Wilcoxon ", format_p(wilcoxon_p),
      ", unadjusted OR per SD = ", sprintf("%.2f", logistic_or_per_sd),
      ", month-adjusted OR per SD = ", sprintf("%.2f", month_adjusted_or_per_sd),
      ", screening FDR = ", sprintf("%.3g", min_fdr),
      ", month-adjusted FDR = ", sprintf("%.3g", month_adjusted_fdr),
      "."
    )
  ) |>
  ungroup() |>
  pull(line)

note_lines <- c(
  "# D3 finished-product MES association with 0.8g disintegration",
  "",
  "- D2 source: `D2_finished_product_physchem_final.xlsx`.",
  "- D3 source: `D3_finished_product_mes_main_final.xlsx`.",
  "- Endpoint: 0.8g finished-product disintegration time and the binary issue label `>10 min`.",
  "- Linkage: D2 quality-testing records were linked to D3 finished-product MES records by finished-product `batch_no`.",
  "- Multi-value MES fields were collapsed to row-level mean features before screening; SD, min, max, range, CV, and count-derived features were not used in the main association analysis.",
  "- Material input amounts were excluded because they represent formulation or batch input quantities rather than process-state or in-process quality parameters.",
  "- Instrument environment parameters recorded during moisture-meter measurements (granulation/final-blend meter temperature and relative humidity) were retained in the source data but excluded from association screening.",
  "- Screening methods: Spearman correlation with continuous disintegration time, Wilcoxon rank-sum test between `<=10 min` and `>10 min`, and univariate logistic regression for `>10 min` issue risk.",
  "- Month-adjusted logistic regression was also calculated as a sensitivity check because the disintegration issue had a clear time-clustered pattern.",
  "- Interpretation: this is association screening, not causal proof.",
  "- Important sensitivity finding: the strongest unadjusted MES signals were partly time-clustered; after production-month adjustment, the top associations were weakened and did not remain significant after BH correction. These variables should therefore be described as process signatures accompanying the disintegration issue, not independent causal drivers.",
  "",
  "## Linkage summary",
  paste0("- ", linkage_summary$item, ": ", linkage_summary$value),
  "",
  "## Top candidate MES associations",
  top_text,
  "",
  "## Outputs",
  "- Result workbook: `tables/D3_MES_finished_disintegration_association_results.xlsx`.",
  "- Figure 1: `figures/01_d3_mes_feature_screening_rank.pdf`.",
  "- Significant feature boxplots: `figures/02_d3_significant_boxplot_*.pdf`.",
  "- Key scatter plot: `figures/05_d3_key_mes_features_vs_disintegration_time.pdf`."
)
writeLines(note_lines, file.path(docs_dir, "D3_MES_finished_disintegration_association_summary.md"))

cat("Linked 0.8g records:", nrow(analysis_df), "\n")
cat("Issue records:", sum(analysis_df$disintegration_issue), "\n")
cat("Features screened:", nrow(feature_screen), "\n")
cat("Saved workbook:", workbook_path, "\n")
