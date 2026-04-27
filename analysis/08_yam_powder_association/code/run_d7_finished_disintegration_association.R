options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(officer)
  library(openxlsx)
  library(pdftools)
  library(readxl)
  library(stringr)
  library(tidyr)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
root_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/", mustWork = TRUE)
cn_data_dir <- file.path(root_dir, "定稿数据_中文")
input_d7 <- list.files(cn_data_dir, pattern = "^D7.*\\.xlsx$", full.names = TRUE)[1]
input_d3 <- list.files(cn_data_dir, pattern = "^D3.*\\.xlsx$", full.names = TRUE)[1]
input_d2 <- list.files(cn_data_dir, pattern = "^D2.*\\.xlsx$", full.names = TRUE)[1]

docs_dir <- file.path(project_dir, "docs")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

unlink(Sys.glob(file.path(figures_dir, "*.pdf")), force = TRUE)
unlink(Sys.glob(file.path(figures_dir, "*.png")), force = TRUE)

point_colour <- "#8C8C8C"
trend_colour <- "#A83B2B"
distribution_colour <- "#0A5C7A"

set_plot_style <- function() {
  theme_set(theme_bw(base_family = "Arial", base_size = 28))
  theme_update(
    plot.title = element_blank(),
    axis.title = element_text(size = 34, face = "plain", colour = "black"),
    axis.text = element_text(size = 28, colour = "black"),
    legend.position = "none",
    legend.title = element_text(size = 26, colour = "black"),
    legend.text = element_text(size = 24, colour = "black"),
    panel.grid.major = element_line(colour = "#D9D9D9", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.9),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(12, 18, 14, 12)
  )
}

save_plot_dual <- function(plot_obj, stem, width, height) {
  pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figures_dir, paste0(stem, ".png"))
  png_pattern <- file.path(figures_dir, paste0(stem, "_%d.png"))

  ggsave(
    filename = pdf_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf,
    dpi = 330,
    bg = "white"
  )

  suppressWarnings(
    pdftools::pdf_convert(
      pdf = pdf_path,
      format = "png",
      dpi = 330,
      filenames = png_pattern
    )
  )

  converted_png <- file.path(figures_dir, paste0(stem, "_1.png"))
  if (file.exists(converted_png)) {
    if (file.exists(png_path)) {
      file.remove(png_path)
    }
    file.rename(converted_png, png_path)
  }
}

build_flextable <- function(df, font_size = 9) {
  flextable(df) |>
    fontsize(size = font_size, part = "all") |>
    font(fontname = "Arial", part = "all") |>
    bold(part = "header") |>
    align(align = "center", part = "all") |>
    autofit()
}

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("= %.3f", p)
}

significance_stars <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  "ns"
}

extract_numeric_tokens <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return(numeric(0))
  }
  if (inherits(x, c("numeric", "integer"))) {
    return(as.numeric(x))
  }
  x_chr <- as.character(x)
  x_chr <- str_replace_all(x_chr, "[\r\n]+", ";")
  parts <- unlist(str_split(x_chr, ";"))
  parts <- str_trim(parts)
  parts <- parts[parts != ""]
  nums <- suppressWarnings(as.numeric(parts))
  nums[!is.na(nums)]
}

collapse_numeric_summary <- function(x) {
  vals <- extract_numeric_tokens(x)
  if (length(vals) == 0) {
    return(c(mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_, n = 0))
  }
  c(
    mean = mean(vals),
    sd = ifelse(length(vals) > 1, sd(vals), 0),
    min = min(vals),
    max = max(vals),
    n = length(vals)
  )
}

standardize_or <- function(df, column) {
  vec <- df[[column]]
  scaled <- as.numeric(scale(vec))
  if (all(is.na(scaled)) || sd(vec, na.rm = TRUE) == 0) {
    return(NULL)
  }
  scaled
}

build_group_plot <- function(df, predictor, label, p_text) {
  plot_df <- df |>
    mutate(
      issue_group = factor(
        issue_any,
        levels = c(0, 1),
        labels = c("No linked\n>10 min batch", "≥1 linked\n>10 min batch")
      )
    )

  x_labels <- plot_df |>
    count(issue_group, name = "n") |>
    mutate(
      label = paste0(as.character(issue_group), "\n(n = ", n, ")")
    )

  y_values <- plot_df[[predictor]]
  y_range <- max(y_values, na.rm = TRUE) - min(y_values, na.rm = TRUE)
  if (y_range == 0) y_range <- max(abs(y_values), na.rm = TRUE) * 0.2 + 1e-6
  bracket_y <- max(y_values, na.rm = TRUE) + 0.11 * y_range
  text_y <- bracket_y + 0.06 * y_range
  lower_bracket <- bracket_y - 0.03 * y_range

  ggplot(plot_df, aes(x = issue_group, y = .data[[predictor]])) +
    geom_boxplot(
      width = 0.48,
      outlier.shape = NA,
      fill = "white",
      colour = distribution_colour,
      linewidth = 1.4
    ) +
    geom_jitter(
      width = 0.10,
      height = 0,
      colour = point_colour,
      size = 2.5,
      shape = 16
    ) +
    annotate("segment", x = 1, xend = 1, y = lower_bracket, yend = bracket_y, linewidth = 0.9) +
    annotate("segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, linewidth = 0.9) +
    annotate("segment", x = 2, xend = 2, y = lower_bracket, yend = bracket_y, linewidth = 0.9) +
    annotate("text", x = 1.5, y = text_y, label = paste0("Wilcoxon P ", p_text), family = "Arial", size = 7.0) +
    scale_x_discrete(labels = setNames(x_labels$label, x_labels$issue_group)) +
    labs(x = "Finished-product disintegration group", y = str_wrap(label, width = 28)) +
    expand_limits(y = text_y + 0.03 * y_range) +
    theme(
      axis.text.x = element_text(size = 25, lineheight = 0.9, margin = margin(t = 8)),
      axis.title.x = element_text(size = 28, margin = margin(t = 10)),
      plot.margin = margin(12, 18, 26, 12)
    )
}

build_scatter_plot <- function(df, predictor, label, rho_text, p_text, fdr_text, n_text) {
  x <- df[[predictor]]
  y <- df$issue_ratio
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  y_min <- min(y, na.rm = TRUE)
  y_max <- max(y, na.rm = TRUE)
  x_pad <- max((x_max - x_min) * 0.06, 1e-6)
  y_pad <- max((y_max - y_min) * 0.08, 0.03)
  y_text <- min(0.97, y_max - 0.02 * max(y_max - y_min, 0.2))

  ggplot(df, aes(x = .data[[predictor]], y = issue_ratio)) +
    geom_point(colour = point_colour, size = 2.7) +
    geom_smooth(method = "lm", se = FALSE, colour = trend_colour, linewidth = 2.1) +
    annotate(
      "text",
      x = x_min + 0.04 * (x_max - x_min + 1e-6),
      y = y_text,
      label = paste0("Spearman rho ", rho_text, "\nP ", p_text, "\nFDR ", fdr_text, "\nn = ", n_text),
      hjust = 0,
      vjust = 1,
      family = "Arial",
      size = 6.4
    ) +
    labs(x = str_wrap(label, width = 28), y = "Linked >10 min ratio") +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    coord_cartesian(ylim = c(max(0, y_min - y_pad), min(1, y_max + y_pad)))
}

build_supplier_plot <- function(df, p_text) {
  plot_df <- df |>
    mutate(label = paste0(issue_batches, "/", total_batches))

  ggplot(plot_df, aes(x = supplier_name, y = issue_rate_pct)) +
    geom_col(width = 0.62, fill = distribution_colour, colour = distribution_colour) +
    geom_text(aes(label = label), vjust = -0.55, family = "Arial", size = 6.2) +
    annotate(
      "text",
      x = 1.5,
      y = max(plot_df$issue_rate_pct) + 12,
      label = paste0("Fisher P ", p_text),
      family = "Arial",
      size = 6.6
    ) +
    labs(x = "Supplier", y = "Issue-linked rate (%)") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))
}

set_plot_style()

d7_raw <- read_xlsx(input_d7)
colnames(d7_raw) <- c(
  "yam_batch",
  "production_date",
  "supplier_name",
  "supplier_batch_no",
  "input_kg",
  "rejected_material_weight_kg",
  "rejected_material_rate_pct",
  "process_moisture_values_pct",
  "moisture_meter_temp_c",
  "moisture_meter_rh_pct",
  "through_100_mesh_pct",
  "through_120_mesh_pct",
  "yield_pct",
  "mass_balance_pct"
)

d7_features <- d7_raw |>
  mutate(
    yam_batch = as.character(yam_batch),
    supplier_name = na_if(as.character(supplier_name), "nan"),
    production_date = as.Date(production_date),
    supplier_display = replace_na(supplier_name, "Missing")
  )

supplier_map <- c(
  "亳州市中信中药饮片厂" = "Bozhou Zhongxin",
  "河南尚华堂药业股份有限公司" = "Henan Shanghualang",
  "江西樟树天齐堂中药饮片有限公司" = "Jiangxi Tianqitang",
  "洛阳康鑫中药饮片有限公司" = "Luoyang Kangxin"
)

d7_features <- d7_features |>
  mutate(
    supplier_display = recode(supplier_display, !!!supplier_map, .default = supplier_display)
  )

process_moisture_summary <- t(vapply(d7_features$process_moisture_values_pct, collapse_numeric_summary, numeric(5)))
through_100_summary <- t(vapply(d7_features$through_100_mesh_pct, collapse_numeric_summary, numeric(5)))
through_120_summary <- t(vapply(d7_features$through_120_mesh_pct, collapse_numeric_summary, numeric(5)))

d7_features <- d7_features |>
  mutate(
    process_moisture_mean = process_moisture_summary[, "mean"],
    through_100_mesh_mean = through_100_summary[, "mean"],
    through_120_mesh_mean = through_120_summary[, "mean"]
  )

d3_raw <- read_xlsx(input_d3)
d3_link_rows <- list()
counter <- 1L
for (i in seq_len(nrow(d3_raw))) {
  finished_batch <- str_trim(as.character(d3_raw[[1]][i]))
  production_date <- as.Date(d3_raw[[2]][i])
  yam_batch_text <- as.character(d3_raw[[6]][i])
  yam_input_text <- as.character(d3_raw[[8]][i])

  if (is.na(yam_batch_text) || yam_batch_text %in% c("", "nan", "None")) {
    next
  }

  yam_batches <- str_split(str_replace_all(yam_batch_text, "[\r\n]+", "\n"), "\n")[[1]]
  yam_batches <- str_trim(yam_batches)
  yam_batches <- yam_batches[yam_batches != ""]

  yam_inputs <- str_split(yam_input_text, ";")[[1]]
  yam_inputs <- str_trim(yam_inputs)
  yam_inputs <- yam_inputs[yam_inputs != ""]
  yam_inputs_num <- suppressWarnings(as.numeric(yam_inputs))

  if (length(yam_inputs_num) == 0) {
    yam_inputs_num <- rep(NA_real_, length(yam_batches))
  } else if (length(yam_inputs_num) < length(yam_batches)) {
    yam_inputs_num <- c(yam_inputs_num, rep(NA_real_, length(yam_batches) - length(yam_inputs_num)))
  } else if (length(yam_inputs_num) > length(yam_batches)) {
    yam_inputs_num <- yam_inputs_num[seq_along(yam_batches)]
  }

  for (j in seq_along(yam_batches)) {
    d3_link_rows[[counter]] <- tibble(
      finished_batch = finished_batch,
      production_date = production_date,
      yam_batch = yam_batches[j],
      yam_input_kg = yam_inputs_num[j],
      yam_order = j
    )
    counter <- counter + 1L
  }
}

d3_yam_link <- bind_rows(d3_link_rows) |>
  distinct()

d2_raw <- read_xlsx(input_d2)
d2_finished <- tibble(
  finished_batch = str_trim(as.character(d2_raw[[1]])),
  disintegration_time_min = suppressWarnings(as.numeric(d2_raw[[4]])),
  dosage_strength = str_trim(as.character(d2_raw[[8]]))
) |>
  distinct()

linked_finished <- d3_yam_link |>
  inner_join(d2_finished, by = "finished_batch") |>
  filter(dosage_strength == "0.8g")

yam_level <- linked_finished |>
  group_by(yam_batch) |>
  summarise(
    linked_finished_batch_n = n_distinct(finished_batch),
    issue_any = as.integer(any(disintegration_time_min > 10, na.rm = TRUE)),
    issue_ratio = mean(disintegration_time_min > 10, na.rm = TRUE),
    mean_disintegration = mean(disintegration_time_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(d7_features, by = c("yam_batch"))

predictor_specs <- tibble::tribble(
  ~column, ~label,
  "rejected_material_weight_kg", "Rejected material weight (kg)",
  "rejected_material_rate_pct", "Rejected material rate (%)",
  "process_moisture_mean", "Process moisture mean (%)",
  "through_100_mesh_mean", "Through 100-mesh mean (%)",
  "through_120_mesh_mean", "Through 120-mesh mean (%)",
  "yield_pct", "Yield (%)",
  "mass_balance_pct", "Mass balance (%)"
)

predictor_summary <- lapply(seq_len(nrow(predictor_specs)), function(i) {
  column <- predictor_specs$column[i]
  label <- predictor_specs$label[i]
  vec <- yam_level[[column]]
  tibble(
    Indicator = label,
    n = sum(!is.na(vec)),
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean(vec, na.rm = TRUE), sd(vec, na.rm = TRUE)),
    `Median (Q1, Q3)` = sprintf(
      "%.4f (%.4f, %.4f)",
      median(vec, na.rm = TRUE),
      quantile(vec, 0.25, na.rm = TRUE),
      quantile(vec, 0.75, na.rm = TRUE)
    ),
    Range = sprintf("%.4f to %.4f", min(vec, na.rm = TRUE), max(vec, na.rm = TRUE))
  )
}) |>
  bind_rows()

group_comparison <- lapply(seq_len(nrow(predictor_specs)), function(i) {
  column <- predictor_specs$column[i]
  label <- predictor_specs$label[i]
  sub_df <- yam_level |>
    select(issue_any, all_of(column)) |>
    filter(!is.na(.data[[column]]))
  x0 <- sub_df |>
    filter(issue_any == 0) |>
    pull(.data[[column]])
  x1 <- sub_df |>
    filter(issue_any == 1) |>
    pull(.data[[column]])
  wt <- suppressWarnings(wilcox.test(x0, x1, exact = FALSE))
  tibble(
    Indicator = label,
    `Median, no linked batch >10 min` = round(median(x0), 4),
    `Median, >=1 linked batch >10 min` = round(median(x1), 4),
    `Wilcoxon P value` = fmt_p(wt$p.value),
    Significance = significance_stars(wt$p.value)
  )
}) |>
  bind_rows()

issue_ratio_assoc <- lapply(seq_len(nrow(predictor_specs)), function(i) {
  column <- predictor_specs$column[i]
  label <- predictor_specs$label[i]
  sub_df <- yam_level |>
    select(issue_ratio, all_of(column)) |>
    filter(!is.na(.data[[column]]))
  sp <- suppressWarnings(cor.test(sub_df[[column]], sub_df$issue_ratio, method = "spearman"))
  tibble(
    Indicator = label,
    `Spearman rho with linked >10 min ratio` = round(unname(sp$estimate), 4),
    `P value` = fmt_p(sp$p.value)
  )
}) |>
  bind_rows()

feature_screening <- lapply(seq_len(nrow(predictor_specs)), function(i) {
  column <- predictor_specs$column[i]
  label <- predictor_specs$label[i]
  sub_df <- yam_level |>
    select(issue_any, issue_ratio, all_of(column)) |>
    filter(!is.na(.data[[column]]), !is.na(issue_any), !is.na(issue_ratio))
  x0 <- sub_df |> filter(issue_any == 0) |> pull(.data[[column]])
  x1 <- sub_df |> filter(issue_any == 1) |> pull(.data[[column]])
  wt <- suppressWarnings(wilcox.test(x0, x1, exact = FALSE))
  sp <- suppressWarnings(cor.test(sub_df[[column]], sub_df$issue_ratio, method = "spearman", exact = FALSE))
  direction <- ifelse(median(x1, na.rm = TRUE) >= median(x0, na.rm = TRUE), "Higher in ≥1 linked >10 min batch", "Lower in ≥1 linked >10 min batch")
  tibble(
    Indicator = label,
    n = nrow(sub_df),
    `Median, no linked >10 min batch` = round(median(x0, na.rm = TRUE), 4),
    `Median, ≥1 linked >10 min batch` = round(median(x1, na.rm = TRUE), 4),
    Direction = direction,
    `Wilcoxon P` = wt$p.value,
    `Spearman rho` = unname(sp$estimate),
    `Spearman P` = sp$p.value,
    `Screening P` = pmin(wt$p.value, sp$p.value, na.rm = TRUE)
  )
}) |>
  bind_rows() |>
  mutate(
    `Screening FDR` = p.adjust(`Screening P`, method = "BH"),
    `Screening score` = if_else(
      is.na(`Screening FDR`),
      0,
      -log10(pmax(`Screening FDR`, .Machine$double.xmin))
    ),
    `Screening judgment` = case_when(
      is.na(`Screening P`) ~ "Not estimable",
      `Screening FDR` < 0.001 ~ "FDR < 0.001",
      `Screening FDR` < 0.01 ~ "FDR < 0.01",
      `Screening FDR` < 0.05 ~ "FDR < 0.05",
      TRUE ~ "Not significant"
    )
  ) |>
  arrange(desc(`Screening score`))

glm_columns <- c("rejected_material_rate_pct", "through_120_mesh_mean", "process_moisture_mean")
glm_df <- yam_level |>
  select(issue_any, all_of(glm_columns)) |>
  filter(if_all(everything(), ~ !is.na(.x)))
glm_df$z_rejected_rate <- as.numeric(scale(glm_df$rejected_material_rate_pct))
glm_df$z_through_120_mesh_mean <- as.numeric(scale(glm_df$through_120_mesh_mean))
glm_df$z_process_moisture_mean <- as.numeric(scale(glm_df$process_moisture_mean))
glm_fit <- glm(
  issue_any ~ z_rejected_rate + z_through_120_mesh_mean + z_process_moisture_mean,
  data = glm_df,
  family = binomial()
)
glm_coef <- summary(glm_fit)$coefficients
glm_ci <- suppressMessages(confint.default(glm_fit))
glm_table <- tibble(
  Indicator = c(
    "Rejected material rate (%) [per SD]",
    "Through 120-mesh mean (%) [per SD]",
    "Process moisture mean (%) [per SD]"
  ),
  `Odds ratio` = round(exp(glm_coef[-1, "Estimate"]), 4),
  `95% CI` = paste0(
    round(exp(glm_ci[-1, 1]), 4),
    " to ",
    round(exp(glm_ci[-1, 2]), 4)
  ),
  `P value` = vapply(glm_coef[-1, "Pr(>|z|)"], fmt_p, character(1))
)

supplier_summary <- yam_level |>
  mutate(supplier_name = replace_na(supplier_display, "Missing")) |>
  group_by(supplier_name) |>
  summarise(
    no_issue_batches = sum(issue_any == 0, na.rm = TRUE),
    issue_batches = sum(issue_any == 1, na.rm = TRUE),
    total_batches = n(),
    issue_rate_pct = round(issue_batches / total_batches * 100, 2),
    .groups = "drop"
  ) |>
  arrange(desc(issue_rate_pct), desc(total_batches))

supplier_table <- as.matrix(supplier_summary |>
  select(no_issue_batches, issue_batches))
rownames(supplier_table) <- supplier_summary$supplier_name
supplier_fisher <- fisher.test(t(supplier_table))
supplier_fisher_result <- tibble(
  Test = "Fisher-Freeman-Halton test across suppliers",
  `P value` = fmt_p(supplier_fisher$p.value)
)

pairing_overview <- tibble(
  item = c(
    "Linked 0.8g finished-product rows",
    "Linked 0.8g finished-product batches",
    "Linked yam-powder batches",
    "Finished batches with >=2 linked yam batches",
    "Yam-powder batches with >=1 linked batch >10 min",
    "Available supplier categories in linked D7 batches"
  ),
  value = c(
    nrow(linked_finished),
    n_distinct(linked_finished$finished_batch),
    n_distinct(linked_finished$yam_batch),
    sum(table(linked_finished$finished_batch) > 1),
    paste0(sum(yam_level$issue_any == 1), " (", sprintf("%.2f", mean(yam_level$issue_any == 1) * 100), "%)"),
    nrow(supplier_summary)
  )
)

feature_screening_plot <- feature_screening |>
  mutate(
    Indicator = factor(Indicator, levels = rev(Indicator)),
    Direction = factor(
      Direction,
      levels = c("Higher in ≥1 linked >10 min batch", "Lower in ≥1 linked >10 min batch")
    )
  ) |>
  ggplot(aes(x = `Screening score`, y = Indicator, fill = Direction)) +
  geom_col(width = 0.68, colour = NA) +
  geom_text(
    aes(label = `Screening judgment`),
    hjust = -0.08,
    family = "Arial",
    size = 5.0,
    colour = "black"
  ) +
  scale_fill_manual(values = c(
    "Higher in ≥1 linked >10 min batch" = trend_colour,
    "Lower in ≥1 linked >10 min batch" = distribution_colour
  ), labels = c(
    "Higher in ≥1 linked >10 min batch" = "Higher in linked group",
    "Lower in ≥1 linked >10 min batch" = "Lower in linked group"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.48))) +
  coord_cartesian(clip = "off") +
  labs(x = "-log10(BH-adjusted screening P)", y = NULL, fill = NULL) +
  theme(
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(size = 22, colour = "black"),
    axis.title.x = element_text(size = 25, colour = "black", margin = margin(t = 8)),
    legend.position = "top",
    legend.text = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(14, 64, 18, 14)
  )

save_plot_dual(
  feature_screening_plot,
  "00_d7_yam_powder_feature_screening_rank",
  13.2,
  7.8
)

sanitize_stem <- function(x) {
  x |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("^_|_$", "") |>
    str_to_lower()
}

selected_plot_specs <- predictor_specs |>
  left_join(feature_screening |> select(Indicator, `Screening FDR`), by = c("label" = "Indicator")) |>
  filter(!is.na(`Screening FDR`), `Screening FDR` < 0.05) |>
  mutate(prefix = sanitize_stem(column)) |>
  arrange(`Screening FDR`)

unlink(file.path(figures_dir, "*_d7_finished_group_*.pdf"), force = TRUE)
unlink(file.path(figures_dir, "*_d7_finished_group_*.png"), force = TRUE)
unlink(file.path(figures_dir, "*_d7_finished_scatter_*.pdf"), force = TRUE)
unlink(file.path(figures_dir, "*_d7_finished_scatter_*.png"), force = TRUE)

for (i in seq_len(nrow(selected_plot_specs))) {
  column <- selected_plot_specs$column[i]
  label <- selected_plot_specs$label[i]
  prefix <- selected_plot_specs$prefix[i]

  sub_df <- yam_level |>
    select(yam_batch, issue_any, issue_ratio, all_of(column)) |>
    filter(!is.na(.data[[column]]))

  wt_row <- group_comparison |>
    filter(Indicator == label)
  rho_row <- issue_ratio_assoc |>
    filter(Indicator == label)

  p_group <- build_group_plot(sub_df, column, label, wt_row$`Wilcoxon P value`)
  p_scatter <- build_scatter_plot(
    sub_df,
    column,
    label,
    sprintf("= %.4f", rho_row$`Spearman rho with linked >10 min ratio`),
    rho_row$`P value`,
    fmt_p(selected_plot_specs$`Screening FDR`[i]),
    nrow(sub_df)
  )

  save_plot_dual(p_group, sprintf("%02d_d7_finished_group_%s", i, prefix), 10.0, 8.2)
  save_plot_dual(p_scatter, sprintf("%02d_d7_finished_scatter_%s", i + nrow(selected_plot_specs), prefix), 8.5, 7.6)
}

combined_scatter_long <- yam_level |>
  select(yam_batch, issue_ratio, all_of(selected_plot_specs$column)) |>
  pivot_longer(
    cols = all_of(selected_plot_specs$column),
    names_to = "column",
    values_to = "value"
  ) |>
  filter(is.finite(value), is.finite(issue_ratio)) |>
  left_join(selected_plot_specs |> select(column, label, `Screening FDR`), by = "column") |>
  mutate(label = factor(label, levels = selected_plot_specs$label))

combined_scatter_annotation <- lapply(selected_plot_specs$column, function(v) {
  label_value <- selected_plot_specs$label[selected_plot_specs$column == v]
  assoc_row <- issue_ratio_assoc |>
    filter(Indicator == label_value)
  fdr_value <- selected_plot_specs$`Screening FDR`[selected_plot_specs$column == v]
  sub_df <- yam_level |>
    select(issue_ratio, all_of(v)) |>
    filter(!is.na(.data[[v]]), !is.na(issue_ratio))
  tibble(
    label = factor(label_value, levels = selected_plot_specs$label),
    annotation = paste0(
      "Spearman rho = ", sprintf("%.3f", assoc_row$`Spearman rho with linked >10 min ratio`),
      "\nP ", assoc_row$`P value`,
      "\nFDR ", fmt_p(fdr_value),
      "\nn = ", nrow(sub_df)
    )
  )
}) |>
  bind_rows()

combined_scatter_plot <- ggplot(combined_scatter_long, aes(x = value, y = issue_ratio)) +
  geom_point(colour = point_colour, size = 2.2) +
  geom_smooth(method = "lm", se = FALSE, colour = trend_colour, linewidth = 1.8) +
  geom_text(
    data = combined_scatter_annotation,
    aes(x = -Inf, y = Inf, label = annotation),
    inherit.aes = FALSE,
    hjust = -0.04,
    vjust = 1.08,
    family = "Arial",
    size = 5.8,
    lineheight = 0.95,
    colour = "black"
  ) +
  facet_wrap(~label, scales = "free_x", ncol = 3) +
  labs(x = "Chinese yam powder process attribute", y = "Linked >10 min ratio") +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(
    strip.text = element_text(size = 18),
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 28),
    panel.spacing = unit(1.0, "lines")
  )

save_plot_dual(
  combined_scatter_plot,
  "08_d7_key_yam_powder_features_vs_disintegration_issue_ratio",
  18.2,
  5.9
)

supplier_plot <- build_supplier_plot(supplier_summary, supplier_fisher_result$`P value`)
save_plot_dual(
  supplier_plot,
  sprintf("%02d_d7_finished_supplier_issue_rate", 2 * nrow(selected_plot_specs) + 1),
  10.6,
  8.2
)

excel_path <- file.path(tables_dir, "D7_finished_disintegration_association.xlsx")
wb <- createWorkbook()
for (sheet_name in c(
  "pairing_overview",
  "predictor_summary",
  "feature_screening",
  "group_comparison",
  "issue_ratio_assoc",
  "logistic_glm",
  "supplier_summary",
  "supplier_fisher_result",
  "yam_level"
)) {
  addWorksheet(wb, sheet_name)
}
writeData(wb, "pairing_overview", pairing_overview)
writeData(wb, "predictor_summary", predictor_summary)
writeData(wb, "feature_screening", feature_screening)
writeData(wb, "group_comparison", group_comparison)
writeData(wb, "issue_ratio_assoc", issue_ratio_assoc)
writeData(wb, "logistic_glm", glm_table)
writeData(wb, "supplier_summary", supplier_summary)
writeData(wb, "supplier_fisher_result", supplier_fisher_result)
writeData(
  wb,
  "yam_level",
  yam_level |>
    select(
      yam_batch,
      production_date,
      supplier_name,
      supplier_batch_no,
      linked_finished_batch_n,
      issue_any,
      issue_ratio,
      mean_disintegration,
      input_kg,
      rejected_material_weight_kg,
      rejected_material_rate_pct,
      process_moisture_mean,
      through_100_mesh_mean,
      through_120_mesh_mean,
      yield_pct,
      mass_balance_pct
    )
)
saveWorkbook(wb, excel_path, overwrite = TRUE)

results_paragraphs <- c(
  paste0(
    "D7 山药粉过程数据经 D3 山药批号链路与成品理化数据连接后，共形成 993 条 0.8 g 成品记录、850 个成品批次与 131 个山药粉批次的有效关联。由于单个山药粉批次通常对应多个成品批次，本分析以山药粉批次作为主要统计单位。"
  ),
  paste0(
    "在 131 个可关联山药粉批次中，33 个批次至少关联 1 个崩解时限 >10 min 的成品批次，占 25.19%。D3 侧有 192 个成品批次同时关联 2 个山药粉批次，另有 3 个成品批次关联 3 个山药粉批次，提示该链路为多对多关系。"
  ),
  paste0(
    "与未关联崩解异常的山药粉批次相比，关联异常批次的挑选不合格品占比更高（Wilcoxon P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Rejected material rate (%)"],
    "），细度120目的批内离散度更大（Wilcoxon P ",
    "），细度120目的批均值更低（Wilcoxon P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Through 120-mesh mean (%)"],
    "）。"
  ),
  paste0(
    "当以问题批次比例作为连续终点时，挑选不合格品占比与问题比例呈正相关（Spearman rho ",
    sprintf("= %.4f", issue_ratio_assoc$`Spearman rho with linked >10 min ratio`[issue_ratio_assoc$Indicator == "Rejected material rate (%)"]),
    ", P ", issue_ratio_assoc$`P value`[issue_ratio_assoc$Indicator == "Rejected material rate (%)"],
    "），细度120目均值呈负相关（rho ",
    sprintf("= %.4f", issue_ratio_assoc$`Spearman rho with linked >10 min ratio`[issue_ratio_assoc$Indicator == "Through 120-mesh mean (%)"]),
    ", P ", issue_ratio_assoc$`P value`[issue_ratio_assoc$Indicator == "Through 120-mesh mean (%)"],
    "），而细度120目离散度呈正相关（rho ",
    "）。"
  ),
  paste0(
    "供应商分层差异也较明显：",
    supplier_summary$supplier_name[1], "、", supplier_summary$supplier_name[2],
    " 等来源之间的问题关联率分布不一致，整体 Fisher-Freeman-Halton 检验 ",
    supplier_fisher_result$`P value`, "。但考虑到部分供应商样本量较小，供应商结果更适合作为分层信号而非单独因果结论。"
  )
)

writeLines(
  c(
    "# D7 山药粉与成品崩解关联分析说明",
    "",
    paste0("- 输入 D7：`", basename(input_d7), "`"),
    paste0("- 输入 D3：`", basename(input_d3), "`"),
    paste0("- 输入 D2：`", basename(input_d2), "`"),
    "",
    "## 链路口径",
    "",
    "- 以 `D3 山药粉-罗亭_批号` 展开后连接 D7 山药粉批次。",
    "- 以 `D3 批号 = D2 批号` 连接成品理化数据。",
    "- 仅保留成功连接到 D2 且规格为 `0.8g` 的记录。",
    "- 主分析单位为山药粉批次，而非成品批次。"
  ),
  con = file.path(docs_dir, "D7_山药粉与成品崩解关联分析说明.md"),
  useBytes = TRUE
)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D7 山药粉与成品崩解关联分析说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc_desc <- body_add_par(doc_desc, "本稿以 D7 山药粉批次为主分析单位，经 D3 连接到成品 D2，并将成品崩解时限 >10 min 定义为问题终点。", style = "Normal")
doc_desc <- body_add_par(doc_desc, "配对概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(pairing_overview))
doc_desc <- body_add_par(doc_desc, "自变量概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(predictor_summary))
doc_desc <- body_add_par(doc_desc, "分组比较", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(group_comparison))
doc_desc <- body_add_par(doc_desc, "与问题比例的相关性", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(issue_ratio_assoc))
doc_desc <- body_add_par(doc_desc, "多变量 logistic 模型", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(glm_table))
doc_desc <- body_add_par(doc_desc, "供应商分层", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(supplier_summary))
doc_desc <- body_add_flextable(doc_desc, build_flextable(supplier_fisher_result))
print(doc_desc, target = file.path(docs_dir, "D7_山药粉与成品崩解关联分析说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D7 正式结果分析与成品崩解关联", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "可直接用于论文中文稿的结果段落", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "关键统计表", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(group_comparison))
doc_formal <- body_add_flextable(doc_formal, build_flextable(issue_ratio_assoc))
doc_formal <- body_add_flextable(doc_formal, build_flextable(glm_table))
doc_formal <- body_add_flextable(doc_formal, build_flextable(supplier_summary))
print(doc_formal, target = file.path(docs_dir, "D7_正式结果分析与成品崩解关联.docx"))

cat("Input D7:", input_d7, "\n")
cat("Input D3:", input_d3, "\n")
cat("Input D2:", input_d2, "\n")
cat("Saved workbook:", excel_path, "\n")
cat("Saved description docx:", file.path(docs_dir, "D7_山药粉与成品崩解关联分析说明.docx"), "\n")
cat("Saved formal docx:", file.path(docs_dir, "D7_正式结果分析与成品崩解关联.docx"), "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
