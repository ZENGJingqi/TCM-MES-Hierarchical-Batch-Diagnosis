options(warn = 1)

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(ggsci)
  library(lubridate)
  library(officer)
  library(openxlsx)
  library(patchwork)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
root_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/", mustWork = TRUE)
cn_data_dir <- file.path(root_dir, "定稿数据_中文")
input_file <- list.files(cn_data_dir, pattern = "^D2.*\\.xlsx$", full.names = TRUE)[1]

docs_dir <- file.path(project_dir, "docs")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

unlink(Sys.glob(file.path(figures_dir, "*.pdf")), force = TRUE)
unlink(Sys.glob(file.path(figures_dir, "*.png")), force = TRUE)

cn_to_en <- c(
  "批号" = "batch_no",
  "生产日期" = "production_date",
  "包衣片重(g)" = "coated_tablet_weight_g",
  "崩解时限(min)" = "disintegration_time_min",
  "成分含量(mg/片)" = "active_content_mg_per_tablet",
  "需氧菌总数(cfu/g)" = "total_aerobic_count_cfu_per_g",
  "霉菌及酵母菌(cfu/g)" = "mold_yeast_count_cfu_per_g",
  "规格" = "dosage_strength"
)

display_labels <- c(
  coated_tablet_weight_g = "Coated tablet weight (g)",
  disintegration_time_min = "Disintegration time (min)",
  active_content_mg_per_tablet = "Active content (mg/tablet)",
  total_aerobic_count_cfu_per_g = "Total aerobic count (CFU/g)",
  mold_yeast_count_cfu_per_g = "Mold and yeast count (CFU/g)",
  dosage_strength = "Dosage strength",
  sample_count = "Sample count",
  production_month = "Production month"
)

field_role_map <- c(
  batch_no = "Primary batch identifier",
  production_date = "Time field",
  coated_tablet_weight_g = "Finished-product physicochemical indicator",
  disintegration_time_min = "Finished-product physicochemical indicator",
  active_content_mg_per_tablet = "Finished-product physicochemical indicator",
  total_aerobic_count_cfu_per_g = "Microbiological overview indicator",
  mold_yeast_count_cfu_per_g = "Microbiological overview indicator",
  dosage_strength = "Grouping field"
)

nejm_palette <- ggsci::pal_nejm("default")(8)
point_colour <- "#8C8C8C"
trend_colour <- "#A83B2B"
distribution_colour <- "#0A5C7A"
reference_line_colour <- "#6A6A6A"

metric_info <- tibble::tribble(
  ~metric, ~label, ~stem,
  "coated_tablet_weight_g", "Coated tablet weight (g)", "coated_tablet_weight",
  "disintegration_time_min", "Disintegration time (min)", "disintegration_time",
  "active_content_mg_per_tablet", "Active content (mg/tablet)", "active_content"
)

prettify_strength <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", "")
  x <- str_replace(x, "^([0-9]+(?:\\.[0-9]+)?)g$", "\\1 g")
  x
}

strength_to_stub <- function(x) {
  x |>
    str_replace_all("\\s+", "") |>
    str_replace_all("\\.", "p")
}

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

  if (file.exists(png_path)) {
    file.remove(png_path)
  }

  suppressWarnings(
    pdftools::pdf_convert(
      pdf = pdf_path,
      format = "png",
      dpi = 330,
      filenames = png_path
    )
  )
}

pretty_month_breaks <- function(dates) {
  dates <- sort(unique(as.Date(dates)))
  n_dates <- length(dates)

  if (n_dates <= 10) {
    return(dates)
  }

  step <- if (n_dates <= 16) 2 else 3
  breaks <- dates[seq(1, n_dates, by = step)]

  if (!identical(tail(breaks, 1), tail(dates, 1))) {
    breaks <- c(breaks, tail(dates, 1))
  }

  unique(breaks)
}

pretty_month_angle <- function(dates) {
  n_dates <- length(unique(as.Date(dates)))

  if (n_dates <= 10) {
    return(0)
  }

  if (n_dates <= 16) {
    return(45)
  }

  60
}

build_flextable <- function(df) {
  flextable(df) |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Arial", part = "all") |>
    bold(part = "header") |>
    align(align = "center", part = "all") |>
    autofit()
}

build_metric_overview_plot <- function(
  raw_df,
  monthly_df,
  metric,
  metric_label,
  point_colour,
  trend_colour,
  distribution_colour,
  threshold = NA_real_
) {
  plot_df <- raw_df |>
    select(production_date, production_month_date, all_of(metric)) |>
    rename(metric_value = all_of(metric)) |>
    filter(!is.na(metric_value), !is.na(production_date))

  monthly_metric_all <- monthly_df |>
    filter(metric == !!metric) |>
    mutate(
      ymin = mean - sd,
      ymax = mean + sd
    ) |>
    arrange(production_month_date)

  monthly_metric_df <- monthly_metric_all |>
    filter(!is.na(mean))

  x_breaks <- pretty_month_breaks(monthly_metric_all$production_month_date)
  x_angle <- pretty_month_angle(monthly_metric_all$production_month_date)

  y_min <- min(c(plot_df$metric_value, monthly_metric_df$ymin), na.rm = TRUE)
  y_max <- max(c(plot_df$metric_value, monthly_metric_df$ymax), na.rm = TRUE)
  y_pad <- max((y_max - y_min) * 0.08, 0.02 * max(abs(c(y_min, y_max)), na.rm = TRUE), 1e-6)
  y_limits <- c(y_min - y_pad, y_max + y_pad)

  p_main <- ggplot() +
    geom_point(
      data = plot_df,
      aes(x = production_date, y = metric_value),
      colour = point_colour,
      size = 2.3
    ) +
    geom_errorbar(
      data = monthly_metric_df,
      aes(x = production_month_date, ymin = ymin, ymax = ymax),
      colour = trend_colour,
      linewidth = 1.05,
      width = 8 / 31
    ) +
    geom_line(
      data = monthly_metric_all,
      aes(x = production_month_date, y = mean, group = 1),
      colour = trend_colour,
      linewidth = 2.4,
      na.rm = FALSE
    ) +
    geom_point(
      data = monthly_metric_df,
      aes(x = production_month_date, y = mean),
      shape = 21,
      size = 4.4,
      stroke = 1.2,
      colour = trend_colour,
      fill = "white"
    ) +
    scale_x_date(
      date_labels = "%Y-%m",
      breaks = x_breaks,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      limits = y_limits,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(x = display_labels["production_month"], y = metric_label) +
    coord_cartesian(clip = "off") +
    theme(
      axis.text.x = element_text(angle = x_angle, hjust = ifelse(x_angle == 0, 0.5, 1), vjust = 1),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(12, 0, 14, 12)
    )

  if (!is.na(threshold)) {
    p_main <- p_main +
      geom_hline(
        yintercept = threshold,
        colour = reference_line_colour,
        linewidth = 1.8,
        linetype = "22"
      )
  }

  p_dist <- ggplot(plot_df, aes(x = "", y = metric_value)) +
    geom_violin(
      fill = distribution_colour,
      colour = NA,
      width = 0.92,
      trim = TRUE
    ) +
    geom_boxplot(
      width = 0.16,
      fill = "white",
      colour = distribution_colour,
      linewidth = 0.95,
      outlier.shape = NA
    ) +
    geom_point(
      position = position_jitter(width = 0.08, height = 0),
      colour = distribution_colour,
      size = 1.9
    ) +
    scale_y_continuous(
      limits = y_limits,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(12, 12, 14, 0)
    )

  p_main + p_dist + plot_layout(widths = c(5.8, 1.4))
}

set_plot_style()

df_cn <- read_xlsx(input_file)
df <- df_cn
names(df) <- unname(cn_to_en[names(df_cn)])
df <- df |>
  mutate(
    production_date = as.Date(production_date),
    production_month_date = floor_date(production_date, unit = "month"),
    production_month = format(production_month_date, "%Y-%m"),
    dosage_strength = prettify_strength(dosage_strength)
  )

numeric_cols <- c(
  "coated_tablet_weight_g",
  "disintegration_time_min",
  "active_content_mg_per_tablet",
  "total_aerobic_count_cfu_per_g",
  "mold_yeast_count_cfu_per_g"
)

df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
dosage_order <- df |>
  distinct(dosage_strength) |>
  mutate(sort_value = as.numeric(str_extract(dosage_strength, "[0-9]+(?:\\.[0-9]+)?"))) |>
  arrange(sort_value) |>
  pull(dosage_strength)

df$dosage_strength <- factor(df$dosage_strength, levels = dosage_order)

batch_counts <- as.character(df$batch_no)
overall_overview <- tibble(
  item = c(
    "Data source",
    "Total records",
    "Unique batches",
    "Duplicated batch rows",
    "Date start",
    "Date end",
    "Dosage strengths",
    "Missing cells"
  ),
  value = c(
    basename(input_file),
    as.character(nrow(df)),
    as.character(n_distinct(df$batch_no)),
    as.character(sum(duplicated(batch_counts))),
    as.character(min(df$production_date, na.rm = TRUE)),
    as.character(max(df$production_date, na.rm = TRUE)),
    paste(dosage_order, collapse = ", "),
    as.character(sum(is.na(df)))
  )
)

spec_overview <- df |>
  group_by(dosage_strength) |>
  summarise(
    record_count = n(),
    unique_batches = n_distinct(batch_no),
    duplicated_batch_rows = sum(duplicated(as.character(batch_no))),
    date_start = min(production_date, na.rm = TRUE),
    date_end = max(production_date, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(record_pct = round(record_count / sum(record_count) * 100, 2)) |>
  select(dosage_strength, record_count, record_pct, unique_batches, duplicated_batch_rows, date_start, date_end)

descriptive_stats <- df |>
  group_by(dosage_strength) |>
  summarise(
    across(
      all_of(numeric_cols),
      list(
        n = ~sum(!is.na(.x)),
        mean = ~round(mean(.x, na.rm = TRUE), 4),
        sd = ~round(sd(.x, na.rm = TRUE), 4),
        median = ~round(median(.x, na.rm = TRUE), 4),
        q1 = ~round(quantile(.x, 0.25, na.rm = TRUE), 4),
        q3 = ~round(quantile(.x, 0.75, na.rm = TRUE), 4),
        min = ~round(min(.x, na.rm = TRUE), 4),
        max = ~round(max(.x, na.rm = TRUE), 4)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

descriptive_long <- descriptive_stats |>
  pivot_longer(-dosage_strength, names_to = c("metric", ".value"), names_sep = "__") |>
  mutate(display_name = display_labels[metric]) |>
  select(dosage_strength, metric, display_name, n, mean, sd, median, q1, q3, min, max)

core_metric_summary <- descriptive_long |>
  filter(metric %in% metric_info$metric) |>
  transmute(
    dosage_strength,
    indicator = display_name,
    n,
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd),
    `Median (Q1, Q3)` = sprintf("%.4f (%.4f, %.4f)", median, q1, q3),
    Range = sprintf("%.4f to %.4f", min, max)
  )

original_en_names <- unname(cn_to_en[names(df_cn)])

field_dictionary <- tibble(
  column_name_cn = names(df_cn),
  column_name_en = original_en_names,
  dtype = sapply(df_cn, function(x) class(x)[1]),
  completeness_pct = round(sapply(df_cn, function(x) mean(!is.na(x)) * 100), 2),
  example_value = sapply(df_cn, function(x) {
    value <- x[which(!is.na(x))[1]]
    if (length(value) == 0 || is.na(value)) "" else as.character(value)
  }),
  role = field_role_map[original_en_names]
)

monthly_metric_stats_raw <- df |>
  group_by(dosage_strength, production_month_date, production_month) |>
  summarise(
    sample_count = n(),
    across(
      all_of(c("coated_tablet_weight_g", "disintegration_time_min", "active_content_mg_per_tablet")),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

monthly_month_grid <- df |>
  group_by(dosage_strength) |>
  summarise(
    month_min = min(production_month_date, na.rm = TRUE),
    month_max = max(production_month_date, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(production_month_date = list(seq(month_min, month_max, by = "month"))) |>
  ungroup() |>
  select(dosage_strength, production_month_date) |>
  unnest(production_month_date) |>
  mutate(production_month = format(production_month_date, "%Y-%m"))

monthly_metric_stats <- monthly_month_grid |>
  left_join(
    monthly_metric_stats_raw,
    by = c("dosage_strength", "production_month_date", "production_month")
  ) |>
  mutate(
    sample_count = replace_na(sample_count, 0),
    across(ends_with("__sd"), ~replace_na(.x, 0))
  )

monthly_metric_long <- monthly_metric_stats |>
  pivot_longer(
    cols = matches("__(mean|sd)$"),
    names_to = c("metric", ".value"),
    names_sep = "__"
  ) |>
  mutate(display_name = display_labels[metric]) |>
  select(dosage_strength, production_month_date, production_month, sample_count, metric, display_name, mean, sd)

trend_summary <- monthly_metric_long |>
  arrange(dosage_strength, metric, production_month_date) |>
  group_by(dosage_strength, metric, display_name) |>
  summarise(
    n_months = sum(!is.na(mean)),
    first_month = first(production_month[!is.na(mean)]),
    last_month = last(production_month[!is.na(mean)]),
    first_mean = round(first(mean[!is.na(mean)]), 4),
    last_mean = round(last(mean[!is.na(mean)]), 4),
    delta_last_minus_first = round(last(mean[!is.na(mean)]) - first(mean[!is.na(mean)]), 4),
    min_monthly_mean = round(min(mean, na.rm = TRUE), 4),
    max_monthly_mean = round(max(mean, na.rm = TRUE), 4),
    .groups = "drop"
  )

spec_split <- split(df, df$dosage_strength)
monthly_split <- split(monthly_metric_long, monthly_metric_long$dosage_strength)
trend_split <- split(trend_summary, trend_summary$dosage_strength)

# Excel output
excel_path <- file.path(tables_dir, "D2_descriptive_summary.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "overall_overview")
writeData(wb, "overall_overview", overall_overview)

addWorksheet(wb, "spec_overview")
writeData(wb, "spec_overview", spec_overview)

addWorksheet(wb, "core_metric_summary")
writeData(wb, "core_metric_summary", core_metric_summary)

for (strength in names(spec_split)) {
  stub <- strength_to_stub(strength)
  addWorksheet(wb, paste0("spec_", stub, "_desc"))
  writeData(
    wb,
    paste0("spec_", stub, "_desc"),
    descriptive_long |> filter(dosage_strength == strength)
  )
  addWorksheet(wb, paste0("spec_", stub, "_monthly"))
  writeData(
    wb,
    paste0("spec_", stub, "_monthly"),
    monthly_metric_long |> filter(dosage_strength == strength)
  )
  addWorksheet(wb, paste0("spec_", stub, "_trend"))
  writeData(
    wb,
    paste0("spec_", stub, "_trend"),
    trend_summary |> filter(dosage_strength == strength)
  )
}

addWorksheet(wb, "field_dictionary")
writeData(wb, "field_dictionary", field_dictionary)
saveWorkbook(wb, excel_path, overwrite = TRUE)

# Composite figures by specification and metric
figure_index <- 1

for (strength in names(spec_split)) {
  stub <- strength_to_stub(strength)
  df_spec <- spec_split[[strength]]
  monthly_spec <- monthly_split[[strength]]

  for (i in seq_len(nrow(metric_info))) {
    metric <- metric_info$metric[i]
    label <- metric_info$label[i]
    stem <- metric_info$stem[i]
    threshold <- if (strength == "0.8 g" && metric == "disintegration_time_min") 10 else NA_real_

    p_overview <- build_metric_overview_plot(
      raw_df = df_spec,
      monthly_df = monthly_spec,
      metric = metric,
      metric_label = label,
      point_colour = point_colour,
      trend_colour = trend_colour,
      distribution_colour = distribution_colour,
      threshold = threshold
    )

    save_plot_dual(
      p_overview,
      sprintf("%02d_%s_overview_%s", figure_index, stub, stem),
      14.2,
      8.8
    )

    figure_index <- figure_index + 1
  }
}

# Markdown note
md_lines <- c(
  "# D2 成品数据描述（按规格分开）",
  "",
  paste0("- 数据源：`", basename(input_file), "`"),
  paste0("- 总记录数：`", nrow(df), "`"),
  paste0("- 唯一批号数：`", n_distinct(df$batch_no), "`"),
  paste0("- 时间范围：`", min(df$production_date, na.rm = TRUE), " ~ ", max(df$production_date, na.rm = TRUE), "`"),
  paste0("- 规格：`", paste(dosage_order, collapse = ", "), "`"),
  "",
  "## 输出原则",
  "",
  "- 不再把 0.5 g 与 0.8 g 放在同一张图中直接比较。",
  "- 按规格分别进行描述和时间序列展示。",
  "- 月度时间序列图使用月均值 ± SD 误差线。",
  "- 每个规格补充三项核心理化指标分布图。",
  "- PDF 与 PNG 保存在同一 `figures` 目录。"
)
writeLines(md_lines, con = file.path(docs_dir, "D2_成品数据描述说明.md"), useBytes = TRUE)

# Word document
doc <- read_docx()
doc <- body_add_par(doc, "D2 成品数据描述（按规格分开）", style = "heading 1")
doc <- body_add_par(doc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc <- body_add_par(doc, "本稿按规格分别描述 D2 成品数据，不在图上直接对比 0.5 g 和 0.8 g。时间序列变化统一使用月均值 ± SD 误差线展示。", style = "Normal")

doc <- body_add_par(doc, "总体概况", style = "heading 2")
doc <- body_add_flextable(doc, build_flextable(overall_overview))
doc <- body_add_par(doc, "规格概况", style = "heading 2")
doc <- body_add_flextable(doc, build_flextable(spec_overview))

for (strength in names(spec_split)) {
  stub <- strength_to_stub(strength)
  spec_df <- spec_split[[strength]]
  monthly_df <- monthly_split[[strength]]
  trend_df <- trend_split[[strength]]
  spec_overview_text <- tibble(
    item = c(
      "Dosage strength",
      "Record count",
      "Unique batches",
      "Duplicated batch rows",
      "Date start",
      "Date end"
    ),
    value = c(
      strength,
      as.character(nrow(spec_df)),
      as.character(n_distinct(spec_df$batch_no)),
      as.character(sum(duplicated(as.character(spec_df$batch_no)))),
      as.character(min(spec_df$production_date, na.rm = TRUE)),
      as.character(max(spec_df$production_date, na.rm = TRUE))
    )
  )

  doc <- body_add_par(doc, paste0(strength, " 描述"), style = "heading 2")
  doc <- body_add_flextable(doc, build_flextable(spec_overview_text))

  doc <- body_add_par(doc, paste0(strength, " 三项核心理化指标描述统计"), style = "heading 3")
  doc <- body_add_flextable(
    doc,
    build_flextable(
      descriptive_long |>
        filter(dosage_strength == strength, metric %in% metric_info$metric)
    )
  )

  doc <- body_add_par(doc, paste0(strength, " 月度时间序列统计"), style = "heading 3")
  doc <- body_add_flextable(
    doc,
    build_flextable(
      monthly_df |>
        select(production_month, sample_count, display_name, mean, sd)
    )
  )

  doc <- body_add_par(doc, paste0(strength, " 月度变化概况"), style = "heading 3")
  doc <- body_add_flextable(doc, build_flextable(trend_df))
}

doc <- body_add_par(doc, "字段字典", style = "heading 2")
doc <- body_add_flextable(doc, build_flextable(field_dictionary))
print(doc, target = file.path(docs_dir, "D2_成品数据描述说明.docx"))

cat("Input:", input_file, "\n")
cat("Saved table workbook:", excel_path, "\n")
cat("Saved docx:", file.path(docs_dir, "D2_成品数据描述说明.docx"), "\n")
cat("Saved markdown:", file.path(docs_dir, "D2_成品数据描述说明.md"), "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
