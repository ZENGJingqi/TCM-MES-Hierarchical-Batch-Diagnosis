options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(lubridate)
  library(officer)
  library(openxlsx)
  library(patchwork)
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
input_file <- list.files(cn_data_dir, pattern = "^D7.*\\.xlsx$", full.names = TRUE)[1]

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

build_flextable <- function(df, font_size = 8.5) {
  flextable(df) |>
    fontsize(size = font_size, part = "all") |>
    font(fontname = "Arial", part = "all") |>
    bold(part = "header") |>
    align(align = "center", part = "all") |>
    autofit()
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

wrap_label <- function(x) {
  str_wrap(x, width = 26)
}

build_metric_overview_plot <- function(raw_df, monthly_df, metric_label) {
  monthly_metric_all <- monthly_df |>
    arrange(production_month_date)

  monthly_metric_df <- monthly_metric_all |>
    filter(!is.na(mean))

  x_breaks <- pretty_month_breaks(monthly_metric_all$production_month_date)
  x_angle <- pretty_month_angle(monthly_metric_all$production_month_date)

  y_min <- min(c(raw_df$value, monthly_metric_df$mean - monthly_metric_df$sd), na.rm = TRUE)
  y_max <- max(c(raw_df$value, monthly_metric_df$mean + monthly_metric_df$sd), na.rm = TRUE)
  y_pad <- max((y_max - y_min) * 0.08, 0.02 * max(abs(c(y_min, y_max)), na.rm = TRUE), 1e-6)
  y_limits <- c(y_min - y_pad, y_max + y_pad)

  p_main <- ggplot() +
    geom_point(
      data = raw_df,
      aes(x = production_date, y = value),
      colour = point_colour,
      size = 1.5
    ) +
    geom_errorbar(
      data = monthly_metric_df,
      aes(x = production_month_date, ymin = mean - sd, ymax = mean + sd),
      colour = trend_colour,
      linewidth = 1.05,
      width = 8 / 31
    ) +
    geom_line(
      data = monthly_metric_all,
      aes(x = production_month_date, y = mean, group = 1),
      colour = trend_colour,
      linewidth = 2.5,
      na.rm = FALSE
    ) +
    geom_point(
      data = monthly_metric_df,
      aes(x = production_month_date, y = mean),
      shape = 21,
      size = 4.2,
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
    labs(x = "Production month", y = wrap_label(metric_label)) +
    coord_cartesian(clip = "off") +
    theme(
      axis.text.x = element_text(angle = x_angle, hjust = ifelse(x_angle == 0, 0.5, 1), vjust = 1),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(12, 0, 14, 18)
    )

  p_dist <- ggplot(raw_df, aes(x = "", y = value)) +
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

d7_raw <- read_xlsx(input_file)
colnames(d7_raw) <- c(
  "batch_no",
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

d7 <- d7_raw |>
  mutate(
    batch_no = as.character(batch_no),
    production_date = as.Date(production_date),
    production_month_date = floor_date(production_date, unit = "month"),
    production_month = format(production_month_date, "%Y-%m"),
    supplier_name = na_if(as.character(supplier_name), "nan")
  )

supplier_map <- c(
  "亳州市中信中药饮片厂" = "Bozhou Zhongxin",
  "河南尚华堂药业股份有限公司" = "Henan Shanghualang",
  "江西樟树天齐堂中药饮片有限公司" = "Jiangxi Tianqitang",
  "洛阳康鑫中药饮片有限公司" = "Luoyang Kangxin"
)

metric_specs <- tibble::tribble(
  ~column, ~label, ~variable_class, ~stage,
  "input_kg", "Input amount (kg)", "Scalar numeric variable", "Material input",
  "rejected_material_weight_kg", "Rejected material weight (kg)", "Scalar numeric variable", "Sorting stage",
  "rejected_material_rate_pct", "Rejected material rate (%)", "Scalar numeric variable", "Sorting stage",
  "process_moisture_values_pct", "Process moisture values (%)", "Expanded multi-value measurement variable", "Process moisture",
  "moisture_meter_temp_c", "Moisture analyzer temperature (°C)", "Scalar numeric variable", "Process moisture",
  "moisture_meter_rh_pct", "Moisture analyzer humidity (%)", "Scalar numeric variable", "Process moisture",
  "through_100_mesh_pct", "Through 100-mesh (%)", "Expanded multi-value measurement variable", "Particle size",
  "through_120_mesh_pct", "Through 120-mesh (%)", "Expanded multi-value measurement variable", "Particle size",
  "yield_pct", "Yield (%)", "Scalar numeric variable", "Yield and balance",
  "mass_balance_pct", "Mass balance (%)", "Scalar numeric variable", "Yield and balance"
)

analysis_long_list <- lapply(seq_len(nrow(metric_specs)), function(i) {
  spec <- metric_specs[i, ]
  token_list <- lapply(d7[[spec$column]], extract_numeric_tokens)
  token_n <- lengths(token_list)

  if (sum(token_n) == 0) {
    return(NULL)
  }

  tibble(
    batch_no = rep(d7$batch_no, token_n),
    production_date = rep(d7$production_date, token_n),
    production_month_date = rep(d7$production_month_date, token_n),
    production_month = rep(d7$production_month, token_n),
    supplier_name = rep(d7$supplier_name, token_n),
    variable = spec$column,
    label = spec$label,
    variable_class = spec$variable_class,
    stage = spec$stage,
    value = unlist(token_list, use.names = FALSE)
  )
})

analysis_long <- bind_rows(analysis_long_list) |>
  filter(!is.na(value))

metric_summary <- analysis_long |>
  group_by(variable, label, variable_class, stage) |>
  summarise(
    batch_n = n_distinct(batch_no),
    value_n = n(),
    month_n = n_distinct(production_month),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    unique_value_n = n_distinct(value),
    .groups = "drop"
  ) |>
  mutate(
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd),
    `Median (Q1, Q3)` = sprintf("%.4f (%.4f, %.4f)", median, q1, q3),
    Range = sprintf("%.4f to %.4f", min, max)
  )

monthly_summary <- analysis_long |>
  group_by(variable, label, variable_class, stage, production_month_date, production_month) |>
  summarise(
    batch_n = n_distinct(batch_no),
    value_n = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(variable, production_month_date)

monthly_extrema <- monthly_summary |>
  group_by(variable, label, variable_class, stage) |>
  summarise(
    first_month = first(production_month),
    last_month = last(production_month),
    first_mean = first(mean),
    last_mean = last(mean),
    delta_last_minus_first = last(mean) - first(mean),
    min_month = production_month[which.min(mean)][1],
    min_monthly_mean = min(mean, na.rm = TRUE),
    max_month = production_month[which.max(mean)][1],
    max_monthly_mean = max(mean, na.rm = TRUE),
    amplitude = max(mean, na.rm = TRUE) - min(mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(amplitude))

supplier_summary <- d7 |>
  mutate(
    supplier_name = replace_na(supplier_name, "Missing"),
    supplier_name = recode(supplier_name, !!!supplier_map, .default = supplier_name)
  ) |>
  count(supplier_name, name = "record_n") |>
  mutate(share_pct = round(record_n / sum(record_n) * 100, 2)) |>
  arrange(desc(record_n))

overall_overview <- tibble(
  item = c(
    "Data source",
    "Total records",
    "Unique yam-powder batches",
    "Time range start",
    "Time range end",
    "Represented months",
    "Supplier categories",
    "Analyzed variables",
    "Expanded multi-value variables",
    "Scalar numeric variables",
    "Expanded numeric observations"
  ),
  value = c(
    basename(input_file),
    as.character(nrow(d7)),
    as.character(n_distinct(d7$batch_no)),
    as.character(min(d7$production_date, na.rm = TRUE)),
    as.character(max(d7$production_date, na.rm = TRUE)),
    as.character(n_distinct(d7$production_month)),
    as.character(nrow(supplier_summary)),
    as.character(nrow(metric_specs)),
    as.character(sum(metric_specs$variable_class != "Scalar numeric variable")),
    as.character(sum(metric_specs$variable_class == "Scalar numeric variable")),
    as.character(nrow(analysis_long))
  )
)

field_dictionary <- tibble(
  column_name_cn = names(read_xlsx(input_file)),
  column_name_en = names(d7_raw),
  variable_class = c(
    "Identifier", "Time field", "Supplier field", "Supplier batch field",
    "Scalar numeric variable", "Scalar numeric variable", "Scalar numeric variable",
    "Expanded multi-value measurement variable", "Scalar numeric variable", "Scalar numeric variable",
    "Expanded multi-value measurement variable", "Expanded multi-value measurement variable",
    "Scalar numeric variable", "Scalar numeric variable"
  ),
  completeness_pct = round(sapply(d7_raw, function(x) mean(!is.na(x)) * 100), 2),
  example_value = sapply(d7_raw, function(x) {
    idx <- which(!is.na(x))[1]
    if (length(idx) == 0 || is.na(idx)) "" else as.character(x[idx])
  })
)

plot_specs <- metric_summary |>
  filter(unique_value_n > 1) |>
  select(variable, label)

excel_path <- file.path(tables_dir, "D7_descriptive_summary.xlsx")
wb <- createWorkbook()
for (sheet_name in c(
  "overall_overview",
  "supplier_summary",
  "metric_summary",
  "monthly_summary",
  "monthly_extrema",
  "field_dictionary"
)) {
  addWorksheet(wb, sheet_name)
}
writeData(wb, "overall_overview", overall_overview)
writeData(wb, "supplier_summary", supplier_summary)
writeData(
  wb,
  "metric_summary",
  metric_summary |>
    select(label, stage, variable_class, batch_n, value_n, month_n, unique_value_n, `Mean ± SD`, `Median (Q1, Q3)`, Range)
)
writeData(
  wb,
  "monthly_summary",
  monthly_summary |>
    select(label, stage, variable_class, production_month, batch_n, value_n, mean, sd, median, q1, q3)
)
writeData(
  wb,
  "monthly_extrema",
  monthly_extrema |>
    select(label, stage, variable_class, first_month, last_month, first_mean, last_mean, delta_last_minus_first, min_month, min_monthly_mean, max_month, max_monthly_mean, amplitude)
)
writeData(wb, "field_dictionary", field_dictionary)
saveWorkbook(wb, excel_path, overwrite = TRUE)

figure_index <- 1
for (i in seq_len(nrow(plot_specs))) {
  plot_var <- plot_specs$variable[i]
  plot_label <- plot_specs$label[i]
  raw_df <- analysis_long |>
    filter(variable == !!plot_var)
  monthly_df <- monthly_summary |>
    filter(variable == !!plot_var)
  p <- build_metric_overview_plot(raw_df, monthly_df, plot_label)
  save_plot_dual(
    p,
    sprintf("%02d_d7_%s", figure_index, plot_var),
    14.2,
    8.8
  )
  figure_index <- figure_index + 1
}

constant_vars <- metric_summary |>
  filter(unique_value_n == 1) |>
  pull(label)

largest_changes <- monthly_extrema |>
  slice_max(order_by = amplitude, n = 5) |>
  pull(label)

results_paragraphs <- c(
  paste0(
    "D7 山药粉 MES 主表共纳入 ", nrow(d7), " 条记录，对应 ", n_distinct(d7$batch_no),
    " 个山药粉批次，时间范围为 ", min(d7$production_date, na.rm = TRUE), " 至 ",
    max(d7$production_date, na.rm = TRUE), "，覆盖 ", n_distinct(d7$production_month), " 个自然月份。"
  ),
  paste0(
    "D7 共包含 ", nrow(metric_specs), " 个可分析数值变量，其中标量数值变量 ",
    sum(metric_specs$variable_class == "Scalar numeric variable"), " 个，多值展开变量 ",
    sum(metric_specs$variable_class != "Scalar numeric variable"), " 个。对过程水分、细度100目和细度120目等字段，本稿按原始值展开分析，而非直接以行均值替代。"
  ),
  paste0(
    "供应商结构上，当前 D7 以 ",
    supplier_summary$supplier_name[1], "（", supplier_summary$record_n[1], " 批, ",
    supplier_summary$share_pct[1], "%）和 ", supplier_summary$supplier_name[2], "（",
    supplier_summary$record_n[2], " 批, ", supplier_summary$share_pct[2], "%）为主；另有 ",
    sum(supplier_summary$supplier_name == "Missing"), " 批记录缺失供应商名称。"
  ),
  paste0(
    "从变量波动看，月度振幅较大的变量主要集中在 ",
    paste(largest_changes, collapse = "、"), "。其中细度100目(%) 在当前定稿数据中恒定为 100.0%，不构成区分性过程信号，因此仅在表格中保留，不作为主图重点。"
  )
)

md_lines_desc <- c(
  "# D7 山药粉 MES 主表数据描述说明",
  "",
  paste0("- 数据源：`", basename(input_file), "`"),
  paste0("- 总记录数：`", nrow(d7), "`"),
  paste0("- 唯一山药粉批次：`", n_distinct(d7$batch_no), "`"),
  paste0("- 时间范围：`", min(d7$production_date, na.rm = TRUE), " ~ ", max(d7$production_date, na.rm = TRUE), "`"),
  paste0("- 供应商类别：`", nrow(supplier_summary), "`"),
  "",
  "## 变量处理口径",
  "",
  "- 标量数值字段按批次记录直接分析。",
  "- 多值字段按原始值展开，不以批次均值代替。",
  "- `Through 100-mesh (%)` 在当前数据中恒定为 100.0，仅保留在统计表中。"
)
writeLines(md_lines_desc, con = file.path(docs_dir, "D7_山药粉MES主表数据描述说明.md"), useBytes = TRUE)

writeLines(
  c(
    "# D7 正式结果分析与文字整理",
    "",
    "## 一、结果概述",
    "",
    paste0("- ", results_paragraphs),
    "",
    "## 二、供应商结构",
    "",
    paste(capture.output(print(supplier_summary)), collapse = "\n")
  ),
  con = file.path(docs_dir, "D7_正式结果分析与文字整理.md"),
  useBytes = TRUE
)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D7 山药粉 MES 主表数据描述说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc_desc <- body_add_par(doc_desc, "本稿对 D7 山药粉 MES 主表进行独立描述，重点说明时间覆盖、供应商结构以及多值过程字段的展开口径。", style = "Normal")
doc_desc <- body_add_par(doc_desc, "总体概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(overall_overview))
doc_desc <- body_add_par(doc_desc, "供应商结构", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(supplier_summary))
doc_desc <- body_add_par(doc_desc, "主要统计摘要", style = "heading 2")
doc_desc <- body_add_flextable(
  doc_desc,
  build_flextable(
    metric_summary |>
      select(label, stage, variable_class, batch_n, value_n, unique_value_n, `Mean ± SD`, `Median (Q1, Q3)`, Range),
    font_size = 7.8
  )
)
doc_desc <- body_add_par(doc_desc, "月度极值概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(monthly_extrema))
doc_desc <- body_add_par(doc_desc, "字段字典", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(field_dictionary, font_size = 7.8))
print(doc_desc, target = file.path(docs_dir, "D7_山药粉MES主表数据描述说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D7 正式结果分析与文字整理", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "结果概述", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "供应商结构", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(supplier_summary))
doc_formal <- body_add_par(doc_formal, "主要统计摘要", style = "heading 2")
doc_formal <- body_add_flextable(
  doc_formal,
  build_flextable(
    metric_summary |>
      select(label, stage, variable_class, batch_n, value_n, `Mean ± SD`, `Median (Q1, Q3)`, Range),
    font_size = 7.6
  )
)
print(doc_formal, target = file.path(docs_dir, "D7_正式结果分析与文字整理.docx"))

cat("Input:", input_file, "\n")
cat("Saved table workbook:", excel_path, "\n")
cat("Saved description docx:", file.path(docs_dir, "D7_山药粉MES主表数据描述说明.docx"), "\n")
cat("Saved formal docx:", file.path(docs_dir, "D7_正式结果分析与文字整理.docx"), "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
