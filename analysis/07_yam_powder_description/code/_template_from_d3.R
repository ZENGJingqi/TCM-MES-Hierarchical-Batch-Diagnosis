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
input_file <- list.files(cn_data_dir, pattern = "^D3.*\\.xlsx$", full.names = TRUE)[1]

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

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

to_markdown_table <- function(df) {
  df_chr <- df |>
    mutate(across(everything(), as.character))
  header <- paste(names(df_chr), collapse = " | ")
  separator <- paste(rep("---", ncol(df_chr)), collapse = " | ")
  body <- apply(df_chr, 1, function(x) paste(x, collapse = " | "))
  c(
    paste0("| ", header, " |"),
    paste0("| ", separator, " |"),
    paste0("| ", body, " |")
  )
}

replace_keys <- c(
  "包衣片_平均片重(g)",
  "包衣片_片重数量",
  "包衣片_片重数据(g)",
  "包衣片_平均硬度(N)",
  "包衣片_硬度数量",
  "包衣片_硬度数据(N)",
  "素片_平均片重(g)",
  "素片_数量",
  "素片_片重数据(g)",
  "外观_残缺片量(g)",
  "外观_占比(‰)",
  "片重_平均值(g)",
  "片重_个数",
  "片重_所有数据(g)",
  "硬度_平均值(N)",
  "硬度_个数",
  "硬度_所有数据(N)",
  "缺片_重量(g)",
  "缺片_占比(‰)",
  "磨损_重量(g)",
  "磨损_占比(‰)",
  "粒度X(>30目%)",
  "粒度Y(<100目%)",
  "出锅水分(%)",
  "水分仪温度(℃)",
  "水分仪湿度(%)",
  "未过60目(g)",
  "过80目(%)",
  "过60目(%)",
  "物料平衡(%)",
  "得率(%)",
  "投入(kg)",
  "健胃消食片浸膏粉",
  "山药粉-罗亭",
  "13%糊精浆",
  "硬脂酸镁",
  "山楂香精",
  "枸橼酸",
  "混合粉Ⅱ",
  "混合粉II",
  "混合粉Ⅰ",
  "混合粉I",
  "35%乙醇",
  "欧巴代",
  "包衣液",
  "制粒",
  "总混",
  "粉碎",
  "压片",
  "包衣",
  "成品",
  "糊精",
  "批号",
  "供应商",
  "水分(%)"
)

replace_values <- c(
  "coated tablet weight mean (g)",
  "coated tablet weight sample size",
  "coated tablet weight values (g)",
  "coated tablet hardness mean (N)",
  "coated tablet hardness sample size",
  "coated tablet hardness values (N)",
  "core tablet weight mean (g)",
  "core tablet sample size",
  "core tablet weight values (g)",
  "appearance defective tablet mass (g)",
  "appearance defective tablet rate (‰)",
  "tablet weight mean (g)",
  "tablet weight sample size",
  "tablet weight values (g)",
  "hardness mean (N)",
  "hardness sample size",
  "hardness values (N)",
  "missing tablet mass (g)",
  "missing tablet rate (‰)",
  "attrition mass (g)",
  "attrition rate (‰)",
  "fraction >30 mesh (%)",
  "fraction <100 mesh (%)",
  "endpoint moisture (%)",
  "moisture analyzer temperature (°C)",
  "moisture analyzer humidity (%)",
  "retained on 60-mesh (g)",
  "passed through 80-mesh (%)",
  "passed through 60-mesh (%)",
  "mass balance (%)",
  "yield (%)",
  "input (kg)",
  "Extract powder",
  "Chinese yam powder",
  "Dextrin slurry (13%)",
  "Magnesium stearate",
  "Hawthorn flavor",
  "Citric acid",
  "Blend II",
  "Blend II",
  "Blend I",
  "Blend I",
  "35% ethanol",
  "Opadry",
  "Coating suspension",
  "Granulation",
  "Final blending",
  "Milling",
  "Compression",
  "Coating",
  "Finished product",
  "Dextrin",
  "batch no",
  "supplier",
  "moisture (%)"
)

translate_label <- function(x) {
  out <- x
  for (i in seq_along(replace_keys)) {
    out <- str_replace_all(out, fixed(replace_keys[i]), replace_values[i])
  }
  out <- str_replace_all(out, "_", " - ")
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_trim(out)
  out
}

to_snake_code <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "")
}

classify_stage <- function(cn) {
  case_when(
    cn %in% c("批号", "生产日期") ~ "Record metadata",
    str_detect(cn, "^健胃消食片浸膏粉") ~ "Extract powder material",
    str_detect(cn, "^山药粉-罗亭") ~ "Chinese yam powder material",
    str_detect(cn, "^糊精") ~ "Dextrin material",
    str_detect(cn, "^枸橼酸") ~ "Citric acid material",
    str_detect(cn, "^混合粉Ⅱ|^混合粉II") ~ "Blend II material",
    str_detect(cn, "^混合粉Ⅰ|^混合粉I") ~ "Blend I material",
    str_detect(cn, "^13%糊精浆") ~ "13% dextrin slurry",
    str_detect(cn, "^山楂香精") ~ "Hawthorn flavor material",
    str_detect(cn, "^硬脂酸镁") ~ "Magnesium stearate material",
    str_detect(cn, "^35%乙醇") ~ "35% ethanol material",
    str_detect(cn, "^欧巴代") ~ "Opadry material",
    str_detect(cn, "^包衣液") ~ "Coating suspension material",
    str_detect(cn, "^制粒") ~ "Granulation stage",
    str_detect(cn, "^总混") ~ "Final blending stage",
    str_detect(cn, "^粉碎") ~ "Milling stage",
    str_detect(cn, "^压片") ~ "Compression stage",
    str_detect(cn, "^包衣") ~ "Coating stage",
    str_detect(cn, "^成品") ~ "Finished-product stage",
    TRUE ~ "Other"
  )
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

detect_numeric_pattern <- function(x) {
  non_na <- x[!is.na(x)]
  if (length(non_na) == 0) {
    return(FALSE)
  }
  parsed_n <- sum(vapply(non_na, function(v) length(extract_numeric_tokens(v)) > 0, logical(1)))
  parsed_n / length(non_na) >= 0.8
}

detect_multi_value <- function(x) {
  non_na <- as.character(x[!is.na(x)])
  if (length(non_na) == 0) {
    return(FALSE)
  }
  any(str_detect(non_na, ";|[\r\n]"))
}

build_metric_overview_plot <- function(raw_df, monthly_df, metric_label) {
  metric_label_wrapped <- str_wrap(metric_label, width = 26)
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
      size = 1.3
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
      linewidth = 2.6,
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
    labs(x = "Production month", y = metric_label_wrapped) +
    coord_cartesian(clip = "off") +
    theme(
      axis.text.x = element_text(
        angle = x_angle,
        hjust = ifelse(x_angle == 0, 0.5, 1),
        vjust = 1
      ),
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

df_cn <- read_xlsx(input_file)
base_df <- df_cn |>
  transmute(
    batch_no = as.character(`批号`),
    production_date = as.Date(`生产日期`),
    production_month_date = floor_date(production_date, unit = "month"),
    production_month = format(production_month_date, "%Y-%m")
  )

field_dictionary <- tibble(
  column_name_cn = names(df_cn),
  column_name_en = vapply(names(df_cn), translate_label, character(1)),
  stage = vapply(names(df_cn), classify_stage, character(1)),
  dtype = sapply(df_cn, function(x) class(x)[1]),
  completeness_pct = round(sapply(df_cn, function(x) mean(!is.na(x)) * 100), 2),
  example_value = sapply(df_cn, function(x) {
    idx <- which(!is.na(x))[1]
    if (length(idx) == 0 || is.na(idx)) "" else as.character(x[idx])
  }),
  contains_multi_values = sapply(df_cn, detect_multi_value),
  numeric_pattern = sapply(df_cn, detect_numeric_pattern)
)

analysis_catalog <- field_dictionary |>
  mutate(
    variable_class = case_when(
      column_name_cn %in% c("批号", "生产日期") ~ "Identifier",
      str_detect(column_name_cn, "批号$") ~ "Batch link field",
      str_detect(column_name_cn, "供应商$") ~ "Supplier field",
      !numeric_pattern ~ "Non-numeric field",
      contains_multi_values & str_detect(column_name_cn, "投入\\(kg\\)$") ~ "Expanded multi-value feed-event variable",
      contains_multi_values ~ "Expanded multi-value measurement variable",
      TRUE ~ "Scalar numeric variable"
    ),
    included_in_analysis = variable_class %in% c(
      "Expanded multi-value feed-event variable",
      "Expanded multi-value measurement variable",
      "Scalar numeric variable"
    ),
    analysis_unit = case_when(
      variable_class == "Expanded multi-value feed-event variable" ~ "Expanded event-level values",
      variable_class == "Expanded multi-value measurement variable" ~ "Expanded observation-level values",
      variable_class == "Scalar numeric variable" ~ "Recorded batch-level value",
      TRUE ~ "Not plotted"
    )
  ) |>
  mutate(
    column_code = vapply(column_name_en, to_snake_code, character(1))
  )

analysis_vars <- analysis_catalog |>
  filter(included_in_analysis) |>
  select(
    column_name_cn,
    column_name_en,
    column_code,
    stage,
    variable_class,
    analysis_unit,
    completeness_pct
  )

expanded_list <- lapply(seq_len(nrow(analysis_vars)), function(i) {
  cn_name <- analysis_vars$column_name_cn[i]
  values <- df_cn[[cn_name]]
  token_list <- lapply(values, extract_numeric_tokens)
  token_n <- lengths(token_list)

  if (sum(token_n) == 0) {
    return(NULL)
  }

  tibble(
    batch_no = rep(base_df$batch_no, token_n),
    production_date = rep(base_df$production_date, token_n),
    production_month_date = rep(base_df$production_month_date, token_n),
    production_month = rep(base_df$production_month, token_n),
    variable_cn = cn_name,
    variable_en = analysis_vars$column_name_en[i],
    variable_code = analysis_vars$column_code[i],
    stage = analysis_vars$stage[i],
    variable_class = analysis_vars$variable_class[i],
    value = unlist(token_list, use.names = FALSE)
  )
})

analysis_long <- bind_rows(expanded_list) |>
  filter(!is.na(value))

variable_overview <- analysis_catalog |>
  count(variable_class, included_in_analysis, name = "variable_n") |>
  arrange(desc(included_in_analysis), variable_class)

analysis_data_overview <- tibble(
  item = c(
    "Data source",
    "Total D3 records",
    "Unique finished-product batches",
    "Time range start",
    "Time range end",
    "Total columns",
    "Analyzed variables",
    "Scalar numeric variables",
    "Expanded multi-value variables",
    "Excluded identifier/batch/supplier/non-numeric fields",
    "Expanded numeric observations"
  ),
  value = c(
    basename(input_file),
    as.character(nrow(df_cn)),
    as.character(n_distinct(base_df$batch_no)),
    as.character(min(base_df$production_date, na.rm = TRUE)),
    as.character(max(base_df$production_date, na.rm = TRUE)),
    as.character(ncol(df_cn)),
    as.character(nrow(analysis_vars)),
    as.character(sum(analysis_vars$variable_class == "Scalar numeric variable")),
    as.character(sum(analysis_vars$variable_class != "Scalar numeric variable")),
    as.character(sum(!analysis_catalog$included_in_analysis)),
    as.character(nrow(analysis_long))
  )
)

metric_summary <- analysis_long |>
  group_by(variable_cn, variable_en, variable_code, stage, variable_class) |>
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
    .groups = "drop"
  ) |>
  mutate(
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd),
    `Median (Q1, Q3)` = sprintf("%.4f (%.4f, %.4f)", median, q1, q3),
    Range = sprintf("%.4f to %.4f", min, max)
  ) |>
  arrange(stage, variable_en)

monthly_summary <- analysis_long |>
  group_by(variable_cn, variable_en, variable_code, stage, variable_class, production_month_date, production_month) |>
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
  arrange(stage, variable_en, production_month_date)

monthly_extrema <- monthly_summary |>
  group_by(variable_cn, variable_en, variable_code, stage, variable_class) |>
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

stage_overview <- analysis_vars |>
  count(stage, variable_class, name = "variable_n") |>
  arrange(stage, variable_class)

excel_path <- file.path(tables_dir, "D3_descriptive_summary.xlsx")
wb <- createWorkbook()
for (sheet_name in c(
  "overall_overview",
  "variable_overview",
  "stage_overview",
  "analysis_catalog",
  "metric_summary",
  "monthly_summary",
  "monthly_extrema",
  "field_dictionary"
)) {
  addWorksheet(wb, sheet_name)
}

writeData(wb, "overall_overview", analysis_data_overview)
writeData(wb, "variable_overview", variable_overview)
writeData(wb, "stage_overview", stage_overview)
writeData(wb, "analysis_catalog", analysis_vars)
writeData(
  wb,
  "metric_summary",
  metric_summary |>
    select(
      variable_en,
      stage,
      variable_class,
      batch_n,
      value_n,
      month_n,
      `Mean ± SD`,
      `Median (Q1, Q3)`,
      Range
    )
)
writeData(
  wb,
  "monthly_summary",
  monthly_summary |>
    select(
      variable_en,
      stage,
      variable_class,
      production_month,
      batch_n,
      value_n,
      mean,
      sd,
      median,
      q1,
      q3
    )
)
writeData(
  wb,
  "monthly_extrema",
  monthly_extrema |>
    select(
      variable_en,
      stage,
      variable_class,
      first_month,
      last_month,
      first_mean,
      last_mean,
      delta_last_minus_first,
      min_month,
      min_monthly_mean,
      max_month,
      max_monthly_mean,
      amplitude
    )
)
writeData(wb, "field_dictionary", analysis_catalog)
saveWorkbook(wb, excel_path, overwrite = TRUE)

figure_index <- 1
for (i in seq_len(nrow(analysis_vars))) {
  variable_code <- analysis_vars$column_code[i]
  variable_label <- analysis_vars$column_name_en[i]
  raw_df <- analysis_long |>
    filter(variable_code == !!variable_code)
  monthly_df <- monthly_summary |>
    filter(variable_code == !!variable_code)

  if (nrow(raw_df) == 0 || nrow(monthly_df) == 0) {
    next
  }

  p_overview <- build_metric_overview_plot(
    raw_df = raw_df,
    monthly_df = monthly_df,
    metric_label = variable_label
  )

  save_plot_dual(
    p_overview,
    sprintf("%02d_d3_%s", figure_index, variable_code),
    14.2,
    8.8
  )

  figure_index <- figure_index + 1
}

key_raw_metrics <- analysis_catalog |>
  filter(
    included_in_analysis,
    str_detect(column_name_cn, "所有数据|片重数据|硬度数据|出锅水分")
  ) |>
  pull(column_code)

key_metric_text <- metric_summary |>
  filter(variable_code %in% key_raw_metrics) |>
  select(variable_en, value_n, `Mean ± SD`, `Median (Q1, Q3)`)

largest_amplitude <- monthly_extrema |>
  slice_max(order_by = amplitude, n = 10) |>
  select(variable_en, stage, variable_class, min_month, max_month, amplitude)

results_paragraphs <- c(
  paste0(
    "D3 成品 MES 主表共包含 ", nrow(df_cn), " 条记录，对应 ", n_distinct(base_df$batch_no),
    " 个成品批次，时间范围为 ", min(base_df$production_date, na.rm = TRUE), " 至 ",
    max(base_df$production_date, na.rm = TRUE), "。全表共 ", ncol(df_cn), " 个字段，其中 ",
    nrow(analysis_vars), " 个字段纳入数值型时间序列描述。"
  ),
  paste0(
    "在纳入分析的字段中，标量数值变量 ",
    sum(analysis_vars$variable_class == "Scalar numeric variable"),
    " 个，多值展开变量 ",
    sum(analysis_vars$variable_class != "Scalar numeric variable"),
    " 个。对于含分号或换行的字段，本稿不以单纯均值替代，而是按原始值展开；因此，D3 描述不仅覆盖批次层记录，也覆盖批内多次投料或多次测量值。"
  ),
  paste0(
    "从阶段结构看，D3 同时包含原辅料投入、制粒、总混、粉碎、压片、包衣及成品阶段变量，其中压片和包衣阶段的多值测量最为丰富。仅压片片重原始值、压片硬度原始值、包衣素片片重原始值、包衣包衣片片重原始值和包衣包衣片硬度原始值等关键字段，即累计展开出 ",
    sum(key_metric_text$value_n), " 条数值观测。"
  ),
  paste0(
    "从月度时间序列看，D3 更适合作为过程变量池和阶段性漂移监测层，而不是单一问题终点。不同变量的波动幅度与计量层级差异较大，后续进入问题驱动分析时，应优先按工艺阶段和变量结构分层筛选，而不是将所有字段在同一层面直接比较。"
  )
)

md_lines_desc <- c(
  "# D3 成品 MES 主表数据描述说明",
  "",
  paste0("- 数据源：`", basename(input_file), "`"),
  paste0("- 总记录数：`", nrow(df_cn), "`"),
  paste0("- 唯一成品批次：`", n_distinct(base_df$batch_no), "`"),
  paste0("- 时间范围：`", min(base_df$production_date, na.rm = TRUE), " ~ ", max(base_df$production_date, na.rm = TRUE), "`"),
  paste0("- 总字段数：`", ncol(df_cn), "`"),
  paste0("- 纳入时间序列描述的变量数：`", nrow(analysis_vars), "`"),
  "",
  "## 变量处理口径",
  "",
  "- 标识类字段（批号、供应商、物料批号）不做数值型时间序列图。",
  "- 标量数值字段按批次记录直接分析。",
  "- 含分号或换行的多值字段按原始值展开分析，不以均值替代。",
  "- 对于多次投料字段，展开后的数值代表事件级投入值；对于片重、硬度等字段，展开后的数值代表批内原始测量值。"
)
writeLines(md_lines_desc, con = file.path(docs_dir, "D3_成品MES主表数据描述说明.md"), useBytes = TRUE)

md_lines_formal <- c(
  "# D3 正式结果分析与文字整理",
  "",
  "## 一、结果概述",
  "",
  paste0("- ", results_paragraphs),
  "",
  "## 二、变量结构概况",
  "",
  to_markdown_table(variable_overview),
  "",
  "## 三、工艺阶段变量概况",
  "",
  to_markdown_table(stage_overview),
  "",
  "## 四、多值关键字段示例",
  "",
  to_markdown_table(key_metric_text)
)
writeLines(md_lines_formal, con = file.path(docs_dir, "D3_正式结果分析与文字整理.md"), useBytes = TRUE)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D3 成品 MES 主表数据描述说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc_desc <- body_add_par(doc_desc, "本稿对 D3 成品 MES 主表进行独立描述，重点说明变量结构、时间覆盖、变量分层以及多值字段的展开口径。", style = "Normal")
doc_desc <- body_add_par(doc_desc, "总体概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(analysis_data_overview))
doc_desc <- body_add_par(doc_desc, "变量结构概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(variable_overview))
doc_desc <- body_add_par(doc_desc, "工艺阶段变量概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(stage_overview))
doc_desc <- body_add_par(doc_desc, "纳入分析变量清单", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(analysis_vars, font_size = 7.8))
doc_desc <- body_add_par(doc_desc, "主要统计摘要", style = "heading 2")
doc_desc <- body_add_flextable(
  doc_desc,
  build_flextable(
    metric_summary |>
      select(variable_en, stage, variable_class, batch_n, value_n, `Mean ± SD`, `Median (Q1, Q3)`, Range),
    font_size = 7.2
  )
)
doc_desc <- body_add_par(doc_desc, "月度振幅最大的变量（前10）", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(largest_amplitude))
doc_desc <- body_add_par(doc_desc, "字段字典", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(analysis_catalog, font_size = 7.0))
print(doc_desc, target = file.path(docs_dir, "D3_成品MES主表数据描述说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D3 正式结果分析与文字整理", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "结果概述", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "变量结构概况", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(variable_overview))
doc_formal <- body_add_par(doc_formal, "工艺阶段变量概况", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(stage_overview))
doc_formal <- body_add_par(doc_formal, "多值关键字段示例", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(key_metric_text))
print(doc_formal, target = file.path(docs_dir, "D3_正式结果分析与文字整理.docx"))

cat("Input:", input_file, "\n")
cat("Saved table workbook:", excel_path, "\n")
cat("Saved description docx:", file.path(docs_dir, "D3_成品MES主表数据描述说明.docx"), "\n")
cat("Saved formal docx:", file.path(docs_dir, "D3_正式结果分析与文字整理.docx"), "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
