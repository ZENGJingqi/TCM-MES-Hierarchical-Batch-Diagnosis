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
  library(readxl)
  library(stringr)
  library(tidyr)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
root_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/", mustWork = TRUE)
docs_dir <- file.path(project_dir, "docs")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

unlink(Sys.glob(file.path(figures_dir, "*.pdf")), force = TRUE)
unlink(Sys.glob(file.path(figures_dir, "*.png")), force = TRUE)

choose_source_file <- function(prefix) {
  files <- list.files(
    path = root_dir,
    pattern = paste0("^", prefix, ".*\\.xlsx$"),
    recursive = TRUE,
    full.names = TRUE
  )
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  files <- files[!grepl("/docs/|/tables/|/figures/|/code/", files)]
  if (length(files) == 0) {
    stop("No source file found for prefix: ", prefix)
  }

  rel_path <- substring(files, nchar(root_dir) + 2)
  path_depth <- str_count(rel_path, fixed("/"))
  files <- files[path_depth == min(path_depth)]

  preferred <- files[!grepl("final", basename(files), ignore.case = TRUE)]
  if (length(preferred) > 0) {
    return(preferred[1])
  }

  files[1]
}

input_d4 <- choose_source_file("D4")
input_d3 <- choose_source_file("D3")

point_colour <- "#8C8C8C"
trend_colour <- "#A83B2B"
distribution_colour <- "#0A5C7A"

display_labels <- c(
  approximate_batch_month = "Production month",
  moisture_pct = "Moisture (%)",
  total_ash_pct = "Total ash (%)",
  extract_pct = "Extract (%)",
  hesperidin_mg_g = "Hesperidin content (mg/g)"
)

metric_info <- tibble::tribble(
  ~metric, ~label, ~stem,
  "moisture_pct", display_labels[["moisture_pct"]], "moisture",
  "total_ash_pct", display_labels[["total_ash_pct"]], "total_ash",
  "extract_pct", display_labels[["extract_pct"]], "extract",
  "hesperidin_mg_g", display_labels[["hesperidin_mg_g"]], "hesperidin_content"
)

set_plot_style <- function() {
  theme_set(theme_bw(base_family = "Arial", base_size = 28))
  theme_update(
    plot.title = element_blank(),
    axis.title = element_text(size = 34, face = "plain", colour = "black"),
    axis.text = element_text(size = 28, colour = "black"),
    legend.position = "none",
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

build_flextable <- function(df, font_size = 9) {
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

build_metric_overview_plot <- function(raw_df, monthly_df, metric, metric_label) {
  plot_df <- raw_df |>
    select(pseudo_date, inferred_month, all_of(metric)) |>
    rename(metric_value = all_of(metric)) |>
    filter(!is.na(metric_value), !is.na(pseudo_date))

  monthly_metric_all <- monthly_df |>
    filter(metric == !!metric) |>
    mutate(
      ymin = mean - sd,
      ymax = mean + sd
    ) |>
    arrange(inferred_month)

  monthly_metric_df <- monthly_metric_all |>
    filter(!is.na(mean))

  x_breaks <- pretty_month_breaks(monthly_metric_all$inferred_month)
  x_angle <- pretty_month_angle(monthly_metric_all$inferred_month)

  y_min <- min(c(plot_df$metric_value, monthly_metric_df$ymin), na.rm = TRUE)
  y_max <- max(c(plot_df$metric_value, monthly_metric_df$ymax), na.rm = TRUE)
  y_pad <- max((y_max - y_min) * 0.08, 0.02 * max(abs(c(y_min, y_max)), na.rm = TRUE), 1e-6)
  y_limits <- c(y_min - y_pad, y_max + y_pad)

  p_main <- ggplot() +
    geom_point(
      data = plot_df,
      aes(x = pseudo_date, y = metric_value),
      colour = point_colour,
      size = 2.3
    ) +
    geom_errorbar(
      data = monthly_metric_df,
      aes(x = inferred_month, ymin = ymin, ymax = ymax),
      colour = trend_colour,
      linewidth = 1.05,
      width = 10 / 31
    ) +
    geom_line(
      data = monthly_metric_all,
      aes(x = inferred_month, y = mean, group = 1),
      colour = trend_colour,
      linewidth = 2.8,
      na.rm = FALSE
    ) +
    geom_point(
      data = monthly_metric_df,
      aes(x = inferred_month, y = mean),
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
    labs(x = display_labels[["approximate_batch_month"]], y = metric_label) +
    coord_cartesian(clip = "off") +
    theme(
      axis.text.x = element_text(angle = x_angle, hjust = ifelse(x_angle == 0, 0.5, 1), vjust = 1),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(12, 0, 14, 12)
    )

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

d4_raw <- read_xlsx(input_d4)
if (ncol(d4_raw) < 5) {
  stop("D4 source has fewer than 5 columns.")
}

d4 <- d4_raw |>
  transmute(
    batch_no = as.character(`批号`),
    moisture_pct = as.numeric(`水分%`),
    total_ash_pct = as.numeric(`总灰分%`),
    extract_pct = as.numeric(`浸出物%`),
    hesperidin_mg_g = as.numeric(`橙皮苷含量mg/g`)
  ) |>
  mutate(
    batch_no = str_trim(batch_no),
    batch_ym = str_sub(batch_no, 1, 4),
    inferred_month = ym(paste0("20", batch_ym))
  ) |>
  arrange(inferred_month, batch_no) |>
  group_by(inferred_month) |>
  mutate(
    batch_rank_in_month = row_number(),
    batch_count_in_month = n(),
    month_days = days_in_month(inferred_month),
    pseudo_day_offset = round((batch_rank_in_month - 0.5) / batch_count_in_month * pmax(month_days - 1, 1)),
    pseudo_date = inferred_month + days(pseudo_day_offset)
  ) |>
  ungroup()

d3_raw <- read_xlsx(input_d3)
d3_link <- d3_raw |>
  transmute(
    production_date = as.Date(`生产日期`),
    extract_batch = as.character(`健胃消食片浸膏粉_批号`)
  ) |>
  mutate(extract_batch = str_trim(extract_batch)) |>
  filter(!is.na(extract_batch), extract_batch != "") |>
  mutate(
    extract_batch_ym = str_sub(extract_batch, 1, 4),
    inferred_extract_month = ym(paste0("20", extract_batch_ym)),
    prod_month = floor_date(production_date, unit = "month"),
    month_diff = interval(inferred_extract_month, prod_month) %/% months(1)
  ) |>
  filter(!is.na(inferred_extract_month), !is.na(prod_month))

missing_months <- setdiff(
  format(seq(min(d4$inferred_month), max(d4$inferred_month), by = "month"), "%Y-%m"),
  format(unique(d4$inferred_month), "%Y-%m")
)

overall_overview <- tibble(
  item = c(
    "Data source",
    "Total records",
    "Unique batches",
    "Approximate month start",
    "Approximate month end",
    "Represented months",
    "Missing calendar months in range",
    "Missing cells"
  ),
  value = c(
    basename(input_d4),
    as.character(nrow(d4)),
    as.character(n_distinct(d4$batch_no)),
    as.character(min(d4$inferred_month)),
    as.character(max(d4$inferred_month)),
    as.character(n_distinct(d4$inferred_month)),
    ifelse(length(missing_months) == 0, "None", paste(missing_months, collapse = ", ")),
    as.character(sum(is.na(d4)))
  )
)

batch_month_validation <- tibble(
  item = c(
    "D4 batches with valid YYMM prefix",
    "D4 batches with invalid YYMM prefix",
    "D3 linked extract-batch records",
    "D3 linked extract batches with lag 0-3 months",
    "Median lag from inferred extract month to D3 production month",
    "Lag range from inferred extract month to D3 production month"
  ),
  value = c(
    as.character(sum(!is.na(d4$inferred_month))),
    as.character(sum(is.na(d4$inferred_month))),
    as.character(nrow(d3_link)),
    as.character(sum(d3_link$month_diff >= 0 & d3_link$month_diff <= 3)),
    paste0(median(d3_link$month_diff), " month"),
    paste0(min(d3_link$month_diff), " to ", max(d3_link$month_diff), " months")
  )
)

metric_summary_numeric <- tibble(
  metric = metric_info$metric,
  indicator = metric_info$label,
  n = sapply(metric_info$metric, function(x) sum(!is.na(d4[[x]]))),
  mean = sapply(metric_info$metric, function(x) mean(d4[[x]], na.rm = TRUE)),
  sd = sapply(metric_info$metric, function(x) sd(d4[[x]], na.rm = TRUE)),
  median = sapply(metric_info$metric, function(x) median(d4[[x]], na.rm = TRUE)),
  q1 = sapply(metric_info$metric, function(x) quantile(d4[[x]], 0.25, na.rm = TRUE)),
  q3 = sapply(metric_info$metric, function(x) quantile(d4[[x]], 0.75, na.rm = TRUE)),
  min = sapply(metric_info$metric, function(x) min(d4[[x]], na.rm = TRUE)),
  max = sapply(metric_info$metric, function(x) max(d4[[x]], na.rm = TRUE))
)

core_metric_summary <- metric_summary_numeric |>
  transmute(
    Indicator = indicator,
    n,
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd),
    `Median (Q1, Q3)` = sprintf("%.4f (%.4f, %.4f)", median, q1, q3),
    Range = sprintf("%.4f to %.4f", min, max)
  )

monthly_grid <- tibble(
  inferred_month = seq(min(d4$inferred_month), max(d4$inferred_month), by = "month")
) |>
  mutate(month_label = format(inferred_month, "%Y-%m"))

monthly_stats_raw <- d4 |>
  group_by(inferred_month) |>
  summarise(
    sample_count = n(),
    across(
      all_of(metric_info$metric),
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

monthly_stats <- monthly_grid |>
  left_join(monthly_stats_raw, by = "inferred_month") |>
  mutate(
    sample_count = replace_na(sample_count, 0L),
    across(ends_with("__sd"), ~replace_na(.x, 0))
  )

monthly_metric_long <- monthly_stats |>
  pivot_longer(
    cols = matches("__(mean|sd)$"),
    names_to = c("metric", ".value"),
    names_sep = "__"
  ) |>
  mutate(display_name = display_labels[metric]) |>
  select(inferred_month, month_label, sample_count, metric, display_name, mean, sd)

monthly_count_table <- monthly_stats |>
  select(inferred_month, month_label, sample_count) |>
  distinct() |>
  arrange(inferred_month)

monthly_extrema <- monthly_metric_long |>
  filter(!is.na(mean)) |>
  group_by(metric, display_name) |>
  summarise(
    min_month = month_label[which.min(mean)][1],
    min_monthly_mean = min(mean, na.rm = TRUE),
    max_month = month_label[which.max(mean)][1],
    max_monthly_mean = max(mean, na.rm = TRUE),
    amplitude = max(mean, na.rm = TRUE) - min(mean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(amplitude))

lag_distribution <- d3_link |>
  count(month_diff, name = "record_count") |>
  arrange(month_diff)

field_dictionary <- tibble(
  column_name_cn = names(d4_raw),
  column_name_en = c("batch_no", "moisture_pct", "total_ash_pct", "extract_pct", "hesperidin_mg_g"),
  dtype = sapply(d4_raw, function(x) class(x)[1]),
  completeness_pct = round(sapply(d4_raw, function(x) mean(!is.na(x)) * 100), 2),
  example_value = sapply(d4_raw, function(x) {
    idx <- which(!is.na(x))[1]
    if (length(idx) == 0 || is.na(idx)) "" else as.character(x[idx])
  })
)

excel_path <- file.path(tables_dir, "D4_descriptive_summary.xlsx")
wb <- createWorkbook()
for (sheet_name in c(
  "overall_overview",
  "batch_month_validation",
  "core_metric_summary",
  "monthly_batch_count",
  "monthly_metric_long",
  "monthly_extrema",
  "lag_distribution",
  "field_dictionary"
)) {
  addWorksheet(wb, sheet_name)
}

writeData(wb, "overall_overview", overall_overview)
writeData(wb, "batch_month_validation", batch_month_validation)
writeData(wb, "core_metric_summary", core_metric_summary)
writeData(wb, "monthly_batch_count", monthly_count_table)
writeData(wb, "monthly_metric_long", monthly_metric_long)
writeData(wb, "monthly_extrema", monthly_extrema)
writeData(wb, "lag_distribution", lag_distribution)
writeData(wb, "field_dictionary", field_dictionary)
saveWorkbook(wb, excel_path, overwrite = TRUE)

figure_index <- 1
for (i in seq_len(nrow(metric_info))) {
  metric <- metric_info$metric[i]
  label <- metric_info$label[i]
  stem <- metric_info$stem[i]
  p_overview <- build_metric_overview_plot(
    raw_df = d4,
    monthly_df = monthly_metric_long,
    metric = metric,
    metric_label = label
  )
  save_plot_dual(
    p_overview,
    sprintf("%02d_d4_overview_%s", figure_index, stem),
    14.2,
    8.8
  )
  figure_index <- figure_index + 1
}

sample_count_min_row <- monthly_count_table |> filter(sample_count == min(sample_count[month_label %in% unique(monthly_metric_long$month_label[!is.na(monthly_metric_long$mean)])]))
sample_count_max_row <- monthly_count_table |> filter(sample_count == max(sample_count))

moisture_row <- monthly_extrema |> filter(metric == "moisture_pct")
ash_row <- monthly_extrema |> filter(metric == "total_ash_pct")
extract_row <- monthly_extrema |> filter(metric == "extract_pct")
hesperidin_row <- monthly_extrema |> filter(metric == "hesperidin_mg_g")

results_paragraphs <- c(
  paste0(
    "D4 浸膏粉理化数据共纳入 ", nrow(d4), " 条记录，对应 ", n_distinct(d4$batch_no),
    " 个唯一批次。由于该表未提供显式检测日期，本分析采用批号前四位 YYMM 作为近似批次月份。"
  ),
  paste0(
    "该月份代理口径可被 D3 中可连接的浸膏粉批次支持：共 ", nrow(d3_link),
    " 条关联记录中，成品生产月份均落在推断浸膏粉月份之后的 0–3 个月内，中位滞后为 ",
    median(d3_link$month_diff), " 个月，说明用批号推断批次月份用于描述性时间结构是可接受的。"
  ),
  paste0(
    "按批号映射后，D4 覆盖的近似月份范围为 ", format(min(d4$inferred_month), "%Y-%m"),
    " 至 ", format(max(d4$inferred_month), "%Y-%m"), "，共 ",
    n_distinct(d4$inferred_month), " 个批次月份；其中 ",
    ifelse(length(missing_months) == 0, "不存在缺口月份", paste0("未见 ", paste(missing_months, collapse = "、"), " 的批号前缀")),
    "。月度样本量在 ", sample_count_min_row$month_label[1], " 的 ", sample_count_min_row$sample_count[1],
    " 批至 ", sample_count_max_row$month_label[1], " 的 ", sample_count_max_row$sample_count[1], " 批之间变化。"
  ),
  paste0(
    "总体上，水分的均值为 ", fmt_num(metric_summary_numeric$mean[metric_summary_numeric$metric == "moisture_pct"]),
    "%，总灰分均值为 ", fmt_num(metric_summary_numeric$mean[metric_summary_numeric$metric == "total_ash_pct"]),
    "%，浸出物均值为 ", fmt_num(metric_summary_numeric$mean[metric_summary_numeric$metric == "extract_pct"]),
    "%，橙皮苷含量均值为 ", fmt_num(metric_summary_numeric$mean[metric_summary_numeric$metric == "hesperidin_mg_g"]),
    " mg/g。"
  ),
  paste0(
    "从月度变化看，浸出物的月均值振幅最大，由 ", extract_row$min_month, " 的 ",
    fmt_num(extract_row$min_monthly_mean), "% 升至 ", extract_row$max_month, " 的 ",
    fmt_num(extract_row$max_monthly_mean), "%，振幅为 ", fmt_num(extract_row$amplitude),
    " 个百分点；总灰分次之，由 ", ash_row$min_month, " 的 ",
    fmt_num(ash_row$min_monthly_mean), "% 升至 ", ash_row$max_month, " 的 ",
    fmt_num(ash_row$max_monthly_mean), "%。"
  ),
  paste0(
    "水分月均值在 ", moisture_row$min_month, " 至 ", moisture_row$max_month,
    " 间由 ", fmt_num(moisture_row$min_monthly_mean), "% 变化至 ",
    fmt_num(moisture_row$max_monthly_mean), "%，表现为中等幅度波动；橙皮苷含量月均值由 ",
    hesperidin_row$min_month, " 的 ", fmt_num(hesperidin_row$min_monthly_mean),
    " mg/g 升至 ", hesperidin_row$max_month, " 的 ",
    fmt_num(hesperidin_row$max_monthly_mean), " mg/g，整体更接近阶段性抬升而非突发异常。"
  ),
  "因此，D4 更接近原料/中间体理化组成的阶段性漂移，不呈现类似 D2 中 0.8 g 崩解时限那样的单一阈值型异常信号。若后续进入问题驱动分析，更适合将 D4 作为可迁移的上游理化状态层，而不是独立定义为问题终点。"
)

formal_intro <- c(
  "# D4 正式结果分析与文字整理",
  "",
  "## 一、结果概述",
  "",
  results_paragraphs[1],
  "",
  results_paragraphs[2],
  "",
  "## 二、四项理化指标描述统计",
  ""
)

formal_mid <- c(
  "",
  "## 三、月度变化与关键观察",
  "",
  paste0("- ", results_paragraphs[3]),
  paste0("- ", results_paragraphs[4]),
  paste0("- ", results_paragraphs[5]),
  paste0("- ", results_paragraphs[6]),
  "",
  "## 四、可直接用于论文中文稿的结果段落",
  "",
  as.vector(rbind(results_paragraphs, "")),
  "",
  "## 五、对应图件",
  "",
  "- 01_d4_overview_moisture",
  "- 02_d4_overview_total_ash",
  "- 03_d4_overview_extract",
  "- 04_d4_overview_hesperidin_content"
)

md_lines_desc <- c(
  "# D4 浸膏粉理化数据描述说明",
  "",
  paste0("- 数据源：`", basename(input_d4), "`"),
  paste0("- 总记录数：`", nrow(d4), "`"),
  paste0("- 唯一批次数：`", n_distinct(d4$batch_no), "`"),
  paste0("- 近似月份范围：`", format(min(d4$inferred_month), "%Y-%m"), " ~ ", format(max(d4$inferred_month), "%Y-%m"), "`"),
  paste0("- 缺失月份：`", ifelse(length(missing_months) == 0, "None", paste(missing_months, collapse = ", ")), "`"),
  "",
  "## 批号月份代理规则",
  "",
  "- D4 无显式检测日期，采用批号前四位 `YYMM` 作为近似批次月份。",
  paste0(
    "- 该月份代理已用 D3 可连接浸膏粉批次验证：", nrow(d3_link),
    " 条关联记录全部落在推断月份后 `0-3` 个月内，中位滞后 `",
    median(d3_link$month_diff), "` 个月。"
  ),
  "",
  "## 输出原则",
  "",
  "- 图顶不放标题。",
  "- 点为灰色，月均值与误差线为红色，右侧分布图为蓝色。",
  "- 所有图使用 Arial 字体，先导出 PDF，再转 PNG。",
  "- 横坐标统一为 `Production month`。"
)

md_lines_formal <- c(
  formal_intro,
  to_markdown_table(core_metric_summary),
  formal_mid
)

writeLines(md_lines_desc, con = file.path(docs_dir, "D4_浸膏粉理化数据描述说明.md"), useBytes = TRUE)
writeLines(md_lines_formal, con = file.path(docs_dir, "D4_正式结果分析与文字整理.md"), useBytes = TRUE)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D4 浸膏粉理化数据描述说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc_desc <- body_add_par(doc_desc, "D4 未提供显式检测日期，因此本稿采用批号前四位 YYMM 作为近似批次月份，并结合 D3 中可连接的浸膏粉批次进行月份代理验证。", style = "Normal")
doc_desc <- body_add_par(doc_desc, "总体概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(overall_overview))
doc_desc <- body_add_par(doc_desc, "批号月份代理验证", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(batch_month_validation))
doc_desc <- body_add_par(doc_desc, "四项核心理化指标描述统计", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(core_metric_summary))
doc_desc <- body_add_par(doc_desc, "月度样本量", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(monthly_count_table))
doc_desc <- body_add_par(doc_desc, "月度统计明细", style = "heading 2")
doc_desc <- body_add_flextable(
  doc_desc,
  build_flextable(
    monthly_metric_long |>
      select(month_label, sample_count, display_name, mean, sd),
    font_size = 8
  )
)
doc_desc <- body_add_par(doc_desc, "月度极值概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(monthly_extrema))
doc_desc <- body_add_par(doc_desc, "字段字典", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(field_dictionary))
print(doc_desc, target = file.path(docs_dir, "D4_浸膏粉理化数据描述说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D4 正式结果分析与文字整理", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "结果概述", style = "heading 2")
for (paragraph in results_paragraphs[1:2]) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "四项理化指标描述统计", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(core_metric_summary))
doc_formal <- body_add_par(doc_formal, "月度变化与关键观察", style = "heading 2")
for (paragraph in results_paragraphs[3:7]) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "可直接用于论文中文稿的结果段落", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "对应图件", style = "heading 2")
doc_formal <- body_add_par(doc_formal, "01_d4_overview_moisture", style = "Normal")
doc_formal <- body_add_par(doc_formal, "02_d4_overview_total_ash", style = "Normal")
doc_formal <- body_add_par(doc_formal, "03_d4_overview_extract", style = "Normal")
doc_formal <- body_add_par(doc_formal, "04_d4_overview_hesperidin_content", style = "Normal")
print(doc_formal, target = file.path(docs_dir, "D4_正式结果分析与文字整理.docx"))

cat("Input D4:", input_d4, "\n")
cat("Input D3:", input_d3, "\n")
cat("Saved table workbook:", excel_path, "\n")
cat("Saved description docx:", file.path(docs_dir, "D4_浸膏粉理化数据描述说明.docx"), "\n")
cat("Saved formal docx:", file.path(docs_dir, "D4_正式结果分析与文字整理.docx"), "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
