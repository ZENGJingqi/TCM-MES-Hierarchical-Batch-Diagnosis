options(encoding = "UTF-8")
suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(officer)
  library(openxlsx)
  library(pdftools)
  library(readxl)
  library(tidyr)
})

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0) {
    normalizePath(sys.frames()[[1]]$ofile)
  } else {
    normalizePath(sub("^--file=", "", file_arg[1]))
  }
}

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("= %.3f", p)
}

fmt_mean_sd <- function(x) {
  sprintf("%.2f ± %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

fmt_median_iqr <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf("%.2f (%.2f, %.2f)", q[[2]], q[[1]], q[[3]])
}

fmt_range <- function(x) {
  sprintf("%.2f to %.2f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
}

cliffs_delta <- function(x, y) {
  cmp <- outer(x, y, "-")
  (sum(cmp > 0) - sum(cmp < 0)) / (length(x) * length(y))
}

significance_stars <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  "ns"
}

build_flextable <- function(df) {
  flextable(df) |>
    fontsize(size = 10, part = "all") |>
    font(fontname = "Arial", part = "all") |>
    autofit()
}

save_plot_dual <- function(plot_obj, filename, width, height, figures_dir) {
  pdf_path <- file.path(figures_dir, paste0(filename, ".pdf"))
  png_path <- file.path(figures_dir, paste0(filename, ".png"))
  png_pattern <- file.path(figures_dir, paste0(filename, "_%d.png"))
  ggsave(
    filename = pdf_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf,
    bg = "white"
  )
  pdftools::pdf_convert(
    pdf = pdf_path,
    format = "png",
    dpi = 330,
    filenames = png_pattern
  )
  converted_png <- file.path(figures_dir, paste0(filename, "_1.png"))
  if (file.exists(converted_png)) {
    if (file.exists(png_path)) file.remove(png_path)
    file.rename(converted_png, png_path)
  }
}

script_path <- get_script_path()
script_dir <- dirname(script_path)
analysis_dir <- normalizePath(file.path(script_dir, ".."))
project_dir <- normalizePath(file.path(analysis_dir, ".."))

code_dir <- file.path(analysis_dir, "code")
docs_dir <- file.path(analysis_dir, "docs")
figures_dir <- file.path(analysis_dir, "figures")
tables_dir <- file.path(analysis_dir, "tables")

dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

input_path <- file.path(project_dir, "定稿数据_中文", "D6_陈皮检测数据_定稿.xlsx")
d6_raw <- read_excel(input_path)
names(d6_raw) <- c(
  "date",
  "name",
  "batch_no",
  "origin",
  "moisture_pct",
  "hesperidin_pct",
  "impurities_pct"
)

origin_map <- c("江西宜春" = "Jiangxi Yichun", "浙江" = "Zhejiang")

d6 <- d6_raw |>
  mutate(
    batch_no = as.character(batch_no),
    origin_cn = as.character(origin),
    origin = recode(origin_cn, !!!origin_map),
    origin = factor(origin, levels = c("Jiangxi Yichun", "Zhejiang"))
  ) |>
  select(date, batch_no, origin_cn, origin, moisture_pct, hesperidin_pct, impurities_pct)

origin_overview <- d6 |>
  count(origin, name = "Records") |>
  mutate(`Share (%)` = round(Records / sum(Records) * 100, 2))

metric_specs <- tibble::tribble(
  ~metric, ~label, ~filename,
  "moisture_pct", "Moisture (%)", "01_d6_origin_moisture",
  "hesperidin_pct", "Hesperidin (%)", "02_d6_origin_hesperidin",
  "impurities_pct", "Impurities (%)", "03_d6_origin_impurities"
)

origin_summary <- lapply(seq_len(nrow(metric_specs)), function(i) {
  metric <- metric_specs$metric[i]
  label <- metric_specs$label[i]
  d6 |>
    group_by(origin) |>
    summarise(
      Indicator = label,
      n = n(),
      `Mean ± SD` = fmt_mean_sd(.data[[metric]]),
      `Median (Q1, Q3)` = fmt_median_iqr(.data[[metric]]),
      Range = fmt_range(.data[[metric]]),
      .groups = "drop"
    ) |>
    relocate(Indicator)
}) |>
  bind_rows()

comparison_tests <- lapply(seq_len(nrow(metric_specs)), function(i) {
  metric <- metric_specs$metric[i]
  label <- metric_specs$label[i]
  x <- d6 |> filter(origin == "Jiangxi Yichun") |> pull(.data[[metric]])
  y <- d6 |> filter(origin == "Zhejiang") |> pull(.data[[metric]])
  wt <- wilcox.test(x, y, exact = FALSE)
  delta <- cliffs_delta(x, y)
  tibble(
    Indicator = label,
    `Jiangxi Yichun median` = round(median(x), 2),
    `Zhejiang median` = round(median(y), 2),
    `Median difference` = round(median(x) - median(y), 2),
    `Cliff's delta` = round(delta, 4),
    `Wilcoxon P value` = fmt_p(wt$p.value),
    Significance = significance_stars(wt$p.value)
  )
}) |>
  bind_rows()

analysis_ready <- d6 |>
  select(
    batch_no,
    origin_cn,
    origin,
    moisture_pct,
    hesperidin_pct,
    impurities_pct
  )

origin_axis_labels <- d6 |>
  count(origin, name = "n") |>
  mutate(label = paste0(as.character(origin), "\n(n = ", n, ")"))

point_colour <- "#8C8C8C"
trend_colour <- "#A83B2B"
distribution_colour <- "#0A5C7A"

base_theme <- theme_classic(base_family = "Arial") +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(size = 26, colour = "black"),
    axis.text = element_text(size = 22, colour = "black"),
    axis.line = element_line(linewidth = 0.9, colour = "black"),
    axis.ticks = element_line(linewidth = 0.9, colour = "black"),
    legend.position = "none",
    plot.margin = margin(8, 16, 8, 8)
  )

for (i in seq_len(nrow(metric_specs))) {
  metric <- metric_specs$metric[i]
  label <- metric_specs$label[i]
  filename <- metric_specs$filename[i]
  wt_text <- comparison_tests$`Wilcoxon P value`[comparison_tests$Indicator == label]

  y_values <- d6[[metric]]
  y_range <- max(y_values, na.rm = TRUE) - min(y_values, na.rm = TRUE)
  if (y_range == 0) y_range <- max(y_values, na.rm = TRUE) * 0.2
  bracket_y <- max(y_values, na.rm = TRUE) + 0.10 * y_range
  text_y <- bracket_y + 0.06 * y_range
  lower_bracket <- bracket_y - 0.03 * y_range

  plot_obj <- ggplot(d6, aes(x = origin, y = .data[[metric]])) +
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
      size = 2.6,
      shape = 16
    ) +
    annotate("segment", x = 1, xend = 1, y = lower_bracket, yend = bracket_y, linewidth = 0.9) +
    annotate("segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, linewidth = 0.9) +
    annotate("segment", x = 2, xend = 2, y = lower_bracket, yend = bracket_y, linewidth = 0.9) +
    annotate(
      "text",
      x = 1.5,
      y = text_y,
      label = paste0("Wilcoxon P ", wt_text),
      family = "Arial",
      size = 7.4
    ) +
    scale_x_discrete(labels = setNames(origin_axis_labels$label, origin_axis_labels$origin)) +
    labs(x = "Chenpi origin", y = label) +
    expand_limits(y = text_y + 0.03 * y_range) +
    base_theme +
    theme(
      axis.text.x = element_text(size = 22, lineheight = 0.9, margin = margin(t = 8)),
      plot.margin = margin(8, 16, 24, 8)
    )

  save_plot_dual(plot_obj, filename, 7.2, 8.0, figures_dir)
}

wb <- createWorkbook()
addWorksheet(wb, "dataset_overview")
addWorksheet(wb, "origin_summary")
addWorksheet(wb, "comparison_tests")
addWorksheet(wb, "analysis_ready")

writeData(
  wb,
  "dataset_overview",
  tibble(
    Item = c("Total D6 records", "Unique origins", "Jiangxi Yichun records", "Zhejiang records"),
    Value = c(nrow(d6), n_distinct(d6$origin), sum(d6$origin == "Jiangxi Yichun"), sum(d6$origin == "Zhejiang"))
  )
)
writeData(wb, "origin_summary", origin_summary)
writeData(wb, "comparison_tests", comparison_tests)
writeData(wb, "analysis_ready", analysis_ready)
saveWorkbook(wb, file.path(tables_dir, "D6_origin_comparison.xlsx"), overwrite = TRUE)

moisture_p <- comparison_tests$`Wilcoxon P value`[comparison_tests$Indicator == "Moisture (%)"]
hesperidin_p <- comparison_tests$`Wilcoxon P value`[comparison_tests$Indicator == "Hesperidin (%)"]
impurities_p <- comparison_tests$`Wilcoxon P value`[comparison_tests$Indicator == "Impurities (%)"]

results_paragraphs <- c(
  paste0(
    "D6 共纳入 44 条陈皮检验记录，覆盖两个产地，江西宜春与浙江各 22 条。三项质量属性均在 D6 内部独立比较，不引入浸膏粉或成品追溯链。"
  ),
  paste0(
    "与浙江相比，江西宜春陈皮的水分水平更高，组间 Wilcoxon 检验 ",
    moisture_p,
    "；其描述统计结果为江西宜春 ",
    origin_summary$`Mean ± SD`[origin_summary$Indicator == "Moisture (%)" & origin_summary$origin == "Jiangxi Yichun"],
    "，浙江 ",
    origin_summary$`Mean ± SD`[origin_summary$Indicator == "Moisture (%)" & origin_summary$origin == "Zhejiang"],
    "。"
  ),
  paste0(
    "橙皮苷含量在江西宜春组亦高于浙江组，Wilcoxon 检验 ",
    hesperidin_p,
    "；江西宜春为 ",
    origin_summary$`Mean ± SD`[origin_summary$Indicator == "Hesperidin (%)" & origin_summary$origin == "Jiangxi Yichun"],
    "，浙江为 ",
    origin_summary$`Mean ± SD`[origin_summary$Indicator == "Hesperidin (%)" & origin_summary$origin == "Zhejiang"],
    "。"
  ),
  paste0(
    "杂质含量在两个产地之间差异较弱，Wilcoxon 检验 ",
    impurities_p,
    "；江西宜春为 ",
    origin_summary$`Mean ± SD`[origin_summary$Indicator == "Impurities (%)" & origin_summary$origin == "Jiangxi Yichun"],
    "，浙江为 ",
    origin_summary$`Mean ± SD`[origin_summary$Indicator == "Impurities (%)" & origin_summary$origin == "Zhejiang"],
    "。"
  ),
  "综合来看，D6 层面的产地差异主要体现在水分与橙皮苷两个指标，而杂质差异不构成当前数据中的主信号。"
)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D6 陈皮产地差异分析说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, "数据概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(tibble(
  Item = c("总记录数", "产地数", "江西宜春记录数", "浙江记录数"),
  Value = c(nrow(d6), n_distinct(d6$origin), sum(d6$origin == "Jiangxi Yichun"), sum(d6$origin == "Zhejiang"))
)))
doc_desc <- body_add_par(doc_desc, "分组描述统计", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(origin_summary))
doc_desc <- body_add_par(doc_desc, "组间差异检验", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(comparison_tests))
print(doc_desc, target = file.path(docs_dir, "D6_陈皮产地差异分析说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D6 正式结果分析与文字整理", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "描述统计", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(origin_summary))
doc_formal <- body_add_par(doc_formal, "差异检验", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(comparison_tests))
doc_formal <- body_add_par(doc_formal, "可直接用于论文中文稿的结果段落", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
print(doc_formal, target = file.path(docs_dir, "D6_正式结果分析_产地差异.docx"))

md_lines <- c(
  "# D6 正式结果分析与文字整理",
  "",
  "## 描述统计",
  knitr::kable(origin_summary, format = "markdown"),
  "",
  "## 差异检验",
  knitr::kable(comparison_tests, format = "markdown"),
  "",
  "## 可直接用于论文中文稿的结果段落",
  "",
  paste0("- ", results_paragraphs)
)
writeLines(md_lines, con = file.path(docs_dir, "D6_正式结果分析_产地差异.md"), useBytes = TRUE)

message("Input D6: ", input_path)
message("Saved workbook: ", file.path(tables_dir, "D6_origin_comparison.xlsx"))
message("Saved figures: ", nrow(metric_specs))
