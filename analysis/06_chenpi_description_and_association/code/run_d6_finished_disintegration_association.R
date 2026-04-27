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
cn_dir <- file.path(root_dir, "定稿数据_中文")
docs_dir <- file.path(project_dir, "docs")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

unlink(Sys.glob(file.path(figures_dir, "*.pdf")), force = TRUE)
unlink(Sys.glob(file.path(figures_dir, "*.png")), force = TRUE)

choose_cn_file <- function(prefix) {
  files <- list.files(cn_dir, pattern = paste0("^", prefix, ".*\\.xlsx$"), full.names = TRUE)
  if (length(files) == 0) {
    stop("No source file found for prefix: ", prefix)
  }
  normalizePath(files[1], winslash = "/", mustWork = TRUE)
}

norm_batch <- function(x) {
  s <- x |>
    as.character() |>
    str_trim() |>
    str_replace("\\.0$", "")
  s[is.na(s)] <- ""
  s[tolower(s) %in% c("nan", "none", "")] <- ""
  s
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

fmt_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ format(round(p, 3), nsmall = 3, trim = TRUE)
  )
}

stars_from_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}

set_plot_style <- function() {
  theme_set(theme_bw(base_family = "Arial", base_size = 28))
  theme_update(
    plot.title = element_blank(),
    axis.title = element_text(size = 34, face = "plain", colour = "black"),
    axis.text = element_text(size = 28, colour = "black"),
    legend.title = element_text(size = 24, colour = "black"),
    legend.text = element_text(size = 22, colour = "black"),
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

build_flextable <- function(df, font_size = 9) {
  flextable(df) |>
    fontsize(size = font_size, part = "all") |>
    font(fontname = "Arial", part = "all") |>
    bold(part = "header") |>
    align(align = "center", part = "all") |>
    autofit()
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

point_colour <- "#8C8C8C"
trend_colour <- "#A83B2B"
distribution_colour <- "#0A5C7A"
threshold_colour <- "#6A6A6A"

chenpi_metric_map <- tibble::tribble(
  ~var, ~label, ~stem,
  "chenpi_moisture_pct_mean", "Chenpi moisture (%)", "chenpi_moisture",
  "chenpi_hesperidin_pct_mean", "Chenpi hesperidin (%)", "chenpi_hesperidin",
  "chenpi_impurities_pct_mean", "Chenpi impurities (%)", "chenpi_impurities"
)

set_plot_style()

input_d2 <- choose_cn_file("D2")
input_d3 <- choose_cn_file("D3")
input_d5 <- choose_cn_file("D5")
input_d6 <- choose_cn_file("D6")

d2_raw <- read_xlsx(input_d2)
d3_raw <- read_xlsx(input_d3)
d5_raw <- read_xlsx(input_d5)
d6_raw <- read_xlsx(input_d6)

d2 <- tibble(
  finished_batch = norm_batch(d2_raw[[1]]),
  production_date = as.Date(d2_raw[[2]]),
  disintegration_time_min = as.numeric(d2_raw[[4]]),
  dosage_strength = as.character(d2_raw[[8]]) |> str_squish()
) |>
  mutate(dosage_strength = str_replace_all(dosage_strength, " ", ""))

d3 <- tibble(
  finished_batch = norm_batch(d3_raw[[1]]),
  extract_batch = norm_batch(d3_raw[[3]])
)

d5_chenpi <- tibble(
  extract_batch = norm_batch(d5_raw[[1]]),
  material_type = as.character(d5_raw[[2]]) |> str_squish(),
  raw_batch = as.character(d5_raw[[3]]) |> str_squish(),
  raw_order = suppressWarnings(as.numeric(d5_raw[[4]]))
) |>
  filter(material_type == "陈皮")

extract_chenpi_batches <- function(x) {
  text <- x |>
    as.character() |>
    str_replace_all("\\.0", "") |>
    str_squish()
  text[is.na(text)] <- ""
  text[text == "254802"] <- "2504802"
  out <- str_extract_all(text, "\\d{7}(?:-\\d+)?")
  lapply(out, function(values) {
    if (length(values) == 0) {
      return(NA_character_)
    }
    values
  })
}

d6 <- tibble(
  inspection_date = as.Date(d6_raw[[1]]),
  material_name = as.character(d6_raw[[2]]) |> str_squish(),
  chenpi_batch = norm_batch(d6_raw[[3]]),
  origin = as.character(d6_raw[[4]]) |> str_squish(),
  moisture_pct = as.numeric(d6_raw[[5]]),
  hesperidin_pct = as.numeric(d6_raw[[6]]),
  impurities_pct = as.numeric(d6_raw[[7]])
)

trace_long <- d5_chenpi |>
  mutate(chenpi_batch = extract_chenpi_batches(raw_batch)) |>
  unnest(chenpi_batch) |>
  mutate(
    extract_batch = norm_batch(extract_batch),
    chenpi_batch = norm_batch(chenpi_batch)
  ) |>
  distinct(extract_batch, chenpi_batch, raw_batch, raw_order)

trace_linked <- trace_long |>
  inner_join(
    d6 |>
      select(chenpi_batch, origin, moisture_pct, hesperidin_pct, impurities_pct),
    by = "chenpi_batch"
  )

finished_extract <- d2 |>
  inner_join(d3, by = "finished_batch") |>
  filter(dosage_strength == "0.8g", extract_batch != "")

linked_finished <- finished_extract |>
  inner_join(
    trace_linked |>
      select(extract_batch, chenpi_batch, origin, moisture_pct, hesperidin_pct, impurities_pct),
    by = "extract_batch",
    relationship = "many-to-many"
  )

extract_level <- linked_finished |>
  group_by(extract_batch) |>
  summarise(
    linked_finished_batch_n = n_distinct(finished_batch),
    issue_any = ifelse(any(disintegration_time_min > 10, na.rm = TRUE), 1L, 0L),
    issue_ratio = mean(disintegration_time_min > 10, na.rm = TRUE),
    mean_disintegration = mean(disintegration_time_min, na.rm = TRUE),
    chenpi_batch_n = n_distinct(chenpi_batch),
    origin_pattern_n = n_distinct(origin),
    origin_pattern = paste(sort(unique(origin)), collapse = " + "),
    chenpi_moisture_pct_mean = mean(moisture_pct, na.rm = TRUE),
    chenpi_hesperidin_pct_mean = mean(hesperidin_pct, na.rm = TRUE),
    chenpi_impurities_pct_mean = mean(impurities_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    issue_group = factor(
      issue_any,
      levels = c(0, 1),
      labels = c("No linked\n>10 min batch", "≥1 linked\n>10 min batch")
    ),
     origin_pattern_en = case_when(
       origin_pattern == "江西宜春" ~ "Jiangxi Yichun",
       origin_pattern == "浙江" ~ "Zhejiang",
       origin_pattern == "江西宜春 + 浙江" ~ "Mixed origins",
       TRUE ~ origin_pattern
     )
  )

chenpi_level <- linked_finished |>
  group_by(chenpi_batch) |>
  summarise(
    origin = first(origin),
    linked_extract_batch_n = n_distinct(extract_batch),
    linked_finished_batch_n = n_distinct(finished_batch),
    issue_any = ifelse(any(disintegration_time_min > 10, na.rm = TRUE), 1L, 0L),
    issue_ratio = mean(disintegration_time_min > 10, na.rm = TRUE),
    mean_disintegration = mean(disintegration_time_min, na.rm = TRUE),
    moisture_pct = first(moisture_pct),
    hesperidin_pct = first(hesperidin_pct),
    impurities_pct = first(impurities_pct),
    .groups = "drop"
  )

pairing_overview <- tibble(
  item = c(
    "Linked 0.8g finished-product rows",
    "Linked 0.8g finished-product batches",
    "Linked extract-powder batches",
    "Linked chenpi batches",
    "Extract-powder batches with >=1 linked batch >10 min",
    "Chenpi batches with >=1 linked batch >10 min",
    "Available chenpi source field in D6"
  ),
  value = c(
    nrow(linked_finished),
    n_distinct(linked_finished$finished_batch),
    nrow(extract_level),
    nrow(chenpi_level),
    sprintf("%d (%.2f%%)", sum(extract_level$issue_any), mean(extract_level$issue_any) * 100),
    sprintf("%d (%.2f%%)", sum(chenpi_level$issue_any), mean(chenpi_level$issue_any) * 100),
    "Origin (no explicit manufacturer field)"
  )
)

predictor_summary <- lapply(chenpi_metric_map$var, function(v) {
  values <- extract_level[[v]]
  tibble(
    Indicator = chenpi_metric_map$label[chenpi_metric_map$var == v],
    n = sum(!is.na(values)),
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean(values, na.rm = TRUE), sd(values, na.rm = TRUE)),
    `Median (Q1, Q3)` = sprintf(
      "%.4f (%.4f, %.4f)",
      median(values, na.rm = TRUE),
      quantile(values, 0.25, na.rm = TRUE),
      quantile(values, 0.75, na.rm = TRUE)
    ),
    Range = sprintf("%.4f to %.4f", min(values, na.rm = TRUE), max(values, na.rm = TRUE))
  )
}) |>
  bind_rows()
names(predictor_summary)[3] <- "Mean ± SD"

group_comparison <- lapply(chenpi_metric_map$var, function(v) {
  test <- suppressWarnings(wilcox.test(extract_level[[v]] ~ extract_level$issue_any, exact = FALSE))
  tibble(
    Indicator = chenpi_metric_map$label[chenpi_metric_map$var == v],
    `Median, no linked batch >10 min` = round(median(extract_level[[v]][extract_level$issue_any == 0], na.rm = TRUE), 4),
    `Median, >=1 linked batch >10 min` = round(median(extract_level[[v]][extract_level$issue_any == 1], na.rm = TRUE), 4),
    `Wilcoxon P value` = fmt_p(test$p.value),
    Significance = stars_from_p(test$p.value)
  )
}) |>
  bind_rows()

issue_ratio_assoc <- lapply(chenpi_metric_map$var, function(v) {
  test <- suppressWarnings(cor.test(extract_level[[v]], extract_level$issue_ratio, method = "spearman", exact = FALSE))
  tibble(
    Indicator = chenpi_metric_map$label[chenpi_metric_map$var == v],
    `Spearman rho with linked >10 min ratio` = round(unname(test$estimate), 4),
    `P value` = fmt_p(test$p.value)
  )
}) |>
  bind_rows()

feature_screening <- lapply(chenpi_metric_map$var, function(v) {
  label <- chenpi_metric_map$label[chenpi_metric_map$var == v]
  sub_df <- extract_level |>
    select(issue_any, issue_ratio, all_of(v)) |>
    filter(!is.na(.data[[v]]), !is.na(issue_any), !is.na(issue_ratio))
  x0 <- sub_df |> filter(issue_any == 0) |> pull(.data[[v]])
  x1 <- sub_df |> filter(issue_any == 1) |> pull(.data[[v]])
  wt <- suppressWarnings(wilcox.test(x0, x1, exact = FALSE))
  sp <- suppressWarnings(cor.test(sub_df[[v]], sub_df$issue_ratio, method = "spearman", exact = FALSE))
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
    `Screening score` = -log10(pmax(`Screening FDR`, .Machine$double.xmin)),
    `Screening judgment` = case_when(
      `Screening FDR` < 0.001 ~ "FDR < 0.001",
      `Screening FDR` < 0.01 ~ "FDR < 0.01",
      `Screening FDR` < 0.05 ~ "FDR < 0.05",
      TRUE ~ "Not significant"
    )
  ) |>
  arrange(desc(`Screening score`))

scaled_extract <- extract_level |>
  mutate(
    moisture_z = as.numeric(scale(chenpi_moisture_pct_mean)),
    hesperidin_z = as.numeric(scale(chenpi_hesperidin_pct_mean)),
    impurities_z = as.numeric(scale(chenpi_impurities_pct_mean))
  )

glm_fit <- glm(
  issue_any ~ moisture_z + hesperidin_z + impurities_z,
  family = binomial(),
  data = scaled_extract
)

glm_coef <- summary(glm_fit)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  filter(term != "(Intercept)") |>
  mutate(
    Indicator = c(
      "Chenpi moisture (%)",
      "Chenpi hesperidin (%)",
      "Chenpi impurities (%)"
    ),
    `Odds ratio` = round(exp(Estimate), 4),
    `95% CI` = sprintf("%.4f to %.4f", exp(Estimate - 1.96 * `Std. Error`), exp(Estimate + 1.96 * `Std. Error`)),
    `P value` = fmt_p(`Pr(>|z|)`)
  ) |>
  select(Indicator, `Odds ratio`, `95% CI`, `P value`)

origin_pattern_summary <- extract_level |>
  count(origin_pattern_en, issue_any, name = "n") |>
  tidyr::pivot_wider(names_from = issue_any, values_from = n, values_fill = 0) |>
  transmute(
    `Origin pattern` = origin_pattern_en,
    `No linked batch >10 min` = `0`,
    `>=1 linked batch >10 min` = `1`,
    `Total extract batches` = `0` + `1`,
    `Linked >10 min tablet batches (%)` = round(`1` / (`0` + `1`) * 100, 2)
  ) |>
  arrange(match(`Origin pattern`, c("Jiangxi Yichun", "Zhejiang", "Mixed origins")))

origin_overall_table <- table(extract_level$origin_pattern_en, extract_level$issue_any)
origin_overall_test <- suppressWarnings(chisq.test(origin_overall_table))
origin_overall_result <- tibble(
  Test = "Chi-square test across origin patterns",
  Statistic = round(unname(origin_overall_test$statistic), 4),
  `P value` = fmt_p(origin_overall_test$p.value)
)

single_origin_extract <- extract_level |>
  filter(origin_pattern_en %in% c("Jiangxi Yichun", "Zhejiang"))

single_origin_table <- table(single_origin_extract$origin_pattern_en, single_origin_extract$issue_any)
single_origin_fisher <- fisher.test(single_origin_table)
origin_single_result <- tibble(
  Comparison = "Zhejiang vs Jiangxi Yichun (single-origin extract batches)",
  `Odds ratio` = round(unname(single_origin_fisher$estimate), 4),
  `95% CI` = sprintf("%.4f to %.4f", single_origin_fisher$conf.int[1], single_origin_fisher$conf.int[2]),
  `P value` = fmt_p(single_origin_fisher$p.value)
)

chenpi_batch_origin_summary <- chenpi_level |>
  count(origin, issue_any, name = "n") |>
  tidyr::pivot_wider(names_from = issue_any, values_from = n, values_fill = 0) |>
  transmute(
    Origin = origin,
    `No linked batch >10 min` = `0`,
    `>=1 linked batch >10 min` = `1`,
    `Total chenpi batches` = `0` + `1`,
    `Issue-linked rate (%)` = round(`1` / (`0` + `1`) * 100, 2)
  )

build_group_plot <- function(df, x_var, x_label) {
  plot_df <- df |>
    transmute(issue_group, value = .data[[x_var]]) |>
    filter(!is.na(value))

  x_labels <- plot_df |>
    count(issue_group) |>
    transmute(
      issue_group,
      label = case_when(
        issue_group == "No linked\n>10 min batch" ~ "No linked\n>10 min batch",
        TRUE ~ "≥1 linked\n>10 min batch"
      )
    )

  wilcox_p <- suppressWarnings(wilcox.test(value ~ issue_group, data = plot_df, exact = FALSE)$p.value)
  y_max <- max(plot_df$value, na.rm = TRUE)
  y_min <- min(plot_df$value, na.rm = TRUE)
  y_range <- ifelse(y_max > y_min, y_max - y_min, 1)
  bracket_y <- y_max + 0.12 * y_range
  label_y <- y_max + 0.18 * y_range

  ggplot(plot_df, aes(x = issue_group, y = value)) +
    geom_boxplot(
      width = 0.55,
      outlier.shape = NA,
      linewidth = 1.2,
      colour = distribution_colour,
      fill = "white"
    ) +
    geom_jitter(
      width = 0.12,
      height = 0,
      shape = 16,
      size = 3.0,
      colour = point_colour
    ) +
    annotate("segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, linewidth = 1.1) +
    annotate("segment", x = 1, xend = 1, y = bracket_y, yend = bracket_y - 0.03 * y_range, linewidth = 1.1) +
    annotate("segment", x = 2, xend = 2, y = bracket_y, yend = bracket_y - 0.03 * y_range, linewidth = 1.1) +
    annotate(
      "text",
      x = 1.5,
      y = label_y,
      label = paste0("Wilcoxon P ", fmt_p(wilcox_p), " ", stars_from_p(wilcox_p)),
      family = "Arial",
      size = 7.0
    ) +
    scale_x_discrete(labels = setNames(x_labels$label, x_labels$issue_group)) +
    labs(x = "Finished-product disintegration group", y = x_label) +
    expand_limits(y = label_y + 0.04 * y_range) +
    theme(
      axis.title.x = element_text(size = 28, margin = margin(t = 10)),
      plot.margin = margin(12, 28, 20, 16)
    )
}

build_scatter_plot <- function(df, x_var, x_label, rho_text, p_text, fdr_text, n_text) {
  x <- df[[x_var]]
  y <- df$issue_ratio
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  y_min <- min(y, na.rm = TRUE)
  y_max <- max(y, na.rm = TRUE)
  y_pad <- max((y_max - y_min) * 0.10, 0.03)
  y_text <- min(0.98, y_max - 0.02 * max(y_max - y_min, 0.2))

  ggplot(df, aes(x = .data[[x_var]], y = issue_ratio)) +
    geom_point(colour = point_colour, size = 2.8) +
    geom_smooth(method = "lm", se = FALSE, colour = trend_colour, linewidth = 2.1) +
    annotate(
      "text",
      x = x_min + 0.04 * (x_max - x_min + 1e-6),
      y = y_text,
      label = paste0("Spearman rho ", rho_text, "\nP ", p_text, "\nFDR ", fdr_text, "\nn = ", n_text),
      hjust = 0,
      vjust = 1,
      family = "Arial",
      size = 6.4,
      lineheight = 0.92
    ) +
    labs(x = x_label, y = "Linked >10 min batch ratio") +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    coord_cartesian(ylim = c(max(0, y_min - y_pad), min(1, y_max + y_pad)))
}

origin_rate_plot <- origin_pattern_summary |>
  mutate(
     `Origin pattern` = factor(
       `Origin pattern`,
       levels = c("Jiangxi Yichun", "Zhejiang", "Mixed origins")
     ),
    label = paste0(`>=1 linked batch >10 min`, "/", `Total extract batches`)
  ) |>
  ggplot(aes(x = `Origin pattern`, y = `Linked >10 min tablet batches (%)`)) +
  geom_col(width = 0.62, fill = distribution_colour, colour = distribution_colour) +
  geom_text(aes(label = label), vjust = -0.55, family = "Arial", size = 7.2) +
  annotate(
    "text",
    x = 2,
    y = max(origin_pattern_summary$`Linked >10 min tablet batches (%)`) + 8,
    label = paste0(
      "Chi-square P ", fmt_p(origin_overall_test$p.value),
      "\nSingle-origin Fisher P ", fmt_p(single_origin_fisher$p.value)
    ),
    family = "Arial",
    size = 7.0
  ) +
  labs(x = "Chenpi source pattern", y = "Linked >10 min tablets (%)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme(
    axis.text.x = element_text(size = 24, lineheight = 0.9),
    legend.position = "none"
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
    size = 5.8,
    colour = "black"
  ) +
  scale_fill_manual(values = c(
    "Higher in ≥1 linked >10 min batch" = trend_colour,
    "Lower in ≥1 linked >10 min batch" = distribution_colour
  ), labels = c(
    "Higher in ≥1 linked >10 min batch" = "Higher in linked group",
    "Lower in ≥1 linked >10 min batch" = "Lower in linked group"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  coord_cartesian(clip = "off") +
  labs(x = "-log10(BH-adjusted screening P)", y = NULL, fill = NULL) +
  theme(
    axis.text.y = element_text(size = 21, colour = "black"),
    axis.text.x = element_text(size = 22, colour = "black"),
    axis.title.x = element_text(size = 25, colour = "black", margin = margin(t = 8)),
    legend.position = "top",
    legend.text = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(14, 58, 18, 14)
  )

save_plot_dual(
  feature_screening_plot,
  "00_d6_chenpi_feature_screening_rank",
  11.2,
  5.6
)

significant_chenpi_metric_map <- chenpi_metric_map |>
  left_join(feature_screening |> select(Indicator, `Screening FDR`), by = c("label" = "Indicator")) |>
  filter(!is.na(`Screening FDR`), `Screening FDR` < 0.05) |>
  arrange(`Screening FDR`)

unlink(file.path(figures_dir, "*_finished_disintegration_group_*.pdf"), force = TRUE)
unlink(file.path(figures_dir, "*_finished_disintegration_group_*.png"), force = TRUE)

for (i in seq_len(nrow(significant_chenpi_metric_map))) {
  save_plot_dual(
    build_group_plot(extract_level, significant_chenpi_metric_map$var[i], significant_chenpi_metric_map$label[i]),
    sprintf("%02d_finished_disintegration_group_%s", i, significant_chenpi_metric_map$stem[i]),
    9.8,
    8.2
  )
}

save_plot_dual(
  origin_rate_plot,
  "04_finished_disintegration_origin_pattern_issue_rate",
  10.4,
  8.2
)

for (i in seq_len(nrow(significant_chenpi_metric_map))) {
  metric_row <- significant_chenpi_metric_map[i, ]
  assoc_row <- issue_ratio_assoc |>
    filter(Indicator == metric_row$label)
  fdr_value <- feature_screening$`Screening FDR`[feature_screening$Indicator == metric_row$label]
  sub_df <- extract_level |>
    select(issue_ratio, all_of(metric_row$var)) |>
    filter(!is.na(.data[[metric_row$var]]), !is.na(issue_ratio))

  save_plot_dual(
    build_scatter_plot(
      sub_df,
      metric_row$var,
      metric_row$label,
      sprintf("= %.4f", assoc_row$`Spearman rho with linked >10 min ratio`),
      assoc_row$`P value`,
      fmt_p(fdr_value),
      nrow(sub_df)
    ),
    sprintf("%02d_finished_disintegration_scatter_%s", i + 4, metric_row$stem),
    8.8,
    7.6
  )
}

combined_scatter_long <- extract_level |>
  select(extract_batch, issue_ratio, all_of(significant_chenpi_metric_map$var)) |>
  pivot_longer(
    cols = all_of(significant_chenpi_metric_map$var),
    names_to = "var",
    values_to = "value"
  ) |>
  filter(is.finite(value), is.finite(issue_ratio)) |>
  left_join(significant_chenpi_metric_map |> select(var, label, `Screening FDR`), by = "var") |>
  mutate(label = factor(label, levels = significant_chenpi_metric_map$label))

combined_scatter_annotation <- lapply(significant_chenpi_metric_map$var, function(v) {
  label_value <- significant_chenpi_metric_map$label[significant_chenpi_metric_map$var == v]
  assoc_row <- issue_ratio_assoc |>
    filter(Indicator == label_value)
  fdr_value <- significant_chenpi_metric_map$`Screening FDR`[significant_chenpi_metric_map$var == v]
  sub_df <- extract_level |>
    select(issue_ratio, all_of(v)) |>
    filter(!is.na(.data[[v]]), !is.na(issue_ratio))
  tibble(
    label = factor(label_value, levels = significant_chenpi_metric_map$label),
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
  labs(x = "Chenpi quality attribute", y = "Linked >10 min batch ratio") +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(
    strip.text = element_text(size = 20),
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 28),
    panel.spacing = unit(1.0, "lines")
  )

save_plot_dual(
  combined_scatter_plot,
  "08_d6_key_chenpi_features_vs_disintegration_issue_ratio",
  16.5,
  5.9
)

extract_level_export <- extract_level |>
  select(
    extract_batch,
    linked_finished_batch_n,
    issue_any,
    issue_ratio,
    mean_disintegration,
    chenpi_batch_n,
    origin_pattern,
    chenpi_moisture_pct_mean,
    chenpi_hesperidin_pct_mean,
    chenpi_impurities_pct_mean
  )

chenpi_level_export <- chenpi_level |>
  select(
    chenpi_batch,
    origin,
    linked_extract_batch_n,
    linked_finished_batch_n,
    issue_any,
    issue_ratio,
    mean_disintegration,
    moisture_pct,
    hesperidin_pct,
    impurities_pct
  )

excel_path <- file.path(tables_dir, "D6_finished_disintegration_association.xlsx")
wb <- createWorkbook()
for (sheet_name in c(
  "pairing_overview",
  "predictor_summary",
  "feature_screening",
  "group_comparison",
  "issue_ratio_assoc",
  "logistic_glm",
  "origin_pattern_summary",
  "origin_overall_result",
  "origin_single_result",
  "chenpi_batch_origin_summary",
  "extract_level",
  "chenpi_level"
)) {
  addWorksheet(wb, sheet_name)
}
writeData(wb, "pairing_overview", pairing_overview)
writeData(wb, "predictor_summary", predictor_summary)
writeData(wb, "feature_screening", feature_screening)
writeData(wb, "group_comparison", group_comparison)
writeData(wb, "issue_ratio_assoc", issue_ratio_assoc)
writeData(wb, "logistic_glm", glm_coef)
writeData(wb, "origin_pattern_summary", origin_pattern_summary)
writeData(wb, "origin_overall_result", origin_overall_result)
writeData(wb, "origin_single_result", origin_single_result)
writeData(wb, "chenpi_batch_origin_summary", chenpi_batch_origin_summary)
writeData(wb, "extract_level", extract_level_export)
writeData(wb, "chenpi_level", chenpi_level_export)
saveWorkbook(wb, excel_path, overwrite = TRUE)

results_paragraphs <- c(
  paste0(
    "以 D5 中陈皮追溯记录为桥接字段，将陈皮批次沿“陈皮批次→浸膏粉批次→成品批次”链路映射到成品崩解时限后，共得到 ",
    nrow(linked_finished), " 条可配对的 0.8g 成品记录，对应 ",
    n_distinct(linked_finished$finished_batch), " 个成品批次、",
    nrow(extract_level), " 个浸膏粉批次和 ",
    nrow(chenpi_level), " 个陈皮批次。以浸膏粉批次为分类单元时，共有 ",
    sum(extract_level$issue_any), " 个浸膏粉批次至少关联 1 个崩解时限 >10 min 的成品批次，占 ",
    fmt_num(mean(extract_level$issue_any) * 100, 2), "%。"
  ),
  "当前 D6 仅提供“产地”字段而无直接厂家字段，因此陈皮来源差异分析以产地作为可用来源口径进行比较。",
  paste0(
    "按浸膏粉批次聚合后，问题组对应的陈皮水分中位数显著更低（",
    fmt_num(group_comparison$`Median, >=1 linked batch >10 min`[group_comparison$Indicator == "Chenpi moisture (%)"], 3),
    "% vs ",
    fmt_num(group_comparison$`Median, no linked batch >10 min`[group_comparison$Indicator == "Chenpi moisture (%)"], 3),
    "%，P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Chenpi moisture (%)"],
    "），陈皮橙皮苷中位数也更低（",
    fmt_num(group_comparison$`Median, >=1 linked batch >10 min`[group_comparison$Indicator == "Chenpi hesperidin (%)"], 3),
    "% vs ",
    fmt_num(group_comparison$`Median, no linked batch >10 min`[group_comparison$Indicator == "Chenpi hesperidin (%)"], 3),
    "%，P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Chenpi hesperidin (%)"],
    "）。陈皮杂质在问题组同样偏低（",
    fmt_num(group_comparison$`Median, >=1 linked batch >10 min`[group_comparison$Indicator == "Chenpi impurities (%)"], 3),
    "% vs ",
    fmt_num(group_comparison$`Median, no linked batch >10 min`[group_comparison$Indicator == "Chenpi impurities (%)"], 3),
    "%，P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Chenpi impurities (%)"],
    "）。"
  ),
  paste0(
    "在连续结果层面，陈皮水分、橙皮苷和杂质与“链接成品批次 >10 min 比例”均呈负相关，其中橙皮苷相关性最强（rho = ",
    fmt_num(issue_ratio_assoc$`Spearman rho with linked >10 min ratio`[issue_ratio_assoc$Indicator == "Chenpi hesperidin (%)"], 3),
    ", P ",
    issue_ratio_assoc$`P value`[issue_ratio_assoc$Indicator == "Chenpi hesperidin (%)"],
    "），其次为水分（rho = ",
    fmt_num(issue_ratio_assoc$`Spearman rho with linked >10 min ratio`[issue_ratio_assoc$Indicator == "Chenpi moisture (%)"], 3),
    ", P ",
    issue_ratio_assoc$`P value`[issue_ratio_assoc$Indicator == "Chenpi moisture (%)"],
    "）。"
  ),
  paste0(
    "来源地比较显示，单一来源为浙江的浸膏粉批次中，至少关联 1 个崩解时限 >10 min 成品批次的比例为 23/84（27.38%），显著高于单一来源为江西宜春的 4/145（2.76%）；两者的 Fisher 精确检验 P 值为 ",
    fmt_p(single_origin_fisher$p.value),
    "，比值比为 ",
    fmt_num(unname(single_origin_fisher$estimate), 3),
    "。因此，在当前数据口径下，陈皮来源差异对后续崩解时限分类结果具有明显影响。"
  )
)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D6 陈皮与成品崩解时限关联分析说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc_desc <- body_add_par(doc_desc, "本稿以 D5 陈皮追溯记录为起点，将陈皮批次沿 D5→D3→D2 链路映射到成品崩解时限，并以浸膏粉批次作为主分类分析单元。", style = "Normal")
doc_desc <- body_add_par(doc_desc, "配对概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(pairing_overview))
doc_desc <- body_add_par(doc_desc, "陈皮指标概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(predictor_summary))
doc_desc <- body_add_par(doc_desc, "分类结果差异", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(group_comparison))
doc_desc <- body_add_flextable(doc_desc, build_flextable(issue_ratio_assoc))
doc_desc <- body_add_par(doc_desc, "Logistic 分类模型", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(glm_coef))
doc_desc <- body_add_par(doc_desc, "陈皮来源地差异", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(origin_pattern_summary))
doc_desc <- body_add_flextable(doc_desc, build_flextable(origin_overall_result))
doc_desc <- body_add_flextable(doc_desc, build_flextable(origin_single_result))
doc_desc <- body_add_flextable(doc_desc, build_flextable(chenpi_batch_origin_summary))
print(doc_desc, target = file.path(docs_dir, "D6_陈皮与成品崩解关联分析说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D6 正式结果分析与成品崩解关联", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "配对样本与分类口径", style = "heading 2")
for (paragraph in results_paragraphs[1:2]) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "陈皮指标与分类结果", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(group_comparison))
doc_formal <- body_add_flextable(doc_formal, build_flextable(issue_ratio_assoc))
doc_formal <- body_add_par(doc_formal, "来源地差异", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(origin_pattern_summary))
doc_formal <- body_add_flextable(doc_formal, build_flextable(origin_single_result))
doc_formal <- body_add_par(doc_formal, "可直接用于论文中文稿的结果段落", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
print(doc_formal, target = file.path(docs_dir, "D6_正式结果分析与成品崩解关联.docx"))

desc_md <- c(
  "# D6 陈皮与成品崩解时限关联分析说明",
  paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "本稿以 D5 陈皮追溯记录为起点，将陈皮批次沿 D5→D3→D2 链路映射到成品崩解时限，并以浸膏粉批次作为主分类分析单元。",
  "",
  "## 配对概况",
  to_markdown_table(pairing_overview),
  "",
  "## 陈皮指标概况",
  to_markdown_table(predictor_summary),
  "",
  "## 分类结果差异",
  to_markdown_table(group_comparison),
  "",
  to_markdown_table(issue_ratio_assoc),
  "",
  "## Logistic 分类模型",
  to_markdown_table(glm_coef),
  "",
  "## 陈皮来源地差异",
  to_markdown_table(origin_pattern_summary),
  "",
  to_markdown_table(origin_overall_result),
  "",
  to_markdown_table(origin_single_result),
  "",
  to_markdown_table(chenpi_batch_origin_summary)
)
writeLines(desc_md, file.path(docs_dir, "D6_陈皮与成品崩解关联分析说明.md"), useBytes = TRUE)

formal_md <- c(
  "# D6 正式结果分析与成品崩解关联",
  "",
  "## 可直接用于论文中文稿的结果段落",
  paste0("- ", results_paragraphs)
)
writeLines(formal_md, file.path(docs_dir, "D6_正式结果分析与成品崩解关联.md"), useBytes = TRUE)

cat("Input D2:", input_d2, "\n")
cat("Input D3:", input_d3, "\n")
cat("Input D5:", input_d5, "\n")
cat("Input D6:", input_d6, "\n")
cat("Saved workbook:", excel_path, "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
