options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(officer)
  library(openxlsx)
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

metric_map <- tibble::tribble(
  ~var, ~label, ~stem,
  "moisture_pct", "Moisture (%)", "moisture",
  "total_ash_pct", "Total ash (%)", "total_ash",
  "extract_pct", "Extract (%)", "extract",
  "hesperidin_mg_g", "Hesperidin content (mg/g)", "hesperidin_content"
)

set_plot_style()

input_d2 <- choose_source_file("D2")
input_d3 <- choose_source_file("D3")
input_d4 <- choose_source_file("D4")

d2_raw <- read_xlsx(input_d2)
d3_raw <- read_xlsx(input_d3)
d4_raw <- read_xlsx(input_d4)

d2 <- tibble(
  finished_batch = norm_batch(d2_raw[[1]]),
  production_date = as.Date(d2_raw[[2]]),
  disintegration_time_min = as.numeric(d2_raw[[4]]),
  dosage_strength = d2_raw[[8]] |> as.character() |> str_trim() |> str_replace_all("\\s+", "")
)

d3 <- tibble(
  finished_batch = norm_batch(d3_raw[[1]]),
  extract_batch = norm_batch(d3_raw[[3]])
)

d4 <- tibble(
  extract_batch = norm_batch(d4_raw[[1]]),
  moisture_pct = as.numeric(d4_raw[[2]]),
  total_ash_pct = as.numeric(d4_raw[[3]]),
  extract_pct = as.numeric(d4_raw[[4]]),
  hesperidin_mg_g = as.numeric(d4_raw[[5]])
)

d2_d3 <- d2 |>
  inner_join(d3, by = "finished_batch")

matched_finished <- d2_d3 |>
  filter(extract_batch != "") |>
  inner_join(d4, by = "extract_batch")

matched_finished_08 <- matched_finished |>
  filter(dosage_strength == "0.8g")

matched_extract <- matched_finished_08 |>
  group_by(extract_batch) |>
  summarise(
    finished_batch_n = n_distinct(finished_batch),
    disintegration_mean = mean(disintegration_time_min, na.rm = TRUE),
    disintegration_median = median(disintegration_time_min, na.rm = TRUE),
    disintegration_max = max(disintegration_time_min, na.rm = TRUE),
    gt10_n = sum(disintegration_time_min > 10, na.rm = TRUE),
    gt10_ratio = mean(disintegration_time_min > 10, na.rm = TRUE),
    gt10_any = ifelse(any(disintegration_time_min > 10, na.rm = TRUE), 1L, 0L),
    moisture_pct = first(moisture_pct),
    total_ash_pct = first(total_ash_pct),
    extract_pct = first(extract_pct),
    hesperidin_mg_g = first(hesperidin_mg_g),
    .groups = "drop"
  ) |>
  mutate(
    issue_group = factor(
      gt10_any,
      levels = c(0, 1),
      labels = c("No linked\nbatch >10 min", "At least one linked\nbatch >10 min")
    )
  )

pairing_overview <- tibble(
  item = c(
    "D2 total records",
    "D2 unique finished batches",
    "D2-D3 matched finished batches",
    "D2-D3 rows with non-empty extract batch",
    "D2-D3-D4 matched finished batches",
    "D2-D3-D4 matched extract batches",
    "Matched finished records with >10 min",
    "Matched extract batches with at least one linked batch >10 min"
  ),
  value = c(
    nrow(d2),
    n_distinct(d2$finished_batch),
    n_distinct(d2_d3$finished_batch),
    sum(d2_d3$extract_batch != "", na.rm = TRUE),
    n_distinct(matched_finished_08$finished_batch),
    n_distinct(matched_extract$extract_batch),
    sum(matched_finished_08$disintegration_time_min > 10, na.rm = TRUE),
    sum(matched_extract$gt10_any)
  )
)

matched_finished_summary <- tibble(
  Specification = "0.8g",
  `Matched finished records` = nrow(matched_finished_08),
  `Matched finished batches` = n_distinct(matched_finished_08$finished_batch),
  `Matched extract batches` = n_distinct(matched_extract$extract_batch),
  `Disintegration >10 min, n (%)` = sprintf(
    "%d (%.2f%%)",
    sum(matched_finished_08$disintegration_time_min > 10, na.rm = TRUE),
    mean(matched_finished_08$disintegration_time_min > 10, na.rm = TRUE) * 100
  )
)

extract_linkage_summary <- tibble(
  item = c(
    "Finished batches per extract batch, mean",
    "Finished batches per extract batch, median",
    "Finished batches per extract batch, Q1",
    "Finished batches per extract batch, Q3",
    "Extract batches with any linked finished batch >10 min, n (%)"
  ),
  value = c(
    fmt_num(mean(matched_extract$finished_batch_n), 3),
    fmt_num(median(matched_extract$finished_batch_n), 3),
    fmt_num(quantile(matched_extract$finished_batch_n, 0.25), 3),
    fmt_num(quantile(matched_extract$finished_batch_n, 0.75), 3),
    sprintf(
      "%d (%.2f%%)",
      sum(matched_extract$gt10_any),
      mean(matched_extract$gt10_any) * 100
    )
  )
)

predictor_summary <- matched_extract |>
  summarise(
    across(
      all_of(metric_map$var),
      list(
        n = ~sum(!is.na(.x)),
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        q1 = ~quantile(.x, 0.25, na.rm = TRUE),
        q3 = ~quantile(.x, 0.75, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("var", ".value"),
    names_sep = "__"
  ) |>
  left_join(metric_map, by = c("var")) |>
  transmute(
    Indicator = label,
    n,
    `Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd),
    `Median (Q1, Q3)` = sprintf("%.4f (%.4f, %.4f)", median, q1, q3),
    Range = sprintf("%.4f to %.4f", min, max)
  )
names(predictor_summary)[3] <- "Mean ± SD"

univariate_mean_assoc <- lapply(metric_map$var, function(v) {
  test <- suppressWarnings(cor.test(matched_extract[[v]], matched_extract$disintegration_mean, method = "spearman", exact = FALSE))
  tibble(
    Indicator = metric_map$label[metric_map$var == v],
    `Spearman rho with mean disintegration` = round(unname(test$estimate), 4),
    `P value` = fmt_p(test$p.value)
  )
}) |>
  bind_rows()

univariate_ratio_assoc <- lapply(metric_map$var, function(v) {
  test <- suppressWarnings(cor.test(matched_extract[[v]], matched_extract$gt10_ratio, method = "spearman", exact = FALSE))
  tibble(
    Indicator = metric_map$label[metric_map$var == v],
    `Spearman rho with linked >10 min ratio` = round(unname(test$estimate), 4),
    `P value` = fmt_p(test$p.value)
  )
}) |>
  bind_rows()

group_comparison <- lapply(metric_map$var, function(v) {
  test <- suppressWarnings(wilcox.test(matched_extract[[v]] ~ matched_extract$gt10_any, exact = FALSE))
  median_0 <- median(matched_extract[[v]][matched_extract$gt10_any == 0], na.rm = TRUE)
  median_1 <- median(matched_extract[[v]][matched_extract$gt10_any == 1], na.rm = TRUE)
  tibble(
    Indicator = metric_map$label[metric_map$var == v],
    `Median, no linked batch >10 min` = round(median_0, 4),
    `Median, >=1 linked batch >10 min` = round(median_1, 4),
    `Wilcoxon P value` = fmt_p(test$p.value),
    Significance = stars_from_p(test$p.value)
  )
}) |>
  bind_rows()

feature_screening <- lapply(metric_map$var, function(v) {
  label <- metric_map$label[metric_map$var == v]
  sub_df <- matched_extract |>
    select(gt10_any, gt10_ratio, all_of(v)) |>
    filter(!is.na(.data[[v]]), !is.na(gt10_any), !is.na(gt10_ratio))
  x0 <- sub_df |> filter(gt10_any == 0) |> pull(.data[[v]])
  x1 <- sub_df |> filter(gt10_any == 1) |> pull(.data[[v]])
  wt <- suppressWarnings(wilcox.test(x0, x1, exact = FALSE))
  sp <- suppressWarnings(cor.test(sub_df[[v]], sub_df$gt10_ratio, method = "spearman", exact = FALSE))
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

predictor_correlation <- matched_extract |>
  select(all_of(metric_map$var)) |>
  cor(method = "spearman") |>
  round(4) |>
  as.data.frame() |>
  tibble::rownames_to_column("Indicator")

matched_extract_scaled <- matched_extract |>
  mutate(
    moisture_z = as.numeric(scale(moisture_pct)),
    total_ash_z = as.numeric(scale(total_ash_pct)),
    extract_z = as.numeric(scale(extract_pct)),
    hesperidin_z = as.numeric(scale(hesperidin_mg_g))
  )

lm_fit <- lm(
  disintegration_mean ~ moisture_z + total_ash_z + extract_z + hesperidin_z,
  data = matched_extract_scaled
)

lm_coef <- summary(lm_fit)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  filter(term != "(Intercept)") |>
  mutate(
    Indicator = c("Moisture (%)", "Total ash (%)", "Extract (%)", "Hesperidin content (mg/g)"),
    `Standardized beta` = round(Estimate, 4),
    `95% CI` = sprintf("%.4f to %.4f", Estimate - 1.96 * `Std. Error`, Estimate + 1.96 * `Std. Error`),
    `P value` = fmt_p(`Pr(>|t|)`)
  ) |>
  select(Indicator, `Standardized beta`, `95% CI`, `P value`)

glm_fit <- glm(
  cbind(gt10_n, finished_batch_n - gt10_n) ~ moisture_z + total_ash_z + extract_z + hesperidin_z,
  family = binomial(),
  data = matched_extract_scaled
)

glm_coef <- summary(glm_fit)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  filter(term != "(Intercept)") |>
  mutate(
    Indicator = c("Moisture (%)", "Total ash (%)", "Extract (%)", "Hesperidin content (mg/g)"),
    OR = exp(Estimate),
    lower = exp(Estimate - 1.96 * `Std. Error`),
    upper = exp(Estimate + 1.96 * `Std. Error`),
    `OR per 1 SD increase` = round(OR, 4),
    `95% CI` = sprintf("%.4f to %.4f", lower, upper),
    `P value` = fmt_p(`Pr(>|z|)`)
  ) |>
  select(Indicator, `OR per 1 SD increase`, `95% CI`, `P value`)

matched_finished_export <- matched_finished_08 |>
  select(
    finished_batch,
    production_date,
    dosage_strength,
    disintegration_time_min,
    extract_batch,
    moisture_pct,
    total_ash_pct,
    extract_pct,
    hesperidin_mg_g
  ) |>
  arrange(production_date, finished_batch)

matched_extract_export <- matched_extract |>
  select(
    extract_batch,
    finished_batch_n,
    disintegration_mean,
    disintegration_median,
    disintegration_max,
    gt10_n,
    gt10_ratio,
    gt10_any,
    moisture_pct,
    total_ash_pct,
    extract_pct,
    hesperidin_mg_g
  ) |>
  arrange(extract_batch)

scatter_annotation <- lapply(metric_map$var, function(v) {
  test <- suppressWarnings(cor.test(matched_extract[[v]], matched_extract$disintegration_mean, method = "spearman", exact = FALSE))
  fdr_value <- feature_screening$`Screening FDR`[feature_screening$Indicator == metric_map$label[metric_map$var == v]]
  tibble(
    var = v,
    label = paste0(
      "Spearman rho = ", fmt_num(unname(test$estimate), 3),
      "\nP ", fmt_p(test$p.value),
      "\nFDR ", fmt_p(fdr_value),
      "\nn = ", sum(complete.cases(matched_extract[[v]], matched_extract$disintegration_mean))
    )
  )
}) |>
  bind_rows()

group_annotation <- lapply(metric_map$var, function(v) {
  test <- suppressWarnings(wilcox.test(matched_extract[[v]] ~ matched_extract$gt10_any, exact = FALSE))
  group_stats <- matched_extract |>
    group_by(issue_group, gt10_any) |>
    summarise(
      n = n(),
      median = median(.data[[v]], na.rm = TRUE),
      q3 = quantile(.data[[v]], 0.75, na.rm = TRUE),
      ymax = max(.data[[v]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(gt10_any)

  tibble(
    var = v,
    p_label = paste0("Wilcoxon P ", fmt_p(test$p.value), " (", stars_from_p(test$p.value), ")"),
    group_0_label = paste0("No linked\n>10 min batch\n(n=", group_stats$n[group_stats$gt10_any == 0], ")"),
    group_1_label = paste0("\u22651 linked\n>10 min batch\n(n=", group_stats$n[group_stats$gt10_any == 1], ")"),
    median_0 = group_stats$median[group_stats$gt10_any == 0],
    median_1 = group_stats$median[group_stats$gt10_any == 1],
    q3_0 = group_stats$q3[group_stats$gt10_any == 0],
    q3_1 = group_stats$q3[group_stats$gt10_any == 1],
    ymax_all = max(group_stats$ymax, na.rm = TRUE)
  )
}) |>
  bind_rows()

build_scatter_plot <- function(df, x_var, x_label) {
  ann <- scatter_annotation |>
    filter(var == x_var)
  x_range <- range(df[[x_var]], na.rm = TRUE)
  y_range <- range(df$disintegration_mean, na.rm = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)

  x_left <- x_range[1] + ifelse(x_span == 0, 0, x_span * 0.04)
  x_right <- x_range[2] - ifelse(x_span == 0, 0, x_span * 0.04)
  y_top <- y_range[2] - ifelse(y_span == 0, 0.05, y_span * 0.05)
  y_bottom <- y_range[1] + ifelse(y_span == 0, 0.05, y_span * 0.10)
  x_cut_left <- x_range[1] + ifelse(x_span == 0, 1, x_span * 0.42)
  x_cut_right <- x_range[2] - ifelse(x_span == 0, 1, x_span * 0.42)
  y_cut_top <- y_range[2] - ifelse(y_span == 0, 1, y_span * 0.32)
  y_cut_bottom <- y_range[1] + ifelse(y_span == 0, 1, y_span * 0.32)

  candidate_df <- tibble::tribble(
    ~corner, ~x, ~y, ~hjust, ~vjust,
    "top_left", x_left, y_top, 0, 1,
    "top_right", x_right, y_top, 1, 1,
    "bottom_left", x_left, y_bottom, 0, 0,
    "bottom_right", x_right, y_bottom, 1, 0
  ) |>
    mutate(
      point_count = c(
        sum(df[[x_var]] <= x_cut_left & df$disintegration_mean >= y_cut_top, na.rm = TRUE),
        sum(df[[x_var]] >= x_cut_right & df$disintegration_mean >= y_cut_top, na.rm = TRUE),
        sum(df[[x_var]] <= x_cut_left & df$disintegration_mean <= y_cut_bottom, na.rm = TRUE),
        sum(df[[x_var]] >= x_cut_right & df$disintegration_mean <= y_cut_bottom, na.rm = TRUE)
      )
    ) |>
    arrange(point_count, desc(corner %in% c("top_left", "top_right")))

  ann_x <- candidate_df$x[1]
  ann_y <- candidate_df$y[1]
  ann_hjust <- candidate_df$hjust[1]
  ann_vjust <- candidate_df$vjust[1]

  ggplot(df, aes(x = .data[[x_var]], y = disintegration_mean)) +
    geom_point(colour = point_colour, size = 2.8) +
    geom_smooth(method = "lm", se = FALSE, colour = trend_colour, linewidth = 2.5) +
    annotate(
      "text",
      x = ann_x,
      y = ann_y,
      label = ann$label[1],
      hjust = ann_hjust,
      vjust = ann_vjust,
      size = 8.0,
      lineheight = 0.95,
      family = "Arial",
      colour = "black"
    ) +
    labs(
      x = x_label,
      y = "Mean disintegration time (min)"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.02, 0.12))
    ) +
    theme(
      panel.grid.major.x = element_blank()
    )
}

build_group_plot <- function(df, x_var, x_label) {
  ann <- group_annotation |>
    filter(var == x_var)
  y_range <- range(df[[x_var]], na.rm = TRUE)
  y_span <- diff(y_range)
  y_pad <- ifelse(y_span == 0, 0.15, y_span * 0.12)
  bracket_y <- ann$ymax_all[1] + y_pad * 0.55
  text_y <- bracket_y + y_pad * 0.35

  ggplot(df, aes(x = issue_group, y = .data[[x_var]])) +
    geom_boxplot(
      width = 0.46,
      fill = "white",
      colour = distribution_colour,
      linewidth = 1.2,
      outlier.shape = NA
    ) +
    geom_point(
      position = position_jitter(width = 0.12, height = 0),
      colour = point_colour,
      size = 2.4
    ) +
    annotate("segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, colour = "black", linewidth = 0.9) +
    annotate("segment", x = 1, xend = 1, y = bracket_y - y_pad * 0.12, yend = bracket_y, colour = "black", linewidth = 0.9) +
    annotate("segment", x = 2, xend = 2, y = bracket_y - y_pad * 0.12, yend = bracket_y, colour = "black", linewidth = 0.9) +
    annotate(
      "text",
      x = 1.5,
      y = text_y,
      label = ann$p_label[1],
      size = 7.6,
      family = "Arial",
      colour = "black"
    ) +
    labs(
      x = "Finished-product disintegration group",
      y = x_label
    ) +
    scale_x_discrete(labels = c(ann$group_0_label[1], ann$group_1_label[1])) +
    scale_y_continuous(
      expand = expansion(mult = c(0.02, 0.18))
    ) +
    theme(
      axis.text.x = element_text(size = 24, lineheight = 0.9),
      axis.title.x = element_text(size = 28, margin = margin(t = 10)),
      plot.margin = margin(12, 28, 20, 16)
    )
}

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
  "00_d4_extract_feature_screening_rank",
  11.2,
  5.8
)

figure_index <- 1
for (i in seq_len(nrow(metric_map))) {
  x_var <- metric_map$var[i]
  x_label <- metric_map$label[i]
  stem <- metric_map$stem[i]

  save_plot_dual(
    build_scatter_plot(matched_extract, x_var, x_label),
    sprintf("%02d_d4_finished_disintegration_scatter_%s", figure_index, stem),
    10.4,
    8.2
  )
  figure_index <- figure_index + 1
}

significant_metric_map <- metric_map |>
  left_join(feature_screening |> select(Indicator, `Screening FDR`), by = c("label" = "Indicator")) |>
  filter(!is.na(`Screening FDR`), `Screening FDR` < 0.05) |>
  arrange(`Screening FDR`)

combined_scatter_long <- matched_extract |>
  select(extract_batch, disintegration_mean, all_of(significant_metric_map$var)) |>
  pivot_longer(
    cols = all_of(significant_metric_map$var),
    names_to = "var",
    values_to = "value"
  ) |>
  filter(is.finite(value), is.finite(disintegration_mean)) |>
  left_join(significant_metric_map |> select(var, label, `Screening FDR`), by = "var") |>
  mutate(label = factor(label, levels = significant_metric_map$label))

combined_scatter_annotation <- lapply(significant_metric_map$var, function(v) {
  label_value <- significant_metric_map$label[significant_metric_map$var == v]
  test <- suppressWarnings(cor.test(matched_extract[[v]], matched_extract$disintegration_mean, method = "spearman", exact = FALSE))
  fdr_value <- significant_metric_map$`Screening FDR`[significant_metric_map$var == v]
  tibble(
    label = factor(label_value, levels = significant_metric_map$label),
    annotation = paste0(
      "Spearman rho = ", fmt_num(unname(test$estimate), 3),
      "\nP ", fmt_p(test$p.value),
      "\nFDR ", fmt_p(fdr_value),
      "\nn = ", sum(complete.cases(matched_extract[[v]], matched_extract$disintegration_mean))
    )
  )
}) |>
  bind_rows()

combined_scatter_plot <- ggplot(combined_scatter_long, aes(x = value, y = disintegration_mean)) +
  geom_point(colour = point_colour, size = 2.0) +
  geom_smooth(method = "lm", se = FALSE, colour = trend_colour, linewidth = 1.7) +
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
  labs(x = "Extract-powder quality attribute", y = "Mean disintegration time (min)") +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.18))) +
  theme(
    strip.text = element_text(size = 20),
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 28),
    panel.spacing = unit(1.0, "lines")
  )

save_plot_dual(
  combined_scatter_plot,
  "08_d4_key_extract_features_vs_disintegration_time",
  16.5,
  5.9
)

unlink(file.path(figures_dir, "*_d4_finished_disintegration_group_*.pdf"), force = TRUE)
unlink(file.path(figures_dir, "*_d4_finished_disintegration_group_*.png"), force = TRUE)

for (i in seq_len(nrow(significant_metric_map))) {
  x_var <- significant_metric_map$var[i]
  x_label <- significant_metric_map$label[i]
  stem <- significant_metric_map$stem[i]

  save_plot_dual(
    build_group_plot(matched_extract, x_var, x_label),
    sprintf("%02d_d4_finished_disintegration_group_%s", figure_index, stem),
    10.0,
    8.2
  )
  figure_index <- figure_index + 1
}

excel_path <- file.path(tables_dir, "D4_finished_disintegration_association.xlsx")
wb <- createWorkbook()
for (sheet_name in c(
  "pairing_overview",
  "matched_finished_summary",
  "extract_linkage_summary",
  "predictor_summary",
  "feature_screening",
  "univariate_mean_assoc",
  "univariate_ratio_assoc",
  "group_comparison",
  "multivariable_lm",
  "multivariable_glm",
  "predictor_correlation",
  "matched_finished_level",
  "matched_extract_level"
)) {
  addWorksheet(wb, sheet_name)
}
writeData(wb, "pairing_overview", pairing_overview)
writeData(wb, "matched_finished_summary", matched_finished_summary)
writeData(wb, "extract_linkage_summary", extract_linkage_summary)
writeData(wb, "predictor_summary", predictor_summary)
writeData(wb, "feature_screening", feature_screening)
writeData(wb, "univariate_mean_assoc", univariate_mean_assoc)
writeData(wb, "univariate_ratio_assoc", univariate_ratio_assoc)
writeData(wb, "group_comparison", group_comparison)
writeData(wb, "multivariable_lm", lm_coef)
writeData(wb, "multivariable_glm", glm_coef)
writeData(wb, "predictor_correlation", predictor_correlation)
writeData(wb, "matched_finished_level", matched_finished_export)
writeData(wb, "matched_extract_level", matched_extract_export)
saveWorkbook(wb, excel_path, overwrite = TRUE)

problem_extract_n <- sum(matched_extract$gt10_any)
problem_finished_n <- sum(matched_finished_08$disintegration_time_min > 10, na.rm = TRUE)

results_paragraphs <- c(
  paste0(
    "以 D3 中记录的浸膏粉批号为桥接字段，将 D2 成品理化数据与 D4 浸膏粉理化数据配对后，共得到 ",
    nrow(matched_finished_08), " 条可直接关联的成品记录，对应 ",
    n_distinct(matched_finished_08$finished_batch), " 个成品批次和 ",
    n_distinct(matched_extract$extract_batch), " 个浸膏粉批次。全部可配对成品记录均来自 0.8g 规格，因此后续关联分析聚焦于 0.8g 成品崩解时限。"
  ),
  paste0(
    "在这 ", nrow(matched_finished_08), " 条配对成品记录中，崩解时限 >10 min 的记录共有 ",
    problem_finished_n, " 条，占 ", fmt_num(mean(matched_finished_08$disintegration_time_min > 10, na.rm = TRUE) * 100, 2),
    "%；按浸膏粉批次聚合后，共有 ", problem_extract_n, " 个浸膏粉批次至少对应 1 个崩解时限 >10 min 的成品批次，占全部配对浸膏粉批次的 ",
    fmt_num(mean(matched_extract$gt10_any) * 100, 2), "%。每个浸膏粉批次对应的成品批次数中位数为 ",
    fmt_num(median(matched_extract$finished_batch_n), 0), "，四分位区间为 ",
    fmt_num(quantile(matched_extract$finished_batch_n, 0.25), 0), "–",
    fmt_num(quantile(matched_extract$finished_batch_n, 0.75), 0), "。"
  ),
  paste0(
    "以浸膏粉批次为分析单元时，总灰分与配对成品平均崩解时限呈正相关（Spearman rho = ",
    fmt_num(as.numeric(univariate_mean_assoc$`Spearman rho with mean disintegration`[univariate_mean_assoc$Indicator == "Total ash (%)"]), 3),
    ", P ", univariate_mean_assoc$`P value`[univariate_mean_assoc$Indicator == "Total ash (%)"],
    "），浸出物与配对成品平均崩解时限呈负相关（rho = ",
    fmt_num(as.numeric(univariate_mean_assoc$`Spearman rho with mean disintegration`[univariate_mean_assoc$Indicator == "Extract (%)"]), 3),
    ", P ", univariate_mean_assoc$`P value`[univariate_mean_assoc$Indicator == "Extract (%)"],
    "）。水分与平均崩解时限未见显著关联。"
  ),
  paste0(
    "若直接以“是否至少关联 1 个崩解时限 >10 min 的成品批次”定义问题组，则问题组浸膏粉的总灰分中位数高于非问题组（",
    fmt_num(group_comparison$`Median, >=1 linked batch >10 min`[group_comparison$Indicator == "Total ash (%)"], 3), "% vs ",
    fmt_num(group_comparison$`Median, no linked batch >10 min`[group_comparison$Indicator == "Total ash (%)"], 3), "%，P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Total ash (%)"],
    "），浸出物中位数更低（",
    fmt_num(group_comparison$`Median, >=1 linked batch >10 min`[group_comparison$Indicator == "Extract (%)"], 3), "% vs ",
    fmt_num(group_comparison$`Median, no linked batch >10 min`[group_comparison$Indicator == "Extract (%)"], 3), "%，P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Extract (%)"],
    "），橙皮苷含量也更高（",
    fmt_num(group_comparison$`Median, >=1 linked batch >10 min`[group_comparison$Indicator == "Hesperidin content (mg/g)"], 3), " vs ",
    fmt_num(group_comparison$`Median, no linked batch >10 min`[group_comparison$Indicator == "Hesperidin content (mg/g)"], 3), " mg/g，P ",
    group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Hesperidin content (mg/g)"],
    "）；水分差异不显著（P ", group_comparison$`Wilcoxon P value`[group_comparison$Indicator == "Moisture (%)"], "）。"
  ),
  paste0(
    "在四指标同时纳入模型后，崩解时限问题更接近一种组合型理化模式，而不只是单个指标的线性漂移。线性模型中，总灰分（标准化系数 ",
    fmt_num(lm_coef$`Standardized beta`[lm_coef$Indicator == "Total ash (%)"], 3),
    "）和浸出物（", fmt_num(lm_coef$`Standardized beta`[lm_coef$Indicator == "Extract (%)"], 3),
    "）仍是幅度最大的主信号，水分则在校正其他指标后表现为负向条件关联（",
    fmt_num(lm_coef$`Standardized beta`[lm_coef$Indicator == "Moisture (%)"], 3),
    "）。二项模型中，总灰分每升高 1 个标准差，对应成品 >10 min 比例的 OR 为 ",
    fmt_num(glm_coef$`OR per 1 SD increase`[glm_coef$Indicator == "Total ash (%)"], 3),
    "，浸出物每升高 1 个标准差，对应 OR 为 ",
    fmt_num(glm_coef$`OR per 1 SD increase`[glm_coef$Indicator == "Extract (%)"], 3),
    "，橙皮苷含量和水分也在条件模型中保留显著效应。综合来看，D4 中最值得优先关注的主信号是总灰分升高与浸出物降低，而水分和橙皮苷更适合作为联合判别时的辅助信息。"
  )
)

desc_md <- c(
  "# D4 浸膏粉与成品崩解时限关联分析说明",
  "",
  paste0("- 数据源：`", basename(input_d2), "`、`", basename(input_d3), "`、`", basename(input_d4), "`"),
  paste0("- 桥接逻辑：`D2 finished batch -> D3 finished batch -> D3 extract batch -> D4 extract batch`"),
  paste0("- 可配对成品记录：`", nrow(matched_finished_08), "`"),
  paste0("- 可配对浸膏粉批次：`", n_distinct(matched_extract$extract_batch), "`"),
  paste0("- 问题定义：`Finished-product disintegration time >10 min`"),
  "",
  "## 统计口径",
  "",
  "- 主分析单元为浸膏粉批次，避免同一 D4 批次对应多个成品批次时被重复计权。",
  "- 连续关联使用浸膏粉批次对应的成品平均崩解时限。",
  "- 问题分组使用“该浸膏粉批次是否至少关联 1 个成品批次崩解时限 >10 min”。",
  "- 图顶不放标题，Arial 字体，先导出 PDF，再转 PNG。"
)

formal_md <- c(
  "# D4 正式结果分析与成品崩解关联",
  "",
  "## 一、配对样本概况",
  "",
  results_paragraphs[1],
  "",
  results_paragraphs[2],
  "",
  "## 二、单变量关联结果",
  "",
  to_markdown_table(univariate_mean_assoc),
  "",
  to_markdown_table(group_comparison),
  "",
  "## 三、多变量模型结果",
  "",
  to_markdown_table(lm_coef),
  "",
  to_markdown_table(glm_coef),
  "",
  "## 四、可直接用于论文中文稿的结果段落",
  "",
  as.vector(rbind(results_paragraphs, "")),
  "",
  "## 五、对应图件",
  "",
  "- 01–04：D4 四指标与配对成品平均崩解时限散点回归图",
  "- 05–08：D4 四指标按成品崩解问题组的箱线图"
)

writeLines(desc_md, con = file.path(docs_dir, "D4_浸膏粉与成品崩解时限关联分析说明.md"), useBytes = TRUE)
writeLines(formal_md, con = file.path(docs_dir, "D4_正式结果分析与成品崩解关联.md"), useBytes = TRUE)

doc_desc <- read_docx()
doc_desc <- body_add_par(doc_desc, "D4 浸膏粉与成品崩解时限关联分析说明", style = "heading 1")
doc_desc <- body_add_par(doc_desc, paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")
doc_desc <- body_add_par(doc_desc, "本稿以 D3 中记录的浸膏粉批号为桥接字段，将 D2 成品崩解时限与 D4 浸膏粉理化指标配对，并将浸膏粉批次作为主分析单元。", style = "Normal")
doc_desc <- body_add_par(doc_desc, "配对概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(pairing_overview))
doc_desc <- body_add_par(doc_desc, "配对成品与浸膏粉概况", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(matched_finished_summary))
doc_desc <- body_add_flextable(doc_desc, build_flextable(extract_linkage_summary))
doc_desc <- body_add_par(doc_desc, "D4 四指标分布", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(predictor_summary))
doc_desc <- body_add_par(doc_desc, "单变量关联", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(univariate_mean_assoc))
doc_desc <- body_add_flextable(doc_desc, build_flextable(univariate_ratio_assoc))
doc_desc <- body_add_flextable(doc_desc, build_flextable(group_comparison))
doc_desc <- body_add_par(doc_desc, "多变量模型", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(lm_coef))
doc_desc <- body_add_flextable(doc_desc, build_flextable(glm_coef))
doc_desc <- body_add_par(doc_desc, "预测变量相关性", style = "heading 2")
doc_desc <- body_add_flextable(doc_desc, build_flextable(predictor_correlation))
print(doc_desc, target = file.path(docs_dir, "D4_浸膏粉与成品崩解时限关联分析说明.docx"))

doc_formal <- read_docx()
doc_formal <- body_add_par(doc_formal, "D4 正式结果分析与成品崩解关联", style = "heading 1")
doc_formal <- body_add_par(doc_formal, "配对样本概况", style = "heading 2")
for (paragraph in results_paragraphs[1:2]) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
doc_formal <- body_add_par(doc_formal, "单变量关联结果", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(univariate_mean_assoc))
doc_formal <- body_add_flextable(doc_formal, build_flextable(group_comparison))
doc_formal <- body_add_par(doc_formal, "多变量模型结果", style = "heading 2")
doc_formal <- body_add_flextable(doc_formal, build_flextable(lm_coef))
doc_formal <- body_add_flextable(doc_formal, build_flextable(glm_coef))
doc_formal <- body_add_par(doc_formal, "可直接用于论文中文稿的结果段落", style = "heading 2")
for (paragraph in results_paragraphs) {
  doc_formal <- body_add_par(doc_formal, paragraph, style = "Normal")
}
print(doc_formal, target = file.path(docs_dir, "D4_正式结果分析与成品崩解关联.docx"))

cat("Input D2:", input_d2, "\n")
cat("Input D3:", input_d3, "\n")
cat("Input D4:", input_d4, "\n")
cat("Saved workbook:", excel_path, "\n")
cat("Saved figures:", length(list.files(figures_dir)), "\n")
