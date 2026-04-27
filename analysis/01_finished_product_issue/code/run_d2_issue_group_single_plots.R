options(warn = 1)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(openxlsx)
  library(readxl)
  library(scales)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
if (length(script_path) == 0 || identical(script_path, character(0))) {
  project_dir <- normalizePath("成品问题驱动_D2描述", winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
}
root_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/", mustWork = TRUE)

input_candidates <- list.files(
  root_dir,
  pattern = "^D2_finished_product_physchem_final\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)
input_file <- input_candidates[grepl("定稿数据_英文", input_candidates, fixed = TRUE)][1]
if (is.na(input_file)) {
  input_file <- input_candidates[1]
}
if (is.na(input_file) || !file.exists(input_file)) {
  stop("D2_finished_product_physchem_final.xlsx was not found.")
}

figures_dir <- file.path(project_dir, "figures")
tables_dir <- file.path(project_dir, "tables")
docs_dir <- file.path(project_dir, "docs")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

point_colour <- "#8C8C8C"
box_line_colour <- "#0A5C7A"
box_fill_colour <- "#EAF2F5"
text_colour <- "#464F5F"
grid_colour <- "#DADDE2"

theme_set(theme_bw(base_family = "Arial", base_size = 22))
theme_update(
  plot.title = element_blank(),
  axis.title = element_text(size = 25, colour = "black"),
  axis.text = element_text(size = 22, colour = "black"),
  panel.grid.major = element_line(colour = grid_colour, linewidth = 0.55),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.9),
  axis.line = element_line(colour = "black", linewidth = 0.9),
  legend.position = "none",
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.margin = margin(16, 22, 16, 18)
)

format_wilcoxon_label <- function(p_value) {
  stars <- dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )

  p_text <- if (is.na(p_value)) {
    "P = NA"
  } else if (p_value < 0.001) {
    "P < 0.001"
  } else {
    paste0("P = ", formatC(p_value, format = "f", digits = 3))
  }

  paste0("Wilcoxon ", p_text, " (", stars, ")")
}

save_plot <- function(plot_obj, stem) {
  pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figures_dir, paste0(stem, ".png"))

  ggsave(
    filename = pdf_path,
    plot = plot_obj,
    width = 6.6,
    height = 5.4,
    units = "in",
    device = cairo_pdf,
    dpi = 330,
    bg = "white"
  )
  ggsave(
    filename = png_path,
    plot = plot_obj,
    width = 6.6,
    height = 5.4,
    units = "in",
    dpi = 330,
    bg = "white"
  )

  c(pdf = pdf_path, png = png_path)
}

build_issue_plot <- function(df, variable, y_label, stat_label) {
  plot_df <- df |>
    filter(!is.na(.data[[variable]])) |>
    mutate(
      issue_group = factor(
        issue_group,
        levels = c("Disintegration <= 10 min", "Disintegration > 10 min")
      )
    )

  group_n <- plot_df |>
    count(issue_group, name = "n") |>
    mutate(
      x_label = dplyr::case_when(
        issue_group == "Disintegration <= 10 min" ~ paste0("<= 10 min\n(n = ", comma(n), ")"),
        issue_group == "Disintegration > 10 min" ~ paste0("> 10 min\n(n = ", comma(n), ")"),
        TRUE ~ paste0(as.character(issue_group), "\n(n = ", comma(n), ")")
      )
    )
  label_map <- setNames(group_n$x_label, as.character(group_n$issue_group))

  y_range <- range(plot_df[[variable]], na.rm = TRUE)
  y_pad <- diff(y_range) * 0.15
  if (!is.finite(y_pad) || y_pad == 0) {
    y_pad <- max(abs(y_range), na.rm = TRUE) * 0.05
  }
  y_segment <- y_range[2] + y_pad * 0.40
  y_label_pos <- y_range[2] + y_pad * 0.72

  ggplot(plot_df, aes(x = issue_group, y = .data[[variable]])) +
    geom_boxplot(
      width = 0.58,
      outlier.shape = NA,
      fill = box_fill_colour,
      colour = box_line_colour,
      linewidth = 1.05
    ) +
    geom_jitter(
      width = 0.16,
      size = 1.9,
      colour = point_colour
    ) +
    annotate(
      "segment",
      x = 1,
      xend = 2,
      y = y_segment,
      yend = y_segment,
      colour = text_colour,
      linewidth = 0.85
    ) +
    annotate(
      "text",
      x = 1.5,
      y = y_label_pos,
      label = stat_label,
      family = "Arial",
      size = 6.3,
      colour = text_colour
    ) +
    scale_x_discrete(labels = label_map) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.14))) +
    labs(x = "Disintegration time group", y = y_label) +
    coord_cartesian(clip = "off") +
    theme(
      axis.text.x = element_text(size = 21, lineheight = 0.95),
      axis.title.x = element_text(size = 23, margin = margin(t = 8)),
      axis.title.y = element_text(size = 24, margin = margin(r = 8))
    )
}

df <- read_xlsx(input_file) |>
  mutate(
    batch_no = as.character(batch_no),
    dosage_strength = gsub("\\s+", "", as.character(dosage_strength))
  ) |>
  filter(dosage_strength == "0.8g", !is.na(disintegration_time_min)) |>
  mutate(
    issue_group = if_else(
      disintegration_time_min > 10,
      "Disintegration > 10 min",
      "Disintegration <= 10 min"
    )
  )

variables <- tibble::tribble(
  ~variable, ~indicator, ~stem,
  "coated_tablet_weight_g", "Coated tablet weight (g)", "08_0p8g_disintegration_group_coated_tablet_weight",
  "active_content_mg_per_tablet", "Active content (mg/tablet)", "09_0p8g_disintegration_group_active_content"
)

test_table <- variables |>
  rowwise() |>
  mutate(
    wilcoxon_p = wilcox.test(df[[variable]] ~ df$issue_group, exact = FALSE)$p.value,
    statistical_label = format_wilcoxon_label(wilcoxon_p)
  ) |>
  ungroup()

summary_table <- df |>
  group_by(issue_group) |>
  summarise(
    records = n(),
    unique_batches = n_distinct(batch_no),
    coated_tablet_weight_mean = mean(coated_tablet_weight_g, na.rm = TRUE),
    coated_tablet_weight_sd = sd(coated_tablet_weight_g, na.rm = TRUE),
    coated_tablet_weight_median = median(coated_tablet_weight_g, na.rm = TRUE),
    coated_tablet_weight_q1 = quantile(coated_tablet_weight_g, 0.25, na.rm = TRUE),
    coated_tablet_weight_q3 = quantile(coated_tablet_weight_g, 0.75, na.rm = TRUE),
    active_content_mean = mean(active_content_mg_per_tablet, na.rm = TRUE),
    active_content_sd = sd(active_content_mg_per_tablet, na.rm = TRUE),
    active_content_median = median(active_content_mg_per_tablet, na.rm = TRUE),
    active_content_q1 = quantile(active_content_mg_per_tablet, 0.25, na.rm = TRUE),
    active_content_q3 = quantile(active_content_mg_per_tablet, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

saved <- list()
for (i in seq_len(nrow(variables))) {
  p <- build_issue_plot(
    df = df,
    variable = variables$variable[i],
    y_label = variables$indicator[i],
    stat_label = test_table$statistical_label[i]
  )
  saved[[variables$stem[i]]] <- save_plot(p, variables$stem[i])
}

old_combined <- file.path(
  figures_dir,
  c(
    "07_0p8g_disintegration_issue_weight_content_comparison.pdf",
    "07_0p8g_disintegration_issue_weight_content_comparison.png"
  )
)
unlink(old_combined[file.exists(old_combined)], force = TRUE)

workbook_path <- file.path(tables_dir, "D2_0p8g_disintegration_issue_group_comparison.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "group_summary")
writeData(wb, "group_summary", summary_table)
addWorksheet(wb, "wilcoxon_tests")
writeData(wb, "wilcoxon_tests", test_table)
saveWorkbook(wb, workbook_path, overwrite = TRUE)

note_lines <- c(
  "# 0.8g disintegration-threshold grouping",
  "",
  "- Grouping rule: records with disintegration time > 10 min were assigned to the issue group; records with disintegration time <= 10 min were assigned to the reference group.",
  "- Statistical method: two-sided Wilcoxon rank-sum test.",
  paste0("- Coated tablet weight: ", test_table$statistical_label[test_table$variable == "coated_tablet_weight_g"], "."),
  paste0("- Active content: ", test_table$statistical_label[test_table$variable == "active_content_mg_per_tablet"], "."),
  "",
  "## Outputs",
  "",
  paste0("- Coated tablet weight figure: `", saved[[variables$stem[1]]][["pdf"]], "`."),
  paste0("- Active content figure: `", saved[[variables$stem[2]]][["pdf"]], "`."),
  paste0("- Statistical workbook: `", workbook_path, "`.")
)
writeLines(note_lines, file.path(docs_dir, "D2_0p8g_disintegration_group_single_plots.md"))

cat("Input:", input_file, "\n")
cat("Saved figures:\n")
print(saved)
cat("Saved workbook:", workbook_path, "\n")
