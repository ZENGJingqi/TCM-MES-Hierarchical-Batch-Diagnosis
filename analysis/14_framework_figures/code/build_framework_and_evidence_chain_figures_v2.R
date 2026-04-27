options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(grid)
  library(openxlsx)
  library(readxl)
  library(stringr)
})

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
analysis_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)
figures_dir <- file.path(analysis_dir, "figures")
tables_dir <- file.path(analysis_dir, "tables")
docs_dir <- file.path(analysis_dir, "docs")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

red <- "#A83B2B"
blue <- "#0A5C7A"
grey <- "#8C8C8C"
dark <- "#222222"
cream <- "#F7F3ED"
ice <- "#EEF4F4"
paper <- "#FAFAF8"
line_grey <- "#7E7E7E"
soft_grey <- "#EFEFEF"

joint_file <- file.path(project_dir, "联合建模_成品崩解问题驱动", "tables", "publication_joint_model_tables.xlsx")
time_file <- file.path(project_dir, "时序分析_成品崩解问题", "tables", "成品崩解时序分析结果.xlsx")
graph_file <- file.path(project_dir, "批次证据图模型_成品崩解问题", "tables", "批次证据图评分结果.xlsx")

model_perf <- read_xlsx(joint_file, sheet = "publication_model_performance")
time_window <- read_xlsx(time_file, sheet = "abnormal_window_summary")
graph_top <- read_xlsx(graph_file, sheet = "top_30_evidence_batches")
graph_layer <- read_xlsx(graph_file, sheet = "layer_summary")

get_auc <- function(model_name) {
  model_perf |>
    filter(adjustment == "No month adjustment", model == model_name) |>
    summarise(value = first(AUC)) |>
    pull(value)
}

baseline_auc <- get_auc("Baseline")
full_auc <- get_auc("Full core hierarchy")
abnormal_rate <- time_window |>
  filter(str_detect(window_group, "Abnormal")) |>
  summarise(value = first(issue_rate)) |>
  pull(value)

top_by_layer <- graph_top |>
  group_by(layer) |>
  slice_max(risk_priority_score, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    layer,
    top_batch = upstream_batch,
    issue_link = paste0(issue_n, "/", finished_n),
    top_score = round(risk_priority_score, 1)
  )

top_extract <- top_by_layer |> filter(layer == "Extract-powder quality")
top_chenpi <- top_by_layer |> filter(layer == "Chenpi quality")
top_yam <- top_by_layer |> filter(layer == "Chinese yam powder MES")

render_dual <- function(stem, width, height, draw_fun) {
  cairo_pdf(file.path(figures_dir, paste0(stem, ".pdf")), width = width, height = height, family = "Arial")
  grid.newpage()
  draw_fun()
  dev.off()

  png(file.path(figures_dir, paste0(stem, ".png")), width = width, height = height, units = "in", res = 330, type = "cairo")
  grid.newpage()
  draw_fun()
  dev.off()
}

u <- function(x) unit(x, "npc")

draw_round_box <- function(x, y, w, h, fill = "white", col = dark, lwd = 1.2, radius = 0.018) {
  grid.roundrect(
    x = u(x), y = u(y), width = u(w), height = u(h), r = unit(radius, "snpc"),
    gp = gpar(fill = fill, col = col, lwd = lwd)
  )
}

draw_text <- function(label, x, y, size = 11, fontface = "plain", col = dark, just = "centre", lineheight = 0.92) {
  grid.text(
    label,
    x = u(x), y = u(y), just = just,
    gp = gpar(fontfamily = "Arial", fontsize = size, fontface = fontface, col = col, lineheight = lineheight)
  )
}

draw_card <- function(x, y, w, h, title, subtitle = NULL, fill = "white", border = dark, title_size = 11.5, subtitle_size = 8.8) {
  draw_round_box(x, y, w, h, fill = fill, col = border, lwd = 1.25)
  draw_text(title, x, y + h * 0.16, size = title_size, fontface = "bold")
  if (!is.null(subtitle) && nzchar(subtitle)) {
    draw_text(subtitle, x, y - h * 0.23, size = subtitle_size, lineheight = 0.88)
  }
}

draw_pill <- function(x, y, w, h, label, fill = soft_grey, size = 8.5, col = dark) {
  draw_round_box(x, y, w, h, fill = fill, col = col, lwd = 0.9, radius = 0.015)
  draw_text(label, x, y, size = size, fontface = "bold")
}

draw_arrow <- function(x0, y0, x1, y1, col = line_grey, lwd = 2.0, length = 0.13) {
  grid.segments(
    x0 = u(x0), y0 = u(y0), x1 = u(x1), y1 = u(y1),
    arrow = arrow(length = unit(length, "inches"), type = "closed"),
    gp = gpar(col = col, lwd = lwd, lineend = "round")
  )
}

draw_plain_line <- function(x0, y0, x1, y1, col = line_grey, lwd = 1.5) {
  grid.segments(
    x0 = u(x0), y0 = u(y0), x1 = u(x1), y1 = u(y1),
    gp = gpar(col = col, lwd = lwd, lineend = "round")
  )
}

draw_figure_framework <- function() {
  grid.rect(gp = gpar(fill = "white", col = NA))

  # Phase bands.
  draw_round_box(0.165, 0.80, 0.21, 0.08, fill = paper, col = "#D5D5D5", lwd = 0.8)
  draw_round_box(0.405, 0.80, 0.25, 0.08, fill = paper, col = "#D5D5D5", lwd = 0.8)
  draw_round_box(0.680, 0.80, 0.31, 0.08, fill = paper, col = "#D5D5D5", lwd = 0.8)
  draw_round_box(0.910, 0.80, 0.13, 0.08, fill = paper, col = "#D5D5D5", lwd = 0.8)
  draw_text("Problem definition", 0.165, 0.80, size = 9, fontface = "bold", col = grey)
  draw_text("Evidence construction", 0.405, 0.80, size = 9, fontface = "bold", col = grey)
  draw_text("Integrated inference", 0.680, 0.80, size = 9, fontface = "bold", col = grey)
  draw_text("Actionable output", 0.910, 0.80, size = 9, fontface = "bold", col = grey)

  cards <- tibble::tribble(
    ~x, ~title, ~subtitle, ~fill,
    0.070, "1\nIssue\nidentification", "Disintegration\n>10 min", cream,
    0.213, "2\nBatch\nlinkage", "QC + MES\n+ traceability", ice,
    0.356, "3\nLayer-wise\nscreening", "MES, extract,\nChenpi, yam", cream,
    0.499, "4\nJoint\nmodeling", paste0("AUC ", sprintf("%.3f", baseline_auc), "\nto ", sprintf("%.3f", full_auc)), ice,
    0.642, "5\nTemporal\nadjustment", paste0("Window issue\nrate ", sprintf("%.1f%%", abnormal_rate * 100)), cream,
    0.785, "6\nCausal-informed\ndecomposition", "Path attenuation\n+ robustness", ice,
    0.928, "7\nGraph evidence\nscoring", "Risk-priority\nranking", cream
  )

  for (i in seq_len(nrow(cards))) {
    draw_card(cards$x[i], 0.58, 0.112, 0.23, cards$title[i], cards$subtitle[i], fill = cards$fill[i], title_size = 10.4, subtitle_size = 8.2)
    if (i < nrow(cards)) draw_arrow(cards$x[i] + 0.062, 0.58, cards$x[i + 1] - 0.062, 0.58, lwd = 2.2, length = 0.12)
  }

  # Foundation, boundary and output notes.
  draw_card(0.18, 0.25, 0.27, 0.16, "Real-world data foundation", "5,975 valid batch-level records\nfrom QC testing and MES systems", fill = paper, title_size = 10.3, subtitle_size = 8.5)
  draw_card(0.50, 0.25, 0.27, 0.16, "Diagnostic boundary", "Retrospective issue diagnosis,\nnot direct prospective prediction", fill = paper, title_size = 10.3, subtitle_size = 8.5)
  draw_card(0.82, 0.25, 0.27, 0.16, "Investigation output", "Prioritized upstream batches\nfor focused troubleshooting", fill = paper, title_size = 10.3, subtitle_size = 8.5)

  draw_plain_line(0.18, 0.335, 0.22, 0.455, col = "#B0B0B0", lwd = 1.4)
  draw_plain_line(0.50, 0.335, 0.51, 0.455, col = "#B0B0B0", lwd = 1.4)
  draw_plain_line(0.82, 0.335, 0.945, 0.455, col = "#B0B0B0", lwd = 1.4)
}

draw_figure_evidence_chain <- function() {
  grid.rect(gp = gpar(fill = "white", col = NA))

  # Right-to-left finished-product issue trace.
  draw_card(0.880, 0.56, 0.155, 0.16, "Finished-product\nquality testing", "3,728 records\nsentinel issue: >10 min", fill = cream, title_size = 10.5, subtitle_size = 8.2)
  draw_card(0.695, 0.56, 0.155, 0.16, "Tablet MES\nproduction records", "1,243 records\n908 linked to QC", fill = ice, title_size = 10.5, subtitle_size = 8.2)
  draw_card(0.510, 0.56, 0.175, 0.16, "Jianwei Xiaoshi\nextract-powder testing", "618 records\n273 linked forward", fill = cream, title_size = 10.0, subtitle_size = 8.2)
  draw_card(0.380, 0.79, 0.145, 0.14, "Chenpi\nquality testing", "44 records\n41 linked forward", fill = cream, title_size = 10.0, subtitle_size = 7.9)
  draw_card(0.380, 0.31, 0.175, 0.14, "Chinese yam powder\nMES records", "342 records\n176 linked forward", fill = ice, title_size = 10.0, subtitle_size = 7.9)

  draw_arrow(0.802, 0.56, 0.773, 0.56, lwd = 2.5)
  draw_arrow(0.618, 0.56, 0.598, 0.56, lwd = 2.5)
  draw_arrow(0.468, 0.64, 0.415, 0.735, lwd = 2.2)
  draw_arrow(0.468, 0.48, 0.415, 0.365, lwd = 2.2)
  draw_text("issue identification", 0.790, 0.70, size = 9, col = grey)
  draw_text("batch linkage", 0.605, 0.70, size = 9, col = grey)
  draw_text("trace upstream", 0.427, 0.665, size = 8.5, col = grey)
  draw_text("trace upstream", 0.427, 0.455, size = 8.5, col = grey)

  # Evidence scoring engine.
  draw_card(0.170, 0.56, 0.175, 0.18, "Graph-based batch\nevidence scoring", "time-adjusted residual\n+ graph propagation\n+ trace support\n+ quality/process deviation", fill = paper, title_size = 9.8, subtitle_size = 7.5)
  draw_arrow(0.438, 0.79, 0.265, 0.63, lwd = 2.0)
  draw_arrow(0.422, 0.56, 0.265, 0.56, lwd = 2.0)
  draw_arrow(0.438, 0.31, 0.265, 0.49, lwd = 2.0)

  # Risk-priority output.
  draw_round_box(0.150, 0.205, 0.240, 0.24, fill = cream, col = dark, lwd = 1.25)
  draw_text("Risk-priority output", 0.150, 0.290, size = 10.0, fontface = "bold")
  draw_text("Ranked upstream batches for focused investigation", 0.150, 0.255, size = 7.4)
  draw_text(
    paste0(
      "1  Extract-powder  ", top_extract$top_batch, "  (", top_extract$issue_link, "), score ", top_extract$top_score, "\n",
      "2  Chinese yam powder  ", top_yam$top_batch, "  (", top_yam$issue_link, "), score ", top_yam$top_score, "\n",
      "3  Chenpi  ", top_chenpi$top_batch, "  (", top_chenpi$issue_link, "), score ", top_chenpi$top_score
    ),
    0.150, 0.175, size = 6.9, lineheight = 1.05
  )
  draw_text("Values in parentheses show issue/linked records.", 0.150, 0.095, size = 6.5, col = grey)
  draw_arrow(0.170, 0.465, 0.150, 0.325, lwd = 2.0, col = blue)
}

render_dual("Figure_framework_issue_driven_hierarchical_batch_diagnosis", 13.2, 5.6, draw_figure_framework)
render_dual("Figure_evidence_chain_finished_issue_to_upstream_risk_priority", 13.2, 7.0, draw_figure_evidence_chain)

figure_source <- tibble::tribble(
  ~Figure, ~Design_principle, ~Manuscript_role,
  "Figure_framework_issue_driven_hierarchical_batch_diagnosis", "Seven numbered modules grouped into problem definition, evidence construction, integrated inference, and actionable output.", "Defines the overall MES-enabled issue-driven hierarchical batch diagnosis framework.",
  "Figure_evidence_chain_finished_issue_to_upstream_risk_priority", "Right-to-left trace-back chain with a separated evidence-scoring engine and risk-priority output.", "Shows how the sentinel finished-product issue is converted into upstream batch-level investigation priorities."
)

write.xlsx(
  list(
    figure_design = figure_source,
    top_risk_priority_nodes = top_by_layer,
    graph_layer_summary = graph_layer
  ),
  file.path(tables_dir, "framework_and_evidence_chain_figure_source_v2.xlsx"),
  overwrite = TRUE
)

message("Redesigned framework and evidence-chain figures completed.")
