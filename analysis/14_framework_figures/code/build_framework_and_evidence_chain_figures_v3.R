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
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

red <- "#A83B2B"
blue <- "#0A5C7A"
grey <- "#8C8C8C"
dark <- "#222222"
cream <- "#F7F3ED"
ice <- "#EEF4F4"
paper <- "#FAFAF8"
line_grey <- "#777777"
stage_fill <- "#F3F3F0"

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

u <- function(x) unit(x, "npc")

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

draw_round_box <- function(x, y, w, h, fill = "white", col = dark, lwd = 1.1, radius = 0.018, lty = 1) {
  grid.roundrect(
    x = u(x), y = u(y), width = u(w), height = u(h), r = unit(radius, "snpc"),
    gp = gpar(fill = fill, col = col, lwd = lwd, lty = lty)
  )
}

draw_text <- function(label, x, y, size = 11, fontface = "plain", col = dark, just = "centre", lineheight = 0.95) {
  grid.text(
    label,
    x = u(x), y = u(y), just = just,
    gp = gpar(fontfamily = "Arial", fontsize = size, fontface = fontface, col = col, lineheight = lineheight)
  )
}

draw_card <- function(x, y, w, h, title, subtitle = NULL, fill = "white", title_size = 13, subtitle_size = 10) {
  draw_round_box(x, y, w, h, fill = fill, col = dark, lwd = 1.35)
  draw_text(title, x, y + h * 0.14, size = title_size, fontface = "bold", lineheight = 0.9)
  if (!is.null(subtitle) && nzchar(subtitle)) {
    draw_text(subtitle, x, y - h * 0.25, size = subtitle_size, lineheight = 0.92)
  }
}

draw_arrow <- function(x0, y0, x1, y1, col = line_grey, lwd = 2.0, length = 0.13) {
  grid.segments(
    x0 = u(x0), y0 = u(y0), x1 = u(x1), y1 = u(y1),
    arrow = arrow(length = unit(length, "inches"), type = "closed"),
    gp = gpar(col = col, lwd = lwd, lineend = "round")
  )
}

draw_stage <- function(x, y, w, label) {
  draw_round_box(x, y, w, 0.052, fill = stage_fill, col = "#D6D6D6", lwd = 0.85, radius = 0.010)
  draw_text(label, x, y, size = 10.5, fontface = "bold", col = grey)
}

draw_framework <- function() {
  grid.rect(gp = gpar(fill = "white", col = NA))

  top_y <- 0.695
  bottom_y <- 0.330
  card_w <- 0.205
  card_h <- 0.205

  draw_card(0.155, top_y, card_w, card_h, "1  Sentinel issue\nidentification", "Finished-product\ndisintegration >10 min", fill = cream)
  draw_card(0.405, top_y, card_w, card_h, "2  Batch\nlinkage", "QC, MES and traceability\nbatch identifiers", fill = ice)
  draw_card(0.655, top_y, card_w, card_h, "3  Layer-wise\nevidence screening", "Tablet MES, extract-powder,\nChenpi and Chinese yam powder", fill = cream, title_size = 12.5, subtitle_size = 9.4)

  draw_card(0.215, bottom_y, card_w, card_h, "4  Hierarchical\njoint modeling", paste0("Layered diagnostic gain\nAUC ", sprintf("%.3f", baseline_auc), " to ", sprintf("%.3f", full_auc)), fill = ice, title_size = 12.5)
  draw_card(0.465, bottom_y, card_w, card_h, "5  Temporal-window\nadjustment", paste0("Abnormal-window\nissue rate ", sprintf("%.1f%%", abnormal_rate * 100)), fill = cream, title_size = 12.5)
  draw_card(0.715, bottom_y, card_w, card_h, "6  Causal-informed\npath decomposition", "Path attenuation and\nbatch-level robustness", fill = ice, title_size = 12.0)
  draw_card(0.905, bottom_y, 0.165, card_h, "7  Graph-based\nbatch evidence\nscoring", "Upstream batch\nrisk-priority ranking", fill = cream, title_size = 11.0, subtitle_size = 9.1)

  draw_arrow(0.260, top_y, 0.302, top_y, lwd = 2.1)
  draw_arrow(0.510, top_y, 0.552, top_y, lwd = 2.1)
  grid.segments(x0 = u(0.655), y0 = u(top_y - 0.115), x1 = u(0.655), y1 = u(0.505), gp = gpar(col = line_grey, lwd = 2.1, lineend = "round"))
  grid.segments(x0 = u(0.655), y0 = u(0.505), x1 = u(0.215), y1 = u(0.505), gp = gpar(col = line_grey, lwd = 2.1, lineend = "round"))
  draw_arrow(0.215, 0.505, 0.215, bottom_y + 0.115, lwd = 2.1)
  draw_arrow(0.320, bottom_y, 0.362, bottom_y, lwd = 2.1)
  draw_arrow(0.570, bottom_y, 0.612, bottom_y, lwd = 2.1)
  draw_arrow(0.820, bottom_y, 0.835, bottom_y, lwd = 2.1)

  draw_round_box(0.500, 0.090, 0.78, 0.105, fill = paper, col = "#D0D0D0", lwd = 0.9)
  draw_text(
    "Framework boundary: retrospective issue diagnosis and upstream investigation prioritization, not direct proof of material-origin causality or prospective prediction.",
    0.500, 0.090, size = 9.8, col = grey
  )
}

draw_evidence_chain <- function() {
  grid.rect(gp = gpar(fill = "white", col = NA))

  # Clean trace-back backbone.
  y_main <- 0.650
  draw_card(0.875, y_main, 0.165, 0.155, "Finished-product\nquality testing", "3,728 records\nsentinel issue: >10 min", fill = cream, title_size = 11.0, subtitle_size = 8.6)
  draw_card(0.660, y_main, 0.165, 0.155, "Tablet MES\nproduction records", "1,243 records\n908 linked to QC", fill = ice, title_size = 11.0, subtitle_size = 8.6)
  draw_card(0.445, y_main, 0.185, 0.155, "Jianwei Xiaoshi\nextract-powder testing", "618 records\n273 linked forward", fill = cream, title_size = 10.5, subtitle_size = 8.6)
  draw_card(0.230, y_main, 0.155, 0.155, "Chenpi\nquality testing", "44 records\n41 linked forward", fill = cream, title_size = 10.8, subtitle_size = 8.4)

  draw_arrow(0.790, y_main, 0.745, y_main, lwd = 2.4)
  draw_arrow(0.575, y_main, 0.535, y_main, lwd = 2.4)
  draw_arrow(0.352, y_main, 0.310, y_main, lwd = 2.4)

  draw_text("issue identification", 0.875, 0.775, size = 9.2, fontface = "bold", col = grey)
  draw_text("batch linkage", 0.660, 0.775, size = 9.2, fontface = "bold", col = grey)
  draw_text("trace upstream", 0.445, 0.775, size = 9.2, fontface = "bold", col = grey)

  # Branch for Chinese yam powder, kept orthogonal and separate from the backbone.
  draw_card(0.660, 0.435, 0.185, 0.135, "Chinese yam powder\nMES records", "342 records\n176 linked forward", fill = ice, title_size = 10.2, subtitle_size = 8.2)
  grid.segments(x0 = u(0.660), y0 = u(0.572), x1 = u(0.660), y1 = u(0.510), gp = gpar(col = line_grey, lwd = 2.0, lineend = "round"))
  draw_arrow(0.660, 0.510, 0.660, 0.500, lwd = 2.0)
  draw_text("process-material linkage", 0.735, 0.540, size = 8.5, col = grey, just = "left")

  # Upstream evidence envelope and scoring.
  draw_round_box(0.445, 0.580, 0.620, 0.500, fill = NA, col = "#D0D0D0", lwd = 1.0, lty = "dashed")
  draw_text("Batch-linked upstream evidence", 0.445, 0.855, size = 10.5, fontface = "bold", col = grey)

  draw_card(0.445, 0.200, 0.235, 0.175, "Graph-based batch\nevidence scoring", "time-adjusted residual\n+ graph propagation\n+ trace support\n+ quality/process deviation", fill = paper, title_size = 10.7, subtitle_size = 8.0)
  draw_arrow(0.445, 0.330, 0.445, 0.290, lwd = 2.1)

  draw_round_box(0.765, 0.200, 0.350, 0.245, fill = cream, col = dark, lwd = 1.35)
  draw_text("Risk-priority output", 0.765, 0.280, size = 11.2, fontface = "bold")
  draw_text("Ranked upstream batches for focused investigation", 0.765, 0.245, size = 8.2)
  draw_text(
    paste0(
      "1  Extract-powder  ", top_extract$top_batch, "  (", top_extract$issue_link, "), score ", top_extract$top_score, "\n",
      "2  Chinese yam powder  ", top_yam$top_batch, "  (", top_yam$issue_link, "), score ", top_yam$top_score, "\n",
      "3  Chenpi  ", top_chenpi$top_batch, "  (", top_chenpi$issue_link, "), score ", top_chenpi$top_score
    ),
    0.765, 0.170, size = 7.7, lineheight = 1.08
  )
  draw_text("Values in parentheses show issue/linked records.", 0.765, 0.085, size = 7.0, col = grey)
  draw_arrow(0.565, 0.200, 0.595, 0.200, col = blue, lwd = 2.2)
}

render_dual("Figure_framework_issue_driven_hierarchical_batch_diagnosis", 13.8, 7.2, draw_framework)
render_dual("Figure_evidence_chain_finished_issue_to_upstream_risk_priority", 13.8, 7.6, draw_evidence_chain)

write.xlsx(
  list(
    top_risk_priority_nodes = top_by_layer,
    graph_layer_summary = graph_layer,
    figure_notes = tibble::tribble(
      ~Figure, ~Design_note,
      "Framework", "Two-row seven-module framework with larger professional terminology and a clear diagnostic boundary statement.",
      "Evidence chain", "Orthogonal trace-back backbone with separated upstream evidence envelope, scoring module, and risk-priority output."
    )
  ),
  file.path(tables_dir, "framework_and_evidence_chain_figure_source_v3.xlsx"),
  overwrite = TRUE
)

message("Version 3 manuscript framework figures completed.")
