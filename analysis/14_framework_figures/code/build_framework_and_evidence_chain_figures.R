options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
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
light_grey <- "#F2F2F2"
warm_bg <- "#F7F3ED"
cool_bg <- "#EEF4F4"
mid_bg <- "#F1F1F1"

theme_set(theme_void(base_family = "Arial", base_size = 18))

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330, bg = "white")
}

wrap_text <- function(x, width = 24) stringr::str_wrap(x, width = width)

add_arrow <- function(p, x, y, xend, yend, colour = grey, linewidth = 1.2) {
  p + annotate(
    "segment",
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    colour = colour,
    linewidth = linewidth,
    lineend = "round",
    arrow = arrow(length = unit(0.20, "inches"), type = "closed")
  )
}

draw_box <- function(df, size_label = 4.6, size_detail = 3.7) {
  list(
    geom_label(
      data = df,
      aes(x = x, y = y, label = label, fill = fill),
      family = "Arial",
      fontface = "bold",
      size = size_label,
      colour = "black",
      linewidth = 0.45,
      label.padding = unit(0.28, "lines"),
      lineheight = 0.95,
      show.legend = FALSE
    ),
    geom_text(
      data = df,
      aes(x = x, y = y - detail_offset, label = detail),
      family = "Arial",
      size = size_detail,
      colour = "black",
      lineheight = 0.95
    )
  )
}

joint_file <- file.path(project_dir, "联合建模_成品崩解问题驱动", "tables", "publication_joint_model_tables.xlsx")
time_file <- file.path(project_dir, "时序分析_成品崩解问题", "tables", "成品崩解时序分析结果.xlsx")
graph_file <- file.path(project_dir, "批次证据图模型_成品崩解问题", "tables", "批次证据图评分结果.xlsx")

model_perf <- read_xlsx(joint_file, sheet = "publication_model_performance")
time_window <- read_xlsx(time_file, sheet = "abnormal_window_summary")
graph_layer <- read_xlsx(graph_file, sheet = "layer_summary")
graph_top <- read_xlsx(graph_file, sheet = "top_30_evidence_batches")

full_auc <- model_perf |>
  filter(adjustment == "No month adjustment", model == "Full core hierarchy") |>
  summarise(value = first(AUC)) |>
  pull(value)
mes_auc <- model_perf |>
  filter(adjustment == "No month adjustment", model == "Baseline + MES") |>
  summarise(value = first(AUC)) |>
  pull(value)
baseline_auc <- model_perf |>
  filter(adjustment == "No month adjustment", model == "Baseline") |>
  summarise(value = first(AUC)) |>
  pull(value)

abnormal_rate <- time_window |>
  filter(str_detect(window_group, "Abnormal")) |>
  summarise(value = first(issue_rate)) |>
  pull(value)
other_rate <- time_window |>
  filter(window_group == "Other months") |>
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
    top_score = round(risk_priority_score, 1),
    support = support_flag
  )

framework_steps <- tibble::tribble(
  ~step, ~x, ~y, ~label, ~detail, ~fill, ~detail_offset,
  1, 1.0, 4.6, "Issue\nidentification", "Finished-product\ndisintegration >10 min", warm_bg, 0.82,
  2, 2.6, 4.6, "Batch\nlinkage", "QC, MES and\ntraceability identifiers", cool_bg, 0.82,
  3, 4.2, 4.6, "Layer-wise\nscreening", "Finished MES,\nextract powder,\nChenpi, yam powder", warm_bg, 0.82,
  4, 5.8, 4.6, "Joint\nmodeling", paste0("Layered diagnostic gain\nAUC ", sprintf("%.3f", baseline_auc), " to ", sprintf("%.3f", full_auc)), cool_bg, 0.82,
  5, 7.4, 4.6, "Temporal\nadjustment", paste0("Abnormal-window\nissue rate ", sprintf("%.1f%%", abnormal_rate * 100)), warm_bg, 0.82,
  6, 9.0, 4.6, "Causal-informed\ndecomposition", "Path attenuation and\nbatch-level robustness", cool_bg, 0.82,
  7, 10.6, 4.6, "Graph evidence\nscoring", "Risk-priority ranking\nfor upstream batches", warm_bg, 0.82
)

framework_bottom <- tibble::tribble(
  ~x, ~y, ~label, ~detail, ~fill, ~detail_offset,
  2.0, 2.15, "Real-world data foundation", "5,975 valid batch-level records\nfrom QC testing and MES systems", mid_bg, 0.52,
  5.8, 2.15, "Diagnostic model boundary", "Retrospective issue diagnosis,\nnot direct prospective prediction", mid_bg, 0.52,
  9.6, 2.15, "Actionable output", "Prioritized upstream batches\nfor focused investigation", mid_bg, 0.52
)

framework_plot <- ggplot() +
  annotate("rect", xmin = 0.35, xmax = 11.25, ymin = 1.25, ymax = 5.55, fill = "white", colour = NA) +
  draw_box(framework_steps, size_label = 4.15, size_detail = 3.05) +
  draw_box(framework_bottom, size_label = 4.0, size_detail = 3.15) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0.25, 11.35), ylim = c(1.35, 5.15), clip = "off") +
  theme(
    plot.margin = margin(18, 18, 18, 18),
    panel.background = element_rect(fill = "white", colour = NA)
  )

for (i in seq_len(nrow(framework_steps) - 1)) {
  framework_plot <- add_arrow(
    framework_plot,
    framework_steps$x[i] + 0.48,
    framework_steps$y[i],
    framework_steps$x[i + 1] - 0.48,
    framework_steps$y[i + 1],
    colour = grey,
    linewidth = 1.25
  )
}

framework_plot <- framework_plot +
  annotate("segment", x = 2.0, y = 2.55, xend = 2.6, yend = 3.88, colour = grey, linewidth = 0.9, arrow = arrow(length = unit(0.16, "inches"), type = "closed")) +
  annotate("segment", x = 5.8, y = 2.55, xend = 5.8, yend = 3.88, colour = grey, linewidth = 0.9, arrow = arrow(length = unit(0.16, "inches"), type = "closed")) +
  annotate("segment", x = 9.6, y = 2.55, xend = 10.6, yend = 3.88, colour = grey, linewidth = 0.9, arrow = arrow(length = unit(0.16, "inches"), type = "closed"))

save_plot_dual(framework_plot, "Figure_framework_issue_driven_hierarchical_batch_diagnosis", 13.2, 5.2)

entity_nodes <- tibble::tribble(
  ~id, ~x, ~y, ~label, ~detail, ~fill, ~detail_offset,
  "finished_quality", 10.35, 3.35, "Finished-product\nquality testing", "3,728 records\nsentinel issue: >10 min", warm_bg, 0.72,
  "tablet_mes", 8.25, 3.35, "Tablet MES\nproduction records", "1,243 records\n908 linked to QC", cool_bg, 0.72,
  "extract", 6.15, 3.35, "Jianwei Xiaoshi\nextract-powder testing", "618 records\n273 linked forward", warm_bg, 0.72,
  "chenpi", 5.15, 4.82, "Chenpi\nquality testing", "44 records\n41 linked forward", warm_bg, 0.72,
  "yam", 5.15, 1.78, "Chinese yam powder\nMES records", "342 records\n176 linked forward", cool_bg, 0.72
)

score_nodes <- tibble::tribble(
  ~x, ~y, ~label, ~detail, ~fill, ~detail_offset,
  2.35, 3.35, "Graph-based batch\nevidence scoring", "time-adjusted residual\n+ graph propagation\n+ trace support\n+ quality/process deviation", mid_bg, 0.86,
  2.35, 1.62, "Risk-priority\noutput", "ranked upstream batches\nfor focused investigation", warm_bg, 0.65
)

top_extract <- top_by_layer |> filter(layer == "Extract-powder quality")
top_chenpi <- top_by_layer |> filter(layer == "Chenpi quality")
top_yam <- top_by_layer |> filter(layer == "Chinese yam powder MES")

risk_table <- tibble::tribble(
  ~x, ~y, ~layer_label, ~batch_label, ~score_label,
  0.82, 4.30, "Extract-powder", paste0(top_extract$top_batch, " (", top_extract$issue_link, ")"), paste0("score ", top_extract$top_score),
  0.82, 3.35, "Chinese yam powder", paste0(top_yam$top_batch, " (", top_yam$issue_link, ")"), paste0("score ", top_yam$top_score),
  0.82, 2.40, "Chenpi", paste0(top_chenpi$top_batch, " (", top_chenpi$issue_link, ")"), paste0("score ", top_chenpi$top_score)
)

evidence_plot <- ggplot() +
  annotate("rect", xmin = 0.18, xmax = 11.1, ymin = 1.05, ymax = 5.45, fill = "white", colour = NA) +
  draw_box(entity_nodes, size_label = 4.0, size_detail = 3.1) +
  draw_box(score_nodes, size_label = 4.0, size_detail = 3.05) +
  geom_label(
    data = risk_table,
    aes(x = x, y = y, label = paste0(layer_label, "\n", batch_label, "\n", score_label)),
    family = "Arial",
    size = 3.05,
    fontface = "bold",
    colour = "black",
    fill = "white",
    linewidth = 0.35,
    label.padding = unit(0.20, "lines"),
    lineheight = 0.95
  ) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0.15, 11.15), ylim = c(1.0, 5.62), clip = "off") +
  theme(
    plot.margin = margin(18, 18, 18, 18),
    panel.background = element_rect(fill = "white", colour = NA)
  )

evidence_arrows <- list(
  c(9.88, 3.35, 8.78, 3.35),
  c(7.72, 3.35, 6.60, 3.35),
  c(5.88, 3.72, 5.35, 4.38),
  c(5.88, 2.98, 5.35, 2.24),
  c(4.65, 4.55, 2.78, 3.72),
  c(5.52, 3.35, 2.82, 3.35),
  c(4.65, 2.05, 2.78, 3.00),
  c(2.35, 2.70, 2.35, 2.12)
)

for (a in evidence_arrows) {
  evidence_plot <- add_arrow(evidence_plot, a[1], a[2], a[3], a[4], colour = grey, linewidth = 1.15)
}

evidence_plot <- evidence_plot +
  draw_box(entity_nodes, size_label = 4.0, size_detail = 3.1) +
  draw_box(score_nodes, size_label = 4.0, size_detail = 3.05) +
  geom_label(
    data = risk_table,
    aes(x = x, y = y, label = paste0(layer_label, "\n", batch_label, "\n", score_label)),
    family = "Arial",
    size = 3.05,
    fontface = "bold",
    colour = "black",
    fill = "white",
    linewidth = 0.35,
    label.padding = unit(0.20, "lines"),
    lineheight = 0.95
  ) +
  annotate("text", x = 9.35, y = 4.20, label = "issue identification", family = "Arial", size = 3.5, colour = dark) +
  annotate("text", x = 7.20, y = 4.02, label = "batch linkage", family = "Arial", size = 3.5, colour = dark) +
  annotate("text", x = 4.05, y = 4.18, label = "trace upstream", family = "Arial", size = 3.5, colour = dark) +
  annotate("text", x = 4.05, y = 2.48, label = "trace upstream", family = "Arial", size = 3.5, colour = dark) +
  annotate("text", x = 0.9, y = 5.05, label = "Top risk-priority nodes", family = "Arial", fontface = "bold", size = 3.6, colour = dark)

save_plot_dual(evidence_plot, "Figure_evidence_chain_finished_issue_to_upstream_risk_priority", 13.2, 6.7)

framework_table <- tibble::tribble(
  ~Section, ~Purpose, ~Main_output, ~Recommended_manuscript_role,
  "Issue identification", "Define the finished-product quality problem driving the analysis.", "Disintegration time >10 min as sentinel finished-product issue.", "Result 1",
  "Batch linkage", "Connect finished-product QC, MES, intermediate testing, raw-material testing, and process-material MES records.", "Layer-wise linked/traced batch evidence.", "Result 1 / Figure 1",
  "Layer-wise screening", "Identify candidate signals within each manufacturing layer.", "FDR-screened process and quality attributes.", "Result 2",
  "Joint modeling", "Quantify diagnostic gain from integrating MES and upstream data.", paste0("AUC improved from ", sprintf("%.3f", baseline_auc), " to ", sprintf("%.3f", full_auc), "."), "Result 3",
  "Temporal adjustment", "Separate abnormal production-window effects from material/source attribution.", paste0("Abnormal-window issue rate ", sprintf("%.1f%%", abnormal_rate * 100), " versus other-month rate ", sprintf("%.1f%%", other_rate * 100), "."), "Result 4",
  "Causal-informed decomposition", "Evaluate whether upstream source signals remain after pathway and time adjustment.", "Temporal-window signal robust; direct Chenpi-source causality not supported.", "Result 5",
  "Graph evidence scoring", "Convert linked evidence into actionable upstream batch risk-priority scores.", "Graph-based risk-priority ranking across extract powder, Chenpi, and Chinese yam powder.", "Result 6"
)

write.xlsx(
  list(
    framework_sections = framework_table,
    graph_top_by_layer = top_by_layer,
    graph_layer_summary = graph_layer
  ),
  file.path(tables_dir, "framework_and_evidence_chain_figure_source.xlsx"),
  overwrite = TRUE
)

md_lines <- c(
  "# Paper framework fixed for the current manuscript",
  "",
  "## Central logic",
  "",
  "The manuscript is organized as an issue-driven hierarchical batch diagnosis framework. The diagnostic entry point is the finished-product disintegration issue, not untargeted quality mining. MES, QC and traceability records are then linked layer by layer to screen candidate signals, quantify integrated diagnostic gain, adjust for abnormal temporal windows, decompose causal-informed paths, and finally rank upstream batches through a graph-based evidence scoring module.",
  "",
  "## Main figure design",
  "",
  "Figure_framework_issue_driven_hierarchical_batch_diagnosis: the methodological framework, from issue identification to graph evidence scoring.",
  "",
  "Figure_evidence_chain_finished_issue_to_upstream_risk_priority: the applied evidence chain, from finished-product disintegration issue to prioritized upstream material/process batches.",
  "",
  "## Writing boundary",
  "",
  "The framework supports retrospective issue diagnosis and batch investigation prioritization. It should not be written as direct proof that one raw material origin or supplier caused the finished-product issue."
)
writeLines(md_lines, file.path(docs_dir, "论文框架与证据链主图说明.md"), useBytes = TRUE)

message("Framework and evidence-chain figures completed.")
