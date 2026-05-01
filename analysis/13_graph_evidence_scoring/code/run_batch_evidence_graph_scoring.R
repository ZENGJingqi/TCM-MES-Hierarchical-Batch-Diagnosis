options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(flextable)
  library(ggplot2)
  library(officer)
  library(openxlsx)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
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

theme_set(theme_bw(base_family = "Arial", base_size = 18))
theme_update(
  plot.title = element_blank(),
  axis.title = element_text(size = 22, colour = "black"),
  axis.text = element_text(size = 18, colour = "black"),
  strip.text = element_text(size = 18, face = "bold", colour = "black"),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", linewidth = 0.8),
  plot.margin = margin(12, 18, 12, 18)
)

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330, bg = "white")
}

scale_01 <- function(x) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0, length(x)))
  (x - rng[1]) / diff(rng)
}

robust_z <- function(x) {
  x <- as.numeric(x)
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(mad_val) || mad_val == 0) {
    sd_val <- sd(x, na.rm = TRUE)
    if (!is.finite(sd_val) || sd_val == 0) return(rep(0, length(x)))
    return((x - mean(x, na.rm = TRUE)) / sd_val)
  }
  (x - med) / mad_val
}

format_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)

confounding_dir <- file.path(project_dir, "因果建模_成品崩解问题")
confounding_file <- file.path(confounding_dir, "tables", "因果批次分解_第一版结果.xlsx")
robust_file <- file.path(confounding_dir, "tables", "因果稳健性与路径衰减分析.xlsx")

finished_matrix <- read_xlsx(confounding_file, sheet = "confounding_finished_matrix")
extract_batch <- read_xlsx(confounding_file, sheet = "extract_batch_summary")
chenpi_batch <- read_xlsx(confounding_file, sheet = "chenpi_batch_summary")
yam_batch <- read_xlsx(confounding_file, sheet = "yam_batch_summary")
bootstrap_effects <- read_xlsx(robust_file, sheet = "bootstrap_batch_effects")

finished_rates <- finished_matrix |>
  mutate(disintegration_issue = as.integer(disintegration_issue), abnormal_window = as.integer(abnormal_window)) |>
  summarise(
    overall_issue_rate = mean(disintegration_issue, na.rm = TRUE),
    normal_window_issue_rate = mean(disintegration_issue[abnormal_window == 0], na.rm = TRUE),
    abnormal_window_issue_rate = mean(disintegration_issue[abnormal_window == 1], na.rm = TRUE),
    finished_n = n(),
    issue_n = sum(disintegration_issue, na.rm = TRUE)
  )

make_evidence_nodes <- function(df, layer_name, batch_type, quality_vars) {
  quality_df <- df[, quality_vars, drop = FALSE] |>
    mutate(across(everything(), as.numeric))
  quality_burden <- if (length(quality_vars) == 0) {
    rep(0, nrow(df))
  } else {
    z <- quality_df |>
      mutate(across(everything(), ~abs(robust_z(.x))))
    rowMeans(as.data.frame(z), na.rm = TRUE)
  }
  quality_burden[!is.finite(quality_burden)] <- 0

  df |>
    mutate(
      layer = layer_name,
      batch_type = batch_type,
      upstream_batch = as.character(upstream_batch),
      issue_rate = as.numeric(issue_rate),
      abnormal_window_frac = as.numeric(abnormal_window_frac),
      finished_n = as.numeric(finished_n),
      issue_n = as.numeric(issue_n),
      expected_issue_rate_from_time = abnormal_window_frac * finished_rates$abnormal_window_issue_rate +
        (1 - abnormal_window_frac) * finished_rates$normal_window_issue_rate,
      time_adjusted_issue_residual = issue_rate - expected_issue_rate_from_time,
      positive_time_adjusted_residual = pmax(time_adjusted_issue_residual, 0),
      quality_deviation_burden = quality_burden
    )
}

extract_nodes <- make_evidence_nodes(
  extract_batch,
  "Extract-powder quality",
  "Jianwei Xiaoshi extract-powder batch",
  c("extract_moisture_pct", "extract_total_ash_pct", "extract_extract_pct", "extract_hesperidin_mg_g")
)

chenpi_nodes <- make_evidence_nodes(
  chenpi_batch,
  "Chenpi quality",
  "Chenpi batch",
  c("chenpi_moisture_pct", "chenpi_hesperidin_pct", "chenpi_impurities_pct")
)

yam_nodes <- make_evidence_nodes(
  yam_batch,
  "Chinese yam powder MES",
  "Chinese yam powder batch",
  c("yam_rejected_material_rate_pct", "yam_process_moisture_pct", "yam_through_100_mesh_pct", "yam_through_120_mesh_pct", "yam_yield_pct", "yam_mass_balance_pct")
)

all_nodes_raw <- bind_rows(extract_nodes, chenpi_nodes, yam_nodes)

all_nodes <- all_nodes_raw |>
  mutate(
    trace_support_score = scale_01(log1p(finished_n)),
    time_adjusted_residual_score = scale_01(positive_time_adjusted_residual),
    edge_weighted_residual = log1p(finished_n) * positive_time_adjusted_residual,
    graph_propagation_score = scale_01(edge_weighted_residual),
    quality_deviation_score = scale_01(quality_deviation_burden),
    graph_evidence_score = 100 * (
      0.40 * time_adjusted_residual_score +
        0.35 * trace_support_score +
        0.25 * quality_deviation_score
    ),
    graph_evidence_score = round(graph_evidence_score, 2),
    risk_priority_score = 100 * (
      0.35 * time_adjusted_residual_score +
        0.45 * graph_propagation_score +
        0.10 * trace_support_score +
        0.10 * quality_deviation_score
    ),
    risk_priority_score = round(risk_priority_score, 2),
    issue_enrichment_vs_overall = issue_rate / finished_rates$overall_issue_rate,
    evidence_tier = case_when(
      graph_evidence_score >= 75 & finished_n >= 5 ~ "High",
      graph_evidence_score >= 45 & finished_n >= 5 ~ "Moderate",
      TRUE ~ "Low"
    ),
    risk_tier = case_when(
      positive_time_adjusted_residual <= 0 ~ "Low",
      risk_priority_score >= 70 & finished_n >= 5 ~ "High",
      risk_priority_score >= 40 ~ "Moderate",
      TRUE ~ "Low"
    ),
    support_flag = case_when(
      finished_n < 5 ~ "Small downstream support",
      finished_n < 10 ~ "Limited downstream support",
      TRUE ~ "Adequate downstream support"
    ),
    interpretation_class = case_when(
      positive_time_adjusted_residual > 0.05 & abnormal_window_frac < 0.50 ~ "Time-adjusted residual signal",
      positive_time_adjusted_residual > 0.05 & abnormal_window_frac >= 0.50 ~ "Residual signal with temporal exposure",
      abnormal_window_frac >= 0.50 ~ "Temporal-window dominated",
      TRUE ~ "Low residual evidence"
    )
  ) |>
  arrange(desc(risk_priority_score), desc(finished_n))

layer_summary <- all_nodes |>
  group_by(layer) |>
  summarise(
    upstream_batch_n = n(),
    downstream_finished_n = sum(finished_n, na.rm = TRUE),
    downstream_issue_n = sum(issue_n, na.rm = TRUE),
    weighted_issue_rate = downstream_issue_n / downstream_finished_n,
    median_downstream_finished = median(finished_n, na.rm = TRUE),
    max_downstream_finished = max(finished_n, na.rm = TRUE),
    median_abnormal_window_frac = median(abnormal_window_frac, na.rm = TRUE),
    top_graph_evidence_score = max(graph_evidence_score, na.rm = TRUE),
    top_risk_priority_score = max(risk_priority_score, na.rm = TRUE),
    high_evidence_batch_n = sum(evidence_tier == "High"),
    moderate_evidence_batch_n = sum(evidence_tier == "Moderate"),
    high_risk_priority_batch_n = sum(risk_tier == "High"),
    moderate_risk_priority_batch_n = sum(risk_tier == "Moderate"),
    .groups = "drop"
  ) |>
  mutate(
    weighted_issue_rate = round(weighted_issue_rate, 4),
    median_abnormal_window_frac = round(median_abnormal_window_frac, 4),
    top_graph_evidence_score = round(top_graph_evidence_score, 2),
    top_risk_priority_score = round(top_risk_priority_score, 2)
  )

top_nodes <- all_nodes |>
  group_by(layer) |>
  slice_max(risk_priority_score, n = 8, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    layer = factor(layer, levels = c("Extract-powder quality", "Chenpi quality", "Chinese yam powder MES")),
    batch_label = paste0(upstream_batch, "  (", issue_n, "/", finished_n, ")"),
    batch_label = factor(batch_label, levels = rev(unique(batch_label)))
  )

graph_plot <- ggplot(top_nodes, aes(x = risk_priority_score, y = batch_label)) +
  geom_col(aes(fill = risk_tier), width = 0.68, colour = "black", linewidth = 0.25) +
  facet_wrap(~layer, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("High" = red, "Moderate" = blue, "Low" = grey), name = "Risk priority") +
  scale_x_continuous(limits = c(0, max(top_nodes$risk_priority_score, na.rm = TRUE) * 1.05), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Graph-based batch risk-priority score",
    y = "Batch (issue/linked records)"
  ) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 13),
    strip.text = element_text(size = 17, face = "bold"),
    panel.grid.major.y = element_blank()
  )
save_plot_dual(graph_plot, "01_batch_evidence_graph_top_risk_nodes", 11.5, 9.0)

landscape_df <- all_nodes |>
  mutate(
    layer = factor(layer, levels = c("Extract-powder quality", "Chenpi quality", "Chinese yam powder MES")),
    label_node = if_else(risk_priority_score >= quantile(risk_priority_score, 0.97, na.rm = TRUE), upstream_batch, NA_character_)
  )

landscape_plot <- ggplot(landscape_df, aes(x = finished_n, y = 100 * time_adjusted_issue_residual)) +
  geom_hline(yintercept = 0, linewidth = 0.8, linetype = "dashed", colour = "black") +
  geom_point(aes(fill = risk_tier, size = risk_priority_score), shape = 21, colour = "black", stroke = 0.25) +
  geom_text(
    data = landscape_df |> filter(!is.na(label_node)),
    aes(label = label_node),
    family = "Arial",
    size = 3.2,
    vjust = -0.8,
    check_overlap = TRUE
  ) +
  facet_wrap(~layer, scales = "free_x", ncol = 1) +
  scale_x_continuous(trans = "log1p", breaks = c(1, 3, 10, 30, 100), labels = c("1", "3", "10", "30", "100")) +
  scale_fill_manual(values = c("High" = red, "Moderate" = blue, "Low" = grey), name = "Risk priority") +
  scale_size_continuous(range = c(2.5, 7.0), name = "Risk-priority score") +
  labs(x = "Downstream finished-product records linked to upstream batch", y = "Time-adjusted issue residual (pp)") +
  theme(legend.position = "right")
save_plot_dual(landscape_plot, "02_time_adjusted_batch_evidence_landscape", 10.8, 9.0)

layer_plot_df <- all_nodes |>
  mutate(layer = factor(layer, levels = c("Extract-powder quality", "Chenpi quality", "Chinese yam powder MES")))

layer_plot <- ggplot(layer_plot_df, aes(x = layer, y = risk_priority_score)) +
  geom_boxplot(width = 0.48, outlier.shape = NA, fill = light_grey, colour = "black", linewidth = 0.5) +
  geom_jitter(aes(fill = risk_tier), shape = 21, colour = "black", width = 0.16, size = 2.6, stroke = 0.20) +
  scale_fill_manual(values = c("High" = red, "Moderate" = blue, "Low" = grey), guide = "none") +
  labs(x = NULL, y = "Graph-based batch risk-priority score") +
  theme(axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5))
save_plot_dual(layer_plot, "03_layerwise_graph_evidence_score_distribution", 9.2, 5.8)

score_definition <- tibble::tribble(
  ~Component, ~Definition, ~Weight, ~Interpretation,
  "Time-adjusted issue residual score", "Observed upstream-batch issue rate minus the issue rate expected from abnormal-window exposure, rescaled to 0-1.", 0.35, "Prioritizes upstream batches with excess issue burden beyond temporal confounding.",
  "Graph-propagation score", "log(1 + downstream finished-product records linked to the upstream batch) multiplied by the positive time-adjusted issue residual, rescaled to 0-1.", 0.45, "Propagates finished-product issue evidence backward through batch-traceability edges while penalizing weakly supported one-off signals.",
  "Trace-support score", "log(1 + downstream finished-product records linked to the upstream batch), rescaled to 0-1.", 0.10, "Rewards evidence supported by repeated downstream batch reuse.",
  "Quality/process deviation score", "Mean absolute robust z-score across available quality or process attributes, rescaled to 0-1.", 0.10, "Captures whether the upstream batch also shows measurable quality/process deviation.",
  "Graph evidence score", "100 * (0.40 * residual score + 0.35 * trace-support score + 0.25 * quality/process deviation score).", NA_real_, "A general evidence-burden score describing how much diagnostic evidence is attached to a batch.",
  "Risk-priority score", "100 * (0.35 * residual score + 0.45 * graph-propagation score + 0.10 * trace-support score + 0.10 * quality/process deviation score).", 1.00, "The main batch-ranking score used for issue-driven diagnosis; it is not a causal effect size or p-value."
)

top_evidence_table <- all_nodes |>
  select(
    layer, batch_type, upstream_batch, finished_n, issue_n, issue_rate,
    abnormal_window_frac, expected_issue_rate_from_time, time_adjusted_issue_residual,
    quality_deviation_burden, trace_support_score, time_adjusted_residual_score,
    edge_weighted_residual, graph_propagation_score, quality_deviation_score,
    graph_evidence_score, risk_priority_score, evidence_tier,
    risk_tier, support_flag, interpretation_class
  ) |>
  mutate(
    across(c(issue_rate, abnormal_window_frac, expected_issue_rate_from_time, time_adjusted_issue_residual), ~round(.x, 4)),
    across(c(quality_deviation_burden, trace_support_score, time_adjusted_residual_score, edge_weighted_residual, graph_propagation_score, quality_deviation_score), ~round(.x, 3))
  )

write.xlsx(
  list(
    score_definition = score_definition,
    all_batch_evidence_nodes = top_evidence_table,
    layer_summary = layer_summary,
    top_30_evidence_batches = top_evidence_table |> slice_head(n = 30),
    finished_issue_rate_reference = finished_rates,
    bootstrap_context = bootstrap_effects
  ),
  file.path(tables_dir, "批次证据图评分结果.xlsx"),
  overwrite = TRUE
)

md_lines <- c(
  "# 批次证据图模型：Graph-based batch evidence scoring module",
  "",
  "## 目的",
  "",
  "本模块用于把上游批次、下游成品崩解异常和时间窗口混杂整合成一个可解释的批次证据图评分体系。它不是黑箱预测模型，也不是随机因果推断，而是服务于 finished-product-issue-driven hierarchical batch diagnosis 的风险优先级排序模块。",
  "",
  "## 节点和边",
  "",
  "- 节点：浸膏粉批次、陈皮批次、山药粉批次，以及成品崩解异常节点。",
  "- 边：上游批次到下游成品崩解异常的追溯关系，边强度由该上游批次关联的下游成品记录数和证据分数共同体现。",
  "",
  "## 评分公式",
  "",
  "本模块同时输出两个分数：Graph evidence score 和 Risk-priority score。Graph evidence score = 100 * (0.40 * time-adjusted issue residual score + 0.35 * trace-support score + 0.25 * quality/process deviation score)，用于描述一个上游批次承载了多少可追溯证据。Risk-priority score = 100 * (0.35 * time-adjusted issue residual score + 0.45 * graph-propagation score + 0.10 * trace-support score + 0.10 * quality/process deviation score)，用于论文主分析中的批次风险优先级排序。",
  "",
  "其中，time-adjusted issue residual 是观测 issue rate 减去基于 abnormal-window exposure 预期得到的 issue rate。Graph-propagation score 则把该残差按下游追溯记录数进行边权传播，避免 n 很小的一次性波动被过度解释。这个设计避免把异常时间窗口误判为原料或供应商的独立风险，也避免把弱追溯支持的批次写成确定风险。",
  "",
  "## 论文写法",
  "",
  "建议写作 graph-based batch evidence scoring module 或 batch-evidence graph model。它的作用是风险排序和证据链可视化，不应写成因果效应模型。"
)
writeLines(md_lines, file.path(docs_dir, "批次证据图模型说明.md"), useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "批次证据图模型：Graph-based batch evidence scoring module", style = "heading 1")
doc <- body_add_par(doc, "本模块用于把上游批次、下游成品崩解异常和时间窗口混杂整合成一个可解释的批次证据图评分体系。它不是黑箱预测模型，也不是随机因果推断，而是服务于 finished-product-issue-driven hierarchical batch diagnosis 的风险优先级排序模块。", style = "Normal")
doc <- body_add_par(doc, "评分定义", style = "heading 2")
doc <- body_add_flextable(doc, flextable(score_definition) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "层级汇总", style = "heading 2")
doc <- body_add_flextable(doc, flextable(layer_summary) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "Top evidence batches", style = "heading 2")
doc <- body_add_flextable(doc, flextable(top_evidence_table |> slice_head(n = 20)) |> fontsize(size = 6.5, part = "all") |> autofit())
doc <- body_add_par(doc, "论文写法边界", style = "heading 2")
doc <- body_add_par(doc, "建议写作 graph-based batch evidence scoring module 或 batch-evidence graph model。它的作用是风险排序和证据链可视化，不应写成因果效应模型。", style = "Normal")
print(doc, target = file.path(docs_dir, "批次证据图模型说明.docx"))

message("Batch-evidence graph scoring module completed.")
