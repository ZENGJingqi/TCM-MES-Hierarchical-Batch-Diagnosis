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
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")
docs_dir <- file.path(project_dir, "docs")

set_plot_style <- function() {
  theme_set(theme_bw(base_family = "Arial", base_size = 24))
  theme_update(
    plot.title = element_blank(),
    axis.title = element_text(size = 28, colour = "black"),
    axis.text = element_text(size = 24, colour = "black"),
    strip.text = element_text(size = 24, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.8),
    plot.margin = margin(10, 18, 10, 12)
  )
}

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330)
}

markdown_table <- function(df) {
  df <- as.data.frame(df)
  df[] <- lapply(df, as.character)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- apply(df, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

set_plot_style()
red <- "#A83B2B"
blue <- "#0A5C7A"
grey <- "#8C8C8C"

results_path <- file.path(tables_dir, "joint_disintegration_model_results.xlsx")
matrix_path <- file.path(tables_dir, "joint_disintegration_model_matrix.xlsx")

perf <- read_xlsx(results_path, sheet = "model_performance")
time_split <- read_xlsx(results_path, sheet = "time_split_performance")
coef_df <- read_xlsx(results_path, sheet = "elastic_net_coefficients")
shap_df <- read_xlsx(results_path, sheet = "xgboost_shap_importance")
linkage <- read_xlsx(matrix_path, sheet = "linkage_summary")
variable_audit <- read_xlsx(matrix_path, sheet = "variable_audit")

model_order <- c(
  "Baseline",
  "Baseline + MES",
  "Baseline + MES + extract-powder",
  "Baseline + MES + extract-powder + Chenpi",
  "Full core hierarchy",
  "Extended candidates",
  "XGBoost extended candidates"
)

perf <- perf |>
  mutate(
    model = factor(model, levels = model_order),
    adjustment = factor(adjustment, levels = c("No month adjustment", "Month-adjusted"))
  )

auc_pr <- perf |>
  filter(!is.na(AUC), !is.na(PR_AUC)) |>
  select(adjustment, model, AUC, PR_AUC) |>
  pivot_longer(cols = c(AUC, PR_AUC), names_to = "Metric", values_to = "Value")

auc_pr_plot <- ggplot(auc_pr, aes(x = model, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, colour = "black", linewidth = 0.25) +
  facet_wrap(~adjustment, ncol = 1, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c(AUC = red, PR_AUC = blue)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.04))) +
  labs(x = NULL, y = "Cross-validated discrimination", fill = NULL) +
  theme(
    axis.text.y = element_text(size = 20),
    legend.position = "top",
    legend.text = element_text(size = 23)
  )
save_plot_dual(auc_pr_plot, "05_publication_model_auc_prauc", 13.2, 9.8)

brier_plot <- perf |>
  filter(!is.na(Brier)) |>
  ggplot(aes(x = model, y = Brier, fill = adjustment)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62, colour = "black", linewidth = 0.25) +
  coord_flip() +
  scale_fill_manual(values = c("No month adjustment" = blue, "Month-adjusted" = grey)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Brier score (lower is better)", fill = NULL) +
  theme(
    axis.text.y = element_text(size = 20),
    legend.position = "top",
    legend.text = element_text(size = 22)
  )
save_plot_dual(brier_plot, "06_publication_model_brier_score", 12.5, 7.2)

coef_clean <- coef_df |>
  filter(term_type == "Value", !str_detect(term, "^production_month")) |>
  mutate(
    direction = ifelse(coefficient > 0, "Higher issue probability", "Lower issue probability"),
    label = make.unique(label, sep = " "),
    label = factor(label, levels = rev(label))
  )

coef_clean_plot <- ggplot(coef_clean, aes(x = coefficient, y = label, fill = coefficient > 0)) +
  geom_col(width = 0.66, colour = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, linewidth = 0.8, colour = "black") +
  scale_fill_manual(values = c(`TRUE` = red, `FALSE` = blue), guide = "none") +
  labs(x = "Elastic-net coefficient", y = NULL) +
  theme(axis.text.y = element_text(size = 20))
save_plot_dual(coef_clean_plot, "07_publication_clean_core_coefficients", 12.5, 7.2)

evidence <- tibble::tribble(
  ~layer, ~records, ~candidate_variables, ~key_signal, ~direction, ~model_support, ~interpretation,
  "Finished-product quality", "908 linked 0.8 g batches", "2 support variables", "Disintegration time >10 min", "Sentinel endpoint", "Endpoint definition", "Finished-product issue used to initiate upstream batch diagnosis",
  "Finished-product MES", "908 linked MES batches", "25 screened; 9 FDR-significant", "Coating yield; final-blend moisture; coating mass balance", "Lower coating yield; higher final-blend moisture; lower coating mass balance", "Largest performance gain; top Elastic-net and SHAP signals", "Primary process layer explaining the finished-product disintegration issue",
  "Extract-powder quality", "804 linked finished batches", "4 screened; 3 FDR-significant", "Total ash; extract", "Higher total ash; lower extract", "Selected in core Elastic-net; supported by SHAP", "Intermediate quality bridge between finished product and upstream materials",
  "Chenpi quality", "811 linked finished batches", "3 screened; 3 FDR-significant", "Chenpi hesperidin; Chenpi moisture", "Lower hesperidin; lower moisture", "Selected in core Elastic-net; supported by SHAP", "Raw-material quality signal associated with downstream disintegration issue",
  "Chinese yam powder MES", "839 linked finished batches", "6 modelable variables; 3 FDR-significant", "Rejected material rate; through 120-mesh mean", "Higher rejected material rate; lower through 120-mesh mean", "Selected in core Elastic-net; supported by SHAP", "Process-material signal related to sorting loss and fineness control"
)

evidence_plot_df <- evidence |>
  mutate(
    x = c(0.8, 2.25, 3.70, 5.15, 3.70),
    y = c(0.50, 0.50, 0.74, 0.74, 0.26),
    w = c(1.05, 1.15, 1.12, 1.05, 1.18),
    h = c(0.36, 0.38, 0.36, 0.36, 0.36),
    layer_short = c("Finished-product\nquality", "Finished-product\nMES", "Extract-powder\nquality", "Chenpi\nquality", "Chinese yam\npowder MES"),
    key_text = c(
      ">10 min\ndisintegration",
      "Lower coating yield\nHigher final-blend moisture\nLower coating mass balance",
      "Higher total ash\nLower extract",
      "Lower hesperidin\nLower moisture",
      "Higher rejected rate\nLower 120-mesh fraction"
    ),
    support_text = c(
      "Sentinel endpoint",
      "Largest model gain\nTop EN/SHAP signal",
      "Core EN + SHAP",
      "Core EN + SHAP",
      "Core EN + SHAP"
    ),
    fill = c("#F7F4EF", "#EEF5F6", "#F7F4EF", "#F7F4EF", "#EEF5F6")
  )

arrow_df <- tibble(
  x = c(1.36, 2.86, 4.28, 2.82),
  xend = c(1.66, 3.12, 4.62, 3.08),
  y = c(0.50, 0.63, 0.74, 0.37),
  yend = c(0.50, 0.71, 0.74, 0.29)
)

evidence_plot <- ggplot(evidence_plot_df, aes(x = x)) +
  geom_segment(
    data = arrow_df,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    linewidth = 1.5,
    colour = grey,
    arrow = arrow(length = unit(0.18, "inches"), type = "closed")
  ) +
  geom_rect(
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "black",
    linewidth = 0.65
  ) +
  scale_fill_identity() +
  geom_text(
    aes(y = y + h * 0.30, label = layer_short),
    family = "Arial",
    size = 5.2,
    fontface = "bold",
    lineheight = 0.88
  ) +
  geom_text(
    aes(y = y + h * 0.03, label = records),
    family = "Arial",
    size = 3.65,
    lineheight = 0.92
  ) +
  geom_text(
    aes(y = y - h * 0.23, label = key_text),
    family = "Arial",
    size = 3.35,
    lineheight = 0.86
  ) +
  geom_text(
    aes(y = y - h * 0.44, label = support_text),
    family = "Arial",
    size = 3.05,
    lineheight = 0.88,
    colour = "black"
  ) +
  scale_x_continuous(limits = c(0.15, 5.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(16, 16, 16, 16)
  )
save_plot_dual(evidence_plot, "08_hierarchical_batch_diagnosis_evidence_map", 15.5, 6.2)

main_result_table <- evidence |>
  mutate(
    `Data layer` = layer,
    `Linked records/batches` = records,
    `Variables evaluated` = candidate_variables,
    `Key diagnostic signal` = key_signal,
    `Observed direction` = direction,
    `Joint-model support` = model_support,
    `Interpretation` = interpretation
  ) |>
  select(`Data layer`, `Linked records/batches`, `Variables evaluated`, `Key diagnostic signal`, `Observed direction`, `Joint-model support`, `Interpretation`)

publication_perf <- perf |>
  mutate(
    AUC = round(AUC, 3),
    PR_AUC = round(PR_AUC, 3),
    Brier = round(Brier, 3)
  ) |>
  arrange(adjustment, model)

model_layer_map <- tibble::tribble(
  ~model_chr, ~model_role, ~included_layers,
  "Baseline", "Reference model", "Finished-product quality support variables",
  "Baseline + MES", "Process-layer diagnostic model", "Finished-product quality + finished-product MES",
  "Baseline + MES + extract-powder", "Intermediate-quality extension", "Finished-product quality + MES + Jianwei Xiaoshi extract-powder quality",
  "Baseline + MES + extract-powder + Chenpi", "Raw-material extension", "Finished-product quality + MES + extract-powder quality + Chenpi quality",
  "Full core hierarchy", "Primary hierarchical diagnostic model", "Finished-product quality + MES + extract-powder quality + Chenpi quality + Chinese yam powder MES",
  "Extended candidates", "Extended sensitivity model", "Full core hierarchy + additional screened variables",
  "XGBoost extended candidates", "Non-linear validation model", "Extended candidate variables fitted by XGBoost"
)

model_performance_total <- perf |>
  mutate(
    model_chr = as.character(model),
    adjustment_chr = as.character(adjustment)
  ) |>
  left_join(model_layer_map, by = "model_chr") |>
  arrange(adjustment, model) |>
  group_by(adjustment_chr) |>
  mutate(
    delta_auc = if_else(str_detect(model_chr, "XGBoost"), NA_real_, AUC - lag(AUC)),
    delta_pr_auc = if_else(str_detect(model_chr, "XGBoost"), NA_real_, PR_AUC - lag(PR_AUC)),
    delta_brier = if_else(str_detect(model_chr, "XGBoost"), NA_real_, Brier - lag(Brier))
  ) |>
  ungroup() |>
  mutate(
    `Analysis setting` = adjustment_chr,
    `Model` = model_chr,
    `Model role` = model_role,
    `Included data layers` = included_layers,
    `Variables` = n_variables,
    `AUC` = round(AUC, 3),
    `PR-AUC` = round(PR_AUC, 3),
    `Brier score` = round(Brier, 3),
    `Delta AUC vs previous` = if_else(is.na(delta_auc), NA_character_, sprintf("%+.3f", delta_auc)),
    `Delta PR-AUC vs previous` = if_else(is.na(delta_pr_auc), NA_character_, sprintf("%+.3f", delta_pr_auc)),
    `Delta Brier vs previous` = if_else(is.na(delta_brier), NA_character_, sprintf("%+.3f", delta_brier)),
    `Interpretation` = case_when(
      model_chr == "Baseline" & adjustment_chr == "No month adjustment" ~ "Finished-product support variables alone provided limited discrimination.",
      model_chr == "Baseline + MES" & adjustment_chr == "No month adjustment" ~ "Adding finished-product MES variables produced the largest diagnostic gain.",
      model_chr == "Baseline + MES + extract-powder" & adjustment_chr == "No month adjustment" ~ "Extract-powder quality added modest incremental information.",
      model_chr == "Baseline + MES + extract-powder + Chenpi" & adjustment_chr == "No month adjustment" ~ "Chenpi quality further improved PR-AUC and Brier score.",
      model_chr == "Full core hierarchy" & adjustment_chr == "No month adjustment" ~ "The full hierarchy retained high discrimination and the best calibration among logistic models.",
      model_chr == "Extended candidates" & adjustment_chr == "No month adjustment" ~ "Adding more variables did not improve performance, supporting a parsimonious core model.",
      model_chr == "XGBoost extended candidates" ~ "Non-linear validation was consistent with the main model but did not outperform the core logistic hierarchy.",
      adjustment_chr == "Month-adjusted" ~ "Month adjustment tested whether the association was dominated by time-window effects.",
      TRUE ~ "Sensitivity result."
    )
  ) |>
  select(
    `Analysis setting`, `Model`, `Model role`, `Included data layers`, `Variables`,
    `AUC`, `PR-AUC`, `Brier score`,
    `Delta AUC vs previous`, `Delta PR-AUC vs previous`, `Delta Brier vs previous`,
    `Interpretation`
  )

get_perf_row <- function(setting, model_name) {
  perf |>
    filter(as.character(adjustment) == setting, as.character(model) == model_name) |>
    slice(1)
}

comparison_pairs <- tibble::tribble(
  ~comparison, ~setting, ~reference_model, ~test_model, ~interpretation,
  "Finished-product MES added to baseline", "No month adjustment", "Baseline", "Baseline + MES", "Largest improvement; supports MES as the primary diagnostic layer.",
  "Extract-powder quality added after MES", "No month adjustment", "Baseline + MES", "Baseline + MES + extract-powder", "Small but consistent gain; supports intermediate-quality linkage.",
  "Chenpi quality added after extract-powder", "No month adjustment", "Baseline + MES + extract-powder", "Baseline + MES + extract-powder + Chenpi", "Improved PR-AUC and Brier score; supports upstream raw-material contribution.",
  "Chinese yam powder MES completed the core hierarchy", "No month adjustment", "Baseline + MES + extract-powder + Chenpi", "Full core hierarchy", "Limited AUC gain but improved Brier score; retained as mechanistic upstream process evidence.",
  "Extended candidates compared with core hierarchy", "No month adjustment", "Full core hierarchy", "Extended candidates", "No performance gain; favors the more interpretable core hierarchy.",
  "Month-adjusted full hierarchy compared with baseline", "Month-adjusted", "Baseline", "Full core hierarchy", "After accounting for production month, the hierarchy still showed a small incremental gain.",
  "XGBoost validation compared with extended logistic model", "No month adjustment", "Extended candidates", "XGBoost extended candidates", "Non-linear model did not outperform logistic modeling, supporting interpretability."
)

model_comparison_summary <- comparison_pairs |>
  rowwise() |>
  mutate(
    ref = list(get_perf_row(setting, reference_model)),
    test = list(get_perf_row(setting, test_model)),
    `Reference AUC` = round(ref$AUC, 3),
    `Test AUC` = round(test$AUC, 3),
    `Delta AUC` = sprintf("%+.3f", test$AUC - ref$AUC),
    `Reference PR-AUC` = round(ref$PR_AUC, 3),
    `Test PR-AUC` = round(test$PR_AUC, 3),
    `Delta PR-AUC` = sprintf("%+.3f", test$PR_AUC - ref$PR_AUC),
    `Reference Brier` = round(ref$Brier, 3),
    `Test Brier` = round(test$Brier, 3),
    `Delta Brier` = sprintf("%+.3f", test$Brier - ref$Brier),
    `Comparison` = comparison,
    `Analysis setting` = setting,
    `Reference model` = reference_model,
    `Test model` = test_model,
    `Interpretation` = interpretation
  ) |>
  ungroup() |>
  select(
    `Analysis setting`, `Comparison`, `Reference model`, `Test model`,
    `Reference AUC`, `Test AUC`, `Delta AUC`,
    `Reference PR-AUC`, `Test PR-AUC`, `Delta PR-AUC`,
    `Reference Brier`, `Test Brier`, `Delta Brier`,
    `Interpretation`
  )

time_split_summary <- time_split |>
  mutate(
    `Validation design` = "Chronological validation",
    `Training period` = train_period,
    `Test period` = test_period,
    `Training batches` = train_n,
    `Test batches` = test_n,
    `Test >10 min batches` = test_issue_n,
    `AUC` = round(AUC, 3),
    `PR-AUC` = round(PR_AUC, 3),
    `Brier score` = round(Brier, 3),
    `Interpretation` = "The poor chronological AUC and high Brier score indicate strong time-window drift; the model should be presented as retrospective issue-driven diagnosis rather than stable prospective prediction."
  ) |>
  select(
    `Validation design`, `Training period`, `Test period`, `Training batches`,
    `Test batches`, `Test >10 min batches`, `AUC`, `PR-AUC`, `Brier score`, `Interpretation`
  )

write.xlsx(
  list(
    main_result_table = main_result_table,
    publication_model_performance = publication_perf,
    model_performance_total = model_performance_total,
    model_comparison_summary = model_comparison_summary,
    temporal_validation_summary = time_split_summary,
    clean_elastic_net_coefficients = coef_clean |> arrange(desc(abs_coefficient)),
    shap_importance = shap_df,
    evidence_map_source = evidence
  ),
  file.path(tables_dir, "publication_joint_model_tables.xlsx"),
  overwrite = TRUE
)

md_lines <- c(
  "# 联合建模论文图表整理说明",
  "",
  "## 输出图表",
  "",
  "- `05_publication_model_auc_prauc`: 仅展示 AUC 与 PR-AUC，避免与 Brier 的方向混淆。",
  "- `06_publication_model_brier_score`: 单独展示 Brier score，并在坐标轴注明 lower is better。",
  "- `07_publication_clean_core_coefficients`: 仅展示真实变量的 Elastic-net 系数，缺失指示项不进入主图。",
  "- `08_hierarchical_batch_diagnosis_evidence_map`: 用一张图串联成品、MES、浸膏粉、陈皮和山药粉的批次级诊断证据。",
  "",
  "## 主结果表",
  "",
  markdown_table(main_result_table),
  "",
  "## 写作建议",
  "",
  "主文应优先使用 evidence map 和主结果表来表达框架价值；AUC/PR-AUC 和 SHAP 作为模型验证结果。由于时间切分验证提示异常批次强烈集中于 2026-01 至 2026-02，本阶段不宜写成稳定前瞻预测模型，而应写成回顾性问题驱动批次诊断框架。"
)

writeLines(md_lines, file.path(docs_dir, "联合建模论文图表整理说明.md"), useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "联合建模论文图表整理说明", style = "heading 1")
doc <- body_add_par(doc, "本文件整理联合建模结果的论文级图表输出。主文建议优先使用层级诊断证据图和主结果表，模型性能图与 SHAP 排名作为验证性结果。", style = "Normal")
doc <- body_add_par(doc, "主结果表", style = "heading 2")
doc <- body_add_flextable(doc, flextable(main_result_table) |> autofit())
doc <- body_add_par(doc, "写作边界", style = "heading 2")
doc <- body_add_par(doc, "时间切分验证提示异常批次强烈集中于 2026-01 至 2026-02，因此当前模型应表述为回顾性问题驱动批次诊断框架，而不是稳定前瞻预测模型。", style = "Normal")
print(doc, target = file.path(docs_dir, "联合建模论文图表整理说明.docx"))

perf_doc <- read_docx()
perf_doc <- body_add_par(perf_doc, "模型性能与对比分析总表", style = "heading 1")
perf_doc <- body_add_par(perf_doc, "该表用于论文中说明不同层级数据进入模型后的诊断性能变化。主模型以未校正月份的层级 Logistic/Elastic-net 结果为主，月份校正模型作为敏感性分析，XGBoost 作为非线性验证。", style = "Normal")
perf_doc <- body_add_par(perf_doc, "模型性能总表", style = "heading 2")
perf_doc <- body_add_flextable(perf_doc, flextable(model_performance_total) |> fontsize(size = 8, part = "all") |> autofit())
perf_doc <- body_add_par(perf_doc, "层级增量比较", style = "heading 2")
perf_doc <- body_add_flextable(perf_doc, flextable(model_comparison_summary) |> fontsize(size = 8, part = "all") |> autofit())
perf_doc <- body_add_par(perf_doc, "时间切分验证", style = "heading 2")
perf_doc <- body_add_flextable(perf_doc, flextable(time_split_summary) |> fontsize(size = 8, part = "all") |> autofit())
perf_doc <- body_add_par(perf_doc, "结果解释", style = "heading 2")
perf_doc <- body_add_par(perf_doc, "成品 MES 变量带来最大性能提升，说明成品崩解异常首先应在成品制造过程层面进行诊断。浸膏粉、陈皮和山药粉层级的增量主要体现为追溯解释和风险定位，而不是单纯追求更高 AUC。扩展变量和 XGBoost 未超过核心层级模型，因此正文应优先呈现可解释的核心层级模型。时间切分验证提示模型受特定异常时间窗影响明显，应克制表述为回顾性问题驱动诊断框架，而非稳定前瞻预测模型。", style = "Normal")
print(perf_doc, target = file.path(docs_dir, "模型性能与对比分析总表.docx"))

message("Publication-ready figures and tables generated.")
