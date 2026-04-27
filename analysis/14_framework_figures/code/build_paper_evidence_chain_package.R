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

set.seed(20260427)

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
light_grey <- "#F2F2F2"
dark <- "#222222"

theme_set(theme_bw(base_family = "Arial", base_size = 18))
theme_update(
  plot.title = element_blank(),
  axis.title = element_text(size = 22, colour = "black"),
  axis.text = element_text(size = 18, colour = "black"),
  strip.text = element_text(size = 18, face = "bold", colour = "black"),
  panel.grid.minor = element_blank(),
  plot.margin = margin(12, 18, 12, 18)
)

save_plot_dual <- function(plot_obj, stem, width, height) {
  ggsave(file.path(figures_dir, paste0(stem, ".pdf")), plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(figures_dir, paste0(stem, ".png")), plot_obj, width = width, height = height, dpi = 330, bg = "white")
}

auc_rank <- function(y, pred) {
  ok <- is.finite(pred) & !is.na(y)
  y <- as.integer(y[ok])
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  r <- rank(pred, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

pr_auc_step <- function(y, pred) {
  ok <- is.finite(pred) & !is.na(y)
  y <- as.integer(y[ok])
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  ord <- order(pred, decreasing = TRUE)
  y <- y[ord]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  recall <- tp / sum(y == 1)
  precision <- tp / pmax(tp + fp, 1)
  recall <- c(0, recall)
  precision <- c(1, precision)
  sum(diff(recall) * precision[-1])
}

safe_glm_pred <- function(train_x, train_y, test_x) {
  train_df <- as.data.frame(train_x)
  train_df$y <- train_y
  test_df <- as.data.frame(test_x)
  fit <- tryCatch(
    suppressWarnings(glm(y ~ ., data = train_df, family = binomial())),
    error = function(e) NULL
  )
  if (is.null(fit)) return(rep(mean(train_y), nrow(test_df)))
  pred <- tryCatch(
    suppressWarnings(predict(fit, newdata = test_df, type = "response")),
    error = function(e) rep(mean(train_y), nrow(test_df))
  )
  pred <- as.numeric(pred)
  pred[!is.finite(pred)] <- mean(train_y)
  pmin(pmax(pred, 1e-6), 1 - 1e-6)
}

make_model_matrix <- function(train, test, vars, include_missing = TRUE) {
  train_x <- train[, vars, drop = FALSE]
  test_x <- test[, vars, drop = FALSE]
  for (nm in vars) {
    train_x[[nm]] <- suppressWarnings(as.numeric(train_x[[nm]]))
    test_x[[nm]] <- suppressWarnings(as.numeric(test_x[[nm]]))
    miss_train <- is.na(train_x[[nm]])
    miss_test <- is.na(test_x[[nm]])
    med <- median(train_x[[nm]], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    train_x[[nm]][miss_train] <- med
    test_x[[nm]][miss_test] <- med
    if (include_missing) {
      train_x[[paste0(nm, "__missing")]] <- as.integer(miss_train)
      test_x[[paste0(nm, "__missing")]] <- as.integer(miss_test)
    }
  }
  list(train = train_x, test = test_x)
}

stratified_folds <- function(y, k = 5) {
  folds <- integer(length(y))
  for (cls in sort(unique(y))) {
    idx <- which(y == cls)
    idx <- sample(idx)
    folds[idx] <- rep(seq_len(k), length.out = length(idx))
  }
  folds
}

repeated_cv <- function(data, model_specs, repeats = 50, k = 5) {
  out <- vector("list", repeats * length(model_specs))
  ptr <- 1
  y <- as.integer(data$disintegration_issue)
  for (r in seq_len(repeats)) {
    folds <- stratified_folds(y, k = k)
    for (spec_name in names(model_specs)) {
      vars <- model_specs[[spec_name]]
      pred <- rep(NA_real_, nrow(data))
      for (fold in seq_len(k)) {
        train <- data[folds != fold, , drop = FALSE]
        test <- data[folds == fold, , drop = FALSE]
        mm <- make_model_matrix(train, test, vars)
        pred[folds == fold] <- safe_glm_pred(mm$train, train$disintegration_issue, mm$test)
      }
      out[[ptr]] <- tibble(
        repeat_id = r,
        model = spec_name,
        AUC = auc_rank(y, pred),
        PR_AUC = pr_auc_step(y, pred),
        Brier = mean((y - pred)^2)
      )
      ptr <- ptr + 1
    }
  }
  bind_rows(out)
}

joint_dir <- file.path(project_dir, "联合建模_成品崩解问题驱动")
time_dir <- file.path(project_dir, "时序分析_成品崩解问题")
causal_dir <- file.path(project_dir, "因果建模_成品崩解问题")

joint_matrix <- read_xlsx(file.path(joint_dir, "tables", "joint_disintegration_model_matrix.xlsx"), sheet = "joint_model_matrix")
joint_perf <- read_xlsx(file.path(joint_dir, "tables", "joint_disintegration_model_results.xlsx"), sheet = "model_performance")
time_split <- read_xlsx(file.path(joint_dir, "tables", "joint_disintegration_model_results.xlsx"), sheet = "time_split_performance")
linkage_summary <- read_xlsx(file.path(joint_dir, "tables", "joint_disintegration_model_matrix.xlsx"), sheet = "linkage_summary")
time_month <- read_xlsx(file.path(time_dir, "tables", "成品崩解时序分析结果.xlsx"), sheet = "monthly_summary")
time_window <- read_xlsx(file.path(time_dir, "tables", "成品崩解时序分析结果.xlsx"), sheet = "abnormal_window_summary")
causal_robust <- read_xlsx(file.path(causal_dir, "tables", "因果稳健性与路径衰减分析.xlsx"), sheet = "bootstrap_batch_effects")
path_perf <- read_xlsx(file.path(causal_dir, "tables", "因果稳健性与路径衰减分析.xlsx"), sheet = "path_model_performance")

model_data <- joint_matrix |>
  mutate(
    disintegration_issue = as.integer(disintegration_issue),
    abnormal_window = as.integer(production_month %in% c("2024-08", "2024-09", "2026-01", "2026-02"))
  )

baseline_vars <- c("finished_coated_tablet_weight_g", "finished_active_content_mg_per_tablet")
mes_vars <- c("coating_yield_pct", "final_blend_moisture_pct", "coating_mass_balance_pct", "compression_hardness_mean_n", "coated_tablet_hardness_mean_n")
extract_vars <- c("extract_total_ash_pct", "extract_extract_pct", "extract_hesperidin_mg_g")
chenpi_vars <- c("chenpi_moisture_pct_mean", "chenpi_hesperidin_pct_mean", "chenpi_impurities_pct_mean")
yam_vars <- c("yam_rejected_material_rate_pct", "yam_through_120_mesh_mean_pct", "yam_yield_pct")

model_specs <- list(
  "Finished-product quality only" = baseline_vars,
  "Finished-product quality + MES" = c(baseline_vars, mes_vars),
  "Full hierarchical model" = c(baseline_vars, mes_vars, extract_vars, chenpi_vars, yam_vars),
  "Abnormal-window only" = c("abnormal_window"),
  "Abnormal-window + full hierarchy" = c("abnormal_window", baseline_vars, mes_vars, extract_vars, chenpi_vars, yam_vars)
)

cv_raw <- repeated_cv(model_data, model_specs, repeats = 50, k = 5)
cv_summary <- cv_raw |>
  group_by(model) |>
  summarise(
    repeats = n(),
    AUC_mean = mean(AUC, na.rm = TRUE),
    AUC_sd = sd(AUC, na.rm = TRUE),
    PR_AUC_mean = mean(PR_AUC, na.rm = TRUE),
    PR_AUC_sd = sd(PR_AUC, na.rm = TRUE),
    Brier_mean = mean(Brier, na.rm = TRUE),
    Brier_sd = sd(Brier, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~round(.x, 3)))

apparent_core <- joint_perf |>
  filter(adjustment == "No month adjustment") |>
  select(model, n_variables, AUC, PR_AUC, Brier) |>
  mutate(across(c(AUC, PR_AUC, Brier), ~round(.x, 3)))

time_split_summary <- time_split |>
  mutate(across(c(AUC, PR_AUC, Brier), ~round(.x, 3)))

time_key <- time_window |>
  mutate(across(where(is.numeric), ~round(.x, 3)))

robust_key <- causal_robust |>
  filter(term == "abnormal_window_10pct" | robust_direction == "Positive") |>
  transmute(
    layer,
    signal = label,
    bootstrap_OR = round(boot_or, 3),
    CI_low = round(boot_or_low, 3),
    CI_high = if_else(boot_or_high > 1000, NA_real_, round(boot_or_high, 3)),
    robust_direction,
    interpretation = if_else(
      term == "abnormal_window_10pct",
      "Robust temporal-window signal after upstream-batch resampling",
      "Robust non-temporal signal"
    )
  )

framework_summary <- tibble::tribble(
  ~Evidence_layer, ~Diagnostic_question, ~Data_entity, ~Primary_method, ~Key_result, ~Manuscript_role,
  "Sentinel finished-product issue", "What finished-product quality deviation should drive the diagnosis?", "Jianwei Xiaoshi tablet quality testing", "Issue definition and descriptive time-series profiling", "Disintegration time >10 min was used as the sentinel finished-product quality issue.", "Defines the diagnostic target rather than performing untargeted screening.",
  "Temporal context", "Is the issue a stable seasonal pattern or an abnormal production window?", "Finished-product production month and disintegration results", "Monthly issue-rate analysis and abnormal-window sensitivity analysis", "Most issues concentrated in abnormal temporal windows; seasonality was not stable after excluding those windows.", "Prevents false attribution of upstream source effects to material origin alone.",
  "Finished-product MES process layer", "Which finished-product process signals are closest to the quality issue?", "Jianwei Xiaoshi tablet MES production records", "FDR screening, regression, and hierarchical predictive modeling", "MES variables provided the largest diagnostic gain over finished-product quality variables alone.", "Identifies the nearest process layer for batch diagnosis.",
  "Intermediate extract-powder layer", "Can the finished-product issue be traced to intermediate-quality variation?", "Jianwei Xiaoshi extract-powder quality testing", "Batch linkage, feature screening, and path modeling", "Extract-powder quality added traceability evidence but did not replace MES process signals.", "Connects finished-product issues to intermediate-quality pathways.",
  "Upstream raw-material quality layer", "Do upstream raw-material quality records explain the finished-product issue?", "Chenpi quality testing", "Source/quality comparison, linkage analysis, and source-term attenuation modeling", "Chenpi source signals were strongly entangled with time and pathway variables, so direct origin causality was not supported.", "Shows how the framework avoids simplistic source attribution.",
  "Process-material MES layer", "Can process-material production records contribute upstream process evidence?", "Chinese yam powder MES production records", "Batch linkage, feature screening, and upstream-batch issue-ratio analysis", "Chinese yam powder records provided upstream process evidence, but current data support supplier/process interpretation rather than origin causality.", "Extends diagnosis to process-material manufacturing.",
  "Integrated diagnostic evidence chain", "Does layer-wise integration improve diagnostic interpretation?", "MES + QC + batch traceability across all layers", "Layered modeling, repeated cross-validation, time-split validation, and causal-informed path decomposition", "Layer integration improved retrospective diagnosis, while time-split results showed limited prospective extrapolation under abnormal-window shift.", "Defines the paper's framework contribution and use boundary."
)

validation_summary <- tibble::tribble(
  ~Validation_component, ~Purpose, ~Result, ~Interpretation_for_manuscript,
  "Layered apparent model comparison", "Quantify diagnostic gain from adding MES and upstream layers.", paste0("Full hierarchy AUC = ", round(joint_perf$AUC[joint_perf$model == "Full core hierarchy" & joint_perf$adjustment == "No month adjustment"], 3), "; PR-AUC = ", round(joint_perf$PR_AUC[joint_perf$model == "Full core hierarchy" & joint_perf$adjustment == "No month adjustment"], 3), "."), "Supports retrospective issue diagnosis and evidence-chain integration.",
  "Repeated stratified 5-fold cross-validation", "Check whether the integrated model is stable under internal resampling.", paste0("Abnormal-window + full hierarchy mean AUC = ", cv_summary$AUC_mean[cv_summary$model == "Abnormal-window + full hierarchy"], "; mean PR-AUC = ", cv_summary$PR_AUC_mean[cv_summary$model == "Abnormal-window + full hierarchy"], "."), "Internal diagnostic performance is stable, but it is still within the same retrospective data distribution.",
  "Time-split validation", "Test whether the model extrapolates to a later abnormal window.", paste0("Time-split AUC = ", round(time_split$AUC[1], 3), "; PR-AUC = ", round(time_split$PR_AUC[1], 3), "; test issues = ", time_split$test_issue_n[1], "/", time_split$test_n[1], "."), "The framework should be positioned as retrospective diagnosis, not a ready prospective prediction model.",
  "Upstream-batch bootstrap", "Check whether upstream batch-level signals survive resampling at the batch level.", "Abnormal-window exposure was the most robust signal across upstream layers.", "Supports time-confounding control before interpreting material-source differences.",
  "Chenpi-source attenuation analysis", "Evaluate whether source-related signals remain after adding quality/process pathways.", "Chenpi source term was not robust and changed after pathway adjustment.", "Avoids overclaiming direct material-origin causality."
)

recommended_figures <- tibble::tribble(
  ~Figure, ~Recommended_title, ~Content, ~Current_source,
  "Figure 1", "Real-world hierarchical manufacturing dataset and batch linkage", "Five data entities, record counts, and linked/traced batch relationships.", "数据概况图_英文 / Figure_1_dataset_overview_and_batch_linkage",
  "Figure 2", "Finished-product disintegration as a sentinel quality issue", "Disintegration distribution/time trend and >10 min issue definition.", "成品问题驱动_D2描述; 时序分析_成品崩解问题",
  "Figure 3", "Finished-product MES process diagnosis", "MES feature screening and key process features versus disintegration issue.", "D3_成品MES与成品崩解关联分析",
  "Figure 4", "Upstream material and intermediate-quality evidence", "Extract-powder, Chenpi, and Chinese yam powder evidence linked to finished-product issue.", "D4/D6/D7 association folders",
  "Figure 5", "Integrated hierarchical diagnostic modeling", "Layered model performance and selected core diagnostic coefficients.", "联合建模_成品崩解问题驱动",
  "Figure 6", "Temporal confounding and causal-informed path decomposition", "Abnormal-window analysis, upstream-batch bootstrap, and Chenpi-source path adjustment.", "时序分析_成品崩解问题; 因果建模_成品崩解问题",
  "Graphical abstract", "Finished-product-issue-driven hierarchical batch diagnosis", "One-page evidence chain from sentinel issue to upstream material/process risks.", "论文证据链整合_框架完善"
)

framework_nodes <- tibble::tribble(
  ~id, ~x, ~y, ~label, ~detail, ~fill,
  1, 0.8, 3.0, "Sentinel\nissue", "Disintegration\n>10 min", "#F7F7F7",
  2, 2.45, 3.0, "Temporal\ncontext", "Abnormal-window\nscreening", "#F7F7F7",
  3, 4.10, 3.0, "MES\nprocess", "Finished-product\nprocess signals", "#EEF4F6",
  4, 5.75, 3.0, "Extract-powder\nquality", "Intermediate\nquality evidence", "#F8F3EB",
  5, 7.35, 3.75, "Chenpi\nquality", "Raw-material\nsource context", "#F8F3EB",
  6, 7.35, 2.25, "Yam-powder\nMES", "Process-material\nMES evidence", "#EEF4F6",
  7, 9.25, 3.0, "Path\ninterpretation", "Traceable risk\ndecomposition", "#F7F7F7"
)

framework_edges <- tibble::tribble(
  ~x, ~y, ~xend, ~yend, ~label,
  1.48, 3.0, 1.78, 3.0, "define",
  3.13, 3.0, 3.43, 3.0, "control",
  4.78, 3.0, 5.08, 3.0, "link",
  6.43, 3.0, 6.78, 3.55, "trace",
  6.43, 3.0, 6.78, 2.45, "trace",
  7.95, 3.55, 8.58, 3.12, "integrate",
  7.95, 2.45, 8.58, 2.88, "integrate"
)

framework_caption <- tibble(
  x = c(3.1, 5.5, 8.0),
  y = c(1.20, 1.20, 1.20),
  label = c(
    "Largest diagnostic gain:\nfinished-product MES",
    "Upstream layers:\ntraceability evidence",
    "Boundary:\nretrospective diagnosis,\nnot standalone prospective prediction"
  ),
  colour = c(red, blue, dark)
)

framework_plot <- ggplot() +
  geom_rect(
    data = framework_nodes,
    aes(xmin = x - 0.68, xmax = x + 0.68, ymin = y - 0.55, ymax = y + 0.55, fill = fill),
    colour = dark,
    linewidth = 0.45
  ) +
  geom_segment(
    data = framework_edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.22, "cm")),
    linewidth = 1.2,
    colour = grey
  ) +
  geom_text(
    data = framework_nodes,
    aes(x = x, y = y + 0.15, label = label),
    family = "Arial",
    size = 4.0,
    fontface = "bold",
    colour = "black"
  ) +
  geom_text(
    data = framework_nodes,
    aes(x = x, y = y - 0.18, label = detail),
    family = "Arial",
    size = 3.1,
    colour = "black",
    lineheight = 0.95
  ) +
  geom_label(
    data = framework_caption,
    aes(x = x, y = y, label = label, colour = colour),
    family = "Arial",
    size = 3.2,
    lineheight = 0.95,
    fill = "white",
    linewidth = 0.28,
    label.padding = unit(0.20, "lines"),
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0, 10.1), ylim = c(0.95, 4.45), clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = "Arial") +
  theme(
    plot.margin = margin(12, 16, 12, 16),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

save_plot_dual(framework_plot, "01_finished_product_issue_driven_hierarchical_batch_diagnosis_framework", 13.5, 4.8)

cv_plot_df <- cv_summary |>
  select(model, AUC_mean, PR_AUC_mean, Brier_mean) |>
  pivot_longer(cols = c(AUC_mean, PR_AUC_mean, Brier_mean), names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(metric, levels = c("AUC_mean", "PR_AUC_mean", "Brier_mean"), labels = c("AUC", "PR-AUC", "Brier score")),
    model_label = recode(
      model,
      "Finished-product quality only" = "Finished quality",
      "Finished-product quality + MES" = "Finished quality + MES",
      "Full hierarchical model" = "Full hierarchy",
      "Abnormal-window only" = "Time window",
      "Abnormal-window + full hierarchy" = "Time window + full hierarchy"
    ),
    model = factor(
      model_label,
      levels = c("Finished quality", "Finished quality + MES", "Full hierarchy", "Time window", "Time window + full hierarchy")
    )
  )

cv_plot <- ggplot(cv_plot_df, aes(x = value, y = model, fill = metric)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.3f", value)), hjust = -0.10, size = 4.0, family = "Arial") +
  facet_wrap(~metric, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("AUC" = red, "PR-AUC" = blue, "Brier score" = grey), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Repeated 5-fold cross-validation performance", y = NULL) +
  theme(
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 20)
  )

save_plot_dual(cv_plot, "02_repeated_cross_validation_model_robustness", 10.8, 7.8)

write.xlsx(
  list(
    framework_summary = framework_summary,
    validation_boundary = validation_summary,
    recommended_main_figures = recommended_figures,
    repeated_cv_raw = cv_raw,
    repeated_cv_summary = cv_summary,
    apparent_model_perf = apparent_core,
    time_split_performance = time_split_summary,
    abnormal_window_summary = time_key,
    robust_upstream_signals = robust_key,
    path_model_performance = path_perf
  ),
  file.path(tables_dir, "论文证据链整合与验证汇总.xlsx"),
  overwrite = TRUE
)

md_lines <- c(
  "# 论文统一创新点与证据链完善",
  "",
  "## 论文真正解决的问题",
  "",
  "本研究解决的是真实世界中药制造中“成品终检质量异常到上游物料和过程风险之间证据链断裂”的问题。核心不是发现单个显著指标，而是把分散的 MES、QC 和批次追溯数据转化为可解释的层级批次诊断证据链。",
  "",
  "## 统一创新点",
  "",
  "提出 finished-product-issue-driven hierarchical batch diagnosis，即成品问题驱动的层级批次诊断框架。该框架以明确的成品质量问题作为诊断入口，逐层连接时间窗口、成品 MES、浸膏粉质量、陈皮质量和山药粉 MES 过程记录，最终形成从成品异常到上游风险路径的可解释证据链。",
  "",
  "## 已补充内容",
  "",
  "- 证据链总图：`01_finished_product_issue_driven_hierarchical_batch_diagnosis_framework`。",
  "- 层级贡献汇总表：说明每一层回答什么问题、使用什么数据、承担什么论文角色。",
  "- 模型稳健性验证：补充 50 次重复 5 折交叉验证，并与时间分割验证共同说明模型边界。",
  "- 因果解释边界：强调 causal-informed path decomposition，不宣称随机因果；Chenpi source 和山药粉供应商不能直接写成独立原因。",
  "",
  "## 论文写法边界",
  "",
  "这篇论文应定位为 retrospective hierarchical diagnosis framework。可以强调该框架提高了真实世界制造数据的可解释诊断能力，但不应声称已经建立可直接上线的前瞻预测模型，也不应把来源/供应商差异写成确定因果。"
)
writeLines(md_lines, file.path(docs_dir, "论文统一创新点与证据链说明.md"), useBytes = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "论文统一创新点与证据链完善", style = "heading 1")
doc <- body_add_par(doc, "本研究解决的是真实世界中药制造中“成品终检质量异常到上游物料和过程风险之间证据链断裂”的问题。核心不是发现单个显著指标，而是把分散的 MES、QC 和批次追溯数据转化为可解释的层级批次诊断证据链。", style = "Normal")
doc <- body_add_par(doc, "统一创新点", style = "heading 2")
doc <- body_add_par(doc, "提出 finished-product-issue-driven hierarchical batch diagnosis，即成品问题驱动的层级批次诊断框架。该框架以明确的成品质量问题作为诊断入口，逐层连接时间窗口、成品 MES、浸膏粉质量、陈皮质量和山药粉 MES 过程记录，最终形成从成品异常到上游风险路径的可解释证据链。", style = "Normal")
doc <- body_add_par(doc, "层级贡献汇总", style = "heading 2")
doc <- body_add_flextable(doc, flextable(framework_summary) |> fontsize(size = 7, part = "all") |> autofit())
doc <- body_add_par(doc, "验证与边界", style = "heading 2")
doc <- body_add_flextable(doc, flextable(validation_summary) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "重复交叉验证结果", style = "heading 2")
doc <- body_add_flextable(doc, flextable(cv_summary) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "建议主图结构", style = "heading 2")
doc <- body_add_flextable(doc, flextable(recommended_figures) |> fontsize(size = 8, part = "all") |> autofit())
doc <- body_add_par(doc, "论文写法边界", style = "heading 2")
doc <- body_add_par(doc, "这篇论文应定位为 retrospective hierarchical diagnosis framework。可以强调该框架提高了真实世界制造数据的可解释诊断能力，但不应声称已经建立可直接上线的前瞻预测模型，也不应把来源/供应商差异写成确定因果。", style = "Normal")
print(doc, target = file.path(docs_dir, "论文统一创新点与证据链说明.docx"))

message("Paper evidence-chain integration package completed.")
