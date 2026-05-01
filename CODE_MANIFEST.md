# Code manifest

## Dataset overview

- `analysis/00_dataset_overview/code/create_dataset_overview_dictionary.py`: creates dataset-level summaries and variable dictionaries.
- `analysis/00_dataset_overview/code/create_dataset_overview_figure.R`: creates the dataset architecture and batch-linkage overview figure.

## Finished-product issue definition

- `analysis/01_finished_product_issue/code/run_d2_description.R`: describes finished-product quality attributes and monthly trends.
- `analysis/01_finished_product_issue/code/run_d2_issue_group_single_plots.R`: creates separate plots comparing predefined finished-product issue groups.
- `analysis/01_finished_product_issue/code/write_d2_formal_results.py`: writes formal finished-product results summaries.

## Layer-wise description and association

- `analysis/02_tablet_mes_description/code/run_d3_description.R`: describes tablet MES production records.
- `analysis/03_tablet_mes_association/code/run_d3_mes_disintegration_association.R`: screens tablet MES process variables against finished-product disintegration.
- `analysis/04_extract_powder_description/code/run_d4_description.R`: describes Jianwei Xiaoshi extract-powder quality data.
- `analysis/05_extract_powder_association/code/run_d4_finished_disintegration_association.R`: links extract-powder quality data to finished-product disintegration.
- `analysis/06_chenpi_description_and_association/code/run_d6_finished_disintegration_association.R`: links Chenpi quality records to downstream finished-product disintegration.
- `analysis/06_chenpi_description_and_association/code/run_d6_origin_comparison.R`: compares Chenpi quality attributes by source/origin.
- `analysis/07_yam_powder_description/code/run_d7_description.R`: describes Chinese yam powder MES production records.
- `analysis/08_yam_powder_association/code/run_d7_finished_disintegration_association.R`: links Chinese yam powder MES variables to downstream finished-product disintegration.

## Integrated issue-chain analysis

- `analysis/09_issue_chain_summary/code/build_issue_chain_summary.py`: assembles the issue-driven evidence-chain summary.

## Modeling

- `analysis/10_joint_modeling/code/run_joint_disintegration_modeling.R`: builds hierarchical diagnostic models.
- `analysis/10_joint_modeling/code/make_joint_model_publication_outputs.R`: creates publication-oriented model-performance summaries.
- `analysis/10_joint_modeling/code/audit_time_and_origin_design_inputs.R`: audits temporal/origin variables for later sensitivity analysis.
- `analysis/10_joint_modeling/code/audit_causal_batch_structure.R`: audits upstream-downstream batch multiplicity and causal-analysis feasibility.

## Temporal and causal-informed analysis

- `analysis/11_temporal_analysis/code/run_finished_disintegration_time_series.R`: analyzes monthly disintegration patterns and abnormal temporal windows.
- `analysis/12_causal_informed_analysis/code/run_causal_batch_decomposition.R`: decomposes batch-level evidence under temporal-window exposure.
- `analysis/12_causal_informed_analysis/code/run_causal_robust_path_analysis.R`: performs causal-informed robustness and path-attenuation analysis.

## Graph evidence scoring

- `analysis/13_graph_evidence_scoring/code/run_batch_evidence_graph_scoring.R`: computes graph-based evidence scores and upstream risk-priority rankings.

## Manuscript figures and documents

- `analysis/14_framework_figures/code/build_framework_and_evidence_chain_figures_v3.R`: creates the framework and final evidence-chain figures used in the manuscript design.
- `analysis/14_framework_figures/code/build_paper_evidence_chain_package.R`: creates paper-level evidence-chain support outputs.
- `manuscript/code/create_full_submission_design_docx.py`: assembles a manuscript-support design document with key figures and support-material structure.
- `manuscript/code/create_supplementary_table_s2.py`: creates the layer-wise batch-linkage supplementary table.
- `manuscript/code/enhance_manuscript_0426.py`: retained as a public placeholder explaining that result-filled manuscript drafting code is not distributed.

## Public-release boundary

This repository intentionally excludes raw data, standardized analysis-ready data, generated result workbooks, manuscript drafts, and confidential batch-level outputs. The code documents the analysis workflow and public framework design. Users need authorized local datasets to reproduce the full analysis.
