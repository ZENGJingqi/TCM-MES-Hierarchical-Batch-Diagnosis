# Code manifest

## Dataset overview

- `analysis/00_dataset_overview/code/create_dataset_overview_dictionary.py`: creates dataset-level summaries and variable dictionaries.

## Finished-product issue definition

- `analysis/01_finished_product_issue/code/run_d2_description.R`: describes finished-product quality attributes and monthly trends.
- `analysis/01_finished_product_issue/code/run_d2_issue_group_single_plots.R`: creates separate plots comparing predefined finished-product issue groups.

## Layer-wise description and association

- `analysis/02_tablet_mes_description/code/run_d3_description.R`: describes tablet MES production records.
- `analysis/03_tablet_mes_association/code/run_d3_mes_disintegration_association.R`: screens tablet MES process variables against finished-product disintegration.
- `analysis/04_extract_powder_description/code/run_d4_description.R`: describes Jianwei Xiaoshi extract-powder quality data.
- `analysis/05_extract_powder_association/code/run_d4_finished_disintegration_association.R`: links extract-powder quality data to finished-product disintegration.
- `analysis/06_chenpi_description_and_association/code/run_d6_finished_disintegration_association.R`: links Chenpi quality records to downstream finished-product disintegration.
- `analysis/06_chenpi_description_and_association/code/run_d6_origin_comparison.R`: compares Chenpi quality attributes by source/origin.
- `analysis/07_yam_powder_description/code/run_d7_description.R`: describes Chinese yam powder MES production records.
- `analysis/08_yam_powder_association/code/run_d7_finished_disintegration_association.R`: links Chinese yam powder MES variables to downstream finished-product disintegration.

## Modeling

- `analysis/10_joint_modeling/code/run_joint_disintegration_modeling.R`: builds hierarchical diagnostic models.
- `analysis/10_joint_modeling/code/audit_time_and_origin_design_inputs.R`: audits temporal/origin variables for later sensitivity analysis.
- `analysis/10_joint_modeling/code/audit_confounding_batch_structure.R`: audits upstream-downstream batch multiplicity and confounding-aware analysis feasibility.

## Temporal and confounding-aware analysis

- `analysis/11_temporal_analysis/code/run_finished_disintegration_time_series.R`: analyzes monthly disintegration patterns and abnormal temporal windows.
- `analysis/12_confounding_aware_analysis/code/run_confounding_aware_batch_decomposition.R`: decomposes batch-level evidence under temporal-window exposure.
- `analysis/12_confounding_aware_analysis/code/run_confounding_aware_robust_path_analysis.R`: performs confounding-aware robustness and path-attenuation analysis.

## Graph evidence scoring

- `analysis/13_graph_evidence_scoring/code/run_batch_evidence_graph_scoring.R`: computes graph-based evidence scores and upstream risk-priority rankings.

## Public framework asset

- `assets/framework_design.svg`: public schematic of the analysis framework. It is included for orientation only and does not contain confidential data.

## Public-release boundary

This repository intentionally excludes raw data, standardized analysis-ready data, generated result workbooks, draft documents, writing utilities, and confidential batch-level outputs. The code documents the analysis workflow and public framework design. Users need authorized local datasets to reproduce the full analysis.
