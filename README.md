# TCM-MES Hierarchical Batch Diagnosis

Code repository for a manufacturing-execution-system-enabled hierarchical batch diagnosis framework for real-world traditional Chinese medicine manufacturing.

The repository provides reusable analysis scripts for organizing quality-control data, MES production records, upstream material-quality records, process-material records, batch linkage, layered screening, hierarchical modeling, temporal adjustment, causal-informed decomposition, and graph-based batch evidence scoring.

> The manuscript based on this project is still in preparation. Please do not cite this repository as a published study.

## Framework overview

![Dataset overview](assets/dataset_overview.svg)

## Repository scope

This repository is intended to document the code structure and analysis design. It does not report confidential study results and does not include raw manufacturing data.

Raw data and analysis-ready datasets are not open-sourced because they contain company-confidential manufacturing, quality-control, MES, and batch-traceability records. Access to the underlying data may be requested from the authors and is subject to company approval and confidentiality requirements.

## Repository structure

- `analysis/00_dataset_overview`: dataset dictionary and batch-linkage overview scripts.
- `analysis/01_finished_product_issue`: finished-product quality description and issue-definition scripts.
- `analysis/02_tablet_mes_description`: tablet MES descriptive-analysis scripts.
- `analysis/03_tablet_mes_association`: tablet MES and finished-product quality association scripts.
- `analysis/04_extract_powder_description`: extract-powder quality descriptive-analysis scripts.
- `analysis/05_extract_powder_association`: extract-powder and finished-product quality association scripts.
- `analysis/06_chenpi_description_and_association`: Chenpi quality, source comparison, and downstream-linkage scripts.
- `analysis/07_yam_powder_description`: Chinese yam powder MES descriptive-analysis scripts.
- `analysis/08_yam_powder_association`: Chinese yam powder MES and finished-product quality association scripts.
- `analysis/09_issue_chain_summary`: integrated issue-driven evidence-chain summary scripts.
- `analysis/10_joint_modeling`: hierarchical joint-modeling scripts.
- `analysis/11_temporal_analysis`: temporal-pattern and abnormal-window assessment scripts.
- `analysis/12_causal_informed_analysis`: causal-informed path-decomposition and robustness scripts.
- `analysis/13_graph_evidence_scoring`: graph-based batch evidence scoring and upstream-prioritization scripts.
- `analysis/14_framework_figures`: manuscript framework and evidence-chain figure scripts.
- `manuscript`: scripts used to generate manuscript-supporting documents and tables.
- `data/README.md`: local data placement and confidentiality note.

## Analysis design

The workflow is organized around a finished-product-issue-driven diagnostic logic:

1. Define a finished-product quality issue as the diagnostic entry point.
2. Link finished-product quality records to MES production records.
3. Trace related batches to upstream intermediate, material, and process-material layers.
4. Screen candidate process and material-quality signals within each layer.
5. Evaluate diagnostic gain from hierarchical data integration.
6. Account for temporal windows and batch-level confounding.
7. Convert multi-layer evidence into graph-based upstream batch prioritization.

## Methods implemented

- Data dictionary construction and variable-level completeness summaries.
- Batch-linkage and traceability summaries.
- Layer-wise descriptive statistics.
- Spearman correlation and Wilcoxon rank-sum testing.
- Benjamini-Hochberg false-discovery-rate correction.
- Hierarchical diagnostic modeling.
- Regularized regression and tree-based sensitivity modeling.
- Temporal-window analysis.
- Causal-informed path decomposition.
- Bootstrap batch-level robustness checks.
- Graph-based batch evidence scoring.

## Data availability

No raw or standardized data files are included in this repository.

The scripts expect local analysis-ready datasets. Users with authorized access should place the required datasets in a local data directory or modify the input paths in each script.

## How to cite

The associated manuscript is not yet published. Citation information will be added after publication.

