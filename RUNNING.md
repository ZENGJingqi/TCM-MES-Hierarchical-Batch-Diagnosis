# Running the analysis

This repository is a code-only release. It is intended to document and reproduce the analysis workflow when the confidential analysis-ready datasets are available locally.

## Local data folders

The original scripts were run from the project root and typically used these local folders:

- `定稿数据_英文`
- `定稿数据_中文`

These folders are intentionally not included. To rerun the scripts, either:

1. Recreate these folders locally with the analysis-ready data files, or
2. Edit the input path variables near the top of each script to point to your local data location.

## Suggested run order

1. `analysis/00_dataset_overview`
2. `analysis/01_finished_product_issue`
3. `analysis/02_tablet_mes_description`
4. `analysis/03_tablet_mes_association`
5. `analysis/04_extract_powder_description`
6. `analysis/05_extract_powder_association`
7. `analysis/06_chenpi_description_and_association`
8. `analysis/07_yam_powder_description`
9. `analysis/08_yam_powder_association`
10. `analysis/09_issue_chain_summary`
11. `analysis/10_joint_modeling`
12. `analysis/11_temporal_analysis`
13. `analysis/12_causal_informed_analysis`
14. `analysis/13_graph_evidence_scoring`
15. `analysis/14_framework_figures`
16. `manuscript`

## Notes

- R scripts generate figures and Excel summary tables in local output folders.
- Python scripts generate dataset dictionaries, Word summaries, and manuscript-supporting documents.
- Generated outputs are ignored by `.gitignore` and should not be committed unless a journal or reviewer specifically requests non-confidential derived artifacts.

