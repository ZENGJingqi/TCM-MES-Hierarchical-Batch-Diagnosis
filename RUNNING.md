# Running the analysis

This repository is a code-only release. It documents the analysis workflow and can be rerun only when authorized analysis-ready datasets are available locally.

## Local data folders

The original project used local Chinese and English analysis-ready data folders. These folders are intentionally not included. To rerun the scripts:

1. Prepare authorized local datasets for the five required data entities listed in `data/README.md`.
2. Place the files in a local data directory outside the Git repository, or in `data/` while keeping them untracked.
3. Edit the input path variables near the top of each script to point to your local data location.

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

- R scripts generate figures and summary tables in local output folders.
- Python scripts generate dataset dictionaries and manuscript-supporting tables or documents.
- Generated outputs are ignored by `.gitignore` and should not be committed unless they are non-confidential derived artifacts approved for release.
