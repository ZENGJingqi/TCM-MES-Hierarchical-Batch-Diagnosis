# Authorized input contract

## Confidentiality

Place authorized inputs in a local staging directory outside Git. Do not upload raw data, standardized Excel files, individual predictions, QC outcomes, batch keys, upstream mappings, model objects or private project audit records. The `data/` directory contains documentation only.

## Joint matrix

The staged `joint.csv` uses a unique `finished_batch`, `mes_production_date`, `d2_observation_date`, continuous `disintegration_time_min`, and separate `disintegration_issue`. Original date interpretation and outcome aggregation must be retained.

Core MES fields are `coating_yield_pct`, `final_blend_moisture_pct`, `coating_mass_balance_pct`, `compression_yield_pct`, `compression_hardness_mean_n`, `coated_tablet_weight_mean_g`, `final_blend_lt_100_mesh_pct`, `granulation_discharge_moisture_pct_mean`, and `core_tablet_weight_mean_g`.

Frozen material comparisons require the exact source-derived material fields named in the original script. Material relations and upstream QC/MES files are distinct inputs, not inferred from a single finished-product matrix. Null values must not be changed to zero merely to satisfy parsing.

## Saved predictions and helper scripts

October computational tests read original frozen predictions and frozen residual predictions, with continuous endpoint labels retained. A stage must contain `v4_helpers.R` copied from the released September 30 residual-model source and the sibling `calibrate.R`. The staged saved-prediction files are confidential and are not supplied here.

Python audit scripts preserve their original project-relative input paths. Reconstruct those folders under `analysis/current_project/` only with authorized private files; keep every generated data file ignored. Read each `source_files` declaration before running. Original study cohort assertions are intentional and should not be removed to manufacture a passing run.

## Frozen statistical panel objects

The plotting entry takes a directory containing the following authorized objects, without a private project-root assumption:

- A-C objects for Figure 2, Figure 3 and Figure S4.
- A-D objects for Figure 4, Figure 5, Figure S1 and Figure S5.
- A-B objects for Figure 6, Figure S2, Figure S6, Figure S7 and Figure S8.
- One unlettered object, `Figure_S3/Figure_S3.rds`.

Each object is in its figure directory and repeats that prefix (for example, `Figure_S4/Figure_S4_C.rds`). These 36 objects retain data and styling; do not treat them as non-sensitive presentation files. They are not distributed. The assembler alone cannot recreate numerical panels from absent measurements. Only trusted authorized serialized objects should be loaded. Figure 1 is outside this contract.

## Known preprocessing conventions

The previously identified milling material-balance entry has already been corrected in the finalized source while retaining its batch. Microbial values reported below 10 are encoded as 10 according to the author's convention; reporting-limit limitations remain. Neither convention licenses a general deletion or median replacement of all extreme measurements.

## Additional full-QC and EWMA module

`analysis/information_value_20261012/run.py prepare` takes `--study-root` explicitly. This root is an authorized private copy of the current study, not this public repository. It reads:

- `01_定稿数据/定稿数据_英文/D2_finished_product_physchem_final.xlsx`
- `01_定稿数据/定稿数据_英文/D3_finished_product_mes_main_final.xlsx`
- `03_分析结果与论文图/tables/03_joint_model_matrix.csv`
- `03_分析结果与论文图/计算验证扩展_20261001/01_predictions.csv`
- `03_分析结果与论文图/计算验证扩展_20261001/03_fit_audit.csv`
- `02_分析代码/时期与批次信息分解_20260930/analysis.R`
- `02_分析代码/信息价值评价补强_20261012/README.md`, the fixed private analysis protocol.

The protocol SHA-256 used in the reviewed run is `4d6bbb9cef5ba027a5d06f3eddee2899b3b806eee4fb01bd62621638507f0184`. A source hash records identity, not ethical approval or actual data availability. Staging writes `qc_pool.csv`, `joint.csv`, `legacy_predictions.csv`, `legacy_fit.csv`, `v4_helpers.R`, `00_input_QA.json` and a private batch mapping. These outputs must remain outside this repository and must not be uploaded. Preparation retains the study's cohort and endpoint assertions. It neither anonymizes the complete enterprise data nor invents absent timestamps.

`fit` requires a prepared stage and a nonexistent `output` directory. `audit` reads saved fitter outputs, including histories/members, fitted coefficients/preprocessing, training memberships and the original fit audit; it writes verification and aggregate summaries in the same private stage. Work on a copy for a preserved run. The module README describes the actual cutoff assumptions and residual inner-calendar refinement.
