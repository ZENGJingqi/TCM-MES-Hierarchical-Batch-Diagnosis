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

## Known preprocessing conventions

The previously identified milling material-balance entry has already been corrected in the finalized source while retaining its batch. Microbial values reported below 10 are encoded as 10 according to the author's convention; reporting-limit limitations remain. Neither convention licenses a general deletion or median replacement of all extreme measurements.
