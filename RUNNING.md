# Running the current analysis source

## Environment and verification

The original analyses used R 4.5.1 and Python scientific packages. Required R packages inferred from the current released source are listed in `R_PACKAGES.md`; Python requirements are in `requirements.txt`. Exact historical environment versions must be checked against an authorized local run's session information. This release does not replace an environment lockfile.

For the October 5 update, all released R files are syntax-parsed and Python files are parsed without executing data analysis. Source hashes are checked against the local project. No model fitting or end-to-end rerun was performed for the publication update. Missing protected inputs should produce an error, not simulated results.

## Execution entry points

Let `CODE=analysis/current_project/03_当前研究_数字孪生/02_分析代码` and let `<stage>` be a separate authorized local staging directory, outside this repository. Use a fresh stage and fresh output directory for each run. Generated outputs must remain untracked.

| Step | Entry point relative to CODE | Staging rule |
| --- | --- | --- |
| Data/matrix reconstruction | `重分析_20260820/run_full_reanalysis.R` | Read its `TCM_*` environment variables and authorized dataset requirements first |
| Historical source thread | `NC平台前分析_20260827/01_module_A_digital_thread_audit.R` | Original `TCM_NC_*` staging contract |
| Continuous and secondary-event frozen evaluation | `质量评估完善_20260907/analysis.R` | Set `TCM_QUALITY_STAGE=<stage>`; provide `joint.csv` |
| Secondary-event state updating | `状态完善_20260907/analysis.R` | Set `TCM_STATE_STAGE=<stage>`; provide `joint.csv` and `quality.R` copied from the preceding entry |
| Same-information-clock comparisons | `同信息时钟比较_20260907/analysis.R` | Set `TCM_MATCH_STAGE=<stage>`; provide the saved prediction files named in source |
| Shared-source sensitivity | `上游依赖敏感性_20260923/analysis.R` | Set `TCM_P1_DEP_STAGE=<stage>`; provide `joint.csv`, authorized upstream files and the original `p1.R` construction dependency |
| Frozen residual modeling | `时期与批次信息分解_20260930/analysis.R` | `Rscript <entry> <stage>`; provide `joint.csv` and original frozen predictions |
| Update-frequency, bounded tree and wrong-match tests | `计算验证扩展_20261001/analysis.R` | `Rscript <entry> <stage>`; provide `joint.csv`, `old_predictions.csv`, `residual_predictions.csv`, `v4_helpers.R`, and sibling `calibrate.R` |
| Independent history-length baseline recomputation | `近期历史长度敏感性_20261001/verify.R` | `Rscript <entry> <stage>`; provide `joint.csv`; writes all 30/60/90-day frozen/daily predictions |
| Cohort representativeness and saved-prediction summaries | `代表性与最新内部验证_20260929/analyze.py` | Original relative authorized project structure under `analysis/current_project/` is required |

`p1.R` is a copy of `重分析_20260823/run_p1_upstream_robustness.R`; the dependency-sensitivity stage also requires the saved `original.csv` and authorized D2-D7 workbooks.

`v4_helpers.R` is a copy of `时期与批次信息分解_20260930/analysis.R`. The October experiment imports only named function assignments from that file, not its old execution or writes. `calibrate.R` must be beside the staged `analysis.R`.

The above table identifies entry points, not a promise that arbitrary datasets match this study. Read `docs/INPUT_CONTRACT.md` and each script in full. Cohort assertions intentionally remain unchanged. Some upstream audit and historical modules require additional authorized intermediate files. The repository does not provide these inputs, batch mappings or saved individual predictions.

## Historical workflow

The older diagnosis modules are kept intact. Their original instructions are in `docs/HISTORICAL_RUNNING.md`; their binary issue endpoint and results must not be substituted for the current continuous batch-mean analysis.
