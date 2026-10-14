# Current batch reconstruction and information-value code manifest

Seventeen computational source files are copied byte-for-byte from the current project, with source identity rechecked on October 6. No models were refitted for this repository update.

The October 14 update adds the additional QC-history comparisons below. The earlier seventeen scripts remain unchanged; the JSON manifest records separate added-module provenance and all current source hashes.

- `analysis/information_value_20261012/analysis.R`: byte-identical reviewed fitter.
- `analysis/information_value_20261012/verify.py`: byte-identical independent numerical checks and summaries.
- `analysis/information_value_20261012/prepare.py`: original reconciliation/sorting with explicit private-study input and external-output guards.
- `analysis/information_value_20261012/run.py`: portable private prepare/fit/audit entry.
- `tests/test_information_value_cli.py`: four data-free path-guard tests.

- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/重分析_20260820/run_full_reanalysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/NC平台前分析_20260827/01_module_A_digital_thread_audit.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/NC平台前分析_20260827/02_modules_DF_state_update_and_replay.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/NC平台前分析_20260827/03_module_E_time_lineage_isolation.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/NC平台前分析_20260827/04_module_B_state_changepoint_sensitivity.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/谱系依赖审计_20260907/audit.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/质量评估完善_20260907/analysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/状态完善_20260907/analysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/同信息时钟比较_20260907/analysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/上游依赖敏感性_20260923/analysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/代表性与最新内部验证_20260929/analyze.py`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/时期与批次信息分解_20260930/analysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/时期与批次信息分解_20260930/diagnostics.py`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/计算验证扩展_20261001/analysis.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/计算验证扩展_20261001/calibrate.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/近期历史长度敏感性_20261001/verify.R`
- `analysis/current_project/03_当前研究_数字孪生/02_分析代码/重分析_20260823/run_p1_upstream_robustness.R`

Exact source SHA-256 values are recorded in `CURRENT_CODE_MANIFEST.json`. Historical analysis modules remain unchanged and describe the earlier diagnostic workflow, not current manuscript results.

## Supporting code added October 6

- `plotting/reproduce_figures.R`: I/O adaptation of the local October 5 assembler. Explicit panel-root input replaces private paths; it checks files before export, refuses overwrite and defaults to 300-dpi PNG. The 13 statistical figure layout expressions and dimensions are unchanged. No RDS, data, figures or Figure 1 are included.
- `tools/verify_release.py`: standard-library-only tracked/staged source-hash, file-scope, Python-syntax and potential-credential checks; no private input or credential-file access.
- `tests/test_verify_release.py`: five data-free tests for file boundaries, unsafe paths and credential patterns.

The JSON distinguishes `scripts` (seventeen byte-identical analysis sources) from `supporting_scripts` (three additions). The adapted plot entry records its released hash and the original local assembler hash; it is not a byte-identical copy. No manuscript text or internal attachment/audit scripts are included.
