# Additional information-value comparisons

Code for retrospective QC-history and MES process-feature comparisons. This module adds linked/full-QC history pools, frozen/daily 60-day means, fixed-weight EWMA references, matched full-QC residual comparisons and assessment-record checks.

`analysis.R` and `verify.py` are byte-identical copies of the reviewed local analytical sources. `prepare.py` preserves data reconciliation and sorting but accepts an explicit authorized study root; `run.py` supplies portable entry points and refuses repository-local analysis outputs. All outputs are confidential local analysis material. The module does not create a public dataset or generate industrial observations.

## Authorized inputs

Preparation needs the protected finalized D2/D3 workbooks, the original joint matrix, saved original predictions and fit audit, the earlier residual-model helper and the fixed analysis-protocol file. File names and relative placement are in `docs/INPUT_CONTRACT.md`. Batch-level inputs, model fits, predictions and protocol working files are not shipped in the repository.

Run from the repository root, choosing private directories outside this checkout:

```sh
python analysis/information_value_20261012/run.py prepare --study-root /authorized/current-study --output-dir /private/new-stage
python analysis/information_value_20261012/run.py fit --stage /private/new-stage --rscript Rscript
python analysis/information_value_20261012/run.py audit --stage /private/new-stage
```

Preparation only stages inputs. Fitting writes a new `output` directory and refuses existing fitted outputs. Audit independently checks histories, EWMA recursion, new-model coefficients, outcome/cutoff references and paired summaries, and writes verification products into that private stage. Use a copy when auditing a preserved run.

## Fixed settings and limits

The analysis retains both 0/14-day assumed delays, four previously reviewed windows, a 60-day recent mean, EWMA weight 0.2, elastic-net alpha 0.5 and the original 35-penalty grid. Additional conditional intervals use 2,000 fixed-prediction month-bootstrap draws, seed 20261012. Earlier intervals are retained separately. New residual inner cutoffs use calendar-day flooring, so changing the QC pool is not a one-factor ablation.

The full QC pool includes batches without a MES production date. QC observation dates are not verified result-signoff times. All evaluations are exploratory and internal. Computational assessment traceability does not establish actual information availability, factory fault-tracing success, real-time deployment, causal effects or production benefit. Original daily fitted weights were not recovered.

Release checks and syntax parsing are data-free. An authorized local preparation and audit replay can be checked against preserved results, but this is not a clean-machine full refit or independent external validation. The original figure assembler is retained for historical panels; it does not assemble the latest revised Figures 2/3/5/6/S9. Manuscript-production scripts and figures remain excluded.
