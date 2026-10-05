# Current methods and evidence boundaries

The primary displayed endpoint is continuous batch-mean disintegration time. A separate secondary event records whether any QC record exceeds 10 minutes; it is not a batch-release failure definition. Batch, QC-record, source-batch and relationship-row counts are different grains and are not additive sample sizes.

At a scoring cutoff, both production date and assumed label-availability date must precede the cutoff strictly. Assumed availability is QC observation date plus 0 or 14 days, not a verified signoff time. QC observations earlier than MES production are excluded from historical training eligibility rather than silently treated as future production evidence. Same-day labels are unavailable to scoring batches.

The main recent-history duration is 60 days; 30 and 90 days are bounded baseline sensitivity checks, not optimized durations. Frozen and daily predictions have distinct update frequencies. Elastic-net residuals, residual intercepts and the bounded shallow-tree analysis use the documented past-only preprocessing and inner temporal splits.

Uncertainty summaries from saved predictions are conditional on those predictions and do not include full refitting uncertainty or every shared-source dependency. Empirical historical-error intervals can under-cover their nominal level. Training-range flags indicate a simple review score, not validated OOD classification. Restricted wrong-match repeats preserve only the specific staging constraints in code; they do not define exact permutation significance.

The upstream and process association families remain descriptive or exploratory. Do not reselect predictors, mix unrelated FDR families, remove batches after viewing test errors, claim causal effects, describe July as untouched external validation, or infer production benefits from retrospective replay. Earlier event-state and platform-oriented scripts are provenance, not independent proof of live digital-twin operation.
