"""Frozen-data cohort representativeness and latest-window retrospective internal validation.

Reads locked D2/D3, the authorized joint matrix, and saved forward predictions.
Does not tune or refit models; all outputs are a separate dated evidence package.
"""
from pathlib import Path
import hashlib
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
CUR = ROOT / '03_当前研究_数字孪生'
DATA = CUR / '01_定稿数据/定稿数据_英文'
RES = CUR / '03_分析结果与论文图'
OUT = RES / '代表性与最新内部验证_20260929'
OUT.mkdir(parents=True, exist_ok=True)

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def key(series):
    return series.astype(str).str.strip().str.replace(r'\.0$', '', regex=True)

source_files = [DATA/'D2_finished_product_physchem_final.xlsx',
                DATA/'D3_finished_product_mes_main_final.xlsx',
                RES/'tables/03_joint_model_matrix.csv',
                RES/'quality_assessment_20260907/03_predictions.csv',
                RES/'quality_assessment_20260907/04_fit_audit.csv']
provenance = {str(p.relative_to(ROOT)): sha(p) for p in source_files}

d2 = pd.read_excel(source_files[0])
d3 = pd.read_excel(source_files[1])
d2 = d2[d2.dosage_strength.astype(str).str.replace(r'\s', '', regex=True).eq('0.8g')].copy()
d2['batch'] = key(d2.batch_no)
d3['batch'] = key(d3.batch_no)
assert len(d2)==4377 and d2.batch.nunique()==4296
assert len(d3)==d3.batch.nunique()==1608
assert d2[['disintegration_time_min','production_date']].notna().all().all()
assert not d2.batch.isna().any()

cohort = d2.groupby('batch', sort=False).agg(
    y=('disintegration_time_min','mean'),
    qc_observation_date=('production_date','max'),
    qc_records=('disintegration_time_min','size'))
cohort['qc_observation_date'] = pd.to_datetime(cohort.qc_observation_date)
cohort['qc_month'] = cohort.qc_observation_date.dt.to_period('M').astype(str)
cohort['mes_linked'] = cohort.index.isin(set(d3.batch))
cohort['batch_mean_gt10'] = cohort.y.gt(10)
assert cohort.mes_linked.sum()==1477
assert cohort.qc_records.sum()==4377

# All 24 months are shown. MES source starts in April 2025; earlier zeros are source-coverage zeros.
monthly = cohort.groupby(['qc_month','mes_linked'], observed=True).agg(
    batches=('y','size'), mean_min=('y','mean'), median_min=('y','median'),
    batch_mean_gt10_n=('batch_mean_gt10','sum'),
    batch_mean_gt10_rate=('batch_mean_gt10','mean')).reset_index()
totals = cohort.groupby('qc_month').size().rename('full_month_batches')
monthly = monthly.join(totals, on='qc_month')
monthly['share_of_full_month'] = monthly.batches/monthly.full_month_batches
monthly.to_csv(OUT/'01_逐月连接与质量分布.csv',index=False,encoding='utf-8-sig')

coverage = cohort.groupby('qc_month').agg(
    full_qc_batches=('y','size'), mes_linked_batches=('mes_linked','sum'))
coverage['linked_rate'] = coverage.mes_linked_batches/coverage.full_qc_batches
coverage.reset_index().to_csv(OUT/'02_逐月MES连接率.csv',index=False,encoding='utf-8-sig')

# Compare contemporaneous batches, using the full cohort's month mix as common weights.
common = cohort[cohort.qc_month.between('2025-04','2026-07')].copy()
period = common.groupby('mes_linked').agg(n=('y','size'),mean_min=('y','mean'),
   median_min=('y','median'),batch_mean_gt10_n=('batch_mean_gt10','sum'),
   batch_mean_gt10_rate=('batch_mean_gt10','mean'))
assert period.index.tolist()==[False,True] and period.n.sum()==len(common)
weights = common.qc_month.value_counts(normalize=True).sort_index()
strata = common.groupby(['qc_month','mes_linked']).agg(
    n=('y','size'),mean_min=('y','mean'),batch_mean_gt10_rate=('batch_mean_gt10','mean'))
assert all(strata.loc[(m,False),'n']>0 and strata.loc[(m,True),'n']>0 for m in weights.index)
standardized={}
for linked in [False,True]:
    standardized[linked]={k:float(sum(weights[m]*strata.loc[(m,linked),k] for m in weights.index))
                         for k in ['mean_min','batch_mean_gt10_rate']}
summary = {
 'all_finished_qc_batches':len(cohort),'qc_records':len(d2),
 'mes_linked_batches':int(cohort.mes_linked.sum()),
 'overall_mes_linked_rate':float(cohort.mes_linked.mean()),
 'contemporaneous_period_qc_observation_months':'2025-04 to 2026-07',
 'contemporaneous_batches':len(common),
 'linked':{k:(int(v) if k.endswith('_n') or k=='n' else float(v)) for k,v in period.loc[True].to_dict().items()},
 'unlinked':{k:(int(v) if k.endswith('_n') or k=='n' else float(v)) for k,v in period.loc[False].to_dict().items()},
 'raw_linked_minus_unlinked_mean_min':float(period.loc[True,'mean_min']-period.loc[False,'mean_min']),
 'raw_linked_minus_unlinked_event_rate_pp':float(100*(period.loc[True,'batch_mean_gt10_rate']-period.loc[False,'batch_mean_gt10_rate'])),
 'full_month_mix_standardized_linked':standardized[True],
 'full_month_mix_standardized_unlinked':standardized[False],
 'standardized_linked_minus_unlinked_mean_min':standardized[True]['mean_min']-standardized[False]['mean_min'],
 'standardized_linked_minus_unlinked_event_rate_pp':100*(standardized[True]['batch_mean_gt10_rate']-standardized[False]['batch_mean_gt10_rate']),
 'month_rate_min':float(coverage.loc['2025-04':'2026-07','linked_rate'].min()),
 'month_rate_max':float(coverage.loc['2025-04':'2026-07','linked_rate'].max()),
}

matrix = pd.read_csv(source_files[2])
matrix['finished_batch'] = key(matrix.finished_batch)
matrix = matrix.sort_values(['mes_production_date','finished_batch'],kind='stable').reset_index(drop=True)
matrix['id'] = ['Q-%04d'%i for i in range(1,len(matrix)+1)]
assert len(matrix)==1477 and matrix.finished_batch.nunique()==1477
assert matrix.id.is_unique
joined = matrix.join(cohort[['y','qc_observation_date']].rename(columns={'y':'y_d2'}),on='finished_batch')
assert joined.y_d2.notna().all()
assert np.allclose(joined.disintegration_time_min,joined.y_d2,atol=1e-10)

pred = pd.read_csv(source_files[3]);fit = pd.read_csv(source_files[4])
latest = pred[(pred.endpoint=='continuous_mean')&(pred.window==4)].copy()
latest = latest.merge(joined[['id','mes_production_date','qc_observation_date','y_d2']],on='id',validate='many_to_one')
latest.mes_production_date = pd.to_datetime(latest.mes_production_date)
assert latest.month.eq('2026-07').all()
assert latest.id.nunique()==73
assert latest.groupby(['delay_days','model']).size().eq(73).all()
assert np.allclose(latest.y,latest.y_d2,atol=1e-10)
assert latest.mes_production_date.between('2026-07-01','2026-07-31').all()
assert latest.mes_production_date.nunique()==11
latest['error'] = latest.p-latest.y
latest['abs_error'] = latest.error.abs()
latest['squared_error'] = latest.error**2

# The original Q IDs are pseudonyms; no factory batch numbers are exported.
latest[['delay_days','endpoint','window','id','month','mes_production_date','qc_observation_date','model','y','p','error','abs_error']].to_csv(
    OUT/'03_七月逐批预测与质检核对_匿名.csv',index=False,encoding='utf-8-sig')
metrics = latest.groupby(['delay_days','model']).agg(
    n=('id','nunique'),MAE=('abs_error','mean'),MSE=('squared_error','mean'),
    mean_signed_error=('error','mean'),median_abs_error=('abs_error','median'))
metrics['RMSE'] = np.sqrt(metrics.MSE)
metrics.reset_index().drop(columns='MSE').to_csv(OUT/'04_七月内部时间验证指标.csv',index=False,encoding='utf-8-sig')

# Last 3 months are included as context; windows 3 and 4 use separate frozen cutoffs.
context = pred[(pred.endpoint=='continuous_mean') & pred.window.isin([3,4])].copy()
context['abs_error'] = (context.p-context.y).abs()
context['squared_error'] = (context.p-context.y)**2
ctx = context.groupby(['delay_days','window','model']).agg(n=('id','nunique'),MAE=('abs_error','mean'),MSE=('squared_error','mean'))
ctx['RMSE']=np.sqrt(ctx.MSE)
ctx.reset_index().drop(columns='MSE').to_csv(OUT/'05_五月至七月两个冻结窗口对照.csv',index=False,encoding='utf-8-sig')

# Paired fixed-prediction differences; resample 11 production days, not individual rows.
rng=np.random.default_rng(20260929)
rows=[]
for delay in [0,14]:
    z=latest[latest.delay_days==delay].pivot(index='id',columns='model',values='abs_error')
    days=latest[(latest.delay_days==delay)&latest.model.eq('MES')].set_index('id').mes_production_date
    assert len(z)==73 and len(days)==73 and z.notna().all().all()
    unique_days=np.array(sorted(days.unique()))
    for a,b in [('MES','recent60_mean'),('MES','historical_mean'),('MES_material','MES'),('material','recent60_mean')]:
        delta=z[a]-z[b]
        boot=[]
        for _ in range(5000):
            draw=rng.choice(unique_days,size=len(unique_days),replace=True)
            positions=np.concatenate([np.where(days.to_numpy()==day)[0] for day in draw])
            boot.append(delta.to_numpy()[positions].mean())
        lo,hi=np.quantile(boot,[.025,.975])
        rows.append({'delay_days':delay,'first_model':a,'reference_model':b,
            'delta_MAE_first_minus_reference':float(delta.mean()),
            'day_cluster_bootstrap_low':float(lo),'day_cluster_bootstrap_high':float(hi),
            'n_batches':len(delta),'n_production_days':len(unique_days),'resamples':5000})
paired=pd.DataFrame(rows)
paired.to_csv(OUT/'06_七月成对误差差值_按生产日重采样.csv',index=False,encoding='utf-8-sig')

fa=fit[(fit.endpoint=='continuous_mean')&(fit.window==4)].copy()
assert len(fa)==12 and fa.test_n.eq(73).all()
assert set(fa.delay_days)=={0,14}
fa.to_csv(OUT/'07_七月模型训练与状态审计.csv',index=False,encoding='utf-8-sig')

checks={
 'input_sha256':provenance,
 'd2_count_reconciled':True,'batch_grain_reconciled':True,
 'D2_D3_unique_overlap':1477,'matrix_and_D2_y_agree_for_1477':True,
 'July_predictions_and_D2_y_agree_for_73':True,
 'July_test_n':73,'July_production_days':11,
 'July_train_n_by_assumed_delay':{str(k):int(v) for k,v in fa.groupby('delay_days').train_n.first().items()},
 'evaluation_cutoff':'2026-07-01','model_selection_source':'existing 2026-09-07 analysis',
 'selection_caveat':'July predictions were frozen at the window cutoff, but this test month has already appeared in earlier project analyses.',
 'time_caveat':'QC observation date is not confirmed result-signoff date; MES process completion and material result timing remain unverified.',
 'inference_caveat':'Day-cluster intervals condition on fixed predictions and only 11 July production days; no model refit or external generalization.',
}
(OUT/'00_质量与来源核查.json').write_text(json.dumps(checks,ensure_ascii=False,indent=2),encoding='utf-8')
(OUT/'08_代表性关键结果.json').write_text(json.dumps(summary,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps({'summary':summary,'latest_metrics':metrics.reset_index()[['delay_days','model','MAE','RMSE']].to_dict('records'),
                  'paired':paired.to_dict('records')},ensure_ascii=False,indent=2))
