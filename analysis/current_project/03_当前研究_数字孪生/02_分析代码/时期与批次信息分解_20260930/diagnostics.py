"""Independent fixed-prediction diagnostics and source/temporal checks."""
from pathlib import Path
import hashlib,json
import numpy as np
import pandas as pd

ROOT=Path(__file__).resolve().parents[3]
BASE=ROOT/'03_当前研究_数字孪生'
RES=BASE/'03_分析结果与论文图'
OUT=RES/'时期与批次信息分解_20260930'
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
old=pd.read_csv(RES/'quality_assessment_20260907/03_predictions.csv')
old=old[old.endpoint.eq('continuous_mean')].copy()
new=pd.read_csv(OUT/'01_residual_predictions.csv')
pred=pd.concat([old,new],ignore_index=True)
assert not pred.duplicated(['delay_days','model','id']).any()
assert pred.groupby(['delay_days','model']).size().eq(755).all()
assert pred.groupby('id').y.nunique().eq(1).all()
assert new.groupby(['delay_days','window']).id.nunique().tolist()==[226,230,226,73]*2
repro=pd.read_csv(OUT/'06_original_model_reproduction.csv')
assert len(repro)==16 and repro.max_prediction_difference.max()<1e-8
audit=pd.read_csv(OUT/'02_residual_fit_audit.csv')
assert (pd.to_datetime(audit.max_train_available)<pd.to_datetime(audit.cut)).all()
assert (pd.to_datetime(audit.max_train_production)<pd.to_datetime(audit.cut)).all()
hist=pd.read_csv(OUT/'05_training_history_baselines.csv')
assert hist[hist.eligible].prior_n.ge(30).all()
assert not hist[hist.eligible].prior_baseline.isna().any()
joint=pd.read_csv(RES/'tables/03_joint_model_matrix.csv').sort_values(['mes_production_date','finished_batch']).reset_index(drop=True)
joint['id']=[f'Q-{i+1:04d}' for i in range(len(joint))]
prod=pd.to_datetime(joint.mes_production_date)
obs=pd.to_datetime(joint.d2_observation_date)
target=joint.disintegration_time_min.to_numpy()
baseline_checks=[]
for delay in [0,14]:
    available=(obs+pd.Timedelta(days=delay)).mask(obs<prod)
    h=hist[hist.delay_days.eq(delay)].set_index('id').loc[joint.id]
    for i,cut in enumerate(prod):
        prior=(prod<cut)&(available<cut)
        recent=prior&(available>=cut-pd.Timedelta(days=60))
        value=target[recent if recent.any() else prior].mean() if prior.any() else np.nan
        assert int(prior.sum())==h.prior_n.iloc[i]
        assert int(recent.sum())==h.prior_recent_n.iloc[i]
        saved=h.prior_baseline.iloc[i]
        assert (np.isnan(value) and np.isnan(saved)) or abs(value-saved)<1e-10
    baseline_checks.append({'delay_days':delay,'independently_verified_training_baselines':len(joint)})
for r in audit.itertuples(index=False):
    available=(obs+pd.Timedelta(days=r.delay_days)).mask(obs<prod)
    cut=pd.Timestamp(r.cut)
    train=(prod<cut)&(available<cut)
    h=hist[hist.delay_days.eq(r.delay_days)].set_index('id').loc[joint.id]
    eligible=train&h.eligible.to_numpy()
    recent=train&(available>=cut-pd.Timedelta(days=60))
    rb=target[recent if recent.any() else train].mean()
    assert abs(rb-r.frozen_recent_mean)<1e-10
    assert int(eligible.sum())==r.residual_train_n
    assert abs(h.residual.to_numpy()[eligible].mean()-r.residual_training_mean)<1e-10
    reference=old[(old.delay_days==r.delay_days)&(old.window==r.window)&(old.model=='recent60_mean')]
    assert np.max(abs(reference.p-rb))<1e-10
    if r.model=='recent_residual_intercept':
        saved=new[(new.delay_days==r.delay_days)&(new.window==r.window)&(new.model==r.model)]
        assert np.max(abs(saved.p-(rb+r.residual_training_mean)))<1e-10
    if r.model=='recent_residual_MES' and r.nonzero_terms==0:
        a=new[(new.delay_days==r.delay_days)&(new.window==r.window)&(new.model==r.model)].set_index('id')
        b=new[(new.delay_days==r.delay_days)&(new.window==r.window)&(new.model=='recent_residual_intercept')].set_index('id').loc[a.index]
        assert np.max(abs(a.p-b.p))<1e-10

def metrics(g):
    err=g.p-g.y
    mse=np.mean(err**2);bias=err.mean();spread=np.mean((err-bias)**2)
    assert abs(mse-bias*bias-spread)<1e-10
    return {'n':len(g),'observed_mean':g.y.mean(),'prediction_mean':g.p.mean(),
       'MAE':np.mean(abs(err)),'RMSE':np.sqrt(mse),'bias_p_minus_y':bias,
       'bias_squared':bias*bias,'error_spread':spread,'bias_fraction_MSE':bias*bias/mse,
       'prediction_sd':g.p.std(ddof=0),'observed_sd':g.y.std(ddof=0),
       'spearman':g.p.rank(method='average').corr(g.y.rank(method='average')) if g.p.std(ddof=0)>1e-10 and g.y.std(ddof=0)>1e-10 else np.nan,
       'absolute_error_sum':abs(err).sum()}
rows=[]
for (delay,window,model),g in pred.groupby(['delay_days','window','model']):
    rows.append({'delay_days':delay,'window':window,'model':model,**metrics(g)})
windows=pd.DataFrame(rows)
windows['absolute_error_share']=windows.absolute_error_sum/windows.groupby(['delay_days','model']).absolute_error_sum.transform('sum')
windows.to_csv(OUT/'07_window_error_decomposition.csv',index=False,encoding='utf-8-sig')
pool=[]
for (delay,model),g in pred.groupby(['delay_days','model']):
    pool.append({'delay_days':delay,'model':model,**metrics(g)})
pool=pd.DataFrame(pool);pool.to_csv(OUT/'08_pooled_metrics.csv',index=False,encoding='utf-8-sig')

pairs=[('MES','historical_mean'),('MES','recent60_mean'),
       ('recent_residual_intercept','recent60_mean'),
       ('recent_residual_MES','recent60_mean'),
       ('recent_residual_MES','recent_residual_intercept'),
       ('recent_residual_MES_material','recent_residual_MES')]
paired=[];bywin=[];rng=np.random.default_rng(20260930)
for delay in [0,14]:
    q=pred[pred.delay_days.eq(delay)]
    w=q.pivot(index='id',columns='model',values='p').join(q.groupby('id').agg(y=('y','first'),month=('month','first'),window=('window','first')))
    assert len(w)==755
    months=w.month.unique();assert len(months)==7
    groups={m:np.flatnonzero(w.month.to_numpy()==m) for m in months}
    for a,b in pairs:
        v=(w[a]-w.y).abs().to_numpy()-(w[b]-w.y).abs().to_numpy()
        values=np.array([v[np.concatenate([groups[m] for m in rng.choice(months,len(months),replace=True)])].mean() for _ in range(2000)])
        paired.append({'delay_days':delay,'model':a,'baseline':b,'delta_MAE':v.mean(),'low':np.quantile(values,.025),'high':np.quantile(values,.975),'months':7,'bootstrap_n':2000})
        for window in [1,2,3,4]:
            vv=v[w.window.eq(window).to_numpy()]
            bywin.append({'delay_days':delay,'window':window,'model':a,'baseline':b,'n':len(vv),'delta_MAE':vv.mean()})
pd.DataFrame(paired).to_csv(OUT/'09_paired_month_intervals.csv',index=False,encoding='utf-8-sig')
pd.DataFrame(bywin).to_csv(OUT/'10_window_paired_differences.csv',index=False,encoding='utf-8-sig')

# Frozen data baseline from project management; source copy equivalence does not replace this check.
ref=pd.read_csv(ROOT/'00_项目管理/04_整理审计_20260906/data_before.csv')
checks=[]
for r in ref.itertuples(index=False):
    p=ROOT/r.path;checks.append({'path':r.path,'reference_sha256':r.sha256,'current_sha256':sha(p),'unchanged':sha(p).lower()==r.sha256.lower()})
assert len(checks)==20 and all(r['unchanged'] for r in checks)
pd.DataFrame(checks).to_csv(OUT/'11_frozen_input_hashes.csv',index=False,encoding='utf-8-sig')
source=[RES/'tables/03_joint_model_matrix.csv',RES/'quality_assessment_20260907/03_predictions.csv',RES/'quality_assessment_20260907/04_fit_audit.csv']
qa={'frozen_inputs_unchanged':20,'source_hashes':{str(p.relative_to(ROOT)):sha(p) for p in source},
    'original_reproduction_max_difference':float(repro.max_prediction_difference.max()),
    'same_test_batches_per_model':755,'new_model_predictions':len(new),'training_cut_checks_passed':True,
    'predefined_warmup_prior_labels':30,'fixed_prediction_month_clusters':7,'exploratory_previously_reviewed_test_windows':True,
    'independent_past_only_baseline_checks':baseline_checks,
    'frozen_recent_baseline_and_intercept_controls_independently_verified':True,
    'first_window_MES_nonzero_terms':repro[(repro.model=='MES')&(repro.window==1)].nonzero_terms.tolist()}
(OUT/'00_QA.json').write_text(json.dumps(qa,ensure_ascii=False,indent=2),encoding='utf-8')
print(pool[['delay_days','model','MAE','RMSE']].to_string(index=False))
print(pd.DataFrame(paired).to_string(index=False))
