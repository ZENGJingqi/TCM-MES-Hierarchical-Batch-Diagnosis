"""Independent eligibility/recursion/coefficient checks and fixed-prediction summaries.
No private batch keys are written to the supplementary workbook.
"""
from pathlib import Path
import argparse, json, hashlib
import numpy as np
import pandas as pd

def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def main():
    ap=argparse.ArgumentParser();ap.add_argument('--stage',required=True);args=ap.parse_args();st=Path(args.stage);out=st/'output'
    q=pd.read_csv(st/'qc_pool.csv',parse_dates=['obs','mes_date']).set_index('qc_id');j=pd.read_csv(st/'joint.csv',parse_dates=['mes_production_date','d2_observation_date']).set_index('id')
    p=pd.read_csv(out/'01_predictions.csv',parse_dates=['date','cut']);h=pd.read_csv(out/'02_history_sets.csv',parse_dates=['cut']);hm=json.loads((out/'03_history_members.json').read_text());f=pd.read_csv(out/'04_fits.csv',parse_dates=['cut','max_production','max_assumed_available']);fm=json.loads((out/'08_training_members.json').read_text())
    cf=pd.read_csv(out/'05_coefficients.csv');sc=pd.read_csv(out/'06_preprocessing.csv');legacy=pd.read_csv(st/'legacy_predictions.csv',parse_dates=['date','cut']);lf=pd.read_csv(st/'legacy_fit.csv',parse_dates=['cut'])
    checks={};max_history=0.;max_coefficient=0.;max_training=0.
    for z in h.itertuples():
        av=q.obs+pd.Timedelta(days=z.delay_days);ok=(av<z.cut)&~q.invalid_observation_before_production&(q.mes_date.isna()|(q.mes_date<z.cut))
        if z.pool=='linked':ok&=q.linked
        eligible=q[ok];used=eligible
        if z.method=='mean60':
            recent=eligible[av[ok]>=z.cut-pd.Timedelta(days=60)]
            if len(recent):used=recent
            v=used.target.mean()
        else:
            dd=used.groupby(used.obs+pd.Timedelta(days=z.delay_days)).target.mean().sort_index();v=dd.iloc[0] if len(dd) else np.nan
            for val in dd.iloc[1:]:v=.2*val+.8*v
        ids=sorted(used.index);assert ids==hm[z.history_set_id] and len(eligible)==z.eligible_n and len(ids)==z.used_n
        assert hashlib.sha256('\n'.join(ids).encode()).hexdigest()==z.member_sha256
        if np.isfinite(v):max_history=max(max_history,abs(v-z.value));assert abs(v-z.value)<1e-10
        else:assert pd.isna(z.value)
    for delay in [0,14]:
        tr=pd.read_csv(out/f'training_history_{delay}.csv',parse_dates=['mes_production_date','available']).set_index('id')
        hh=h[(h.pool=='full')&(h.method=='mean60')&(h.delay_days==delay)].set_index('cut')
        for z in tr.itertuples():
            zz=hh.loc[z.mes_production_date];assert not isinstance(zz,pd.DataFrame)
            assert z.prior_n==zz.eligible_n
            if pd.notna(z.prior_base):
                assert abs(z.prior_base-zz.value)<1e-10
                assert abs(z.residual-(j.loc[z.Index,'disintegration_time_min']-z.prior_base))<1e-10
        for z in f[f.delay_days==delay].itertuples():
            train=tr[(tr.mes_production_date<z.cut)&(tr.available<z.cut)];rr=train[(train.prior_n>=30)&train.residual.notna()]
            assert sorted(rr.index)==fm[z.fit_id] and len(rr)==z.residual_n and len(train)==z.train_n
            assert abs(rr.residual.mean()-z.residual_intercept)<1e-10
            max_training=max(max_training,abs(rr.residual.mean()-z.residual_intercept))
            pp=p[(p.fit_id==z.fit_id)&(p.model=='MES_residual')];coef=cf[cf.fit_id==z.fit_id].set_index('term').coefficient
            val=np.full(len(pp),coef['(Intercept)']);xx=j.loc[pp.id]
            for a in sc[sc.fit_id==z.fit_id].itertuples():
                raw=xx[a.term].to_numpy(float);miss=np.isnan(raw);v=(np.where(miss,a.median,raw)-a.center)/a.scale
                val+=coef[a.term]*v+coef[a.term+'__missing']*miss
            bases=h.set_index('history_set_id').loc[pp.history_set_id,'value'].to_numpy();dif=np.max(np.abs(bases+val-pp.p))
            assert dif<1e-9;max_coefficient=max(max_coefficient,float(dif))
    for dat in [p,legacy]:
        assert not dat.duplicated(['delay_days','window','id','frequency','model']+(['pool'] if 'pool' in dat else [])).any()
        same=j.loc[dat.id];assert np.max(abs(same.disintegration_time_min.to_numpy()-dat.y))<1e-12
        assert (same.mes_production_date.to_numpy()==dat.date.to_numpy()).all()
        assert (dat.cut<=dat.date).all()
        for w,n in [(1,226),(2,230),(3,226),(4,73)]:assert dat[dat.window==w].id.nunique()==n
        assert dat[['p','y']].notna().all().all()
    assert len(p)==18120 and len(legacy)==12080
    ref=p[(p.pool=='linked')&(p.model=='mean60')].merge(legacy[legacy.model=='recent_mean'],on=['delay_days','window','id','frequency'],suffixes=('','.old'),validate='one_to_one')
    checks['linked_mean_legacy_reproduction_max_abs_difference']=float(np.max(abs(ref.p-ref['p.old'])))
    assert len(ref)==3020 and checks['linked_mean_legacy_reproduction_max_abs_difference']<1e-10
    # Legacy model weights were not archived by the original daily fitter.
    # Audit retained identities, clocks, source/spec/fit-log references, not weight recovery.
    index={};legacy_fit_ids={}
    for i,z in enumerate(lf.itertuples(),1):
        key=(z.delay_days,z.window,z.frequency,z.cut);index[key]=z;legacy_fit_ids[key]=f'LF-{i:04d}'
    for z in legacy.itertuples():
        fit=index[(z.delay_days,z.window,z.frequency,z.cut)]
        hh=h[(h.pool=='linked')&(h.method=='mean60')&(h.delay_days==z.delay_days)&(h.cut==z.cut)].iloc[0]
        assert abs(fit.baseline-hh.value)<1e-10
        if z.model=='recent_mean':assert abs(z.p-fit.baseline)<1e-10
        elif z.model=='residual_intercept':assert abs(z.p-fit.baseline-fit.residual_mean)<1e-10
    # Stable assessment IDs refer to actual saved records; not industrial incidents.
    audit_rows=[]
    for source,dat in [('legacy',legacy),('expanded',p)]:
        for i,z in enumerate(dat.itertuples(),1):
            hid=getattr(z,'history_set_id',None)
            if source=='legacy':hid=h[(h.pool=='linked')&(h.method=='mean60')&(h.delay_days==z.delay_days)&(h.cut==z.cut)].iloc[0].history_set_id
            fid=getattr(z,'fit_id',None) if source!='legacy' else legacy_fit_ids[(z.delay_days,z.window,z.frequency,z.cut)]
            audit_rows.append(dict(assessment_id=f'{source}-{i:05}',source=source,id=z.id,delay_days=z.delay_days,window=z.window,frequency=z.frequency,model=z.model,cut=z.cut.strftime('%Y-%m-%d'),history_set_id=hid,fit_id=fid,model_spec=f'{source}:{z.model}:{z.frequency}',identity_and_outcome=True,cutoff_rule=True,retained_spec_and_source=True,actual_QC_signoff_verified=False,actual_process_field_available_time_verified=False))
    pd.DataFrame(audit_rows).to_csv(out/'09_assessment_audit.csv',index=False)
    allp=pd.concat([legacy.assign(pool='linked').replace({'model':{'recent_mean':'mean60'}}),p[p.pool=='full']],ignore_index=True)
    allp['ae']=abs(allp.p-allp.y);allp['se']=(allp.p-allp.y)**2;allp['err']=allp.p-allp.y
    # Include linked EWMA from new runs; linked mean is identical to saved legacy.
    ew=p[(p.pool=='linked')&(p.model=='ewma')].copy();ew['ae']=abs(ew.p-ew.y);ew['se']=(ew.p-ew.y)**2;ew['err']=ew.p-ew.y
    allp=pd.concat([allp,ew],ignore_index=True)
    rows=[]
    for keys,g in allp.groupby(['delay_days','pool','model','frequency']):
        for label,gg in [('pooled',g),('omit_first',g[g.window!=1])]+[(f'window_{w}',g[g.window==w])for w in [1,2,3,4]]:
            rows.append(dict(zip(['delay_days','pool','model','frequency'],keys),scope=label,n=len(gg),MAE=gg.ae.mean(),RMSE=np.sqrt(gg.se.mean()),bias=gg.err.mean(),prediction_sd=gg.p.std()))
    metrics=pd.DataFrame(rows);metrics.to_csv(out/'10_metrics.csv',index=False)
    contrasts=[]
    specs=[('history_refresh',('linked','mean60','daily'),('linked','mean60','frozen')),
           ('process_increment_linked',('linked','MES_residual','daily'),('linked','residual_intercept','daily')),
           ('process_increment_full',('full','MES_residual','daily'),('full','residual_intercept','daily')),
           ('full_vs_linked_mean',('full','mean60','daily'),('linked','mean60','daily')),
           ('full_vs_linked_EWMA',('full','ewma','daily'),('linked','ewma','daily')),
           ('EWMA_vs_mean_linked',('linked','ewma','daily'),('linked','mean60','daily')),
           ('EWMA_vs_mean_full',('full','ewma','daily'),('full','mean60','daily'))]
    for delay in [0,14]:
        d=allp[allp.delay_days==delay]
        for name,a,b in specs:
            def get(k):return d[(d.pool==k[0])&(d.model==k[1])&(d.frequency==k[2])][['id','window','month','ae']]
            pair=get(a).merge(get(b),on=['id','window','month'],suffixes=('_a','_b'),validate='one_to_one');assert len(pair)==755
            pair['diff']=pair.ae_a-pair.ae_b
            for scope,g in [('pooled',pair),('omit_first',pair[pair.window!=1])]+[(f'window_{w}',pair[pair.window==w])for w in [1,2,3,4]]:
                lo=hi=np.nan
                if scope in ['pooled','omit_first']:
                    groups=[v['diff'].to_numpy() for _,v in g.groupby('month')];rng=np.random.default_rng(20261012);boots=[]
                    for _ in range(2000):boots.append(np.concatenate([groups[i] for i in rng.integers(0,len(groups),len(groups))]).mean())
                    lo,hi=np.quantile(boots,[.025,.975])
                contrasts.append(dict(delay_days=delay,contrast=name,scope=scope,n=len(g),delta_MAE=g['diff'].mean(),conditional_CI_low=lo,conditional_CI_high=hi))
            contrasts.append(dict(delay_days=delay,contrast=name,scope='equal_window',n=755,delta_MAE=pair.groupby('window')['diff'].mean().mean(),conditional_CI_low=np.nan,conditional_CI_high=np.nan))
    effects=pd.DataFrame(contrasts);effects.to_csv(out/'11_contrasts.csv',index=False)
    checks.update(new_prediction_rows=len(p),legacy_prediction_rows=len(legacy),identity_outcome_clock_source_checks_passed=len(p)+len(legacy),history_sets_checked=len(h),new_fits_checked=len(f),history_values_max_abs_difference=max_history,new_model_coefficient_prediction_max_abs_difference=max_coefficient,train_intercept_max_abs_difference=max_training,QC_pool_batches=len(q),QC_pool_batches_without_MES_production_time=int(q.mes_date.isna().sum()),actual_signoff_verified_rows=0,independent_external_validation=False,legacy_daily_fitted_weights_recovered=False)
    checks['source_hashes']={str(z.relative_to(st)):sha(z) for z in [st/'qc_pool.csv',st/'joint.csv',st/'00_input_QA.json',out/'01_predictions.csv',out/'04_fits.csv']}
    checks['code_hashes']={z.name:sha(z) for z in Path(__file__).parent.iterdir() if z.suffix in ['.py','.R','.md']}
    schema={'purpose':'Retrospective analytical record model, not an operational platform',
      'objects':{'Batch':{'key':'joint.csv:id','fields':['mes_production_date','d2_observation_date'],'count':1477},
      'QC':{'key':'qc_pool.csv:qc_id','fields':['target','obs','mes_date','linked','qc_record_n'],'count':4296},
      'SourceLink':{'key':'joint.csv:source identifiers','semantics':'recorded upstream-finished relationship, not causal or independent replicate'},
      'HistorySet':{'key':'02_history_sets.csv:history_set_id','members':'03_history_members.json','fields':['cut','delay_days','pool','method','member_sha256'],'count':len(h)},
      'Fit':{'key':'04_fits.csv:fit_id','members':'08_training_members.json','parameters':['05_coefficients.csv','06_preprocessing.csv'],'count':len(f)},
      'ModelSpec':{'fields':['model','frequency','protocol SHA256','code SHA256','input SHA256'],'source':'00_input_QA.json and verification.json'},
      'Assessment':{'key':'09_assessment_audit.csv:assessment_id','fields':['id','history_set_id','fit_id','model_spec','cut'],'count':30200},
      'Contrast':{'key':'11_contrasts.csv:delay_days,contrast,scope','fields':['n','delta_MAE','conditional_CI_low','conditional_CI_high']}},
      'edges':[['QC','HistorySet','eligible member'],['Batch','Assessment','assessed batch'],['HistorySet','Assessment','reference'],['Fit','Assessment','correction if applicable'],['ModelSpec','Fit','specifies'],['Assessment','Contrast','paired on batch and evaluated clock conditions']],
      'unverified_times':['QC signoff','MES writing','operation completion','upstream measurement availability'],
      'legacy_limitation':'Retained fit log and source/specification are traceable; daily fitted weight recovery is not claimed.'}
    (out/'record_schema.json').write_text(json.dumps(schema,ensure_ascii=False,indent=2),encoding='utf8')
    (out/'verification.json').write_text(json.dumps(checks,ensure_ascii=False,indent=2),encoding='utf8')
    print(metrics[(metrics.scope=='pooled')&(metrics.frequency=='daily')][['delay_days','pool','model','MAE']].to_string(index=False))
    print(effects[effects.scope.isin(['pooled','omit_first'])].to_string(index=False))
    print(json.dumps({k:v for k,v in checks.items() if k!='source_hashes'},ensure_ascii=False))
if __name__=='__main__':main()
