"""Source reconciliation and new staging. Does not change finalized inputs."""
from pathlib import Path
import argparse,json,hashlib,shutil
import pandas as pd,numpy as np
C=Path(__file__).resolve().parent
def sha(p):
    with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
def key(s):return s.astype(str).str.strip().str.replace(r'\.0$','',regex=True)
def main():
    ap=argparse.ArgumentParser();ap.add_argument('--study-root',required=True,type=Path);ap.add_argument('--output-dir',required=True);args=ap.parse_args();out=Path(args.output_dir).resolve()
    S=args.study_root.resolve()
    if not S.is_dir():raise ValueError('Authorized private study root not found')
    repo=C.parents[1]
    if out.is_relative_to(repo):raise ValueError('Private outputs must remain outside the public repository')
    assert not out.exists();out.mkdir(parents=True)
    data=S/'01_定稿数据/定稿数据_英文';res=S/'03_分析结果与论文图'
    paths={'d2':data/'D2_finished_product_physchem_final.xlsx','d3':data/'D3_finished_product_mes_main_final.xlsx',
      'joint':res/'tables/03_joint_model_matrix.csv','legacy_predictions':res/'计算验证扩展_20261001/01_predictions.csv',
      'legacy_fit':res/'计算验证扩展_20261001/03_fit_audit.csv','helpers':S/'02_分析代码/时期与批次信息分解_20260930/analysis.R',
      'protocol':S/'02_分析代码/信息价值评价补强_20261012/README.md'}
    d2=pd.read_excel(paths['d2']);d3=pd.read_excel(paths['d3']);j=pd.read_csv(paths['joint'],dtype={'finished_batch':str})
    d2=d2[d2.dosage_strength.astype(str).str.replace(r'\s','',regex=True).eq('0.8g')].copy();d2['batch']=key(d2.batch_no);d3['batch']=key(d3.batch_no)
    assert len(d2)==4377 and d2.batch.nunique()==4296 and len(d3)==d3.batch.nunique()==1608
    assert d2.disintegration_time_min.notna().all() and d2.production_date.notna().all()
    qc=d2.groupby('batch',sort=True).agg(target=('disintegration_time_min','mean'),obs=('production_date','max'),qc_record_n=('batch','size')).reset_index()
    qc=qc.merge(d3[['batch','production_date']].rename(columns={'production_date':'mes_date'}),on='batch',how='left',validate='one_to_one')
    j['finished_batch']=key(j.finished_batch);j['mes_production_date']=pd.to_datetime(j.mes_production_date);j['d2_observation_date']=pd.to_datetime(j.d2_observation_date)
    j=j.sort_values(['mes_production_date','finished_batch']).reset_index(drop=True);j['id']=[f'Q-{i+1:04d}' for i in range(len(j))]
    assert len(j)==j.finished_batch.nunique()==1477
    qc['linked']=qc.batch.isin(j.finished_batch);qc['qc_id']=[f'QC-{i+1:04d}' for i in range(len(qc))]
    same=qc.set_index('batch').loc[j.finished_batch]
    assert np.allclose(same.target,j.disintegration_time_min,rtol=0,atol=1e-12)
    assert (pd.to_datetime(same.obs).to_numpy()==j.d2_observation_date.to_numpy()).all()
    assert (pd.to_datetime(same.mes_date).to_numpy()==j.mes_production_date.to_numpy()).all()
    qc['invalid_observation_before_production']=qc.mes_date.notna()&(qc.obs<qc.mes_date)
    qc['production_time_known']=qc.mes_date.notna()
    qc.drop(columns='batch').to_csv(out/'qc_pool.csv',index=False,encoding='utf-8-sig')
    j.to_csv(out/'joint.csv',index=False,encoding='utf-8-sig')
    # Private batch keys are retained only in the audit staging, never SourceData.
    qc[['qc_id','batch']].merge(j[['finished_batch','id']],left_on='batch',right_on='finished_batch',how='left').to_csv(out/'private_batch_mapping.csv',index=False,encoding='utf-8-sig')
    for name,src in [('v4_helpers.R',paths['helpers']),('legacy_predictions.csv',paths['legacy_predictions']),('legacy_fit.csv',paths['legacy_fit'])]:shutil.copy2(src,out/name)
    rec={'protocol_sha256':sha(paths['protocol']),'source_hashes':{str(p.relative_to(S)):sha(p) for p in paths.values()},
      'full_QC_batches':len(qc),'linked_QC_batches':int(qc.linked.sum()),'production_date_known':int(qc.production_time_known.sum()),
      'known_invalid_QC_before_production':int(qc.invalid_observation_before_production.sum()),
      'QC_signoff_or_MES_write_or_operation_completion_fields_present':False,
      'exploratory_after_all_four_windows_reviewed':True,'independent_validation':False,'Figure1_modified':False}
    (out/'00_input_QA.json').write_text(json.dumps(rec,ensure_ascii=False,indent=2)+'\n',encoding='utf8')
    print(json.dumps({k:v for k,v in rec.items() if k!='source_hashes'},ensure_ascii=False))
if __name__=='__main__':main()
