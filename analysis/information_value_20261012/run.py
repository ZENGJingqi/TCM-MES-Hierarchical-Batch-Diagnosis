"""Run the additional QC-history comparisons with authorized private inputs.

The public repository contains code, never the required study data or outputs.
"""
from pathlib import Path
import argparse,os,subprocess,sys,tempfile
HERE=Path(__file__).resolve().parent
REPO=HERE.parents[1]

def external_output(value):
    out=Path(value).resolve()
    if out.exists() or out.is_relative_to(REPO):
        raise ValueError('Use a NEW private output directory outside this repository')
    return out

def stage_directory(value):
    stage=Path(value).resolve()
    if not stage.is_dir() or stage.is_relative_to(REPO):
        raise ValueError('Use an existing authorized private stage outside this repository')
    for name in ['qc_pool.csv','joint.csv','v4_helpers.R','legacy_predictions.csv','legacy_fit.csv','00_input_QA.json']:
        if not(stage/name).is_file():raise FileNotFoundError(name)
    return stage

def main():
    p=argparse.ArgumentParser(description=__doc__);sub=p.add_subparsers(dest='command',required=True)
    prep=sub.add_parser('prepare');prep.add_argument('--study-root',required=True,type=Path);prep.add_argument('--output-dir',required=True)
    fit=sub.add_parser('fit');fit.add_argument('--stage',required=True);fit.add_argument('--rscript',default='Rscript')
    audit=sub.add_parser('audit');audit.add_argument('--stage',required=True)
    a=p.parse_args()
    if a.command=='prepare':
        out=external_output(a.output_dir)
        subprocess.run([sys.executable,str(HERE/'prepare.py'),'--study-root',str(a.study_root.resolve()),'--output-dir',str(out)],check=True)
    elif a.command=='fit':
        stage=stage_directory(a.stage)
        if(stage/'output').exists():raise ValueError('Fit refuses an existing output directory')
        with tempfile.TemporaryDirectory(prefix='tcm_r_',dir=stage)as temp:
            env=os.environ.copy();env.update(TMPDIR=temp,TMP=temp,TEMP=temp,TZ='Asia/Shanghai',LC_ALL='English_United States.utf8'if os.name=='nt'else'C.UTF-8')
            subprocess.run([a.rscript,'--vanilla',str(HERE/'analysis.R'),str(stage)],env=env,check=True)
    else:
        stage=stage_directory(a.stage)
        if not(stage/'output').is_dir():raise ValueError('Audit requires saved fitted outputs')
        subprocess.run([sys.executable,str(HERE/'verify.py'),'--stage',str(stage)],check=True)
if __name__=='__main__':main()
