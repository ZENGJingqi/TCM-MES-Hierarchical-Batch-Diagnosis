"""Verify tracked source hashes and the code-only publication boundary.

No scientific packages, private inputs, credential files or network are used.
Use --index to inspect the exact staged blobs, rather than working-tree files.
"""
from pathlib import Path, PurePosixPath
import argparse, ast, hashlib, json, re, subprocess

ALLOWED_SUFFIXES={'.r','.py','.md','.json','.txt','.svg'}
ALLOWED_SPECIAL={'.gitignore','.gitattributes','.gitkeep','LICENSE'}
SECRET_PATTERNS=[
    re.compile(r'gh[pousr]_[A-Za-z0-9_]{30,}'),
    re.compile(r'github_pat_[A-Za-z0-9_]{30,}'),
    re.compile(r'sk-[A-Za-z0-9]{32,}'),
    re.compile(r'-----BEGIN [A-Z ]*PRIVATE KEY-----'),
    re.compile(r'https?://[^/\s]+@'),
]
def path_error(name):
    p=PurePosixPath(name)
    if p.is_absolute() or '..' in p.parts or '\\' in name:return 'unsafe path'
    if p.suffix.lower() not in ALLOWED_SUFFIXES and p.name not in ALLOWED_SPECIAL:return 'non-source file type'
    if p.parts[0]=='data' and name!='data/README.md':return 'data directory is documentation-only'
    if p.parts[0]=='outputs' and name!='outputs/.gitkeep':return 'generated output'
    if p.suffix.lower()=='.json' and name!='docs/CURRENT_CODE_MANIFEST.json':return 'unapproved JSON payload'
    return None
def secret_error(text):
    return any(p.search(text) for p in SECRET_PATTERNS)
def git(root,*args):
    r=subprocess.run(['git','-c',f'safe.directory={root.as_posix()}','-C',str(root),*args],capture_output=True)
    if r.returncode:raise RuntimeError('Local Git inspection failed.')
    return r.stdout
def verify(root,index=False):
    names=[n.decode('utf8') for n in git(root,'ls-files','-z').split(b'\0') if n]
    failures=[];blobs={}
    for name in names:
        error=path_error(name)
        if error:
            failures.append(f'{name}: {error}');continue
        if not index and not (root/name).resolve().is_relative_to(root.resolve()):
            failures.append(f'{name}: link outside repository');continue
        blob=git(root,'show',':'+name) if index else (root/name).read_bytes()
        text=blob.decode('utf-8-sig');blobs[name]=blob
        if secret_error(text):failures.append(f'{name}: potential secret or credential-bearing URL')
        if name.endswith('.py'):
            try:ast.parse(text,filename=name)
            except SyntaxError:failures.append(f'{name}: Python syntax error')
    manifest=json.loads(blobs.get('docs/CURRENT_CODE_MANIFEST.json',b'{}'))
    entries=manifest.get('scripts',[])+manifest.get('supporting_scripts',[])
    if not entries:failures.append('missing source manifest')
    for entry in entries:
        name=entry['released']
        if name not in blobs or hashlib.sha256(blobs[name]).hexdigest()!=entry['sha256']:
            failures.append(f'{name}: manifest mismatch or untracked source')
    if failures:raise ValueError('\n'.join(failures))
    return {'mode':'index' if index else 'working tree','tracked_files':len(names),
            'manifest_scripts':len(entries),'python_syntax_checked':sum(n.endswith('.py') for n in names),
            'code_only_file_check':True,'private_data_used':False}
def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--index',action='store_true')
    args=p.parse_args()
    root=Path(__file__).resolve().parents[1]
    print(json.dumps(verify(root,args.index),indent=2))
if __name__=='__main__':main()
