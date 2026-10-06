"""Data-free tests for release-boundary checks (standard library only)."""
from pathlib import Path
import importlib.util,unittest
path=Path(__file__).resolve().parents[1]/'tools/verify_release.py'
spec=importlib.util.spec_from_file_location('verify_release',path)
guard=importlib.util.module_from_spec(spec);spec.loader.exec_module(guard)
class ReleaseBoundaryTests(unittest.TestCase):
    def test_source_allowed(self):
        for name in ['analysis/model.R','tools/check.py','README.md','docs/CURRENT_CODE_MANIFEST.json','data/README.md','outputs/.gitkeep']:
            self.assertIsNone(guard.path_error(name),name)
    def test_private_and_generated_types_blocked(self):
        for name in ['raw.xlsx','Source_Data.xlsx','predictions.csv','panels.rds','paper.docx','Figure_3.png','slides.pptx','credentials.json','data/values.txt','outputs/result.txt']:
            self.assertIsNotNone(guard.path_error(name),name)
    def test_unsafe_paths_blocked(self):
        for name in ['../data.csv','/tmp/code.py','docs\\values.json']:
            self.assertIsNotNone(guard.path_error(name),name)
    def test_credential_pattern(self):
        self.assertTrue(guard.secret_error('ghp_'+'x'*36))
        scheme='https'
        self.assertTrue(guard.secret_error(f'{scheme}://user:password@example.org'))
        self.assertFalse(guard.secret_error('https://github.com/ZENGJingqi/repo'))
    def test_plain_source_not_flagged(self):
        self.assertFalse(guard.secret_error('read.csv(input_path); seed <- 20261001'))
if __name__=='__main__':unittest.main()
