"""Data-free path/CLI tests; no synthetic records are research results."""
from pathlib import Path
import importlib.util,tempfile,unittest
P=Path(__file__).resolve().parents[1]/'analysis/information_value_20261012/run.py'
spec=importlib.util.spec_from_file_location('information_value_cli',P)
cli=importlib.util.module_from_spec(spec);spec.loader.exec_module(cli)
class PathGuards(unittest.TestCase):
    def test_repository_output_refused(self):
        with self.assertRaises(ValueError):cli.external_output(cli.REPO/'outputs/new_analysis')
    def test_existing_output_refused(self):
        with tempfile.TemporaryDirectory()as tmp:
            with self.assertRaises(ValueError):cli.external_output(tmp)
    def test_missing_private_stage_refused(self):
        with tempfile.TemporaryDirectory()as tmp:
            with self.assertRaises(FileNotFoundError):cli.stage_directory(tmp)
    def test_fresh_external_output_allowed(self):
        with tempfile.TemporaryDirectory()as tmp:
            target=Path(tmp)/'new_stage';self.assertEqual(cli.external_output(target),target.resolve());self.assertFalse(target.exists())
if __name__=='__main__':unittest.main()
