"""Public placeholder for manuscript-drafting utilities.

The original internal script assembled a result-rich manuscript draft from
confidential local outputs. It is not distributed in this public repository
because it contained unpublished numerical results and company-confidential
batch-level context.

Use the other scripts in this folder to generate non-confidential manuscript
supporting material, such as framework documents and supplementary table
templates, after configuring authorized local input paths.
"""

from __future__ import annotations


def main() -> None:
    message = (
        "The internal result-filled manuscript enhancement script is not part "
        "of the public code release. Use create_full_submission_design_docx.py, "
        "create_submission_framework_docx.py, or create_supplementary_table_s2.py "
        "with authorized local data instead."
    )
    print(message)


if __name__ == "__main__":
    main()
