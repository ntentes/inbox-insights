# inbox-insights

Materials for the posit::conf(2026) talk **Inbox Insights with ellmer and Posit
Connect**. The fictitious company is Tumbleweed Logistics. All data is generated
from a fixed seed; no company data or production artifacts are included.

The implementation is in progress. Fixture mode is the default and makes no model
calls. The initial report is an **authored, deliberately misleading teaching
example**, not a captured agent run or a recommended analysis.

## Build the current artifacts

Use R 4.5.0 and restore the pinned packages with `renv::restore()`. From the
project root:

```sh
Rscript prep/generate_data.R
Rscript prep/validate_data.R
Rscript prep/build_cohort.R
Rscript prep/build_initial_report.R
```

The first three commands rebuild the synthetic data. The last regenerates
`fixtures/initial_bad_report.json` without credentials. Run the scripts with
`Rscript`: sourcing them loads functions but does not build their outputs.

For the slide scaffold, install the Quarto CLI separately (the talk uses
1.10.18), then run `quarto render inbox-insights.qmd`. The pinned `quarto` R
package does not install the CLI. The notes are recording prompts, not a script.

## The report contract

`R/report_contract.R` defines the monthly-cohort insight schema, its local
validation, and `submit_insight_tool(snapshot, on_submit)` for ellmer. Tool
submission means **submitted for review**, not permission to publish.

Reporting period, evidence window, data cutoff, and outcome horizon are separate
fields. The worked report covers 28 calendar days through 2026-06-30 and uses
February-May entry cohorts as explicitly historical evidence. Report inputs are
snapshots, not the full cohort table with hindsight.

The envelope records an authored-fixture provenance label and a SHA-256 snapshot
identity. Validation reproduces the evidence with the house recipes and rejects
different data, inconsistent time fields, and undeclared fields.

## Deliberate simplifications

The current contract covers the one monthly-cohort worked example, not arbitrary
analysis types. Its reproducible-code field must match the known recipe expression;
submitted code is never evaluated. A general live REPL and its runtime controls
are a separate, unfinished path.

Numerical validation does not establish that an interpretation or suggested action
is sound. The initial fixture demonstrates exactly that distinction. Human review
is still needed, and an authored fixture is not evidence of model compliance.
