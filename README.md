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
Rscript prep/build_as_of_chart.R
Rscript prep/build_worked_example.R
```

The first three commands rebuild the synthetic data. The report and chart builders
regenerate `fixtures/initial_bad_report.json` and the two explanatory panels.
The worked-example builder replays the captured approvals in
`fixtures/worked_example.json`, seeds the local board, and builds the plain
worked example. No credentials are needed. Run the scripts with
`Rscript`: sourcing them loads functions but does not build their outputs.

Open these local files in a browser:

| File | What it shows |
|---|---|
| `artifacts/initial-report.html` | Deliberately misleading first-run finding |
| `artifacts/approval.html` | Authored correction, captured human approvals, and the archive transition |
| `artifacts/corrected-report.html` | One reviewed corrected insight, in the same email layout |
| `artifacts/corrected-report.json` | Full insight, evidence, reproducible code, and context references |
| `artifacts/context-archive.json` | The canonical approved generation context |
| `artifacts/report-review.json` | Human review bound to that exact corrected report |
| `artifacts/as-of-conversion.png` | Snapshot versus eventual conversion; hindsight is explanatory only |
| `artifacts/equal-30-day-conversion.png` | Snapshot-only equal-horizon comparison with full denominators |

Each chart also exports an aggregate CSV alongside its PNG. The first chart
contains hindsight and must never become report or chat input. The equal-horizon
chart omits partly observed cohorts rather than presenting only their oldest leads.

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

## Approval and shared context

`R/feedback.R` persists a versioned guidance state on a local folder board.
Initialize it explicitly with the source report and snapshot, then use
`record_feedback()` to add a correction. Recording feedback does not approve it.

`approve_incomplete_cohort_rule()` is the separate human action. It requires a
selected feedback ID, approver, and the hash of the state that was displayed.
The saved rule retains the source feedback and approval time. A stale displayed
state is rejected rather than approved silently.

`R/context_archive.R` is the single context builder. It reads the persisted state
and includes definitions and approved rules, never pending feedback or the
misleading source-report prose. The rule and original report survive restarting
the process; exported archives are accepted only when they match current state.

The correction was authored by Copilot; the speaker explicitly approved the rule
and reviewed the corrected report. The fixture preserves their actual attribution
and UTC timestamps. Rebuilding **replays captured approvals**; it does not perform
new human reviews. The HTML sequence is a static replay, not a recording of an app.

`R/corrected_report.R` consumes the approved archive and uses its 30-day horizon.
`review_corrected_report()` is a second, explicit human action bound to the exact
report hash. `require_report_review()` refuses absent reviews, changed content,
unreproducible evidence, and mismatched rule, archive, or snapshot references.
The worked-example builder applies this gate before writing its outputs.

`R/worked_example.R` validates captured approvals before seeding. Existing board
state that differs is rejected, never silently reset to the fixture. For an
independent rebuild, use `INBOX_BOARD=board/replay Rscript prep/build_worked_example.R`.
For a new correction, use a separate board, `initialize_guidance()`, and
`record_feedback()`; inspect the rule before `approve_incomplete_cohort_rule()`,
then inspect the report before `review_corrected_report()`. Only after those human
actions can `capture_worked_fixture()` export a replacement reviewed fixture.

## Deliberate simplifications

The current contract covers the one monthly-cohort worked example, not arbitrary
analysis types. Its reproducible-code field must match the known recipe expression;
submitted code is never evaluated. A general live REPL and its runtime controls
are a separate, unfinished path.

Numerical validation does not establish that an interpretation or suggested action
is sound. The initial fixture demonstrates exactly that distinction. Human review
is still needed, and an authored fixture is not evidence of model compliance.

The approval slice supports one canonical incomplete-cohort rule and assumes one
local writer. The stale-state guard is not a multi-process transaction or access
control. Attribution is self-reported, not authenticated Connect identity.
The folder board uses flat pin names `demo-guidance` and `demo-report-review` because this backend
rejects slashes; deployed board naming and concurrency need their own adapter.
The current archive contains shared definitions and the worked rule, not report
history or a chat interface. The current email is a one-insight preview, not the
finished three-insight email. No email is sent by these scripts.
