# Inbox Insights with ellmer and Posit Connect

The proof of concept behind the posit::conf(2026) talk of the same name, by
Konstantinos Ntentes, Senior Data Scientist at Posit.

A model writes a weekly funnel report for **ChickenCloud**, a fictitious B2B
SaaS company, and Posit Connect emails it. Readers reply through two Shiny apps:
a feedback app that turns a reviewer's correction into a standing directive the
next report must follow, and a chat app that answers questions about the report
from the same data, with the same rules, in a sandboxed R session. The three
pieces never call each other. They coordinate through pins on the Connect
server, which is the point of the talk: the report, the corrections and the
conversation are one system because they share state, not because they share a
process.

Everything here is synthetic. The data comes from a seeded generator, the
company does not exist, and the model's output is labelled as a model's output,
submitted for review and not reviewed.

## The loop

```
                          seeded generator
                                │
                     data/funnel_cohort.csv ──► funnel_snapshot()  (no hindsight)
                                                       │
                                     ┌─────────────────┴──────────────────┐
                                     │  pins on Connect (one board)       │
                                     │  funnel-snapshot   the data        │
                                     │  weekly-headlines  what was said   │
                                     │  demo-guidance     what was approved
                                     └───┬────────────────┬───────────┬───┘
                                         │                │           │
      weekly-report.Rmd ◄────────────────┘   apps/feedback│  apps/chat│
      ellmer + mcp-repl, scheduled            corrections ─┘  questions┘
      ─► email via rsc_email_body_html        ─► directives    ─► answers computed
      ─► weekly-headlines                                        in R, with plots
```

**The report** (`weekly-report.Rmd`) gives a model a sandboxed R session over a
frozen snapshot of the funnel, the house recipes for the calculations the
business relies on, the approved directives, and the headlines of earlier runs.
It asks for candidate questions, three developed headlines, and a submitted
report. Each headline rests on a table the model saved from its own session; the
runner checks the table's shape, the schema and that the code parses, and does
not re-run it. On success the email goes out and the headlines are pinned. On
failure nothing is sent and the page shows the whole conversation.

**The feedback app** (`apps/feedback/app.R`) opens on the latest headlines. A
reader writes a correction about one of them; it is saved as pending feedback
and changes nothing. A reviewer then approves it as a directive, in the
reviewer's words or edited, bound to the exact state the reviewer was shown.
From the next run on, every report and chat is given the directive.

**The chat app** (`apps/chat/app.R`) is a thin interface over the same
accumulated state, not a second AI system: the same snapshot, recipes,
directives and headlines go into its prompt, each session gets its own
`mcp-repl`, and it can save a plot from that session and show it inline.

## Repository layout

```
weekly-report.Rmd            the scheduled report: prompt, tools, budget, email
weekly-report.template.html  a doctype and the body: the Connect page is the email
deploy.R                     publishes all three to Connect, pins the snapshot

apps/chat/app.R              the chat app
apps/feedback/app.R          the feedback and directive-approval app

R/config.R                   paths, the board factory, provider, app URLs
R/snapshot.R                 the snapshot contract: cutoff stamped and verified
R/recipes.R                  the house recipes for conversion, timing, funnel, reach
R/report_contract.R          report envelope, validators, headline metrics
R/live_report.R              a model's report: evidence tables, limits, formatting
R/live_session.R             what report and chat share: REPL, prompts, transcript
R/headline_history.R         the weekly-headlines pin and the prompt built from it
R/snapshot_pin.R             the funnel-snapshot pin
R/feedback.R                 guidance state: feedback, directives, approvals
R/context_archive.R          what the model may know: definitions and directives
R/token_budget.R             caps and the wrap-up notice for an unattended run
R/email_preview.R            the one email layout, page and mail alike
R/theme.R                    palette, stylesheet, inline styles, Shiny theme
R/charts.R                   evidence charts for the authored report kinds

prep/generate_data.R         the seeded generator -> data/funnel_raw.csv
prep/build_cohort.R          raw -> data/funnel_cohort.csv, and funnel_snapshot()
prep/build_initial_report.R  the first-run report the guidance state starts from

images/chickencloud-logo.png the logo, embedded in the email and the apps
config.example.R             the settings, with working defaults
renv.lock, renv/, .Rprofile  the pinned R packages
```

`data/`, `board/`, `artifacts/` and `bin/` are generated and ignored. The talk
material this was built alongside is not part of this repository.

## Running it

R 4.5 and `renv::restore()`. Then, in `~/.Renviron`:

```
ANTHROPIC_API_KEY=...      # the report and the chat call Anthropic through ellmer
CONNECT_SERVER=https://... # your Connect server; also builds the app links in the email
CONNECT_API_KEY=...        # a key for the publishing account
```

Build the data, once:

```sh
Rscript prep/generate_data.R
Rscript prep/build_cohort.R
```

For local runs, install `mcp-repl`
(`curl -fsSL https://raw.githubusercontent.com/posit-dev/mcp-repl/main/scripts/install.sh | sh`).
Locally the board is a versioned folder at `board/`; set `INBOX_BOARD` to use
another folder, or `INBOX_BOARD=connect` to use the Connect board from your
laptop.

```r
rmarkdown::render("weekly-report.Rmd")   # one live run; calls the model
shiny::runApp("apps/feedback")
shiny::runApp("apps/chat")
```

## Deploying

```sh
Rscript deploy.R              # report, chat, feedback, then pins the snapshot
Rscript deploy.R report       # or: chat, feedback
```

The script fetches the Linux `mcp-repl` release into `bin/`, bundles each piece
with the code it sources, registers your server with rsconnect, copies
`ANTHROPIC_API_KEY` into the report's and the chat's environment, and finally
pins the snapshot so the apps have data before the report has run. On Connect
nothing is configured: the server supplies `RSTUDIO_PRODUCT`, `CONNECT_SERVER`
and `CONNECT_API_KEY`, so all three pieces find the same board and the email
knows where the apps live.

Then, on Connect: run the report once and set its schedule and recipients; open
the feedback app and initialize the guidance state from the first-run report
(that approves nothing); open the chat.

The `mcp-repl` release is pinned in `deploy.R` to one whose Linux sandbox runs
inside a Connect container and whose protocol the current mcptools speaks; if
you change the pin, delete `bin/mcp-repl` so the next deploy fetches the new
build, and watch the first run's log.

## What keeps the model honest

- **The snapshot contract.** The model sees `funnel_snapshot()`, a point-in-time
  view with every date censored at the cutoff and the cutoff stamped on the
  table twice. Hindsight is not in the data it is given.
- **The house recipes.** `R/recipes.R` is copied into the sandbox and the model
  is told to use it for conversion, durations, the funnel and campaign reach, so
  its numbers are computed the way the business computes them. The code it
  hands in must still be plain dplyr against the pinned snapshot, because that
  is all a reader has.
- **Evidence as a file.** A headline's table is what the model saved with
  `saveRDS()`, read back by the runner; the model never retypes its numbers. At
  most three value columns and forty rows, so it fits an email. Money is shown
  in dollars, shares as percentages, counts whole, by column name.
- **Directives, not prose.** Pending feedback never reaches a prompt.
  `context_archive()` is the one place that decides what the model may know:
  the definitions and the approved directives, each with who approved it and
  when, its id a hash of its text so a directive edited afterwards is refused.
- **Memory of what was said.** Each run appends its headlines to the
  `weekly-headlines` pin and reads the last four runs back, with instructions
  not to repeat them and to say so when building on one.
- **A budget and a record.** Input, output and tool-call caps end a runaway run;
  past a threshold the next tool result asks the model to wrap up. Every tool
  call is logged to the Connect job log, and a run that ends without a report
  prints the entire conversation on its page.
- **One email, two deliveries.** The rendered page and the mailed body are the
  same HTML: every style inline, tables where the browser would have used flex,
  and the logo as a CID attachment, because mail clients drop `<style>` blocks
  and `data:` images.

## Deliberate simplifications

This is a proof of concept. The model's code is checked for syntax and taken as
its account of the table, not re-executed. A directive's text is approved as
written, with no check that it is sound; the approver is that check. Pins are
read and written without locking, and a folder board orders same-second
versions by hash, which real weekly runs never hit. Access is whatever Connect
grants; identity in the apps is Connect's `session$user`. The email is built for
Gmail and Outlook on the web; Outlook for Windows will show it plainer.

## Configuration

| Variable | Default | What it does |
|---|---|---|
| `INBOX_BOARD` | `board` locally, `connect` on Connect | folder path, or `connect` for the Connect board |
| `INBOX_CHAT_PROVIDER`, `INBOX_CHAT_MODEL` | `anthropic`, `claude-sonnet-5` | any provider ellmer knows; the key goes in `~/.Renviron` |
| `INBOX_REPL_SANDBOX` | `workspace-write` | mcp-repl's sandbox mode |
| `CONNECT_SERVER`, `CONNECT_API_KEY` | set by Connect for its content | the board, and the app links in the email |
| `ANTHROPIC_API_KEY` | | copied to the report and the chat by `deploy.R` |

## License

See `LICENSE`.
