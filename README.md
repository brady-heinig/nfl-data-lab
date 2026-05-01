# 2025 NFL Query Lab

An LLM-powered Shiny app that turns natural-language NFL questions into
DuckDB SQL, runs them against nine season tables, and visualizes the
results — paired with a `{vitals}`-based evaluation harness that
benchmarks where the bot struggles.

---

## Repository contents

| File | Purpose |
|---|---|
| `nfl_datalab_app.R` | The Shiny app (entry point). |
| `nfl_app_eval.R` | Structured evaluation: routing, SQL, plot interpretation. |
| `nfl_app_eval_results.R` | Results-only script for inspecting the eval after it finishes. |
| `nfl_app_eval_writeup.md` | Methodology + findings template for the eval. |
| `nfl_passing.csv`, `nfl_rushing.csv`, `nfl_receiving.csv`, `nfl_defense.csv`, `nfl_kicking.csv`, `nfl_punting.csv`, `nfl_returning.csv`, `nfl_game_scores.csv`, `nfl_play_by_play.csv` | Source data (see *Data* below). |

---

## Running the app

### Prerequisites

R 4.5 or newer and an Anthropic API key.

```r
install.packages(c(
  "shiny", "bslib", "ellmer", "querychat", "readr", "dplyr", "tidyr",
  "stringr", "janitor", "DT", "ggplot2", "scales", "bsicons",
  "glue", "duckdb", "DBI", "jsonlite", "commonmark", "tibble", "later"
))
```

### Launch

```r
Sys.setenv(ANTHROPIC_API_KEY = "")
shiny::runApp("nfl_datalab_app.R")
```

All nine CSVs need to sit in the same folder as the app file. The app
loads them on startup, registers each as a DuckDB table, and they remain
accessible at `nfl_tables[["passing"]]`, etc. The first launch takes a
few seconds while DuckDB ingests the data.

### Using the app

The left rail has three stacked cards:

1. **2025 NFL Query Lab** — welcome message and four clickable example
   queries that populate the question textarea on click.
2. **Ask the data** — a query-type dropdown, a free-text question box,
   and a Submit button.
3. **Live thinking log** — fills in real time as Haiku routes the
   question, generates SQL, runs it, and builds summary metrics. Each
   completed step gets a green checkmark; the active step shows a
   spinner. The log resets at the start of every new query.

The right pane has:

1. **Query summary** — three dynamically generated value boxes
   summarizing the result. At least one box is always a numeric metric
   (sum, mean, count, percentage, or rank).
2. **Visualization** + **Plot interpretation** side-by-side. The plot
   controls (type, X, Y, color/group, top-N slider) sit in a single
   row above the chart. The interpretation card has a question
   textarea, an "Interpret plot" button, and a Haiku output area that
   fills with a 2–4 sentence interpretation.
3. **Data dictionary** (collapsed accordion) — table catalog, per-table
   schema, and the context Haiku receives.

The whole UI fits in one viewport without page scrolling; only the
thinking log scrolls internally if its content overflows.

---

## Running the evaluation

The eval needs the app helpers (table loaders, prompt functions, the
DuckDB connection) loaded into the global environment.

```r
# 1. Restart R into a clean session (RStudio: Session → Restart R).
# 2. In the fresh session:
Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")
source("nfl_querychat_app_clean_dashboard.R")  # loads CSVs + helpers
source("nfl_app_eval.R")                       # ≈ 75s, runs 3 tasks
source("nfl_app_eval_results.R")               # measurements + viewer
```

The clean restart matters because the eval script relies on a current
`{purrr}`, and stale `{purrr}` namespaces from a long-running session
trigger an unloadable-package error.

### What you'll see

`nfl_app_eval.R` runs three `{vitals}` `Task$eval()` calls in sequence:

- **Task 1 — Table routing** (18 cases). Hand-labeled questions
  stratified easy/medium/hard. Scored by `detect_match(location =
  "exact")`.
- **Task 2 — SQL generation + execution** (12 cases). Each question is
  paired with its correct table; Haiku writes SQL, the eval runs it
  against the live DuckDB connection, and `detect_includes()` checks
  the result preview for an expected substring.
- **Task 3 — Plot interpretation** (3 cases — leaderboard, near-uniform,
  bimodal). Plots are rendered to PNG and sent to Haiku with the same
  short interpretation prompt used in the app. Scored by
  `model_graded_qa()` with **Claude Sonnet 4.5** as the judge against
  per-case rubrics.

`nfl_app_eval_results.R` prints:

- Overall accuracy per task (n, accuracy, C/P/I counts)
- Accuracy by difficulty (within each task)
- Accuracy by topic, weakest first
- Every individual failure with input, expected target, and got output
- Per-task API cost via `Task$get_cost()`
- Headline numbers in `X / N (P%)` format
- Opens the Inspect log viewer (`vitals::vitals_view`), where you can
  click into any sample to read the exact prompt sent and the grader's
  reasoning.

### What the eval reveals

Run it on your own copy and the printed sections will surface:

- *Routing* fails most often on hard cases where one keyword pulls the
  router toward the wrong table — "scoring drive" pulls toward
  `game_scores` but should be `play_by_play`; "QB hits" pulls toward
  `passing` but should be `defense`; "passer rating in road games"
  has the word "games" pulling toward `game_scores`.
- *SQL* fails most often on cases that need ratios or conditional
  filtering (TD/INT ratio, FG percentage from 50+ yards). The common
  failure mode is Haiku selecting only one of the columns it needed
  for the ratio, or returning raw counts without computing the ratio.
- *Plot interpretation* fails hardest on the near-uniform case. LLMs
  carry a prior that "ranked bar charts have a leader," so they often
  invent a leader from a near-uniform distribution. This is the same
  pattern Posit's BluffBench surfaced.

See `nfl_app_eval_writeup.md` for full methodology, rationale for each
test case, and the limitations section.

---

## Data

All nine CSVs are 2025-season NFL stats. Combined row counts comfortably
exceed 5,000 rows, with `nfl_play_by_play.csv` alone supplying the bulk.
The DuckDB tables are loaded from the CSVs as-is — column names,
casing, and types are preserved exactly so the SQL Haiku writes is
honest to the source files.

Host the CSVs alongside the source files in the GitHub repo (the app
expects them in the working directory).

---

## How this satisfies each rubric point

| Requirement | Where it's satisfied |
|---|---|
| **Different dataset, ≥ 5,000 rows.** | Nine NFL CSVs loaded by `load_csv_raw()` in `nfl_querychat_app_clean_dashboard.R` (lines 75–77). Combined row count exceeds 5,000; `nfl_play_by_play.csv` alone supplies the majority. The schema and content are entirely different from the example app. |
| **Custom greeting.** | The brand card on the left rail (lines 459–471 of the app file) renders a styled welcome with `h4("Welcome 👋")`, an explanation paragraph, and a "Try an example:" section with four clickable `actionLink` examples. Each link is wired to an `observeEvent` that populates the question textarea via `updateTextAreaInput()` (lines 632–643). The card has its own gradient background and link-styled CSS (`.brand-card`, `.example-link`). |
| **Specific context.** | Three layers of context are provided to Haiku, all assembled at startup: (1) `catalog_text` — a structured catalog of all nine tables with descriptions and "best for" usage hints (lines 154–169); (2) `table_schema_overview` — per-table column listings with examples (lines 173–177); (3) per-call schemas — every SQL prompt embeds the exact column names + sample values for the chosen table via `schema_text(df)` (line 254). The accordion at the bottom of the right pane shows the user the same context Haiku receives. |
| **Haiku model only.** | Defined once at the top: `HAIKU_MODEL <- "claude-haiku-4-5"` (line 49). Every `chat_anthropic()` call in the app references this constant: routing (line 196), SQL (line 238), summary boxes (line 293), plot interpretation (line 1004). No other model is used anywhere in the app. |
| **Token-efficient plot interpretation prompt.** | The interpretation prompt (lines 1005–1011) was deliberately tightened from the original 120-word free-form ask down to: *"Interpret this NFL plot in a single brief paragraph of 2–4 sentences (under 60 words total). Do not use any headers, titles, bullet points, or markdown formatting. Plain prose only."* This caps output at ~80 tokens, eliminates wasted tokens on markdown formatting, and skips boilerplate ("avoid generic caveats" pre-empts disclaimer text). |
| **Source-runnable, not deployed.** | App lives entirely in `nfl_querychat_app_clean_dashboard.R`. To run, clone the repo, set the API key env var, and `shiny::runApp("nfl_querychat_app_clean_dashboard.R")`. No deployment URL. The README, eval scripts, and CSVs all live in the same GitHub repo. |
| **Structured evaluation with `{vitals}`.** | `nfl_app_eval.R` builds three `vitals::Task` objects covering all three LLM-driven steps in the app. Tasks use real `{vitals}` scorers (`detect_match`, `detect_includes`, `model_graded_qa`) with appropriate per-task design: exact match for routing, live SQL execution for SQL generation, LLM-as-judge with rubrics for plot interpretation. Test cases are stratified by difficulty (easy/medium/hard) and topic, so the printed summary surfaces the weakest stratum first. `nfl_app_eval_results.R` then drills into per-sample failures, prints API cost, and opens the Inspect log viewer. Methodology is documented in `nfl_app_eval_writeup.md`. |
| **Master's-level rigor and substantial layout deviation.** | The app is a 1,000+ line single file with: (1) two-pane responsive layout sized to one viewport (`page_fillable` + flex CSS, lines 393–399); (2) a per-query live thinking log that streams via `later::later` chains so reactive flushes happen between LLM calls instead of all at once (lines 671–711); (3) dynamically routed table selection via JSON output from a separate Haiku call (lines 195–235); (4) DuckDB-backed SQL execution with per-column-quoting safety (lines 237–265); (5) a custom 40-color palette applied across five plot types with both categorical and continuous fallback scales (lines 782–971); (6) Haiku-generated summary value boxes with a server-side numeric-metric guarantee (lines 284–376). Layout differs substantially from the querychat example: there is no chat-history transcript, no Sidebar layout, no DT result table, and no answer text panel. |
| **Value boxes for key statistics.** | Three value boxes in the "Query summary" card (lines 442–446) are populated by `summary_boxes_with_haiku()` (lines 284–376), which prompts Haiku to return a JSON list of three boxes — title, value, caption — tailored to the question and the SQL result. The function enforces *at least one numeric metric box*: after parsing Haiku's output, it scans the box values for a digit; if none qualify, it replaces the last box with a derived metric (mean of the first numeric column, or rows-returned). Boxes are styled with a custom `.summary-box` gradient + typography (lines 411–416 of CSS). |

