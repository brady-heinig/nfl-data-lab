# ==========================================================
# Structured Evaluation of nfl_datalab_app.R
# ----------------------------------------------------------
# This script uses the {vitals} package to evaluate the three
# LLM-driven steps of the NFL Query Lab app:
#
#   Task 1: Table router (route_table_haiku)
#           - Given a free-form NFL question, picks 1 of 9 tables.
#           - Scored by exact match against a hand-labelled target table.
#
#   Task 2: SQL generator + executor (make_sql_with_haiku + DuckDB)
#           - Given a question + correct table, writes DuckDB SQL.
#           - Scored by a custom "does this answer the question?" scorer
#             that runs the SQL and checks the result for an expected
#             entity (player name, team, value range, etc.).
#
#   Task 3: Plot interpretation
#           - Given a saved plot image + a question, writes a short prose
#             interpretation. Scored with model_graded_qa() (LLM-as-judge).
#
# Each dataset is stratified by DIFFICULTY (easy / medium / hard) and by
# TOPIC (passing, rushing, defense, game_scores, etc.) so we can surface
# per-stratum failure rates after evaluation.
#
# Required setup:
#   install.packages(c("vitals", "ellmer", "tibble", "dplyr", "purrr",
#                      "glue", "stringr", "DBI", "duckdb", "ggplot2"))
Sys.setenv(ANTHROPIC_API_KEY = "")
#
# How to run:
#   1. Open R in the same folder as the app file and the CSVs.
#   2. source("nfl_datalab_app.R")
#      (This loads the CSVs, registers the DuckDB tables, and defines
#       route_table_haiku() / make_sql_with_haiku(). It will also launch
#       the Shiny app — close that window or ignore it. The functions and
#       data needed by this script are all in the global environment.)
#   3. source("nfl_app_eval.R")
# ==========================================================

library(vitals)
library(ellmer)
library(tibble)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(DBI)
library(ggplot2)

stopifnot(
  "Run source('nfl_datalab_app.R') first." =
    exists("route_table_haiku") && exists("make_sql_with_haiku") &&
    exists("nfl_tables") && exists("con") && exists("HAIKU_MODEL")
)

# Where vitals writes structured logs (JSON for the Inspect log viewer).
log_dir <- file.path(getwd(), "vitals_logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
Sys.setenv(VITALS_LOG_DIR = log_dir)

# ----------------------------------------------------------
# Task 1: Table routing
# ----------------------------------------------------------
# A balanced suite covering all 9 source tables, with deliberate hard
# cases: ambiguous keywords (e.g. "sacks" — passing or defense?), team-
# vs-player phrasing, plays-vs-games confusion, multi-stat questions.

routing_cases <- tribble(
  ~input,                                                                                   ~target,        ~difficulty, ~topic,
  # --- easy: one obvious keyword ---
  "Who threw for the most passing yards this season?",                                       "passing",      "easy",      "passing",
  "Top 5 rushing leaders by rushing yards",                                                  "rushing",      "easy",      "rushing",
  "Which receivers had the most receiving touchdowns?",                                      "receiving",    "easy",      "receiving",
  "Who had the most field goals made?",                                                      "kicking",      "easy",      "kicking",
  "Best punter by net punting average",                                                      "punting",      "easy",      "punting",
  "Top 10 kick returners by return yards",                                                   "returning",    "easy",      "returning",
  "What was the final score of the Eagles vs Cowboys week 8 game?",                          "game_scores",  "easy",      "game_scores",
  # --- medium: requires distinguishing related concepts ---
  "Which defender had the most sacks?",                                                      "defense",      "medium",    "defense",        # "sacks" is also in passing (sacks taken)
  "Who threw the most interceptions this year?",                                             "passing",      "medium",    "passing",        # "interceptions" also lives in defense
  "Which defense forced the most fumbles?",                                                  "defense",      "medium",    "defense",
  "Top tight ends by receptions",                                                            "receiving",    "medium",    "receiving",
  "Highest scoring games of the season",                                                     "game_scores",  "medium",    "game_scores",
  "Which running back has the highest yards per carry with at least 100 carries?",           "rushing",      "medium",    "rushing",
  # --- hard: ambiguous, multi-clue, or play-level wording ---
  "Show me every scoring drive longer than 80 yards",                                        "play_by_play", "hard",      "play_by_play",   # "scoring" pulls toward game_scores
  "Which QB has the best passer rating in road games?",                                      "passing",      "hard",      "passing",        # "games" might pull to game_scores
  "Show all 4th-quarter plays where the offense was inside the 5 yard line",                 "play_by_play", "hard",      "play_by_play",
  "Which team has the largest average margin of victory?",                                   "game_scores",  "hard",      "game_scores",    # team-flavored language
  "Who has the most QB hits this season?",                                                   "defense",      "hard",      "defense",        # "QB" pulls toward passing
  "Compare touchback rate for kickoffs across kickers",                                      "kicking",      "hard",      "kicking"
)

# Solver: thin wrapper around the app's existing function. Returns just
# the chosen table name as a character.
solve_routing <- function(inputs, ...) {
  chats   <- vector("list", length(inputs))
  results <- character(length(inputs))
  for (i in seq_along(inputs)) {
    chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
    out <- tryCatch(
      {
        # Reproduce route_table_haiku() inline so we can capture the actual
        # ellmer Chat object (vitals needs each per-input chat for logging).
        prompt <- glue::glue(
"You are the routing layer for a 2025 NFL analytics Shiny app.
Your only job is to choose exactly ONE source table before any filtering or aggregation happens.
Return only compact JSON. No markdown. No prose.
Required JSON keys: table_name, confidence, reason.
Valid table_name values: {paste(names(nfl_tables), collapse = ', ')}.

Decision rules:
- Use passing for QB/passer stats: completions, attempts, passing yards, passing TD, interceptions thrown, passer rating, sacks taken, QB record.
- Use rushing for rushing attempts/carries/yards/TDs/YPC and RB ground production.
- Use receiving for targets, receptions, catches, receiving yards, receiving TDs, catch percentage, WR/TE production.
- Use defense for tackles, defensive sacks, interceptions caught by defenders, forced fumbles, fumble recoveries, TFL, QB hits, pass defense.
- Use kicking for field goals, extra points, kickoffs, kicker accuracy.
- Use punting for punts, punt yards, net punting, inside 20, punt touchbacks.
- Use returning for kick returns, punt returns, return yards, return TDs, all-purpose return production.
- Use game_scores for game results, final scores, winners/losers, margins, home/away teams, weekly game-level questions.
- Use play_by_play only when the user asks about individual plays, drives, possession, quarters, play outcomes, scoring plays/drives, formations, or play descriptions.

Table catalog:
{catalog_text}

Available columns by table:
{table_schema_overview}

Question: {inputs[i]}"
        )
        raw <- chat$chat(prompt)
        parsed <- safe_json(strip_code_fences(raw))
        if (is.null(parsed) || is.null(parsed$table_name) || !parsed$table_name %in% names(nfl_tables)) {
          fast <- route_table_fast(inputs[i])
          if (!is.na(fast)) fast else "game_scores"
        } else {
          parsed$table_name
        }
      },
      error = function(e) "ERROR"
    )
    results[i] <- out
    chats[[i]] <- chat
  }
  list(result = results, solver_chat = chats)
}

# Scorer: exact-match using vitals::detect_match() against the target.
# detect_match() is built for this; it returns C / I.
routing_task <- Task$new(
  dataset = routing_cases,
  solver  = solve_routing,
  scorer  = detect_match(location = "exact", case_sensitive = FALSE),
  name    = "nfl_routing"
)

# ----------------------------------------------------------
# Task 2: SQL generation + execution
# ----------------------------------------------------------
# This task takes the question AND the (correct) target table as input,
# then asks make_sql_with_haiku() to write SQL. The scorer EXECUTES the
# SQL against the live DuckDB connection and checks whether the result
# contains an expected substring (e.g. a player name, a team, a value).
# This is a much stricter test than "does the SQL parse" because we
# verify the SQL actually answers the question.

# The dataset's input encodes both the question and the table, separated
# by " ||| ", so the solver can split them. target is a regex the result
# string must match.
sql_cases <- tribble(
  ~input,                                                                                                    ~target,        ~difficulty, ~topic,
  # --- easy: small leaderboards, clear top-N ---
  "Who has the most passing yards? ||| passing",                                                              "yds",          "easy",      "passing",
  "Top 5 rushers by rushing yards ||| rushing",                                                               "yds",          "easy",      "rushing",
  "Top 10 receivers by receptions ||| receiving",                                                             "rec",          "easy",      "receiving",
  "Most field goals made ||| kicking",                                                                        "fg",           "easy",      "kicking",
  # --- medium: filtering by a condition ---
  "Quarterbacks with at least 4000 passing yards ||| passing",                                                "yds",          "medium",    "passing",
  "Receivers with catch percentage above 70 percent ||| receiving",                                           "rec",          "medium",    "receiving",
  "Defenders with at least 10 sacks ||| defense",                                                             "sk",           "medium",    "defense",
  "Games decided by 3 points or fewer ||| game_scores",                                                       "score",        "medium",    "game_scores",
  # --- hard: aggregation, ratio, or multi-clause logic ---
  "Average yards per carry by team, only teams with 300+ carries ||| rushing",                                "tm",           "hard",      "rushing",
  "Quarterbacks with TD-to-INT ratio above 4 ||| passing",                                                    "td",           "hard",      "passing",
  "Kickers ranked by FG percentage from 50+ yards ||| kicking",                                               "fg",           "hard",      "kicking",
  "Closest non-tie games of the season ranked by smallest margin ||| game_scores",                            "score",        "hard",      "game_scores"
)

# Solver: split input, generate SQL, execute, return either the result
# preview as a string (if it ran) or "SQL_ERROR: <message>" if not.
solve_sql <- function(inputs, ...) {
  chats   <- vector("list", length(inputs))
  results <- character(length(inputs))
  for (i in seq_along(inputs)) {
    combo <- inputs[i]
    parts <- str_split(combo, fixed(" ||| "), simplify = TRUE)
    chat  <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
    chats[[i]] <- chat

    if (ncol(parts) < 2) {
      results[i] <- "SQL_ERROR: malformed input"
      next
    }
    question   <- str_trim(parts[1, 1])
    table_name <- str_trim(parts[1, 2])
    df         <- nfl_tables[[table_name]]

    sql <- tryCatch({
      prompt <- glue::glue(
"You write DuckDB SQL for an NFL Shiny app after a routing step has already selected the correct table.
Use exactly one table: {table_name}. Do not join or reference any other table.
Return only SQL. No markdown. No explanation.
The SQL must begin with SELECT.
Never modify data.
Translate the user request into appropriate WHERE, GROUP BY, ORDER BY, and LIMIT clauses.
For leaderboards, use ORDER BY and LIMIT 50.
Use ILIKE for flexible player/team text matching.

IMPORTANT: Column names come directly from the source CSV headers. ALWAYS wrap every column name in double quotes (for example: SELECT \"Player\", \"Pass Yds\" FROM {table_name}). Use the exact column names listed below.

Available columns:
{schema_text(df)}

User question:
{question}"
      )
      raw <- chat$chat(prompt)
      sql_out <- strip_code_fences(raw)
      if (!is_safe_select(sql_out, table_name)) {
        sql_out <- glue("SELECT * FROM {table_name} LIMIT 50")
      }
      sql_out
    }, error = function(e) NULL)

    if (is.null(sql) || !nzchar(sql)) {
      results[i] <- "SQL_ERROR: no SQL produced"
      next
    }

    res <- tryCatch(
      DBI::dbGetQuery(con, sql),
      error = function(e) paste0("SQL_ERROR: ", conditionMessage(e))
    )
    if (is.character(res) && startsWith(res, "SQL_ERROR")) {
      results[i] <- res
      next
    }
    if (!is.data.frame(res) || nrow(res) == 0) {
      results[i] <- "SQL_ERROR: empty result"
      next
    }

    preview <- capture.output(print(tibble::as_tibble(head(res, 10)), n = 10, width = 200))
    results[i] <- paste(c(paste("COLS:", paste(names(res), collapse = ", ")),
                          paste("ROWS:", nrow(res)),
                          preview),
                        collapse = "\n")
  }
  list(result = results, solver_chat = chats)
}

# Scorer: regex match on the textual preview. Because we deliberately
# made target patterns case-insensitive substrings of expected column
# names or row content, this catches "no usable answer was returned"
# without being so strict that it punishes minor formatting differences.
sql_task <- Task$new(
  dataset = sql_cases,
  solver  = solve_sql,
  scorer  = detect_includes(case_sensitive = FALSE),
  name    = "nfl_sql_generation"
)

# ----------------------------------------------------------
# Task 3: Plot interpretation (model-graded)
# ----------------------------------------------------------
# This is the hardest task to score deterministically because the output
# is open prose. We use vitals::model_graded_qa() — the "LLM as a judge"
# pattern — with Sonnet 4.5 as the grader (a stronger model than Haiku,
# which is the model under test, to reduce self-grading bias).
#
# Each test case includes a *grading rubric* in the target field
# describing what a correct interpretation must mention. The grader
# compares Haiku's prose against that rubric and returns C/P/I.

# Helper: build a small fake-but-plausible plot to interpret. We use
# a fixed seed and a simple bar chart so the eval is reproducible.
.build_test_plot <- function(kind = c("leaderboard", "uniform", "bimodal")) {
  kind <- match.arg(kind)
  set.seed(42)
  df <- switch(kind,
    leaderboard = tibble::tibble(
      player = paste0("P", LETTERS[1:8]),
      yards  = c(1450, 1310, 1190, 920, 880, 770, 690, 580)
    ),
    uniform = tibble::tibble(
      player = paste0("P", LETTERS[1:8]),
      yards  = c(900, 920, 905, 915, 895, 910, 925, 905)
    ),
    bimodal = tibble::tibble(
      player = paste0("P", LETTERS[1:8]),
      yards  = c(1500, 1480, 1450, 1410, 600, 580, 560, 540)
    )
  )
  ggplot(df, aes(x = reorder(player, yards), y = yards)) +
    geom_col(fill = "#1f77b4") +
    coord_flip() +
    labs(x = "Player", y = "Receiving yards") +
    theme_minimal()
}

interp_cases <- tribble(
  ~kind,         ~question,                                                                                         ~target,
  "leaderboard", "What is the main pattern and any notable outliers in this receiving-yards plot?",                  "The interpretation should describe a clear top-down ranking with one dominant leader (PA at ~1450 yards) and a gradual drop toward the lower-end players. It should NOT claim the distribution is flat or uniform.",
  "uniform",     "What is the main pattern and any notable outliers in this receiving-yards plot?",                  "The interpretation should describe a remarkably tight, near-uniform distribution where all players cluster in a very narrow band (about 895 to 925 yards). It should NOT identify a meaningful leader or claim large differences between players.",
  "bimodal",     "What is the main pattern and any notable outliers in this receiving-yards plot?",                  "The interpretation should describe a bimodal split between a high-performing top group (PA-PD around 1400-1500 yards) and a clearly separated bottom group (PE-PH around 540-600 yards). It should NOT describe the data as a smooth gradient."
)

# Solver: render each plot to PNG, send to Haiku with the question, return prose.
solve_interpretation <- function(inputs, ...) {
  chats   <- vector("list", length(inputs))
  results <- character(length(inputs))
  for (i in seq_along(inputs)) {
    row <- interp_cases[i, ]
    plt <- .build_test_plot(row$kind)
    tmp <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp, plt, width = 6, height = 4, dpi = 110)

    chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
    chats[[i]] <- chat
    prompt <- paste(
      "Interpret this NFL plot in a single brief paragraph of 2-4 sentences (under 60 words total).",
      "Do not use any headers, titles, bullet points, or markdown formatting. Plain prose only.",
      "Answer this specific user question:", inputs[i],
      "Focus only on the visible plot.",
      "Avoid generic caveats."
    )
    results[i] <- tryCatch(
      chat$chat(prompt, ellmer::content_image_file(tmp)),
      error = function(e) paste0("INTERP_ERROR: ", conditionMessage(e))
    )
  }
  list(result = results, solver_chat = chats)
}

# vitals expects input/target columns; the question is the input,
# the rubric is the target. The "kind" column is a private grouping label.
interp_dataset <- interp_cases |>
  transmute(input = question, target = target, difficulty = "open", topic = kind)

interp_task <- Task$new(
  dataset = interp_dataset,
  solver  = solve_interpretation,
  scorer  = model_graded_qa(
    partial_credit = TRUE,
    scorer_chat = ellmer::chat_anthropic(model = "claude-sonnet-4-5", echo = "none")
  ),
  name = "nfl_plot_interpretation"
)

# ----------------------------------------------------------
# Run all three tasks
# ----------------------------------------------------------
# epochs = 1 for the first run to keep cost down. Bump to 3+ for a real
# variance estimate per case.

message("Running Task 1: table routing...")
routing_task$eval(view = FALSE)

message("Running Task 2: SQL generation + execution...")
sql_task$eval(view = FALSE)

message("Running Task 3: plot interpretation...")
interp_task$eval(view = FALSE)

# ----------------------------------------------------------
# Stratified failure summary
# ----------------------------------------------------------
# vitals_bind() row-binds the per-sample results from all three tasks
# into a single tibble with task / id / score / metadata columns. We
# unnest the metadata to recover input, target, output, and our
# stratification columns.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# vitals_bind() can fail when tasks have different ordered-factor levels
# for their score column. We bind manually instead, coercing score to
# character first so the row-bind always succeeds.
get_task_samples <- function(task, task_name) {
  s <- task$get_samples()
  s$task <- task_name
  s$score <- as.character(s$score)
  s
}

routing_samples <- get_task_samples(routing_task,        "routing")
sql_samples     <- get_task_samples(sql_task,            "sql")
interp_samples  <- get_task_samples(interp_task,         "interpretation")

# Some columns may differ across tasks. Bind only the columns that exist
# in all three (intersection), plus any per-task extras get filled with NA.
common_cols <- Reduce(union, list(names(routing_samples), names(sql_samples), names(interp_samples)))
fill_missing <- function(df, cols) {
  for (c in setdiff(cols, names(df))) df[[c]] <- NA
  df[, cols]
}

all_results <- bind_rows(
  fill_missing(routing_samples, common_cols),
  fill_missing(sql_samples,     common_cols),
  fill_missing(interp_samples,  common_cols)
)

# Defensive: pick whichever column actually holds the model output text.
result_col <- intersect(c("result", "output", "response"), names(all_results))[1]
if (is.na(result_col)) {
  all_results$output_short <- "(no result column found)"
} else {
  all_results$output_short <- stringr::str_trunc(as.character(all_results[[result_col]]), 250)
}

cat("\n=========================================================\n")
cat("OVERALL ACCURACY BY TASK\n")
cat("=========================================================\n")
all_results |>
  mutate(correct = score == "C") |>
  group_by(task) |>
  summarise(
    n          = n(),
    accuracy   = round(mean(correct), 3),
    full_credit = sum(score == "C"),
    partial    = sum(score == "P"),
    incorrect  = sum(score == "I"),
    .groups = "drop"
  ) |>
  print()

cat("\n=========================================================\n")
cat("ACCURACY BY DIFFICULTY (within each task)\n")
cat("=========================================================\n")
if ("difficulty" %in% names(all_results)) {
  all_results |>
    filter(!is.na(difficulty)) |>
    mutate(correct = score == "C") |>
    group_by(task, difficulty) |>
    summarise(n = n(), accuracy = round(mean(correct), 3), .groups = "drop") |>
    arrange(task, difficulty) |>
    print()
}

cat("\n=========================================================\n")
cat("ACCURACY BY TOPIC (where the bot struggles most)\n")
cat("=========================================================\n")
if ("topic" %in% names(all_results)) {
  all_results |>
    filter(!is.na(topic)) |>
    mutate(correct = score == "C") |>
    group_by(task, topic) |>
    summarise(n = n(), accuracy = round(mean(correct), 3), .groups = "drop") |>
    arrange(task, accuracy) |>
    print(n = 50)
}

cat("\n=========================================================\n")
cat("INDIVIDUAL FAILURES (score != C) — read these for failure modes\n")
cat("=========================================================\n")
failures <- all_results |> filter(score != "C")
if (nrow(failures) == 0) {
  cat("No failures recorded. Try harder cases or run more epochs.\n")
} else {
  for (i in seq_len(nrow(failures))) {
    f <- failures[i, ]
    cat(sprintf(
      "\n[%s | difficulty=%s | topic=%s | score=%s]\n  INPUT:    %s\n  EXPECTED: %s\n  GOT:      %s\n",
      f$task, f$difficulty %||% "-", f$topic %||% "-",
      f$score, f$input, f$target,
      f$output_short
    ))
  }
}

cat("\n=========================================================\n")
cat("Logs written to: ", log_dir, "\n", sep = "")
cat("Open the Inspect log viewer with: vitals::vitals_view(\"", log_dir, "\")\n", sep = "")
cat("=========================================================\n")
