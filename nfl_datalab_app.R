# Folder structure:
#   app.R
#   nfl_passing.csv
#   nfl_rushing.csv
#   nfl_receiving.csv OR nfl_recieving.csv
#   nfl_defense.csv
#   nfl_kicking.csv
#   nfl_punting.csv
#   nfl_returning.csv
#   nfl_game_scores.csv
#   nfl_play_by_play.csv
#
# Required packages:
# install.packages(c(
#   "shiny", "bslib", "ellmer", "querychat", "readr", "dplyr", "tidyr",
#   "stringr", "janitor", "DT", "ggplot2", "scales", "bsicons",
#   "glue", "duckdb", "DBI", "jsonlite", "commonmark", "tibble"
# ))
#
# Required env var:
Sys.setenv(ANTHROPIC_API_KEY = "")

library(shiny)
library(bslib)
library(ellmer)
library(querychat)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(DT)
library(ggplot2)
library(scales)
library(bsicons)
library(glue)
library(DBI)
library(duckdb)
library(jsonlite)
library(commonmark)
library(tibble)
library(later)

options(shiny.maxRequestSize = 200 * 1024^2)

HAIKU_MODEL <- "claude-haiku-4-5"
DATA_DIR <- "."
MAX_ROWS_TO_CHAT <- 25

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# -------------------------------
# Helpers
# -------------------------------

find_file <- function(pattern) {
  matches <- list.files(DATA_DIR, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(matches) == 0) {
    stop(
      "Could not find a CSV matching pattern: ", pattern,
      "\nPut all CSVs in the same folder as app.R, then restart the app."
    )
  }
  matches[[1]]
}

clean_dupe_names <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | x == "" | str_detect(x, "^Unnamed"), "unknown", x)
  janitor::make_clean_names(x, allow_dupes = FALSE)
}

numericize <- function(df, id_cols) {
  df |>
    mutate(across(
      -any_of(id_cols),
      ~ suppressWarnings(readr::parse_number(as.character(.x)))
    ))
}

load_csv_raw <- function(file) {
  readr::read_csv(file, show_col_types = FALSE)
}

safe_json <- function(x) {
  tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
}

strip_code_fences <- function(x) {
  x <- str_trim(x)
  x <- str_remove(x, "^```[a-zA-Z]*\\s*")
  x <- str_remove(x, "\\s*```$")
  str_trim(x)
}

is_safe_select <- function(sql, table_name) {
  s <- str_squish(str_to_lower(sql))
  startsWith(s, "select") &&
    str_detect(s, paste0("from\\s+", table_name, "\\b")) &&
    !str_detect(s, "\\b(insert|update|delete|drop|alter|create|attach|detach|copy|pragma)\\b")
}

schema_text <- function(df) {
  cols <- names(df)
  types <- vapply(df, function(x) class(x)[1], character(1))
  examples <- vapply(df, function(x) {
    vals <- unique(na.omit(as.character(x)))
    vals <- vals[vals != ""]
    paste(head(vals, 3), collapse = ", ")
  }, character(1))
  paste(sprintf("- %s (%s). Examples: %s", cols, types, ifelse(examples == "", "NA", examples)), collapse = "\n")
}

# -------------------------------
# Load separate tables
# -------------------------------

nfl_tables <- list(
  passing = load_csv_raw(find_file("nfl_passing.*csv$")),
  rushing = load_csv_raw(find_file("nfl_rushing.*csv$")),
  receiving = load_csv_raw(find_file("nfl_rec(ei|ie)ving.*csv$")),
  defense = load_csv_raw(find_file("nfl_defense.*csv$")),
  kicking = load_csv_raw(find_file("nfl_kicking.*csv$")),
  punting = load_csv_raw(find_file("nfl_punting.*csv$")),
  returning = load_csv_raw(find_file("nfl_returning.*csv$")),
  game_scores = load_csv_raw(find_file("nfl_game_scores.*csv$")),
  play_by_play = load_csv_raw(find_file("nfl_play_by_play.*csv$"))
)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
onStop(function() {
  try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
})

for (nm in names(nfl_tables)) {
  DBI::dbWriteTable(con, nm, nfl_tables[[nm]], overwrite = TRUE)
}

# -------------------------------
# Table catalog and routing
# -------------------------------

table_catalog <- tibble::tribble(
  ~table_name, ~description, ~best_for,
  "passing", "Player-season passing statistics, one row per passer.", "passing yards, completions, attempts, TDs, INTs, passer rating, QBR, sacks, QB records",
  "rushing", "Player-season rushing statistics, one row per rusher.", "carries, rushing yards, rushing TDs, yards per attempt, running backs",
  "receiving", "Player-season receiving statistics, one row per receiver.", "targets, receptions, receiving yards, receiving TDs, catch percentage, WR/TE stats",
  "defense", "Player-season defensive statistics, one row per defender.", "tackles, sacks, INTs, forced fumbles, fumble recoveries, TFL, QB hits",
  "kicking", "Player-season kicking statistics, one row per kicker.", "field goals, extra points, FG percentage, kickoffs, touchbacks",
  "punting", "Player-season punting statistics, one row per punter.", "punts, punt yards, yards per punt, net yards, inside 20, touchbacks",
  "returning", "Player-season return statistics, one row per returner.", "punt returns, kick returns, return yards, return touchdowns, all-purpose yards",
  "game_scores", "Game-level scores, one row per game.", "scores, winners, losers, margins, total points, home/away teams, weeks",
  "play_by_play", "Play-level records, one row per play.", "individual plays, drives, possession, quarters, scoring plays, play outcomes, descriptions"
)

catalog_text <- paste(
  glue::glue("- {table_catalog$table_name}: {table_catalog$description} Best for: {table_catalog$best_for}"),
  collapse = "\n"
)

compact_schema <- function(df, max_cols = 70) {
  cols <- names(df)
  types <- vapply(df, function(x) class(x)[1], character(1))
  paste(head(paste0(cols, "<", types, ">"), max_cols), collapse = ", ")
}

table_schema_overview <- paste(
  vapply(names(nfl_tables), function(nm) {
    paste0("- ", nm, ": ", compact_schema(nfl_tables[[nm]]))
  }, character(1)),
  collapse = "\n"
)

route_table_fast <- function(question) {
  q <- str_to_lower(question)
  rules <- list(
    passing = c("pass", "passing", "quarterback", "qb", "completion", "attempt", "interception", "passer", "qbr", "sack", "cmp", "td pass"),
    rushing = c("rush", "rushing", "carry", "carries", "running back", "rb", "yards per carry", "ground"),
    receiving = c("receiv", "reception", "catch", "catches", "target", "wide receiver", "receiver", "wr", "tight end", "te"),
    defense = c("defense", "defensive", "tackle", "sack", "interception", "forced fumble", "fumble recovery", "pass deflection", "qb hit", "tfl"),
    kicking = c("kick", "kicker", "field goal", "extra point", "fg", "xpm", "touchback", "kickoff"),
    punting = c("punt", "punter", "inside 20", "net yards", "blocked punt"),
    returning = c("return", "returner", "punt return", "kick return", "all-purpose", "apy"),
    game_scores = c("score", "scores", "game", "winner", "loser", "margin", "home team", "away team", "total points", "week"),
    play_by_play = c("play by play", "play-by-play", "drive", "quarter", "play outcome", "scoring play", "scoring drive", "possession", "formation", "play description")
  )
  scores <- vapply(rules, function(keys) sum(str_detect(q, fixed(keys, ignore_case = TRUE))), numeric(1))
  if (str_detect(q, "defender|defensive|tackle|qb hit|tfl|forced fumble|pass deflection")) return("defense")
  if (max(scores) >= 1) names(which.max(scores)) else NA_character_
}

route_table_haiku <- function(question) {
  chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
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
- If a player name is present, still choose the table based on the requested statistic.
- If a team name is present, still choose the table based on the requested statistic.
- Do NOT choose play_by_play just because a team or week is mentioned; choose game_scores for game result questions.

Table catalog:
{catalog_text}

Available columns by table:
{table_schema_overview}

Question: {question}"
  )
  out <- chat$chat(prompt)
  parsed <- safe_json(strip_code_fences(out))
  if (is.null(parsed) || is.null(parsed$table_name) || !parsed$table_name %in% names(nfl_tables)) {
    fast <- route_table_fast(question)
    fallback <- if (!is.na(fast)) fast else "game_scores"
    list(table_name = fallback, confidence = 0.25, reason = paste0("Fallback routing selected ", fallback, " because Haiku routing JSON could not be parsed."))
  } else {
    list(table_name = parsed$table_name, confidence = parsed$confidence %||% NA_real_, reason = parsed$reason %||% "Selected by Haiku router.")
  }
}

make_sql_with_haiku <- function(question, table_name) {
  chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
  df <- nfl_tables[[table_name]]
  prompt <- glue::glue(
"You write DuckDB SQL for an NFL Shiny app after a routing step has already selected the correct table.
Use exactly one table: {table_name}. Do not join or reference any other table.
Return only SQL. No markdown. No explanation.
The SQL must begin with SELECT.
Never modify data.
Translate the user request into appropriate WHERE, GROUP BY, ORDER BY, and LIMIT clauses.
For leaderboards, use ORDER BY and LIMIT 50.
For raw row requests, use LIMIT 200.
Use ILIKE for flexible player/team text matching.
If the user asks for a team-level or league-wide stat from a player table, aggregate with GROUP BY team or no GROUP BY as appropriate.

IMPORTANT: Column names come directly from the source CSV headers and may contain spaces, mixed case, or special characters. ALWAYS wrap every column name in double quotes (for example: SELECT \"Player\", \"Pass Yds\" FROM {table_name}). Use the exact column names listed below; do not invent, rename, or normalize them.

Available columns:
{schema_text(df)}

User question:
{question}"
  )
  sql <- strip_code_fences(chat$chat(prompt))
  if (!is_safe_select(sql, table_name)) {
    sql <- glue("SELECT * FROM {table_name} LIMIT 50")
  }
  sql
}

summarize_with_haiku <- function(question, table_name, sql, result_df) {
  chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
  preview <- result_df |> head(MAX_ROWS_TO_CHAT) |> jsonlite::toJSON(dataframe = "rows", na = "null")
  prompt <- glue::glue(
"Answer the NFL question using only the result preview.
Be concise. Mention the selected table and the main finding.
If the result is empty, say no matching rows were found and suggest a more specific query.

Question: {question}
Selected table: {table_name}
SQL: {sql}
Rows returned: {nrow(result_df)}
Preview JSON: {preview}"
  )
  chat$chat(prompt)
}

summary_boxes_with_haiku <- function(question, table_name, sql, result_df) {
  if (nrow(result_df) == 0) {
    return(list(
      list(title = "Rows returned", value = "0", caption = "No rows matched"),
      list(title = "Source table", value = table_name, caption = "From router"),
      list(title = "Status", value = "Empty result", caption = "Try loosening filters")
    ))
  }

  chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
  preview <- result_df |> head(MAX_ROWS_TO_CHAT) |> jsonlite::toJSON(dataframe = "rows", na = "null")
  prompt <- glue::glue(
"You generate exactly 3 summary 'value boxes' that summarize the most useful findings from an NFL SQL query result.
Each box has: title (3-5 words), value (a single number, name, or short phrase, max 25 chars), caption (a short context line, max 40 chars).

REQUIRED: AT LEAST ONE of the three boxes must be a numeric metric. A numeric metric means the value is a raw number, a number with a unit, a percentage, a count, a rank (e.g. '#3'), an average, a sum, or a ratio. Pure name boxes (e.g. value = 'Patrick Mahomes') do not satisfy this. Pure phrase boxes (e.g. value = 'Tie game') do not satisfy this.

Good metric values: '4,183', '67.2%', '32 TDs', '#1 of 32', '24.5 ppg', '+12 margin', '8 games'.

Pick boxes that fit the question. Examples:
- Leaderboard query: top performer's name (text), their headline stat (METRIC), how far ahead of #2 or league average (METRIC).
- Single player query: the headline stat (METRIC), a secondary stat (METRIC), a rank or comparison (METRIC or text).
- Team query: team name (text), total or average of the headline stat (METRIC), games played or count (METRIC).
- Game-level query: matchup (text), final score or margin (METRIC), winner (text).

Return ONLY compact JSON with this exact shape (no markdown, no prose):
{{\"boxes\": [{{\"title\": \"...\", \"value\": \"...\", \"caption\": \"...\"}}, {{\"title\": \"...\", \"value\": \"...\", \"caption\": \"...\"}}, {{\"title\": \"...\", \"value\": \"...\", \"caption\": \"...\"}}]}}

Question: {question}
Selected table: {table_name}
SQL: {sql}
Rows returned: {nrow(result_df)}
Preview JSON: {preview}"
  )
  out <- chat$chat(prompt)
  parsed <- safe_json(strip_code_fences(out))

  if (is.null(parsed) || is.null(parsed$boxes)) {
    return(list(
      list(title = "Rows returned", value = as.character(nrow(result_df)), caption = paste("From", table_name)),
      list(title = "Columns", value = as.character(ncol(result_df)), caption = "In result set"),
      list(title = "Source table", value = table_name, caption = "Selected by router")
    ))
  }

  boxes_df <- parsed$boxes
  boxes <- lapply(seq_len(nrow(boxes_df)), function(i) {
    list(
      title = boxes_df$title[[i]] %||% "—",
      value = boxes_df$value[[i]] %||% "—",
      caption = boxes_df$caption[[i]] %||% ""
    )
  })

  # Enforce: at least one box must contain a numeric metric. If Haiku didn't
  # comply, replace the last (often weakest) box with a derived numeric metric.
  has_metric <- any(vapply(boxes, function(b) {
    str_detect(as.character(b$value %||% ""), "[0-9]")
  }, logical(1)))

  if (!has_metric) {
    num_cols <- names(result_df)[vapply(result_df, is.numeric, logical(1))]
    fallback <- if (length(num_cols) > 0) {
      col <- num_cols[1]
      vals <- result_df[[col]]
      list(
        title = paste("Avg", col),
        value = format(round(mean(vals, na.rm = TRUE), 2), big.mark = ",", trim = TRUE),
        caption = paste0("Across ", nrow(result_df), " rows")
      )
    } else {
      list(
        title = "Rows returned",
        value = format(nrow(result_df), big.mark = ",", trim = TRUE),
        caption = paste("From", table_name)
      )
    }
    boxes[[length(boxes)]] <- fallback
  }

  boxes
}

# -------------------------------
# UI
# -------------------------------

ui <- page_fillable(
  title = "NFL Data Lab",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      html, body { height: 100%; overflow: hidden; }
      body { background: #f5f7fb; }
      .app-shell { height: 100vh; padding: 8px; gap: 8px; }
      .left-panel { height: calc(100vh - 16px); overflow: hidden; display: flex; flex-direction: column; gap: 8px; min-height: 0; }
      .left-panel > .card { flex: 0 0 auto; }
      .left-panel > .thinking-card { flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; }
      .left-panel > .thinking-card > .card-body { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
      .right-panel { height: calc(100vh - 16px); overflow-y: auto; display: flex; flex-direction: column; gap: 8px; }
      .brand-card { background: linear-gradient(135deg, #152a53 0%, #5084c4 100%); color: white; border: 0; }
      .brand-card .card-header { color: white; border-bottom: 1px solid rgba(255,255,255,.25); padding: .4rem .75rem; }
      .example-queries { margin-top: .35rem; display: flex; flex-direction: column; gap: .15rem; }
      .example-label { font-size: .72rem; font-weight: 700; text-transform: uppercase; letter-spacing: .04em; color: rgba(255,255,255,.7); margin-bottom: .15rem; }
      .example-link { display: block; color: #ffffff !important; text-decoration: underline; text-decoration-color: rgba(255,255,255,.45); text-underline-offset: 2px; font-size: .8rem; line-height: 1.25; padding: .1rem 0; cursor: pointer; }
      .example-link:hover { color: #ffe9a8 !important; text-decoration-color: #ffe9a8; }
      .brand-card .card-body { padding: .5rem .75rem; }
      .brand-card h4 { margin: 0 0 .25rem 0; font-size: 1rem; }
      .brand-card p { margin: 0 0 .25rem 0; font-size: .8rem; }
      .stage-list { margin-bottom: 0; padding-left: 1rem; font-size: .8rem; }
      .stage-list li { margin-bottom: .15rem; }
      .thinking-card .card-body { padding: .5rem .75rem; }
      .thinking-empty { color: #6c757d; font-size: .8rem; font-style: italic; }
      .thinking-step { display: flex; align-items: flex-start; gap: .4rem; padding: .25rem 0; font-size: .8rem; line-height: 1.3; border-bottom: 1px dashed #e6e9ef; }
      .thinking-step:last-child { border-bottom: 0; }
      .thinking-num { color: #6c757d; font-weight: 600; min-width: 1.2rem; }
      .thinking-text { flex: 1; }
      .thinking-active .thinking-text { color: #152a53; font-weight: 600; }
      .thinking-done .thinking-text { color: #495057; }
      .thinking-check { color: #2e8b57; font-weight: 700; min-width: 1rem; display: inline-block; }
      .thinking-spinner { width: .9rem; height: .9rem; min-width: .9rem; border: 2px solid #c5d4ec; border-top-color: #152a53; border-radius: 50%; display: inline-block; animation: thinking-spin .8s linear infinite; margin-top: .15rem; }
      @keyframes thinking-spin { to { transform: rotate(360deg); } }
      .compact-card .card-body { padding: .5rem .75rem; }
      .compact-card .card-header { padding: .4rem .75rem; font-size: .9rem; }
      .card-header { padding: .4rem .75rem; font-size: .9rem; }
      .card-body { padding: .5rem .75rem; }
      .form-label { margin-bottom: .15rem; font-size: .8rem; }
      .form-control, .form-select { padding: .25rem .5rem; font-size: .85rem; }
      .plot-card .irs { margin-top: 1.4rem; }
      .plot-card .irs-with-grid { min-height: 0; }
      .query-button { width: 100%; font-weight: 700; padding: .35rem; }
      .data-card { min-height: 0; }
      .plot-card { min-height: 0; }
      .summary-card { flex: 0 0 auto; }
      .summary-card .card-body { padding: .6rem .75rem; overflow: visible; }
      .summary-box { background: linear-gradient(135deg, #f5f9ff 0%, #e8f0fb 100%); border: 1px solid #d6e1f1; border-radius: 10px; padding: .75rem 1rem; min-height: 90px; display: flex; flex-direction: column; justify-content: center; }
      .summary-title { font-size: .75rem; font-weight: 700; text-transform: uppercase; letter-spacing: .03em; color: #5a6b8c; margin-bottom: .25rem; }
      .summary-value { font-size: 1.6rem; font-weight: 800; color: #152a53; line-height: 1.15; margin-bottom: .2rem; word-break: break-word; }
      .summary-caption { font-size: .78rem; color: #6c757d; line-height: 1.25; }
      .interpret-card .card-body { padding: .6rem .75rem; }
      h6 { font-size: .85rem; margin-bottom: .25rem; }
      hr { margin: .4rem 0; }
      .interpret-output { min-height: 100px; max-height: 220px; overflow-y: auto; background: #ffffff; border: 1px solid #dee2e6; border-radius: 8px; padding: 8px; font-size: .85rem; }
      .status-pill { display: inline-block; padding: .2rem .5rem; border-radius: 999px; background: #e8f2ff; color: #152a53; font-weight: 700; margin-bottom: .35rem; font-size: .8rem; }
      .small-muted { color: #6c757d; font-size: .8rem; }
      pre { font-size: .75rem; padding: .4rem; margin-bottom: 0; max-height: 120px; }
      .dataTables_wrapper { font-size: .8rem; }
      table.dataTable { font-size: .8rem; }
    "))
  ),

  layout_columns(
    class = "app-shell",
    col_widths = c(3, 9),

    div(
      class = "left-panel",
      card(
        class = "brand-card compact-card",
        card_header("2025 NFL Query Lab"),
        h4("Welcome 👋"),
        p("Ask any question about NFL players, teams, or games, and this app will respond with answers grounded in data from the 2025 NFL season."),
        div(
          class = "example-queries",
          tags$div(class = "example-label", "Example Questions Include:"),
          actionLink("ex1", "Top 10 receivers by receiving yards", class = "example-link"),
          actionLink("ex2", "Which QBs have the highest passer rating?", class = "example-link"),
          actionLink("ex3", "Most sacks by a defender this season", class = "example-link"),
          actionLink("ex4", "Closest games of the season by margin", class = "example-link")
        )
      ),

      card(
        class = "compact-card",
        card_header("Ask the data"),
        selectInput(
          "query_type",
          "Query type",
          choices = c(
            "Auto-detect" = "auto",
            "Player stat question" = "player",
            "Team stat question" = "team",
            "League-wide leaderboard/summary" = "league",
            "Game score/result question" = "game",
            "Play-by-play question" = "play"
          ),
          selected = "auto"
        ),
        textAreaInput(
          "question",
          "Question",
          rows = 3,
          placeholder = "Example: Which receivers had the most receiving touchdowns?"
        ),
        actionButton("run_query", "Submit query", class = "btn-primary query-button")
      ),

      card(
        class = "compact-card thinking-card",
        card_header("Model Reasoning Log"),
        uiOutput("thinking_log")
      )
    ),

    div(
      class = "right-panel",
      card(
        class = "compact-card summary-card",
        card_header("Query summary"),
        uiOutput("summary_boxes")
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          class = "plot-card",
          card_header("Query Visualization"),
          layout_columns(
            col_widths = c(2, 2, 2, 2, 4),
            selectInput("plot_type", "Plot type", choices = c("Auto", "Bar", "Line", "Scatter", "Histogram", "Boxplot"), selected = "Auto"),
            selectInput("x_var", "X variable", choices = character(0)),
            selectInput("y_var", "Y variable", choices = character(0)),
            selectInput("color_var", "Color/group", choices = "None"),
            sliderInput("top_n", label = NULL, min = 5, max = 40, value = 15, step = 5)
          ),
          plotOutput("main_plot", height = 320)
        ),

        card(
          class = "interpret-card",
          card_header("Plot interpretation"),
          textAreaInput(
            "interpret_question",
            "Interpretation question",
            rows = 2,
            placeholder = "Example: What is the main football takeaway from this plot?",
            value = "What is the main pattern, any notable outliers, or the value we can pull from this plot?"
          ),
          actionButton("interpret_plot", "Interpret plot", class = "btn-primary query-button"),
          div(class = "interpret-output", style = "margin-top: .5rem;", uiOutput("plot_interpretation"))
        )
      ),

      accordion(
        open = FALSE,
        accordion_panel(
          "Data dictionary and app context",
          layout_columns(
            col_widths = c(4, 8),
            card(card_header("Available tables"), DTOutput("catalog_table")),
            card(
              card_header("Selected table schema"),
              selectInput("dict_table", "Choose table", choices = names(nfl_tables), selected = "passing"),
              DTOutput("schema_table")
            )
          ),
          card(
            card_header("Specific context provided to Haiku"),
            p("The app uses Haiku only. It first routes the question to exactly one table, then generates DuckDB SQL against that table only, summarizes the result, and optionally interprets a saved plot image. No tables are combined or joined."),
            verbatimTextOutput("context_text")
          )
        )
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------

server <- function(input, output, session) {
  thinking_steps <- reactiveVal(list())
  selected_table <- reactiveVal(NA_character_)
  route_reason <- reactiveVal("No query has been routed yet.")
  current_sql <- reactiveVal("No SQL generated yet.")
  current_result <- reactiveVal(tibble())
  summary_boxes <- reactiveVal(list(
    list(title = "Awaiting query", value = "—", caption = "Submit a question to populate"),
    list(title = "Awaiting query", value = "—", caption = "Submit a question to populate"),
    list(title = "Awaiting query", value = "—", caption = "Submit a question to populate")
  ))
  plot_interpretation <- reactiveVal("Generate a plot, then click Interpret Plot with Haiku.")

  set_stage <- function(txt) {
    steps <- isolate(thinking_steps())
    if (length(steps) > 0) {
      last_idx <- length(steps)
      steps[[last_idx]]$status <- "done"
    }
    steps[[length(steps) + 1]] <- list(text = txt, status = "active")
    thinking_steps(steps)
  }

  finalize_stages <- function() {
    steps <- isolate(thinking_steps())
    if (length(steps) > 0) {
      steps[[length(steps)]]$status <- "done"
      thinking_steps(steps)
    }
  }

  output$thinking_log <- renderUI({
    steps <- thinking_steps()
    if (length(steps) == 0) {
      return(div(class = "thinking-empty", "Submit a query to see the model's reasoning steps appear here in real time."))
    }
    tagList(
      lapply(seq_along(steps), function(i) {
        s <- steps[[i]]
        icon <- if (identical(s$status, "active")) {
          tags$span(class = "thinking-spinner")
        } else {
          tags$span(class = "thinking-check", HTML("&#10003;"))
        }
        div(
          class = paste0("thinking-step thinking-", s$status),
          icon,
          tags$span(class = "thinking-num", paste0(i, ".")),
          tags$span(class = "thinking-text", s$text)
        )
      })
    )
  })

  output$summary_boxes <- renderUI({
    boxes <- summary_boxes()
    n <- max(length(boxes), 1)
    box_divs <- lapply(boxes, function(b) {
      div(
        class = "summary-box",
        div(class = "summary-title", b$title %||% ""),
        div(class = "summary-value", b$value %||% "—"),
        div(class = "summary-caption", b$caption %||% "")
      )
    })
    do.call(layout_columns, c(list(col_widths = rep(12 %/% n, n)), box_divs))
  })

  output$context_text <- renderText({ catalog_text })

  example_queries <- list(
    ex1 = "Top 10 receivers by receiving yards",
    ex2 = "Which QBs have the highest passer rating?",
    ex3 = "Most sacks by a defender this season",
    ex4 = "Closest games of the season by margin"
  )
  for (eid in names(example_queries)) {
    local({
      this_id <- eid
      this_text <- example_queries[[eid]]
      observeEvent(input[[this_id]], {
        updateTextAreaInput(session, "question", value = this_text)
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$run_query, {
    req(str_squish(input$question) != "")
    question <- str_squish(input$question)
    query_type <- input$query_type %||% "auto"
    routing_question <- if (!identical(query_type, "auto")) {
      paste0(question, "\nUser-selected query type: ", query_type)
    } else {
      question
    }

    thinking_steps(list())

    # Each stage is scheduled via later::later so the reactive log
    # flushes to the UI between steps rather than all at once at the end.
    set_stage("Stage 1/4: using Haiku to identify the single best source table for this question...")

    later::later(function() {
      route <- route_table_haiku(routing_question)
      table_name <- route$table_name
      reason <- paste0("Haiku selected ", table_name, " before SQL generation. Reason: ", route$reason)
      selected_table(table_name)
      route_reason(reason)

      set_stage(reason)
      set_stage(paste0("Stage 2/4: generating SQL for the ", table_name, " table only..."))

      later::later(function() {
        sql <- make_sql_with_haiku(routing_question, table_name)
        current_sql(sql)

        set_stage("Stage 3/4: running the SQL filter/aggregation...")

        later::later(function() {
          executed_sql <- sql
          result <- tryCatch(
            DBI::dbGetQuery(con, sql) |> as_tibble(),
            error = function(e) {
              fallback_sql <- glue("SELECT * FROM {table_name} LIMIT 50")
              executed_sql <<- fallback_sql
              current_sql(fallback_sql)
              DBI::dbGetQuery(con, fallback_sql) |> as_tibble()
            }
          )
          current_result(result)

          set_stage("Stage 4/4: building summary metrics with Haiku...")

          later::later(function() {
            boxes <- summary_boxes_with_haiku(question, table_name, executed_sql, result)
            summary_boxes(boxes)

            set_stage(paste0("Done. Selected table: ", table_name, ". Rows returned: ", nrow(result), "."))
            finalize_stages()
          }, 0.05)
        }, 0.05)
      }, 0.05)
    }, 0.05)
  })

  output$catalog_table <- renderDT({
    datatable(table_catalog, options = list(pageLength = 9, scrollX = TRUE, dom = "t"), rownames = FALSE)
  })

  output$schema_table <- renderDT({
    df <- nfl_tables[[input$dict_table]]
    schema <- tibble(
      column = names(df),
      type = vapply(df, function(x) class(x)[1], character(1)),
      non_missing = vapply(df, function(x) sum(!is.na(x)), integer(1)),
      example_values = vapply(df, function(x) {
        vals <- unique(na.omit(as.character(x)))
        vals <- vals[vals != ""]
        paste(head(vals, 5), collapse = ", ")
      }, character(1))
    )
    datatable(schema, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  observe({
    df <- current_result()
    if (nrow(df) == 0) {
      tbl <- selected_table()
      df <- if (!is.na(tbl) && tbl %in% names(nfl_tables)) nfl_tables[[tbl]] else nfl_tables$passing
    }

    all_cols <- names(df)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]

    if (length(all_cols) == 0) return(NULL)
    updateSelectInput(session, "x_var", choices = all_cols, selected = if (length(cat_cols) > 0) cat_cols[1] else all_cols[1])
    updateSelectInput(session, "y_var", choices = c("None", num_cols), selected = if (length(num_cols) > 0) num_cols[1] else "None")
    updateSelectInput(session, "color_var", choices = c("None", cat_cols), selected = "None")
  })

  auto_plot_type <- reactive({
    df <- current_result()
    req(nrow(df) > 0, input$x_var)
    y <- input$y_var
    x_is_num <- input$x_var %in% names(df) && is.numeric(df[[input$x_var]])
    y_is_num <- !is.null(y) && y != "None" && y %in% names(df) && is.numeric(df[[y]])

    if (input$plot_type != "Auto") return(input$plot_type)
    if (x_is_num && y_is_num) "Scatter" else if (x_is_num && !y_is_num) "Histogram" else "Bar"
  })

  blank_plot <- function(message = "Run a query first, then choose variables for the plot.") {
    ggplot() +
      annotate("text", x = 0, y = 0, label = message, size = 5) +
      theme_void()
  }

  plot_data <- reactive({
    df <- current_result()

    if (is.null(df) || nrow(df) == 0) {
      return(tibble())
    }

    if (is.null(input$x_var) || !input$x_var %in% names(df)) {
      return(tibble())
    }

    pt <- auto_plot_type()

    if (
      identical(pt, "Bar") &&
      !is.null(input$y_var) &&
      input$y_var != "None" &&
      input$y_var %in% names(df)
    ) {
      df |>
        filter(!is.na(.data[[input$x_var]]), !is.na(.data[[input$y_var]])) |>
        arrange(desc(.data[[input$y_var]])) |>
        slice_head(n = input$top_n)
    } else {
      df
    }
  })

  # Vibrant palette for default-coloring when the user has not selected a color_var.
  nfl_palette <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#17becf", "#bcbd22", "#7f7f7f",
    "#003f5c", "#ffa600", "#bc5090", "#58508d", "#ff6361",
    "#00b894", "#fdcb6e", "#0984e3", "#e17055", "#6c5ce7",
    "#00cec9", "#fab1a0", "#74b9ff", "#a29bfe", "#55efc4",
    "#ffeaa7", "#fd79a8", "#81ecec", "#ffeaa7", "#dfe6e9",
    "#2d3436", "#636e72", "#b2bec3", "#dfe6e9", "#ff9f43",
    "#ee5253", "#0abde3", "#10ac84", "#feca57", "#5f27cd"
  )

  recycle_palette <- function(n) {
    if (n <= 0) return(character(0))
    rep_len(nfl_palette, n)
  }

  make_plot <- reactive({
    df <- plot_data()

    if (is.null(df) || nrow(df) == 0) {
      return(blank_plot("No rows are available to plot. Run a query or loosen your filters."))
    }

    if (is.null(input$x_var) || !input$x_var %in% names(df)) {
      return(blank_plot("Choose a valid X variable before plotting."))
    }

    pt <- auto_plot_type()
    color_col <- if (!is.null(input$color_var) && input$color_var != "None" && input$color_var %in% names(df)) input$color_var else NULL

    if (identical(pt, "Scatter")) {
      if (is.null(input$y_var) || input$y_var == "None" || !input$y_var %in% names(df)) {
        return(blank_plot("Scatter plots require a valid numeric Y variable."))
      }
      if (!is.numeric(df[[input$x_var]]) || !is.numeric(df[[input$y_var]])) {
        return(blank_plot("Scatter plots require numeric X and Y variables."))
      }

      p <- ggplot(df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]]))
      if (!is.null(color_col)) {
        p <- p + geom_point(aes(color = .data[[color_col]]), alpha = 0.85, size = 2.5, na.rm = TRUE)
        if (!is.numeric(df[[color_col]])) {
          n_lvl <- length(unique(df[[color_col]]))
          p <- p + scale_color_manual(values = recycle_palette(n_lvl))
        } else {
          p <- p + scale_color_viridis_c(option = "turbo")
        }
      } else {
        # Color by X value bins for visual variety.
        p <- p + geom_point(aes(color = .data[[input$x_var]]), alpha = 0.85, size = 2.5, na.rm = TRUE) +
          scale_color_viridis_c(option = "turbo", guide = "none")
      }
    } else if (identical(pt, "Line")) {
      if (is.null(input$y_var) || input$y_var == "None" || !input$y_var %in% names(df)) {
        return(blank_plot("Line plots require a valid numeric Y variable."))
      }
      if (!is.numeric(df[[input$y_var]])) {
        return(blank_plot("Line plots require a numeric Y variable."))
      }

      x_is_num <- is.numeric(df[[input$x_var]])
      line_df <- df |> filter(!is.na(.data[[input$x_var]]), !is.na(.data[[input$y_var]]))

      if (nrow(line_df) == 0) {
        return(blank_plot("No non-missing X/Y pairs are available to plot."))
      }

      if (!is.null(color_col)) {
        # Multi-line: one line per group level.
        if (x_is_num) {
          line_df <- line_df |> arrange(.data[[input$x_var]])
          p <- ggplot(line_df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = .data[[color_col]], group = .data[[color_col]])) +
            geom_line(linewidth = 1.1, na.rm = TRUE) +
            geom_point(size = 2.2, alpha = 0.9, na.rm = TRUE)
        } else {
          x_levels <- line_df |>
            group_by(.data[[input$x_var]]) |>
            summarise(med = median(.data[[input$y_var]], na.rm = TRUE), .groups = "drop") |>
            arrange(med) |>
            pull(.data[[input$x_var]])
          line_df[[input$x_var]] <- factor(line_df[[input$x_var]], levels = x_levels)
          p <- ggplot(line_df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = .data[[color_col]], group = .data[[color_col]])) +
            geom_line(linewidth = 1.1, na.rm = TRUE) +
            geom_point(size = 2.2, alpha = 0.9, na.rm = TRUE)
        }
        if (!is.numeric(line_df[[color_col]])) {
          n_lvl <- length(unique(line_df[[color_col]]))
          p <- p + scale_color_manual(values = recycle_palette(n_lvl))
        } else {
          p <- p + scale_color_viridis_c(option = "turbo")
        }
      } else {
        # Single line. If X is categorical, aggregate Y per X level so the line is well-defined.
        if (x_is_num) {
          line_df <- line_df |> arrange(.data[[input$x_var]])
          p <- ggplot(line_df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
            geom_line(aes(color = .data[[input$x_var]]), linewidth = 1.2, na.rm = TRUE) +
            geom_point(aes(color = .data[[input$x_var]]), size = 2.6, na.rm = TRUE) +
            scale_color_viridis_c(option = "turbo", guide = "none")
        } else {
          agg_df <- line_df |>
            group_by(.data[[input$x_var]]) |>
            summarise(.value = mean(.data[[input$y_var]], na.rm = TRUE), .groups = "drop") |>
            arrange(.value)
          agg_df[[input$x_var]] <- factor(agg_df[[input$x_var]], levels = agg_df[[input$x_var]])
          n_pts <- nrow(agg_df)
          p <- ggplot(agg_df, aes(x = .data[[input$x_var]], y = .value, group = 1)) +
            geom_line(color = "#152a53", linewidth = 1.2, na.rm = TRUE) +
            geom_point(aes(color = .data[[input$x_var]]), size = 3.2, na.rm = TRUE) +
            scale_color_manual(values = recycle_palette(n_pts), guide = "none") +
            labs(y = paste0("Mean ", input$y_var))
        }
      }
    } else if (identical(pt, "Histogram")) {
      if (!is.numeric(df[[input$x_var]])) {
        return(blank_plot("Histogram requires a numeric X variable."))
      }

      if (!is.null(color_col)) {
        p <- ggplot(df, aes(x = .data[[input$x_var]], fill = .data[[color_col]])) +
          geom_histogram(bins = 30, alpha = 0.85, na.rm = TRUE)
        if (!is.numeric(df[[color_col]])) {
          n_lvl <- length(unique(df[[color_col]]))
          p <- p + scale_fill_manual(values = recycle_palette(n_lvl))
        } else {
          p <- p + scale_fill_viridis_c(option = "turbo")
        }
      } else {
        # Use after_stat(x) so each bin gets its own vibrant color.
        p <- ggplot(df, aes(x = .data[[input$x_var]])) +
          geom_histogram(aes(fill = after_stat(x)), bins = 30, alpha = 0.9, na.rm = TRUE, color = "white") +
          scale_fill_viridis_c(option = "turbo", guide = "none")
      }
    } else if (identical(pt, "Boxplot")) {
      if (is.null(input$y_var) || input$y_var == "None" || !input$y_var %in% names(df) || !is.numeric(df[[input$y_var]])) {
        return(blank_plot("Boxplots require a valid numeric Y variable."))
      }

      p <- ggplot(df, aes(x = as.factor(.data[[input$x_var]]), y = .data[[input$y_var]]))
      if (!is.null(color_col)) {
        p <- p + geom_boxplot(aes(fill = .data[[color_col]]), na.rm = TRUE)
        if (!is.numeric(df[[color_col]])) {
          n_lvl <- length(unique(df[[color_col]]))
          p <- p + scale_fill_manual(values = recycle_palette(n_lvl))
        } else {
          p <- p + scale_fill_viridis_c(option = "turbo")
        }
      } else {
        p <- p + geom_boxplot(aes(fill = as.factor(.data[[input$x_var]])), na.rm = TRUE)
        n_lvl <- length(unique(df[[input$x_var]]))
        p <- p + scale_fill_manual(values = recycle_palette(n_lvl), guide = "none")
      }
      p <- p + coord_flip()
    } else {
      if (is.null(input$y_var) || input$y_var == "None" || !input$y_var %in% names(df)) {
        plot_df <- df |>
          filter(!is.na(.data[[input$x_var]])) |>
          count(.data[[input$x_var]], name = "n", sort = TRUE) |>
          slice_head(n = input$top_n)

        if (nrow(plot_df) == 0) {
          return(blank_plot("No non-missing X values are available to plot."))
        }

        n_bars <- nrow(plot_df)
        p <- ggplot(plot_df, aes(x = reorder(as.factor(.data[[input$x_var]]), n), y = n, fill = as.factor(.data[[input$x_var]]))) +
          geom_col() +
          coord_flip() +
          scale_fill_manual(values = recycle_palette(n_bars), guide = "none") +
          labs(y = "Count")
      } else {
        if (!is.numeric(df[[input$y_var]])) {
          return(blank_plot("Bar plots with a Y variable require numeric Y values."))
        }

        if (!is.null(color_col)) {
          p <- ggplot(df, aes(x = reorder(as.factor(.data[[input$x_var]]), .data[[input$y_var]]), y = .data[[input$y_var]])) +
            geom_col(aes(fill = .data[[color_col]]), na.rm = TRUE)
          if (!is.numeric(df[[color_col]])) {
            n_lvl <- length(unique(df[[color_col]]))
            p <- p + scale_fill_manual(values = recycle_palette(n_lvl))
          } else {
            p <- p + scale_fill_viridis_c(option = "turbo")
          }
        } else {
          n_bars <- length(unique(df[[input$x_var]]))
          p <- ggplot(df, aes(x = reorder(as.factor(.data[[input$x_var]]), .data[[input$y_var]]), y = .data[[input$y_var]], fill = as.factor(.data[[input$x_var]]))) +
            geom_col(na.rm = TRUE) +
            scale_fill_manual(values = recycle_palette(n_bars), guide = "none")
        }
        p <- p + coord_flip()
      }
    }

    p +
      labs(
        x = input$x_var,
        y = ifelse(is.null(input$y_var) || input$y_var == "None", "Count", input$y_var),
        color = color_col,
        fill = color_col
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$main_plot <- renderPlot({ make_plot() })

  observeEvent(input$interpret_plot, {
    # Capture all reactive values up-front, before entering later::later callbacks.
    plot_obj <- make_plot()
    user_plot_question <- str_squish(input$interpret_question %||% "")
    if (identical(user_plot_question, "")) {
      user_plot_question <- "What is the main pattern, any notable outliers, and the football meaning of this plot?"
    }

    plot_interpretation("Interpreting plot with Haiku...")

    later::later(function() {
      tmp <- tempfile(fileext = ".png")
      ggsave(tmp, plot = plot_obj, width = 9, height = 5.5, dpi = 130)

      later::later(function() {
        chat <- ellmer::chat_anthropic(model = HAIKU_MODEL, echo = "none")
        prompt <- paste(
          "Interpret this NFL plot in a single brief paragraph of 2-4 sentences (under 60 words total).",
          "Do not use any headers, titles, bullet points, or markdown formatting. Plain prose only.",
          "Answer this specific user question:", user_plot_question,
          "Focus only on the visible plot and the current NFL query result.",
          "Avoid generic caveats."
        )
        ans <- chat$chat(prompt, ellmer::content_image_file(tmp))
        plot_interpretation(ans)
      }, 0.05)
    }, 0.05)
  })

  output$plot_interpretation <- renderUI({
    HTML(commonmark::markdown_html(plot_interpretation()))
  })
}

shinyApp(ui, server)
