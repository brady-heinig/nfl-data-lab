# ==========================================================
# nfl_app_eval_results.R
# ----------------------------------------------------------
# Run AFTER nfl_app_eval.R has finished its three Task$eval()
# calls. This script reads the task objects already in your
# global environment and prints accuracy summaries, surfaces
# every individual failure, prints per-task cost, and opens
# the Inspect log viewer.
#
# Usage:
#   source("nfl_app_eval_results.R")
# ==========================================================

library(dplyr)
library(stringr)

# ----- Sanity check ---------------------------------------
required <- c("routing_task", "sql_task", "interp_task")
missing  <- required[!vapply(required, exists, logical(1))]
if (length(missing) > 0) {
  stop(
    "Cannot find these task objects in the global environment: ",
    paste(missing, collapse = ", "),
    "\nRun source('nfl_app_eval.R') first."
  )
}

# ----- Pull and bind samples ------------------------------
get_samples <- function(task, name) {
  s <- task$get_samples()
  s$task  <- name
  s$score <- as.character(s$score)   # ordered-factor levels differ across tasks
  s
}

routing_samples <- get_samples(routing_task, "routing")
sql_samples     <- get_samples(sql_task,     "sql")
interp_samples  <- get_samples(interp_task,  "interpretation")

common <- Reduce(union, list(
  names(routing_samples), names(sql_samples), names(interp_samples)
))
pad <- function(df) {
  for (c in setdiff(common, names(df))) df[[c]] <- NA
  df[, common]
}
all_results <- bind_rows(pad(routing_samples), pad(sql_samples), pad(interp_samples))

# Defensive: pick whichever column actually holds the model output text.
result_col <- intersect(c("result", "output", "response"), names(all_results))[1]
if (is.na(result_col)) {
  all_results$output_short <- "(no result column found)"
} else {
  all_results$output_short <- str_trunc(as.character(all_results[[result_col]]), 300)
}

# ----- Helpers --------------------------------------------
section <- function(title) {
  bar <- strrep("=", 60)
  cat("\n", bar, "\n", title, "\n", bar, "\n", sep = "")
}

# ----- 1. Overall accuracy --------------------------------
section("OVERALL ACCURACY BY TASK")
overall <- all_results |>
  mutate(correct = score == "C") |>
  group_by(task) |>
  summarise(
    n          = n(),
    accuracy   = round(mean(correct), 3),
    correct    = sum(score == "C"),
    partial    = sum(score == "P"),
    incorrect  = sum(score == "I"),
    .groups = "drop"
  )
print(overall)

# ----- 2. Accuracy by difficulty --------------------------
if ("difficulty" %in% names(all_results)) {
  section("ACCURACY BY DIFFICULTY (within each task)")
  by_diff <- all_results |>
    filter(!is.na(difficulty)) |>
    mutate(correct = score == "C") |>
    group_by(task, difficulty) |>
    summarise(n = n(), accuracy = round(mean(correct), 3), .groups = "drop") |>
    arrange(task, difficulty)
  print(by_diff)
}

# ----- 3. Accuracy by topic, weakest first ----------------
if ("topic" %in% names(all_results)) {
  section("ACCURACY BY TOPIC (weakest first)")
  by_topic <- all_results |>
    filter(!is.na(topic)) |>
    mutate(correct = score == "C") |>
    group_by(task, topic) |>
    summarise(n = n(), accuracy = round(mean(correct), 3), .groups = "drop") |>
    arrange(task, accuracy)
  print(by_topic, n = 50)
}

# ----- 4. Every individual failure ------------------------
section("INDIVIDUAL FAILURES (score != C)")
failures <- all_results |> filter(score != "C")
if (nrow(failures) == 0) {
  cat("No failures recorded.\n")
} else {
  for (i in seq_len(nrow(failures))) {
    f <- failures[i, ]
    cat(sprintf(
      "\n[%s | difficulty=%s | topic=%s | score=%s]\n  INPUT:    %s\n  EXPECTED: %s\n  GOT:      %s\n",
      f$task,
      ifelse(is.na(f$difficulty), "-", f$difficulty),
      ifelse(is.na(f$topic),      "-", f$topic),
      f$score,
      f$input,
      f$target,
      f$output_short
    ))
  }
}

# ----- 5. Per-task cost -----------------------------------
section("API COST BY TASK")
cost_safe <- function(task, name) {
  out <- tryCatch(task$get_cost(), error = function(e) NULL)
  if (is.null(out)) {
    cat(sprintf("%-15s: cost not available\n", name))
  } else {
    cat(sprintf("%-15s: ", name))
    print(out)
  }
}
cost_safe(routing_task, "routing")
cost_safe(sql_task,     "sql")
cost_safe(interp_task,  "interpretation")

# ----- 6. Headline numbers for the writeup ----------------
section("HEADLINE NUMBERS (paste into the writeup)")
fmt <- function(c, t) sprintf("%d / %d (%.0f%%)", c, t, 100 * c / t)
for (i in seq_len(nrow(overall))) {
  r <- overall[i, ]
  cat(sprintf("- %s: %s\n", r$task, fmt(r$correct, r$n)))
}

# ----- 7. Open the interactive log viewer -----------------
section("INTERACTIVE LOG VIEWER")
log_dir <- Sys.getenv("VITALS_LOG_DIR", unset = file.path(getwd(), "vitals_logs"))
cat("Logs are at:", log_dir, "\n")
cat("Opening the Inspect log viewer...\n")
cat("(If it doesn't open automatically, run:  vitals::vitals_view(\"", log_dir, "\")  )\n", sep = "")
try(vitals::vitals_view(log_dir), silent = TRUE)

invisible(all_results)
