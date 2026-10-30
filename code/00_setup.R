# 00_setup.R
# Packages, settings and helper functions used by all other scripts.
# Every script starts with source("code/00_setup.R").
# Run the scripts in this order, with the release folder as working directory:
#   01_filter.R    filter WildChat-4.8M and build the opening user windows
#   02_retrieve.R  keyword matching, embeddings, candidate set
#   03_label.R     LLM relevance and intent labelling, agreement with the author
#   04_results.R   figure and tables of the paper

library(tidyverse)
library(arrow)
library(DBI)
library(duckdb)

# ---- Settings --------------------------------------------------------------

raw_dir <- "wildchat-4.8m/data"   # folder with the 86 WildChat-4.8M parquet files
work_dir <- "work"                # intermediate files; they contain user text, do not share them
out_dir <- "output"               # tables and figure

min_words <- 3            # a "substantive" message has more than 3 words
short_max_chars <- 24     # short rule: <= 3 words AND <= 24 characters (scripts without spaces)
keep_language <- "English"
template_words <- 30      # template fingerprint = first 30 words of the first substantive message
template_min <- 20        # fingerprint shared by >= 20 conversations = template
window_words <- 190       # opening user window used for retrieval
cosine_cutoff <- 0.50     # embedding-only windows above this value were screened by the author
max_words_msg <- 500      # longer messages are shown to the LLM as excerpts

embedding_model <- "sentence-transformers/all-MiniLM-L6-v2"
python_env <- "py/.venv"  # Python environment with sentence-transformers (see README)
llm_model <- "claude-haiku-5-5"  # needs ANTHROPIC_API_KEY in ~/.Renviron

intent_names <- c(
  A1 = "Factual lookup / explanation",
  A2 = "Content-production delegation",
  A3 = "Personal action advice",
  A4 = "Opinion elicitation / deliberation",
  A5 = "Validation / reassurance seeking",
  A6 = "Skepticism testing / adversarial",
  A7 = "About AI & climate"
)

dir.create(work_dir, showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE)
raw_files <- list.files(raw_dir, pattern = "parquet$", full.names = TRUE)

# ---- Reading the raw data --------------------------------------------------

# WildChat stores each conversation as a nested list of messages. This small
# SQL query (duckdb) turns one parquet file into a flat table with one row per
# user message. A conversation is identified by the turn identifier of its
# first message (conv_id).
read_user_messages <- function(file) {
  con <- dbConnect(duckdb())
  query <- paste0("
    WITH conv AS (
      SELECT conversation[1].turn_identifier AS conv_id,
             list_filter(conversation, lambda m: m.role = 'user') AS msgs
      FROM read_parquet('", file, "')
    )
    SELECT conv_id,
           unnest(generate_series(1, len(msgs))) AS msg_idx,
           unnest(list_transform(msgs, lambda m: replace(coalesce(m.content, ''), chr(0), ''))) AS text,
           unnest(list_transform(msgs, lambda m: m.language)) AS language
    FROM conv")
  messages <- dbGetQuery(con, query)
  dbDisconnect(con, shutdown = TRUE)
  as_tibble(messages) |> arrange(conv_id, msg_idx)
}

# One row per conversation with the country and state of the user
read_conversations <- function(file) {
  con <- dbConnect(duckdb())
  query <- paste0("
    SELECT conversation[1].turn_identifier AS conv_id, country, state
    FROM read_parquet('", file, "')")
  conversations <- dbGetQuery(con, query)
  dbDisconnect(con, shutdown = TRUE)
  as_tibble(conversations)
}

# ---- Small text helpers ----------------------------------------------------

count_words <- function(text) {
  str_count(text, "\\S+")
}

# First n words of a text
first_words <- function(text, n) {
  words <- str_split(str_squish(text), " ")
  map_chr(words, function(w) paste(head(w, n), collapse = " "))
}

# ---- Lexicon ---------------------------------------------------------------

# prompts/lexicon_en_v1.txt: "# ---- Facet ----" starts a facet, one concept
# per line, spelling variants separated by " | ". The first variant is the
# concept name. Returns one row per variant.
read_lexicon <- function() {
  tibble(line = read_lines("prompts/lexicon_en_v1.txt")) |>
    mutate(facet = str_match(line, "^# ---- (.*) ----")[, 2]) |>
    fill(facet) |>
    filter(!str_detect(line, "^#"), line != "") |>
    mutate(variant = str_split(line, " \\| "),
           concept = map_chr(variant, first)) |>
    unnest(variant) |>
    select(facet, concept, variant)
}

# Regular expression for a set of variants: case-insensitive, whole words,
# hyphen and space interchangeable, optional plural -s/-es.
lexicon_regex <- function(variants) {
  variants <- str_replace_all(variants, "[ -]+", "[\\\\s-]+")
  paste0("(?i)\\b(", paste(variants, collapse = "|"), ")(s|es)?\\b")
}

# ---- Agreement -------------------------------------------------------------

cohen_kappa <- function(a, b) {
  categories <- union(a, b)
  tab <- table(factor(a, categories), factor(b, categories))
  observed <- sum(diag(tab)) / sum(tab)
  expected <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  (observed - expected) / (1 - expected)
}
