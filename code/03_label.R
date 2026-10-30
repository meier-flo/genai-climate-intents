# 03_label.R
# LLM labelling with Claude Haiku 5.5 via the Anthropic Batch API (ellmer).
#   8 relevance: substantive / passing / none, plus the number of the first user
#     message substantively about climate change (prompts/relevance_v1.txt)
#   9 intent: one code A1-A7 for that message (prompts/intent_v4.txt)
# The model sees the user messages of the opening window, numbered [1], [2], ...
# (no assistant replies). Messages over 500 words are shown as excerpts.
# Agreement with the author: relevance on 100 random candidates, intent on a
# blind sample of 50 substantive conversations.
# Input:  work/window_messages.parquet, work/candidates.parquet, data/author_*.csv
# Output: work/relevance.parquet, output/intent_labels.csv, output/agreement.csv
# Cost: about 4,000 requests, a few US dollars with the Batch API. Batches can
# take up to a few hours. If the script stops, run it again: ellmer picks up the
# batch from the file in work/ instead of sending the requests again.

source("code/00_setup.R")
library(ellmer)

window_messages <- read_parquet(file.path(work_dir, "window_messages.parquet"))
candidates <- read_parquet(file.path(work_dir, "candidates.parquet"))

# ---- Prompts -------------------------------------------------------------------

# A prompt file has a "### SYSTEM" part and a "### USER" part with placeholders.
read_prompt <- function(file) {
  lines <- read_lines(file)
  user_start <- which(lines == "### USER")
  list(system = str_trim(paste(lines[2:(user_start - 1)], collapse = "\n")),
       user = str_trim(paste(lines[(user_start + 1):length(lines)], collapse = "\n")))
}

# Long messages: keep the first 300 words, 150 words before and after the first
# lexicon term and the last 100 words; omitted parts are marked [...].
climate_regex <- lexicon_regex(read_lexicon()$variant)
make_excerpt <- function(text) {
  words <- str_split(str_trim(text), "\\s+")[[1]]
  n <- length(words)
  if (n <= max_words_msg) return(text)
  keep <- rep(FALSE, n)
  keep[1:300] <- TRUE
  keep[(n - 99):n] <- TRUE
  term_start <- str_locate(str_trim(text), climate_regex)[1, "start"]
  if (!is.na(term_start)) {
    term_word <- count_words(str_sub(str_trim(text), 1, term_start - 1)) + 1
    keep[max(1, term_word - 150):min(n, term_word + 150)] <- TRUE
  }
  words[!keep] <- "[...]"
  body <- str_replace_all(paste(words, collapse = " "), "\\[\\.\\.\\.\\]( \\[\\.\\.\\.\\])+", "[...]")
  paste0("(message of ", format(n, big.mark = ","), " words; excerpts shown, omissions marked [...])\n", body)
}

# numbered messages of each candidate: "[1] ...", "[2] ..."
numbered <- window_messages |>
  filter(conv_id %in% candidates$conv_id) |>
  mutate(text = map_chr(text, make_excerpt)) |>
  group_by(conv_id) |>
  summarise(n_msgs = n(),
            messages = paste0("[", msg_idx, "] ", text, collapse = "\n\n"))

# ---- Step 8: relevance -----------------------------------------------------------

relevance_prompt <- read_prompt("prompts/relevance_v1.txt")
relevance_input <- numbered |>
  mutate(user = str_replace(relevance_prompt$user, fixed("{messages}"), messages))

relevance_type <- type_object(
  climate = type_enum(c("substantive", "passing", "none"), "Is the conversation about climate change?"),
  first_climate_message = type_integer(
    "Number of the first user message substantively about climate change; null unless substantive", required = FALSE),
  rationale = type_string("One short sentence (max. 25 words) justifying the decision")
)
chat <- chat_anthropic(system_prompt = relevance_prompt$system, model = llm_model,
                       params = params(max_tokens = 2000), echo = "none")
relevance_answers <- batch_chat_structured(chat, as.list(relevance_input$user),
                                           type = relevance_type,
                                           path = file.path(work_dir, "relevance_batch.json"))

relevance <- relevance_input |>
  select(conv_id, n_msgs) |>
  bind_cols(as_tibble(relevance_answers)) |>
  mutate(
    first_climate_message = if_else(climate == "substantive", first_climate_message, NA),
    valid = climate %in% c("passing", "none") |
      (climate %in% "substantive" & first_climate_message >= 1 & first_climate_message <= n_msgs)
  )
relevance <- candidates |> left_join(relevance, by = "conv_id")
write_parquet(relevance, file.path(work_dir, "relevance.parquet"))
print(count(relevance, climate, valid))

# ---- Step 9: intent ------------------------------------------------------------------

intent_prompt <- read_prompt("prompts/intent_v4.txt")
intent_input <- relevance |>
  filter(climate == "substantive", valid) |>
  select(conv_id, first_climate_message) |>
  left_join(numbered, by = "conv_id") |>
  mutate(user = str_replace(intent_prompt$user, fixed("{target}"), as.character(first_climate_message)),
         user = str_replace(user, fixed("{messages}"), messages))

intent_type <- type_object(
  code = type_enum(paste0("A", 1:7), "Intent code"),
  rationale = type_string("One short sentence (max. 25 words) justifying the code")
)
chat <- chat_anthropic(system_prompt = intent_prompt$system, model = llm_model,
                       params = params(max_tokens = 2000), echo = "none")
intent_answers <- batch_chat_structured(chat, as.list(intent_input$user),
                                        type = intent_type,
                                        path = file.path(work_dir, "intent_batch.json"))

intent <- intent_input |>
  select(conv_id, first_climate_message) |>
  bind_cols(as_tibble(intent_answers)) |>
  mutate(intent = intent_names[code])
print(count(intent, code, intent))
write_csv(intent |> select(conv_id, code, intent), file.path(out_dir, "intent_labels.csv"))
write_parquet(intent, file.path(work_dir, "intent.parquet"))

# ---- Agreement with the author -------------------------------------------------------

author_relevance <- read_csv("data/author_codes_relevance_pilot.csv")   # conv_id, substantive (yes/no)
compare_relevance <- author_relevance |> inner_join(relevance, by = "conv_id")

author_intent <- read_csv("data/author_codes_intent_validation.csv")    # conv_id, author_code
compare_intent <- author_intent |> inner_join(intent, by = "conv_id")

agreement <- tibble(
  task = c("relevance (substantive or not)", "intent (A1-A7)"),
  n = c(nrow(compare_relevance), nrow(compare_intent)),
  kappa = c(cohen_kappa(compare_relevance$substantive == "yes", compare_relevance$climate == "substantive"),
            cohen_kappa(compare_intent$author_code, compare_intent$code)),
  percent_agreement = c(mean((compare_relevance$substantive == "yes") == (compare_relevance$climate == "substantive")),
                        mean(compare_intent$author_code == compare_intent$code))
)
print(agreement)
write_csv(agreement, file.path(out_dir, "agreement.csv"))
