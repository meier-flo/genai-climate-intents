# 01_filter.R
# Filter cascade (Table "Processing steps", steps 1-5) and opening user windows.
#   1 empty user text
#   2 short: all user messages together <= 3 words (and <= 24 characters)
#   3 not English: WildChat's language label of the first substantive user message
#   4 templated: first 30 words of the first substantive message shared by >= 20 conversations
#   5 exact duplicates of the full user text (the copy with the smallest conv_id is kept)
# Output:
#   work/features.parquet         one row per conversation (no text) with the step it was removed at
#   work/window_messages.parquet  user messages of the opening window of kept conversations (text!)
#   output/filter_cascade.csv
# Runtime: two passes over the 86 raw files, roughly 30-60 minutes.

source("code/00_setup.R")

# ---- Pass 1: one row of features per conversation --------------------------

features <- list()
for (file in raw_files) {
  message("Pass 1: ", basename(file))
  conversations <- read_conversations(file)
  messages <- read_user_messages(file) |>
    mutate(n_words = count_words(text),
           n_chars = nchar(text))

  per_conversation <- messages |>
    group_by(conv_id) |>
    summarise(
      words_total = sum(n_words),
      chars_total = sum(n_chars),
      # index of the first substantive message (more than 3 words); NA if there is none
      first_sub = first(msg_idx[n_words > min_words]),
      # language of that message (or of the first message if there is none)
      language = coalesce(language[first_sub], first(language)),
      sub_text = text[first_sub],
      sub_words = n_words[first_sub],
      full_text = str_squish(paste(text, collapse = " "))
    ) |>
    mutate(
      # template fingerprint: first 30 words of the first substantive message (lower case)
      fingerprint = if_else(sub_words >= template_words,
                            as.character(openssl::md5(first_words(str_to_lower(sub_text), template_words))),
                            NA_character_),
      full_hash = as.character(openssl::md5(full_text))
    ) |>
    select(-sub_text, -full_text)

  features[[file]] <- conversations |> left_join(per_conversation, by = "conv_id")
}
features <- bind_rows(features)

# ---- Filter cascade ---------------------------------------------------------

templates <- features |>
  filter(!is.na(fingerprint)) |>
  count(fingerprint) |>
  filter(n >= template_min)

features <- features |>
  mutate(words_total = replace_na(words_total, 0),
         chars_total = replace_na(chars_total, 0)) |>
  arrange(conv_id) |>
  mutate(
    is_duplicate = duplicated(full_hash),
    removed_at = case_when(
      words_total == 0 ~ 1,
      words_total <= min_words & chars_total <= short_max_chars ~ 2,
      is.na(language) | language != keep_language ~ 3,
      fingerprint %in% templates$fingerprint ~ 4,
      is_duplicate ~ 5,
      TRUE ~ 0
    )
  )
write_parquet(features, file.path(work_dir, "features.parquet"))

cascade <- features |>
  count(removed_at) |>
  arrange(removed_at) |>
  mutate(step = c("kept", "1 empty", "2 short", "3 not English", "4 templated", "5 duplicate")[removed_at + 1])
cascade_table <- tibble(
  step = c("0 all conversations", cascade$step[-1]),
  removed = c(NA, cascade$n[-1]),
  remaining = nrow(features) - c(0, cumsum(cascade$n[-1]))
)
print(cascade_table)
write_csv(cascade_table, file.path(out_dir, "filter_cascade.csv"))
message("Templates removing conversations: ",
        n_distinct(features$fingerprint[features$removed_at == 4]))

# ---- Pass 2: opening user window of the kept conversations ------------------

# The window holds the user messages in order until 190 words are reached.
# We keep every user message that starts within the window.
kept_ids <- features$conv_id[features$removed_at == 0]

window_messages <- list()
for (file in raw_files) {
  message("Pass 2: ", basename(file))
  window_messages[[file]] <- read_user_messages(file) |>
    filter(conv_id %in% kept_ids) |>
    mutate(n_words = count_words(text)) |>
    group_by(conv_id) |>
    filter(cumsum(n_words) - n_words < window_words) |>
    ungroup() |>
    select(conv_id, msg_idx, n_words, text)
}
window_messages <- bind_rows(window_messages)
write_parquet(window_messages, file.path(work_dir, "window_messages.parquet"))
message("Kept conversations: ", length(kept_ids))
