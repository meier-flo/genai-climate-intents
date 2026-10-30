# 04_results.R
# Figure and tables of the paper.
#   output/processing_steps.csv     Table: processing steps from the release to the climate corpus
#   output/intent_shares.csv        Figure: share of intents
#   output/fig_intents.pdf
#   output/conversation_behaviour.csv  Table: conversation behaviour by intent group
#   output/lexicon_facets.csv       lexicon facets of the coded message by intent group
# Intent groups: A1, A2 and A4-A6 pooled; A3 and A7 are too rare and left out.
# Input: work/ files from 01-03 and one more pass over the raw data (about 15 minutes).

source("code/00_setup.R")

features <- read_parquet(file.path(work_dir, "features.parquet"))
candidates <- read_parquet(file.path(work_dir, "candidates.parquet"))
relevance <- read_parquet(file.path(work_dir, "relevance.parquet"))
intent <- read_parquet(file.path(work_dir, "intent.parquet"))
screening <- read_csv("data/author_screening_embedding.csv")

# ---- Processing steps -----------------------------------------------------------

n_windows <- sum(features$removed_at == 0)
n_keyword <- sum(candidates$source == "keyword")
n_embedding <- sum(candidates$source != "keyword")
n_none <- sum(relevance$climate %in% "none" & relevance$valid %in% TRUE)
n_passing <- sum(relevance$climate %in% "passing" & relevance$valid %in% TRUE)
n_no_label <- sum(!relevance$valid %in% TRUE)

steps <- tibble(
  step = c("0 all conversations", "1 empty user text", "2 short (<= 3 words)", "3 not English",
           "4 templated", "5 exact duplicates", "6 keyword match", "7 embedding, author-confirmed",
           "8 not about climate change", "8 climate change in passing only", "8 no valid label",
           "9 intent coded (A1-A7)"),
  change = c(NA, -sum(features$removed_at == 1), -sum(features$removed_at == 2),
             -sum(features$removed_at == 3), -sum(features$removed_at == 4), -sum(features$removed_at == 5),
             n_keyword - n_windows, n_embedding, -n_none, -n_passing, -n_no_label, NA)
) |>
  mutate(remaining = nrow(features) + cumsum(replace_na(change, 0)))
print(steps)
message("Embedding-only windows screened by the author: ", nrow(screening))
write_csv(steps, file.path(out_dir, "processing_steps.csv"))

# ---- Figure: share of intents -------------------------------------------------------

shares <- intent |>
  count(code) |>
  mutate(share = n / sum(n),
         name = paste(code, intent_names[code]),
         label = paste0(round(100 * share, 1), "% (", n, ")"))
write_csv(shares, file.path(out_dir, "intent_shares.csv"))

figure <- ggplot(shares, aes(x = share, y = fct_rev(name))) +
  geom_col(fill = "grey45", width = 0.7) +
  geom_text(aes(label = label), hjust = -0.1, size = 2.5) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = paste0("Share of climate conversations (N = ", nrow(intent), ")"), y = NULL) +
  theme_minimal(base_size = 8)
ggsave(file.path(out_dir, "fig_intents.pdf"), figure, width = 3.33, height = 1.9)

# ---- All user messages of the climate corpus (from the raw data) ----------------------

intent <- intent |>
  mutate(group = case_when(code == "A1" ~ "A1",
                           code == "A2" ~ "A2",
                           code %in% c("A4", "A5", "A6") ~ "A4-A6")) |>
  filter(!is.na(group))

corpus_messages <- list()
for (file in raw_files) {
  message("Reading ", basename(file))
  corpus_messages[[file]] <- read_user_messages(file) |>
    filter(conv_id %in% intent$conv_id)
}
corpus_messages <- bind_rows(corpus_messages) |>
  mutate(n_words = count_words(text)) |>
  left_join(intent |> select(conv_id, group, first_climate_message), by = "conv_id")

# ---- Table: conversation behaviour ----------------------------------------------------
# Whole conversations, regardless of where the climate message appears.

per_conversation <- corpus_messages |>
  group_by(conv_id, group) |>
  summarise(user_messages = n(),
            words_per_message = median(n_words),
            total_words = sum(n_words),
            .groups = "drop")

median_iqr <- function(x) {
  paste0(round(median(x)), " (", round(quantile(x, 0.25)), "-", round(quantile(x, 0.75)), ")")
}
behaviour <- per_conversation |>
  group_by(group) |>
  summarise(n = n(),
            user_messages = median_iqr(user_messages),
            single_message = paste0(round(100 * mean(user_messages == 1), 1), "%"),
            words_per_message = median_iqr(words_per_message),
            total_words = median_iqr(total_words))
print(behaviour)
write_csv(behaviour, file.path(out_dir, "conversation_behaviour.csv"))

# ---- Table: lexicon facets of the coded message ----------------------------------------

coded_messages <- corpus_messages |> filter(msg_idx == first_climate_message)

lexicon <- read_lexicon()
concepts <- lexicon |>
  group_by(facet, concept) |>
  summarise(regex = lexicon_regex(variant), .groups = "drop")

# one row per conversation and matched concept
matches <- list()
for (i in seq_len(nrow(concepts))) {
  found <- coded_messages |> filter(str_detect(text, concepts$regex[i]))
  matches[[i]] <- tibble(conv_id = found$conv_id, facet = concepts$facet[i], concept = concepts$concept[i])
}
matches <- bind_rows(matches)

group_sizes <- count(intent, group, name = "n_group")
facets <- matches |>
  distinct(conv_id, facet) |>
  bind_rows(tibble(conv_id = setdiff(intent$conv_id, matches$conv_id), facet = "No lexicon term")) |>
  left_join(intent |> select(conv_id, group), by = "conv_id") |>
  count(facet, group) |>
  left_join(group_sizes, by = "group") |>
  mutate(percent = round(100 * n / n_group, 1)) |>
  select(facet, group, percent) |>
  pivot_wider(names_from = group, values_from = percent, values_fill = 0)

generic <- c("climate change", "global warming", "global heating", "climate crisis", "climate emergency")
generic_only <- matches |>
  group_by(conv_id) |>
  summarise(generic_only = all(concept %in% generic)) |>
  left_join(intent |> select(conv_id, group), by = "conv_id") |>
  group_by(group) |>
  summarise(percent = round(100 * mean(generic_only), 1)) |>
  pivot_wider(names_from = group, values_from = percent) |>
  mutate(facet = "Generic terms only (of conversations with a term)")

lexicon_facets <- bind_rows(facets, generic_only)
print(lexicon_facets)
print(count(matches, concept, sort = TRUE))
write_csv(lexicon_facets, file.path(out_dir, "lexicon_facets.csv"))
