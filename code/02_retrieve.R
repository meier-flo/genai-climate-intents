# 02_retrieve.R
# Retrieval of climate candidates (Table "Processing steps", steps 6-7).
#   6 keyword match: any of the 35 lexicon concepts in the window messages (untruncated)
#   7 embedding: windows without a keyword match whose maximum cosine similarity to
#     the 35 concept names is >= 0.50 were screened by the author; confirmed ones are added
# Input:  work/window_messages.parquet (01_filter.R), data/author_screening_embedding.csv
# Output: work/candidates.parquet (conv_id, source, max_cos)
# Runtime: keyword matching a few minutes; embeddings about 1 hour on an Apple
# Silicon laptop (mps), longer on CPU.

source("code/00_setup.R")
library(reticulate)

window_messages <- read_parquet(file.path(work_dir, "window_messages.parquet"))

windows <- window_messages |>
  group_by(conv_id) |>
  summarise(
    kw_text = paste(text, collapse = "\n\n"),                       # for keyword matching
    window_text = first_words(paste(text, collapse = " "), window_words)  # for embedding
  )

# ---- Step 6: keyword matching ------------------------------------------------

lexicon <- read_lexicon()
concepts <- lexicon |>
  group_by(facet, concept) |>
  summarise(regex = lexicon_regex(variant), .groups = "drop")

# first a quick check with all variants at once, then concept by concept
windows <- windows |>
  mutate(kw_hit = str_detect(kw_text, lexicon_regex(lexicon$variant)))
hits <- windows |> filter(kw_hit)
for (i in seq_len(nrow(concepts))) {
  hits[[concepts$concept[i]]] <- str_detect(hits$kw_text, concepts$regex[i])
}
concept_counts <- hits |>
  select(all_of(concepts$concept)) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(everything(), names_to = "concept", values_to = "windows")
print(concept_counts, n = 35)
message("Keyword hits: ", nrow(hits))

# ---- Step 7: embeddings -------------------------------------------------------

use_virtualenv(python_env, required = TRUE)
sentence_transformers <- import("sentence_transformers")
torch <- import("torch")
device <- if (torch$backends$mps$is_available()) "mps" else "cpu"
model <- sentence_transformers$SentenceTransformer(embedding_model, device = device)
model$max_seq_length <- 256L

embed <- function(texts) {
  model$encode(as.list(texts), batch_size = 128L, normalize_embeddings = TRUE)
}

# one query vector per concept (its name)
query_vectors <- embed(concepts$concept)

# embed the windows in chunks of 20,000 and keep the best-matching concept
chunks <- split(seq_len(nrow(windows)), ceiling(seq_len(nrow(windows)) / 20000))
similarity <- list()
for (rows in chunks) {
  message("Embedding windows ", min(rows), " to ", max(rows), " of ", nrow(windows))
  vectors <- embed(windows$window_text[rows])
  cosine <- vectors %*% t(query_vectors)   # vectors are normalised, so this is the cosine
  similarity[[length(similarity) + 1]] <- tibble(
    conv_id = windows$conv_id[rows],
    max_cos = apply(cosine, 1, max),
    top_concept = concepts$concept[apply(cosine, 1, which.max)]
  )
}
similarity <- bind_rows(similarity)

# ---- Candidate set ------------------------------------------------------------

screening <- read_csv("data/author_screening_embedding.csv")   # conv_id, climate (yes/no)

embedding_only <- windows |>
  select(conv_id, kw_hit) |>
  left_join(similarity, by = "conv_id") |>
  filter(!kw_hit, max_cos >= cosine_cutoff)
message("Embedding-only windows >= cutoff: ", nrow(embedding_only),
        " (all screened by the author: ", all(embedding_only$conv_id %in% screening$conv_id), ")")

confirmed <- screening |> filter(climate == "yes")

candidates <- windows |>
  select(conv_id, kw_hit) |>
  left_join(similarity, by = "conv_id") |>
  filter(kw_hit | conv_id %in% confirmed$conv_id) |>
  mutate(source = if_else(kw_hit, "keyword", "embedding (author-confirmed)")) |>
  select(conv_id, source, max_cos, top_concept)

print(count(candidates, source))
write_parquet(candidates, file.path(work_dir, "candidates.parquet"))
