# Climate-related conversations in WildChat-4.8M: data and code

Supplementary material for the CHIIR short paper *Delegation over Debate: How People Use Generative AI Chatbots for Climate Change Information*.

The release contains conversation IDs, labels, prompts and R code. It contains **no conversation text**. To reproduce the analysis, download [WildChat-4.8M](https://huggingface.co/datasets/allenai/WildChat-4.8M) yourself and accept its terms of use.

## Contents

| Path | Content |
|---|---|
| `data/intent_labels.csv` | The climate corpus: 1,067 conversation IDs with their intent code (A1–A7) and intent name |
| `prompts/lexicon_en.txt` | Climate lexicon: 35 concepts in six facets, with spelling variants |
| `prompts/intents.txt` | LLM prompt for intent coding, with the codebook (A1–A7) and decision rules R1–R11 |
| `code/` | R scripts, see below |

`conv_id` is the `turn_identifier` of the first message of a WildChat-4.8M conversation (`conversation[1].turn_identifier`).

## Intent codes

| Code | Intent |
|---|---|
| A1 | Factual lookup / explanation |
| A2 | Content-production delegation |
| A3 | Personal action advice |
| A4 | Opinion elicitation / deliberation |
| A5 | Validation / reassurance seeking |
| A6 | Skepticism testing / adversarial |
| A7 | About AI & climate |

The definitions and decision rules are in `prompts/intents.txt`.

## Code

Run the scripts in order, with this folder as the working directory. Settings (paths, thresholds, model names) are at the top of `code/00_setup.R`.

| Script | What it does | Runtime |
|---|---|---|
| `00_setup.R` | Packages, settings, helper functions (sourced by all others) | – |
| `01_filter.R` | Filter steps 1–5 (empty, short, not English, templated, duplicates); opening user windows (190 words) | 30–60 min |
| `02_retrieve.R` | Step 6 keyword matching; step 7 embeddings (all-MiniLM-L6-v2) and author-confirmed candidates | ~1 h |
| `03_label.R` | Steps 8–9: relevance and intent labelling with Claude Haiku 5.5 (Batch API); agreement with the author | a few hours (batch), a few US dollars |
| `04_results.R` | Processing-steps table, intent figure, conversation behaviour, lexicon facets | ~15 min |

Requirements:

- R ≥ 4.3 with `tidyverse`, `arrow`, `duckdb`, `DBI`, `openssl`, `reticulate` and `ellmer`.
- A Python environment with `sentence-transformers` for the embeddings (path in `python_env`). For example: `uv venv py/.venv && uv pip install --python py/.venv sentence-transformers`.
- An Anthropic API key (`ANTHROPIC_API_KEY` in `~/.Renviron`) for `03_label.R`.

The folder `work/` holds intermediate files with conversation text. Do not share it.

Notes on reproducing the paper:

- The LLM labels may differ slightly between runs. The labels used in the paper are in `data/intent_labels.csv`.
- Not included: the dataset inventory, the pilot used to choose the model and refine the codebook, and the qualitative reading of the rare intents.
