# ============================================================
#  New England Revolution — Fan Sentiment & Churn Risk Pipeline
#
#  Applied consulting project, Hult International Business School
#  Author: Coovadia Nyoka — github.com/cnyoka
#
#  Question A: Why are ticket sales declining?
#  Question B: Is the club positioned for World Cup 2026?
#
#  USAGE
#    Put the four CSVs in DATA_DIR, then:
#      source("revolution_nlp_pipeline.R")
#    Plots and summary_metrics.csv are written to OUT_DIR.
# ============================================================

DATA_DIR <- "data"       # folder containing the four input CSVs
OUT_DIR  <- "plots"      # folder for output PNGs

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ── 0. PACKAGES ───────────────────────────────────────────────────────────
packages <- c(
  "tidyverse", "tidytext", "topicmodels", "wordcloud",
  "RColorBrewer", "igraph", "ggraph", "widyr", "scales",
  "lubridate", "SnowballC", "textdata", "reshape2", "gridExtra"
)
to_install <- packages[!packages %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cran.r-project.org", quiet = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse); library(tidytext); library(topicmodels)
  library(wordcloud);  library(RColorBrewer); library(igraph)
  library(ggraph);     library(widyr);        library(scales)
  library(lubridate);  library(SnowballC);    library(textdata)
  library(reshape2);   library(gridExtra)
})

bing <- get_sentiments("bing")
nrc  <- get_sentiments("nrc")

REV_RED  <- "#C8102E"
REV_NAVY <- "#00245D"

cat("[0/8] Packages loaded\n")


# ── 1. LOAD DATA ──────────────────────────────────────────────────────────
cat("\n[1/8] Loading data...\n")

# Dates arrive in mixed formats across sources; keep the leading YYYY-MM-DD
# and return NA rather than guessing when that fails.
safe_as_date <- function(x) {
  x <- as.character(x)
  x <- gsub("Z$", "", x)
  x <- gsub("T", " ", x)
  x <- substr(trimws(x), 1, 10)
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

load_source <- function(filename, label) {
  path <- file.path(DATA_DIR, filename)
  if (!file.exists(path)) {
    cat(sprintf("  ! %-14s not found at %s - skipped\n", label, path))
    return(NULL)
  }
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$source <- label
  df$date   <- safe_as_date(df$date)
  df$text   <- as.character(df$text)
  df$rating <- suppressWarnings(as.numeric(df$rating))
  df <- df[!is.na(df$text) & nchar(trimws(df$text)) > 10, ]
  cat(sprintf("  %-14s %5d rows (unparsed dates: %d)\n",
              label, nrow(df), sum(is.na(df$date))))
  df
}

youtube      <- load_source("youtube_comments.csv",   "youtube")
news         <- load_source("news_articles.csv",      "news")
ticketmaster <- load_source("ticketmaster_data.csv",  "ticketmaster")
app_reviews  <- load_source("app_reviews.csv",        "app_reviews")

ingested <- sum(sapply(list(youtube, news, ticketmaster, app_reviews),
                       function(d) if (is.null(d)) 0 else nrow(d)))
cat(sprintf("\n  Ingested corpus: %d documents\n", ingested))

# Ticketmaster rows carry scheduled fixture dates, so the upper bound
# extends a year forward rather than stopping at today.
all_data <- bind_rows(youtube, news, ticketmaster, app_reviews) %>%
  mutate(date = as.Date(as.character(date))) %>%
  filter(!is.na(date),
         date >= as.Date("2023-01-01"),
         date <= Sys.Date() + 365) %>%
  mutate(year_month = floor_date(date, "month"),
         year       = year(date),
         source     = factor(source)) %>%
  rowid_to_column("doc_id")

cat(sprintf("  After date filter: %d documents | %s to %s\n",
            nrow(all_data), min(all_data$date), max(all_data$date)))
cat(sprintf("  Sources: %s\n", paste(levels(all_data$source), collapse = ", ")))


# ── 2. TEXT PREPROCESSING ─────────────────────────────────────────────────
cat("\n[2/8] Preprocessing...\n")

custom_stopwords <- c(
  stop_words$word,
  "new", "england", "revolution", "revs", "mls", "soccer",
  "team", "game", "just", "get", "got", "one", "can", "like",
  "really", "also", "great", "good", "bad", "app", "will",
  "said", "time", "year", "season", "match", "play", "played",
  "going", "went", "come", "came", "know", "think", "want",
  "http", "https", "www", "com", "gillette", "stadium", "watch",
  "video", "channel", "subscribe", "click", "link", "na", "de"
)

tokens <- all_data %>%
  mutate(text = str_replace_all(text, "https?://\\S+", "")) %>%
  select(doc_id, source, date, year_month, year, text, rating) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% custom_stopwords,
         !str_detect(word, "^[0-9]+$"),
         !str_detect(word, "^@"),
         nchar(word) > 2,
         str_detect(word, "^[a-z]")) %>%
  mutate(word_stem = wordStem(word, language = "english"))

cat(sprintf("  %d tokens from %d documents\n",
            nrow(tokens), n_distinct(tokens$doc_id)))

cat("\n  Top 20 terms:\n")
print(tokens %>% count(word, sort = TRUE) %>% slice_head(n = 20))


# ── 3. SENTIMENT ──────────────────────────────────────────────────────────
cat("\n[3/8] Sentiment analysis...\n")

bing_sentiment <- tokens %>%
  inner_join(bing, by = "word", relationship = "many-to-many") %>%
  count(doc_id, source, date, year_month, rating, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative,
         sentiment_ratio = (positive - negative) / (positive + negative + 1))

monthly_sentiment <- bing_sentiment %>%
  group_by(year_month) %>%
  summarise(avg_ratio    = mean(sentiment_ratio, na.rm = TRUE),
            avg_score    = mean(sentiment_score, na.rm = TRUE),
            n_docs       = n(),
            pct_negative = mean(sentiment_score < 0) * 100,
            .groups = "drop")

source_sentiment <- bing_sentiment %>%
  group_by(source) %>%
  summarise(avg_score    = mean(sentiment_score, na.rm = TRUE),
            pct_negative = mean(sentiment_score < 0) * 100,
            n            = n(),
            .groups = "drop")

# A word can map to several NRC emotions, so counts are token-instances
# rather than documents.
overall_emotions <- tokens %>%
  inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100)

cat("\n  Sentiment by source:\n"); print(source_sentiment)
cat("\n  Emotion breakdown:\n");   print(overall_emotions)


# ── 4. TOPIC MODELING ─────────────────────────────────────────────────────
# Exploratory. Topics are reported by their own top terms rather than
# pre-assigned labels - see README.
cat("\n[4/8] Topic modeling (LDA, k=6)...\n")

dtm <- tokens %>%
  count(doc_id, word_stem) %>%
  filter(n >= 2) %>%
  cast_dtm(doc_id, word_stem, n)
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
cat(sprintf("  DTM: %d documents x %d terms\n", nrow(dtm), ncol(dtm)))

set.seed(2026)
lda_model <- LDA(dtm, k = 6, control = list(seed = 2026))

lda_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>% slice_max(beta, n = 15) %>% ungroup()

cat("\n  Top terms per topic:\n")
print(lda_terms %>%
        group_by(topic) %>% slice_max(beta, n = 8) %>%
        summarise(terms = paste(term, collapse = ", "), .groups = "drop"))

# with_ties = FALSE prevents duplicate rows where gamma ties exactly
doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(doc_id = as.integer(document))

all_data <- all_data %>%
  left_join(doc_topics %>% select(doc_id, topic, gamma), by = "doc_id")

topic_dist <- all_data %>%
  filter(!is.na(topic)) %>%
  count(topic, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100,
         topic_label = paste0("Topic ", topic))

cat("\n  Topic distribution:\n"); print(topic_dist)


# ── 5. TF-IDF ─────────────────────────────────────────────────────────────
cat("\n[5/8] TF-IDF...\n")

sentiment_docs <- bing_sentiment %>%
  mutate(sentiment_group = ifelse(sentiment_score < 0, "negative", "positive"))

tfidf_sentiment <- tokens %>%
  inner_join(sentiment_docs %>% select(doc_id, sentiment_group), by = "doc_id") %>%
  count(sentiment_group, word) %>%
  bind_tf_idf(word, sentiment_group, n) %>%
  arrange(desc(tf_idf))

tfidf_year <- tokens %>%
  count(year, word) %>%
  bind_tf_idf(word, year, n) %>%
  group_by(year) %>% slice_max(tf_idf, n = 15) %>% ungroup()

cat("\n  Distinctive terms by sentiment group:\n")
print(tfidf_sentiment %>%
        group_by(sentiment_group) %>% slice_max(tf_idf, n = 8) %>%
        select(sentiment_group, word, tf_idf))


# ── 6. WORLD CUP 2026 ─────────────────────────────────────────────────────
cat("\n[6/8] World Cup 2026 salience...\n")

all_data <- all_data %>%
  mutate(is_wc = str_detect(str_to_lower(text),
    "world cup|worldcup|fifa|wc2026|wc 2026|host city|foxborough.*2026|2026.*world"))

wc_docs <- all_data %>% filter(is_wc)

cat(sprintf("  World Cup mentions: %d of %d documents (%.1f%%)\n",
            nrow(wc_docs), nrow(all_data), nrow(wc_docs) / nrow(all_data) * 100))

wc_sentiment <- wc_docs %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% custom_stopwords) %>%
  inner_join(bing, by = "word", relationship = "many-to-many") %>%
  count(doc_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

cat(sprintf("  Mean sentiment - World Cup docs: %.2f | all docs: %.2f\n",
            mean(wc_sentiment$sentiment_score, na.rm = TRUE),
            mean(bing_sentiment$sentiment_score, na.rm = TRUE)))

cat("\n  Context terms in World Cup documents:\n")
print(wc_docs %>%
        unnest_tokens(word, text) %>%
        filter(!word %in% custom_stopwords, nchar(word) > 3,
               !str_detect(word, "^[0-9]+$"), !str_detect(word, "^@")) %>%
        count(word, sort = TRUE) %>% slice_head(n = 15))

wc_monthly <- all_data %>%
  group_by(year_month) %>%
  summarise(total = n(), wc_count = sum(is_wc, na.rm = TRUE),
            wc_pct = wc_count / total * 100, .groups = "drop")


# ── 7. CO-OCCURRENCE NETWORK ──────────────────────────────────────────────
cat("\n[7/8] Co-occurrence network...\n")

MIN_COOC <- 8

word_pairs <- tokens %>%
  filter(source %in% c("youtube", "app_reviews")) %>%
  group_by(doc_id) %>% filter(n() >= 3) %>% ungroup() %>%
  pairwise_count(word, doc_id, sort = TRUE, upper = FALSE) %>%
  filter(n >= 5)

cat(sprintf("  %d word pairs co-occurring in >= 5 documents\n", nrow(word_pairs)))

graph <- word_pairs %>%
  filter(n >= MIN_COOC) %>% slice_max(n, n = 80) %>%
  graph_from_data_frame()


# ── 8. CHURN RISK ─────────────────────────────────────────────────────────
# Rule-based index over complaint-term density. A prioritisation tool,
# not a validated predictive model.
cat("\n[8/8] Churn risk scoring...\n")

churn_words <- c(
  "expens", "overpric", "cancel", "quit", "leav", "done",
  "never", "wast", "refund", "disappoint", "frustrat", "worst",
  "terribl", "useless", "broken", "crash", "unusabl", "bore",
  "empt", "pathetic", "joke", "aw", "pointless", "disgust"
)
loyalty_words <- c(
  "love", "amaz", "excit", "fantast", "support", "pride",
  "passion", "loyal", "renew", "famili", "communiti",
  "atmospher", "fun", "enjoy", "worth", "best", "proud"
)

churn_scores <- tokens %>%
  group_by(doc_id, source, date, year_month) %>%
  summarise(churn_score   = sum(word_stem %in% churn_words),
            loyalty_score = sum(word_stem %in% loyalty_words),
            net_score     = loyalty_score - churn_score,
            risk_flag     = churn_score > loyalty_score & churn_score > 0,
            .groups = "drop") %>%
  left_join(all_data %>% select(doc_id, rating), by = "doc_id")

churn_by_source <- churn_scores %>%
  group_by(source) %>%
  summarise(pct_at_risk   = mean(risk_flag, na.rm = TRUE) * 100,
            avg_net_score = mean(net_score, na.rm = TRUE),
            n             = n(),
            .groups = "drop") %>%
  arrange(desc(pct_at_risk))

churn_monthly <- churn_scores %>%
  group_by(year_month) %>%
  summarise(pct_at_risk = mean(risk_flag, na.rm = TRUE) * 100,
            avg_net_score = mean(net_score, na.rm = TRUE), .groups = "drop")

cat("\n  Churn risk by source:\n"); print(churn_by_source)


# ── VISUALISATIONS ────────────────────────────────────────────────────────
cat("\nGenerating plots...\n")

save_plot <- function(p, file, w, h) {
  ggsave(file.path(OUT_DIR, file), p, width = w, height = h, dpi = 150)
  cat(sprintf("  %s\n", file))
}

src_caption <- paste0("Sources: ", paste(levels(all_data$source), collapse = ", "))

# 1 - Sentiment over time
p1 <- monthly_sentiment %>%
  ggplot(aes(year_month, avg_ratio)) +
  geom_line(color = REV_RED, linewidth = 1.2) +
  geom_point(aes(size = n_docs), color = REV_NAVY, alpha = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, alpha = 0.15,
              color = REV_RED, fill = REV_RED) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_size_continuous(range = c(2, 8), name = "Documents") +
  labs(title = "Fan sentiment trend - New England Revolution",
       subtitle = "Monthly sentiment ratio, 2023-2026 (above 0 = net positive)",
       x = NULL, y = "Sentiment ratio", caption = src_caption) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY),
        axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p1, "01_sentiment_trend.png", 12, 6)

# 2 - Sentiment by source (headline chart)
p2 <- source_sentiment %>%
  ggplot(aes(reorder(source, avg_score), avg_score, fill = avg_score > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f  (n=%d)", avg_score, n)),
            hjust = ifelse(source_sentiment$avg_score > 0, -0.1, 1.1), size = 4) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#28A745", "FALSE" = REV_RED)) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25))) +
  labs(title = "Sentiment splits by channel, not by topic",
       subtitle = "Match content is positive; the app store and ticketing are negative",
       x = NULL, y = "Mean sentiment score") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY))
save_plot(p2, "02_sentiment_by_source.png", 10, 5)

# 3 - Emotion breakdown
p3 <- overall_emotions %>%
  mutate(sentiment = str_to_title(sentiment)) %>%
  ggplot(aes(reorder(sentiment, pct), pct, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "RdBu") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Emotion breakdown (NRC lexicon)",
       subtitle = "Share of emotion-bearing tokens across all sources",
       x = NULL, y = "% of emotional tokens",
       caption = "Words map to multiple emotions; counts are token-instances") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY))
save_plot(p3, "03_emotion_breakdown.png", 10, 6)

# 4 - LDA topics by their own terms
p4 <- lda_terms %>%
  mutate(topic_label = paste0("Topic ", topic)) %>%
  group_by(topic_label) %>% slice_max(beta, n = 10) %>% ungroup() %>%
  mutate(term = reorder_within(term, beta, topic_label)) %>%
  ggplot(aes(term, beta, fill = topic_label)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~topic_label, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(title = "LDA topics, labelled by their own top terms",
       subtitle = "Clusters surface general football and off-topic content - the reason relevance filtering was applied",
       x = NULL, y = "Beta (term-topic probability)", caption = "LDA k=6, VEM") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY),
        strip.text = element_text(face = "bold"))
save_plot(p4, "04_lda_topic_terms.png", 14, 10)

# 5 - TF-IDF by sentiment group
p5 <- tfidf_sentiment %>%
  group_by(sentiment_group) %>% slice_max(tf_idf, n = 15) %>% ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, sentiment_group)) %>%
  ggplot(aes(word, tf_idf,
             fill = ifelse(sentiment_group == "negative", REV_RED, "#28A745"))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment_group, scales = "free") +
  scale_x_reordered() + scale_fill_identity() +
  labs(title = "Terms that distinguish negative from positive documents",
       subtitle = "TF-IDF weighting within each sentiment group",
       x = NULL, y = "TF-IDF") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY),
        strip.text = element_text(face = "bold"))
save_plot(p5, "05_tfidf_sentiment.png", 12, 7)

# 6 - TF-IDF by year
p6 <- tfidf_year %>%
  mutate(word = reorder_within(word, tf_idf, year)) %>%
  ggplot(aes(word, tf_idf, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~year, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(title = "Distinctive terms by year",
       subtitle = "TF-IDF weighting within each year",
       x = NULL, y = "TF-IDF") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY),
        strip.text = element_text(face = "bold"))
save_plot(p6, "06_tfidf_by_year.png", 13, 9)

# 7 - Churn risk over time
p7 <- churn_monthly %>%
  ggplot(aes(year_month, pct_at_risk)) +
  geom_area(fill = REV_RED, alpha = 0.15) +
  geom_line(color = REV_RED, linewidth = 1.2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = REV_NAVY, linetype = "dashed") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Churn risk over time",
       subtitle = "Share of documents where complaint language outweighs loyalty language",
       x = NULL, y = "% flagged at risk",
       caption = "Rule-based keyword index, not a validated predictive model") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY),
        axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p7, "07_churn_risk_trend.png", 12, 6)

# 8 - Churn risk by source
p8 <- churn_by_source %>%
  ggplot(aes(reorder(source, pct_at_risk), pct_at_risk, fill = source)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%  (n=%d)", pct_at_risk, n)),
            hjust = -0.05, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Churn risk concentrates in the app store",
       subtitle = "Share of documents flagged at risk, by source",
       x = NULL, y = "% flagged at risk") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY))
save_plot(p8, "08_churn_risk_by_source.png", 10, 5)

# 9 - World Cup salience
p9 <- wc_monthly %>%
  filter(!is.na(year_month), total >= 3) %>%
  ggplot(aes(year_month, wc_pct)) +
  geom_col(fill = "darkgreen", alpha = 0.75) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "World Cup 2026 barely registers in fan conversation",
       subtitle = sprintf("%.1f%% of all documents mention the tournament (%d of %d)",
                          nrow(wc_docs) / nrow(all_data) * 100,
                          nrow(wc_docs), nrow(all_data)),
       x = NULL, y = "% of monthly documents mentioning World Cup") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", color = REV_NAVY),
        axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p9, "09_worldcup_salience.png", 12, 6)

# 10 - Co-occurrence network
tryCatch({
  p10 <- ggraph(graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n),
                   color = REV_NAVY, show.legend = FALSE) +
    geom_node_point(color = REV_RED, size = 3) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.2, color = "gray20") +
    scale_edge_width(range = c(0.3, 2.5)) +
    scale_edge_alpha(range = c(0.2, 0.9)) +
    labs(title = "Term co-occurrence network",
         subtitle = "Terms appearing together across YouTube and app-store documents",
         caption = sprintf("Minimum co-occurrence: %d documents", MIN_COOC)) +
    theme_graph(base_size = 12) +
    theme(plot.title = element_text(face = "bold", color = REV_NAVY))
  save_plot(p10, "10_cooccurrence_network.png", 12, 10)
}, error = function(e) cat(sprintf("  ! network plot failed: %s\n", e$message)))


# ── SUMMARY ───────────────────────────────────────────────────────────────
app_avg_rating <- mean(as.numeric(app_reviews$rating), na.rm = TRUE)

pull_pct <- function(src) {
  v <- source_sentiment$pct_negative[source_sentiment$source == src]
  if (length(v) == 0) NA_real_ else v
}

summary_table <- tibble(
  Metric = c(
    "Documents ingested",
    "Documents after date filter",
    "Date range",
    "Documents with negative sentiment (%)",
    "App store reviews negative (%)",
    "Ticketing content negative (%)",
    "App store mean rating (of 5)",
    "Highest churn-risk source",
    "World Cup 2026 mention rate (%)",
    "Leading emotion",
    "Most distinctive negative term",
    "Most distinctive positive term"
  ),
  Value = c(
    as.character(ingested),
    as.character(nrow(all_data)),
    paste(min(all_data$date), "to", max(all_data$date)),
    sprintf("%.1f", mean(bing_sentiment$sentiment_score < 0, na.rm = TRUE) * 100),
    sprintf("%.1f", pull_pct("app_reviews")),
    sprintf("%.1f", pull_pct("ticketmaster")),
    sprintf("%.1f", app_avg_rating),
    sprintf("%s (%.1f%% at risk, n=%d)",
            churn_by_source$source[1], churn_by_source$pct_at_risk[1],
            churn_by_source$n[1]),
    sprintf("%.1f", nrow(wc_docs) / nrow(all_data) * 100),
    sprintf("%s (%.1f%%)", str_to_title(overall_emotions$sentiment[1]),
            overall_emotions$pct[1]),
    tfidf_sentiment %>% filter(sentiment_group == "negative") %>%
      slice_max(tf_idf, n = 1, with_ties = FALSE) %>% pull(word),
    tfidf_sentiment %>% filter(sentiment_group == "positive") %>%
      slice_max(tf_idf, n = 1, with_ties = FALSE) %>% pull(word)
  )
)

write_csv(summary_table, file.path(OUT_DIR, "summary_metrics.csv"))

cat("\n", strrep("=", 62), "\n", sep = "")
cat("  SUMMARY\n")
cat(strrep("=", 62), "\n", sep = "")
print(summary_table, n = Inf)
cat("\nPlots and summary_metrics.csv written to ", OUT_DIR, "/\n", sep = "")
