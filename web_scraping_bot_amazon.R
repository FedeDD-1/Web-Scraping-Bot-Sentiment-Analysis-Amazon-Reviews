# LOAD LIBRARIES 
library(tidyverse)
library(rvest)
library(RSelenium)
library(netstat)
library(stringr)
library(janitor)
library(cld2)
library(tidytext)
library(textdata)
library(patchwork)
library(scales)
library(purrr)
library(udpipe)
library(GGally)
library(polyglotr)
library(text2vec)
library(glmnet)
library(transltr)

# FUNCTION: scrape review pages with optional star filter 
download_amazon_pages <- function(product_id,
                                  n_pages   = 10,
                                  folder    = "Amazon/",
                                  pause_sec = 5,
                                  user      = NULL,
                                  password  = NULL,
                                  filter_stars = NULL,
                                  sort_by = "helpful") {
  free_port <- netstat::free_port()
  rD <- rsDriver(browser  = "firefox",
                 port     = free_port,
                 verbose  = FALSE,
                 chromever = NULL,
                 check = FALSE,
                 phantomver = NULL,
                 extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless'))))
  remDr <- rD[["client"]]
  
  # Build review URL
  url_base <- sprintf("https://www.amazon.co.uk/product-reviews/%s?sortBy=%s",
                      product_id, sort_by)
  if (!is.null(filter_stars)) {
    url_base <- paste0(url_base, "&filterByStar=", filter_stars)
  }
  
  # Navigate
  tryCatch({
    remDr$navigate(url_base)
    Sys.sleep(3)
  }, error = function(e) {
    cat("Navigation failed or session invalid. Skipping this combination.\n")
    remDr$close(); rD$server$stop()
    stop("Aborting scrape due to navigation failure.")
  })
  
  # Optional login
  if (!is.null(user) && !is.null(password)) {
    tryCatch({
      remDr$findElement("css", "#ap_email")$sendKeysToElement(list(user))
      remDr$findElement("css", "#continue")$clickElement()
      Sys.sleep(2)
      remDr$findElement("css", "#ap_password")$sendKeysToElement(list(password))
      remDr$findElement("css", "#signInSubmit")$clickElement()
      Sys.sleep(3)
    }, error = function(e) {
      cat("Login failed. Proceeding without login.\n")
    })
  }
  
  dir.create(folder, showWarnings = FALSE)
  
  # Save first page
  tryCatch({
    page_html <- remDr$getPageSource()[[1]]
    write(page_html, file = file.path(folder, paste0("Amazon_", sort_by, "_", filter_stars, "_1.html")))
  }, error = function(e) {
    cat("Failed to retrieve or save first page. Skipping this combination.\n")
    remDr$close(); rD$server$stop()
    stop("Aborting scrape due to first page retrieval failure.")
  })
  
  # Loop through pages
  for (i in 2:n_pages) {
    target_file <- file.path(folder, sprintf("Amazon_%s_%s_%d.html", sort_by, filter_stars, i))
    if (!file.exists(target_file)) {
      btn_next <- tryCatch({
        remDr$findElement("css", ".a-last")
      }, error = function(e) {
        cat(sprintf("  - No next page found at page %d. Stopping early.\n", i-1))
        return(NULL)
      })
      
      if (is.null(btn_next)) break
      
      tryCatch({
        btn_next$clickElement()
        Sys.sleep(pause_sec)
        page_html <- remDr$getPageSource()[[1]]
        write(page_html, file = target_file)
      }, error = function(e) {
        cat(sprintf("  - Error during page %d retrieval. Stopping early.\n", i))
        break
      })
    }
  }
  
  remDr$close(); rD$server$stop()
  invisible(TRUE)
}

# STAR FILTERS & SORT OPTIONS TO SCRAPE 
star_filters <- c("five_star", "four_star", "three_star", "two_star", "one_star")

sort_options <- c(helpful = "helpful",
                  recent  = "recent")

# ─── SET YOUR PARAMETERS HERE ──────────────────────────────────────────
product_id <- "B08ZNTM1X1"
n_pages    <- 10
folder     <- "Amazon/"
pause_sec  <- 5
user       <- "" #INSERT YOUR AMAZON.UK ACCOUNT EMAIL
password   <- "" # INSERT THE PASSWORD OF YOUR ACCOUNT
# ───────────────────────────────────────────────────────────────────────

# Run scraper for each sort × star combination
for (sort_by in sort_options) {
  cat("=== Scraping sort:", sort_by, "===\n")
  for (filter in star_filters) {
    cat("  - Scraping:", filter, "\n")
    
    # Wrap entire scrape in error handling so failures don’t stop the loop
    tryCatch({
      download_amazon_pages(product_id  = product_id,
                            n_pages     = n_pages,
                            folder      = folder,
                            pause_sec   = pause_sec,
                            user        = user,
                            password    = password,
                            filter_stars= filter,
                            sort_by     = sort_by)
    }, error = function(e) {
      cat(sprintf("Error scraping %s - %s: %s\n", sort_by, filter, e$message))
    })
    
    gc()  # Free memory between runs
  }
}

# FUNCTION: safely extract reviews from one HTML file 
amazon_reviews <- function(file) {
  result <- tryCatch({
    html  <- read_html(file, encoding = "utf-8")
    nodes <- html_elements(html, "[data-hook='review']")
    
    reviews <- purrr::map_dfr(nodes, function(node) {
      tibble(
        title = node %>%
          html_element("[data-hook='review-title'] span:nth-child(3)") %>%
          html_text2(),
        text  = node %>%
          html_element("[data-hook='review-body'] span") %>%
          html_text2(),
        star  = node %>%
          html_element("[data-hook='review-star-rating'], [data-hook='cmps-review-star-rating'] span") %>%
          html_text2()
      )
    })
    reviews$file <- basename(file)
    reviews
  }, error = function(e) {
    message(sprintf("Skipped file (error): %s", basename(file)))
    tibble(title = character(0), text = character(0), star = character(0), file = character(0))
  })
  result
}

# Set/verify Amazon folder 
dir.create("Amazon", showWarnings = FALSE)

# Gather all saved HTML files
files <- list.files("Amazon", pattern = "\\.html$", full.names = TRUE)

# Combine into one data frame
df <- map_dfr(files, amazon_reviews) %>%
  mutate(doc_id = row_number())

write_csv(df, "Amazon_reviews.csv")      

# 2. IMPORT & CLEAN DATA 
reviews <- read_csv("Amazon_reviews.csv") %>% 
  clean_names() %>%
  mutate(doc_id = as.character(doc_id),
         star   = str_extract(star, "^\\d") %>% as.integer(),
         text   = str_squish(text),
         lang   = detect_language(text, plain_text = TRUE))

# Prepare columns for English translations
reviews$title_en <- NA
reviews$text_en  <- NA

for (i in seq_len(nrow(reviews))) {
  source_lang <- reviews$lang[i]
  if (is.na(source_lang) || source_lang == "en") {   
    reviews$title_en[i] <- reviews$title[i]
    reviews$text_en[i]  <- reviews$text[i]
  } else {                                            
    reviews$title_en[i] <- google_translate(reviews$title[i],
                                            target_language = "en",
                                            source_language = source_lang)
    reviews$text_en[i]  <- google_translate(reviews$text[i],
                                            target_language = "en",
                                            source_language = source_lang)
  }
}

print(reviews)  

# Deduplicate by identical title, text, and star
reviews_unique <- reviews[!duplicated(reviews[c("title", "text", "star")]), ]

write_csv(reviews_unique, "Amazon_reviews_unique.csv")
reviews_unique <- read_csv("Amazon_reviews_unique.csv")

# Keep old texts and switch translated columns to original names
names(reviews_unique)[names(reviews_unique) == "title"] <- "title_old"
names(reviews_unique)[names(reviews_unique) == "text"] <- "text_old"
names(reviews_unique)[names(reviews_unique) == "title_en"] <- "title"
names(reviews_unique)[names(reviews_unique) == "text_en"]  <- "text"

head(reviews_unique)  

reviews <- reviews_unique %>%
  filter(!is.na(text))

# 3. QUICK DESCRIPTIVE PLOTS 
# Star distribution bar chart
p_rating <- ggplot(reviews, aes(star)) +
  geom_bar(fill = "steelblue") +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Review count per rating",
       x = "Amazon stars", y = "Count") +
  theme_minimal()

# Histogram of review length (words)
p_length <- reviews %>%
  mutate(length = str_count(text, "\\w+")) %>%
  ggplot(aes(length)) +
  geom_histogram(bins = 30, fill = "darkorange") +
  labs(title = "Review length (words)",
       x = "Word count", y = "Frequency") +
  theme_minimal()

(p_rating | p_length)  # show side by side

# Basic counts and summaries below … (code unchanged)
reviews %>% count(star) %>% mutate(perc = n / sum(n))
reviews %>% mutate(word_count = str_count(text, "\\w+")) %>%
  summarise(min_words = min(word_count), max_words = max(word_count),
            mean_words = mean(word_count), median_words = median(word_count),
            sd_words = sd(word_count))
reviews %>% mutate(char_count = nchar(text)) %>%
  summarise(min_chars = min(char_count), max_chars = max(char_count),
            mean_chars = mean(char_count), median_chars = median(char_count),
            sd_chars = sd(char_count))
reviews %>% filter(str_length(text) < 10) %>% summarise(n = n())
reviews %>% mutate(word_count = str_count(text, "\\w+")) %>% group_by(star) %>%
  summarise(min = min(word_count), max = max(word_count), mean = mean(word_count),
            median = median(word_count), sd = sd(word_count))

stats_wc <- reviews %>%
  mutate(word_count = str_count(text, "\\w+")) %>%
  summarise(mean_words = mean(word_count),
            median_words = median(word_count))

reviews %>%
  mutate(word_count = str_count(text, "\\w+")) %>%
  ggplot(aes(x = word_count)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "white") +
  geom_vline(aes(xintercept = stats_wc$mean_words), color = "red", linetype = "dashed", linewidth=1) +
  geom_vline(aes(xintercept = stats_wc$median_words), color = "green", linetype = "dotted", linewidth=1) +
  labs(title = "Reviews length: histogram with mean and median",
       x = "Word count", y = "Frequency") +
  theme_minimal() + xlim(0, 300)


ggplot(reviews, aes(x = factor(star))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Star Distribution",     
       x = "Rating (Stars)", y = "Count") +
  theme_minimal()

reviews %>% mutate(word_count = str_count(text, "\\w+")) %>%
  ggplot(aes(x = word_count)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = "Reviews length (number of words)",
       x = "Number of words", y = "Frequency") +
  theme_minimal()

reviews %>% mutate(word_count = str_count(text, "\\w+")) %>%
  ggplot(aes(x = factor(star), y = word_count, fill = factor(star))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Word count per rating",
       x = "Amazon stars", y = "Word") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 300))

# Average word count per star 
reviews %>%
  mutate(word_count = str_count(text, "\\w+")) %>%
  group_by(star) %>%
  summarise(mean_wc = mean(word_count)) %>%
  ggplot(aes(x = factor(star), y = mean_wc, fill = mean_wc)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Average review length by star",
       x = "Stars", y = "Mean words") +
  theme_minimal()

# Build Bing sentiment dictionary 
bing_dict <- get_sentiments("bing") %>%
  mutate(polarity = if_else(sentiment == "positive", 1, -1)) %>%
  select(term = word, polarity)

# Sentiment score via tidytext (Bing) 
tidy_scores <- reviews %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(doc_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(bing = positive - negative) %>%
  select(doc_id, bing) %>%
  mutate(doc_id = as.character(doc_id))
# Output: one row per review with Bing score

# Annotate text with UDPipe 
texts_named <- setNames(reviews$text, reviews$doc_id)   
output <- udpipe(texts_named, "english-gum")            

# Sentiment score via UDPipe (negation, amplifiers)
sent_udpipe <- txt_sentiment(
  x = output,
  term = "lemma",
  polarity_terms = bing_dict,
  polarity_negators = c("not", "neither", "no", "without"),
  polarity_amplifiers = c("really", "very", "definitely", "super"),
  polarity_deamplifiers= c("barely", "hardly"),
  amplifier_weight = 0.8,
  n_before = 2, n_after = 0,
  constrain = FALSE)

# Merge both sentiment scores back to main data 
reviews$udpipe <- sent_udpipe$overall$sentiment_polarity
reviews <- reviews %>%
  mutate(doc_id = as.character(doc_id)) %>%
  left_join(tidy_scores %>% mutate(doc_id = as.character(doc_id)), by = "doc_id") %>%
  replace_na(list(bing = 0))

# Correlation between sentiment and star rating 
reviews %>%
  summarise(cor_bing   = cor(bing,   star, use = "complete.obs"),
            cor_udpipe = cor(udpipe, star, use = "complete.obs"))

# Boxplot of sentiment by rating 
reviews %>%
  pivot_longer(c(bing, udpipe), names_to = "method", values_to = "score") %>%
  ggplot(aes(factor(star), score, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(title = "Sentiment score per rating",
       x = "Stars", y = "Sentiment", fill = "Method")

# Scatter plot Bing vs UDPipe 
ggplot(reviews, aes(bing, udpipe)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tidytext vs UDPipe",
       x = "Bing", y = "UDPipe")

# Summary and scaling (z‑score) 
compare_long <- reviews %>%
  select(doc_id, bing, udpipe) %>%
  pivot_longer(cols = c(bing, udpipe), names_to = "method", values_to = "sentiment")

compare_long %>%
  group_by(method) %>%
  summarise(mean_sentiment = mean(sentiment),
            sd_sentiment = sd(sentiment), n = n())

# Histograms of raw sentiment
a <- compare_long %>%
  ggplot(aes(sentiment)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~method) +
  labs(title = "Raw sentiment distribution")

a

# Histograms of scaled sentiment
compare_scaled <- compare_long %>%
  group_by(method) %>%
  mutate(sentiment_scaled = scale(sentiment)) %>%
  ungroup()

b <- compare_scaled %>%
  ggplot(aes(sentiment_scaled)) +
  geom_histogram(bins = 30, fill = "coral", color = "white") +
  facet_wrap(~method) +
  labs(title = "Scaled sentiment distribution", x = "Scaled sentiment")

b

# Scatter on scaled scores
a_s <- compare_scaled %>%
  select(doc_id, method, sentiment_scaled) %>%
  pivot_wider(names_from = method, values_from = sentiment_scaled)

ggplot(a_s, aes(bing, udpipe)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scaled Sentiment: Bing vs UDPipe",
       x = "Bing (scaled)", y = "UDPipe (scaled)")

# ggpairs correlation matrix
sentiments_wide <- a_s
sentiments_wide %>%
  select(bing, udpipe) %>%
  drop_na() %>%
  mutate(across(everything(), as.numeric)) %>%
  ggpairs()

# Multinomial GLMnet to predict star rating 
#   1) TF‑IDF matrix
tokens000  <- itoken(reviews$text, progressbar = FALSE)
vectorizer000 <- vocab_vectorizer(create_vocabulary(tokens000))
dtm000 <- create_dtm(tokens000, vectorizer000)

#   2) Cross‑validated elastic‑net
cv_model000 <- cv.glmnet(dtm000, reviews$star, family = "multinomial")

#   3) Training‑set accuracy
probs000        <- predict(cv_model000, newx = dtm000, s = "lambda.min", type = "response")
predicted_star0 <- sort(unique(reviews$star))[apply(probs000, 1, which.max)]
cat("Training accuracy:", round(mean(predicted_star0 == reviews$star) * 100, 2), "%\n")

#   4) Train / test split
set.seed(123)
train_idx000 <- sample(seq_len(nrow(reviews)), size = 0.8 * nrow(reviews))
model_cv000 <- cv.glmnet(dtm000[train_idx000, ], reviews$star[train_idx000], family = "multinomial")
probs_test <- predict(model_cv000, newx = dtm000[-train_idx000, ], s = "lambda.min", type = "response")
pred_test <- sort(unique(reviews$star))[apply(probs_test, 1, which.max)]
cat("Test accuracy:", round(mean(pred_test == reviews$star[-train_idx000]) * 100, 2), "%\n")

# Confusion matrices (train / test)
confusion_train000 <- table(Predicted = predicted_star0, Actual = reviews$star)
confusion_test000  <- table(Predicted = pred_test, Actual = reviews$star[-train_idx000])
print(confusion_train000)
print(confusion_test000)

# Extra descriptive plots 
# Distribution of stars
p_stars <- ggplot(reviews, aes(factor(star))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Star distribution",
       x = "Rating (stars)", y = "Count") +
  theme_minimal()

p_stars

# Sentiment summary table
summary_sent <- reviews %>%
  pivot_longer(c(bing, udpipe), names_to = "method", values_to = "sentiment") %>%
  group_by(method) %>%
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE),
            sd_sentiment = sd(sentiment,   na.rm = TRUE),
            min_sentiment = min(sentiment,  na.rm = TRUE),
            max_sentiment = max(sentiment,  na.rm = TRUE),
            median_sentiment = median(sentiment, na.rm = TRUE),
            n = n())
print(summary_sent)

# Mean sentiment per rating
p_mean_sentiment <- reviews %>%
  pivot_longer(c(bing, udpipe), names_to = "method", values_to = "sentiment") %>%
  group_by(star, method) %>%
  summarise(mean_sent = mean(sentiment, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(star), y = mean_sent, fill = method)) +
  geom_col(position = "dodge") +
  labs(title = "Mean sentiment per star",
       x = "Rating (stars)", y = "Mean sentiment", fill = "Method") +
  theme_minimal()

p_mean_sentiment

# Histogram of sentiment scores
p_dist_sentiment <- reviews %>%
  pivot_longer(c(bing, udpipe), names_to = "method", values_to = "sentiment") %>%
  ggplot(aes(sentiment)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~method, scales = "free") +
  labs(title = "Sentiment score distribution",
       x = "Sentiment score", y = "Frequency") +
  theme_minimal()

p_dist_sentiment

# Boxplot of sentiment by rating
p_boxplot <- reviews %>%
  pivot_longer(c(bing, udpipe), names_to = "method", values_to = "sentiment") %>%
  ggplot(aes(factor(star), sentiment, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(title = "Sentiment distribution by rating",
       x = "Rating (stars)", y = "Sentiment", fill = "Method") +
  theme_minimal()

p_boxplot

# Scatter Bing vs UDPipe
p_scatter <- ggplot(reviews, aes(bing, udpipe)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Bing vs UDPipe sentiment",
       x = "Bing", y = "UDPipe") +
  theme_minimal()

p_scatter

# Z‑score histograms
p_scaled <- compare_scaled %>%
  ggplot(aes(sentiment_scaled)) +
  geom_histogram(bins = 30, fill = "coral", color = "white") +
  facet_wrap(~method) +
  labs(title = "Normalised sentiment (z‑score)",
       x = "Scaled sentiment", y = "Frequency") +
  theme_minimal()

p_scaled

# Word‑level exploration 
# Tokenise reviews, remove stopwords/digits
tidy_words111 <- reviews %>%
  unnest_tokens(word111, text) %>%
  anti_join(stop_words, by = c("word111" = "word")) %>%   
  filter(!str_detect(word111, "\\d"))                   

# Global word frequencies
word_freq111 <- tidy_words111 %>% count(word111, sort = TRUE)

# Top 20 words bar plot
p_top_words111 <- word_freq111 %>%
  slice_max(n, n=20) %>%
  mutate(word111 = reorder(word111, n)) %>%
  ggplot(aes(word111, n)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Top 20 most frequent words", x = NULL, y = "Frequency") +
  theme_minimal()

p_top_words111

# Frequent words by low / medium / high stars
tidy_words222 <- tidy_words111 %>%
  mutate(group222 = case_when(
    reviews$star[as.integer(doc_id)] <= 2 ~ "Low stars",
    reviews$star[as.integer(doc_id)] >= 4 ~ "High stars",
    TRUE ~ "Medium stars"))

word_freq_by_group222 <- tidy_words222 %>%
  group_by(group222, word111) %>% summarise(n = n(), .groups = "drop") %>%
  group_by(group222) %>% slice_max(n, n=15)

p_words_by_group222 <- word_freq_by_group222 %>%
  mutate(word111 = tidytext::reorder_within(word111, n, group222)) %>%  # ordinamento per gruppo
  ggplot(aes(word111, n, fill = group222)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~group222, scales = "free") +
  tidytext::scale_x_reordered() +  # serve per far funzionare reorder_within()
  labs(title = "Most frequent words grouped by rating",
       x = NULL, y = "Frequency") +
  theme_minimal()

p_words_by_group222


# Bigrams
bigrams333 <- reviews %>%
  unnest_tokens(bigram333, text, token = "ngrams", n = 2) %>%
  separate(bigram333, c("word1_333", "word2_333"), sep = " ") %>%
  filter(!word1_333 %in% stop_words$word,
         !word2_333 %in% stop_words$word) %>%
  unite(bigram333, word1_333, word2_333, sep = " ") %>%
  count(bigram333, sort = TRUE)

bigrams333 %>% slice_max(n, n = 20)

top_bigrams333 <- bigrams333 %>% 
  slice_max(n, n = 6) %>%
  mutate(bigram333 = reorder(bigram333, n))

ggplot(top_bigrams333, aes(x = bigram333, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 6 bigrammi più frequenti",
       x = NULL, y = "Frequenza") +
  theme_minimal()

bigrams_top6_333 <- bigrams_by_udpipe333_tb %>%
  group_by(sentiment_class333) %>%
  slice_max(n, n = 6) %>%
  ungroup()

bigrams_top6_333

ggplot(bigrams_by_udpipe333_tb, aes(x = reorder_within(bigram333, n, sentiment_class333), y = n, fill = sentiment_class333)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ sentiment_class333, scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Top 10 bigram per sentiment class (UDPipe)",
       x = NULL, y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_blank())





# `Stars vs UDPIPE 

set.seed(123)  # per riproducibilità

n_iterations <- 100  # numero di campioni da estrarre
n_total <- nrow(reviews)  # numero totale recensioni

# calcola numero di recensioni da estrarre per ciascuna stella
sample_sizes <- round(n_total * c(`5`=0.76, `4`=0.16, `3`=0.04, `2`=0.01, `1`=0.03))

# lista per salvare i risultati
results_list <- vector("list", n_iterations)

for (i in seq_len(n_iterations)) {
  # estrai un campione rispettando la distribuzione reale
  sample_reviews <- bind_rows(
    reviews %>% filter(star == 5) %>% slice_sample(n = sample_sizes["5"], replace = TRUE),
    reviews %>% filter(star == 4) %>% slice_sample(n = sample_sizes["4"], replace = TRUE),
    reviews %>% filter(star == 3) %>% slice_sample(n = sample_sizes["3"], replace = TRUE),
    reviews %>% filter(star == 2) %>% slice_sample(n = sample_sizes["2"], replace = TRUE),
    reviews %>% filter(star == 1) %>% slice_sample(n = sample_sizes["1"], replace = TRUE)
  ) %>%
    mutate(sentiment_class_udpipe = case_when(
      udpipe <= -1 ~ "Negative",
      udpipe >= 1  ~ "Positive",
      TRUE         ~ "Neutral"
    )) %>%
    count(sentiment_class_udpipe) %>%
    mutate(perc = n / sum(n),
           iteration = i)
  
  results_list[[i]] <- sample_reviews
}

# combina i risultati di tutte le iterazioni
results_df <- bind_rows(results_list)

# calcola la media e sd delle percentuali per ciascuna classe di sentiment
summary_sentiment <- results_df %>%
  group_by(sentiment_class_udpipe) %>%
  summarise(mean_perc = mean(perc, na.rm=TRUE),
            sd_perc = sd(perc, na.rm=TRUE),
            .groups = "drop")

print(summary_sentiment)


library(ggplot2)

ggplot(summary_sentiment, aes(x = sentiment_class_udpipe, y = mean_perc, fill = sentiment_class_udpipe)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_perc - sd_perc, ymax = mean_perc + sd_perc), width = 0.2) +
  labs(title = "Mean and Standard Deviation of sentiment \n over 100 stratified random samples",
       x = "Sentiment Class (UDPipe)", y = "Mean Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()


