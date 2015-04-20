library(qdap)
library(snow)
library(readr)
library(dplyr)
library(data.table)
source("R/tokenizer.R")

# Used to speed up interactive testing. Set to 1 when ready for production
sample_divisor <- 100

# Process twitter data
twitter <- read_lines("data/twitter.txt")
# Convert to ASCII to remove any weird characters like "â"
# Let's take 10% of the data
twitter_length <- length(twitter)
twitter_sample_length <- as.integer(twitter_length/sample_divisor)
twitter_sample_rows <- sample(1:twitter_length, twitter_sample_length)
twitter_sample <- twitter[twitter_sample_rows]

cl <- makeCluster(16)
  clusterEvalQ(cl, source("R/tokenizer.R"))
  system.time(twitter_lines <- parSapply(cl, twitter_sample, function(x) {clean_line(x)})) 
stopCluster(cl)


#system.time(twitter_grams <- ngrams(twitter_lines, n=3))


# Process blog data


blogs <- read_lines("data/blogs.txt")
# Let's take 10% of the data
blogs_length <- length(blogs)
blogs_sample_length <- as.integer(blogs_length/sample_divisor)
blogs_sample_rows <- sample(1:blogs_length, blogs_sample_length)
blogs_sample <- blogs[blogs_sample_rows]

cl <- makeCluster(14)
clusterEvalQ(cl, source("R/tokenizer.R"))
system.time(blogs_lines <- parSapply(cl, blogs_sample, function(x) {clean_line(x)})) 
stopCluster(cl)


#system.time(blogs_grams <- ngrams(blogs_lines, n=3))
 
 
# Process news

news <- read_lines("data/news.txt")
# Let's take 10% of the data
news_length <- length(news)
news_sample_length <- as.integer(news_length/sample_divisor)
news_sample_rows <- sample(1:news_length, news_sample_length)
news_sample <- news[news_sample_rows]

cl <- makeCluster(14)
clusterEvalQ(cl, source("R/tokenizer.R"))
system.time(news_lines <- parSapply(cl, news_sample, function(x) {clean_line(x)})) 
stopCluster(cl)



all_phrases <- paste(twitter_lines, news_lines, blogs_lines)
n_grams <- ngrams(all_phrases, n=3)

# Let's pull out just the data we need
n_1 <- n_grams$all_n$n_1
n_2 <- n_grams$all_n$n_2
n_3 <- n_grams$all_n$n_3

# Get counts on n_3 to use for priority of prediction. Used in place of
# probability

# turn the list of n terms into sentences
n_3_sentence <- sapply(n_3, function(x) {paste(x[[1]], x[[2]], x[[3]])})
# Get the counts
n_3_table <- data.table(n_3_sentence)
n_3_counts <- n_3_table %>%
  group_by(n_3_sentence) %>%
  summarize(count = n())

# Now that we have counts, let's pull out the first two terms to use as keys
#n_3_terms <- n_3_counts$n_3_sentence

n_3_final <- n_3_counts %>%
  mutate(
    two_words = gsub("(.+\\s.+)\\s.+", "\\1", n_3_sentence),
    predict_term = gsub(".+\\s.+\\s", "", n_3_sentence)
  ) %>%
  select(
    count,
    two_words,
    predict_term) %>%
  arrange(
    two_words,
    desc(count))

 
 
 
 
 
 
 
 
 
 
 
 
# twitter <- readLines("data/twitter.txt")
# twitter_lines <- length(twitter)
# 
# 
# source("R/tokenizer.R")
#   # Be sure to use the correct number of cores for your system.
#   cl <- makeCluster(16)
#   clusterEvalQ(cl, source("R/tokenizer.R"))
#   returned <- parSapply(cl, twitter, function(x) {get_tokens(x)})  
#   stopCluster(cl)
# 
# # Break up the long list into a list of smaller lists
# breaks <- 1000
# words <- as.list(1:breaks)
# 
# 
# segment_size <- as.integer(twitter_lines / breaks)
# segment_start <- 1
# for(i in 1:breaks) {
#   segment_end   <- segment_start + segment_size
#   words[[i]] <- returned[segment_start:segment_end]
#   segment_start <- i * breaks
# }
# 
# cl <- makeCluster(16)
#   system.time(twitter_word_count <- parSapply(cl, words, function(x) { sum(length(unlist(x))) }))
# stopCluster(cl)
# 
# all_twitter_word_count <- sum(unlist(twitter_word_count))
# 
# n_grams <- ngrams(words[[1]])
# plot(n_grams)