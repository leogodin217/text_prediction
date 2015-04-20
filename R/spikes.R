# source("tokenizer.R")
# 
# system.time(data <- readLines("../data/twitter.txt"))
# 
# #system.time(tokens <- sapply(data, function(x) {get_tokens(x)}))
# 
# library(parallel)
# 
# #system.time(tokens <- mclapply(data, function(x) {get_tokens(x)}))
# 
# 
# library(snow)
# cl <- makeCluster(16)
# #registerDoSNOW(cl)
# clusterEvalQ(cl, source("tokenizer.R"))
# clusterEvalQ(cl, library(qdap))
# 
# #system.time(tokens <- clusterApply(cl, data, function(x) {get_tokens(x)}))
# #system.time(tokens <- clusterApplyLB(cl, data, function(x) {get_tokens(x)}))
# system.time(tokens  <- parSapply(cl, data[1:1000], function(x) {get_tokens(x)}))
# system.time(n_grams <- parSapply(cl, my_tokens, function(x) {ngrams(x)}))
# #registerDoSEQ()
# stopCluster(cl)
# 
# system.time(tokens  <- lapply(data[1:100000], function(x) {get_tokens(x)}))
# system.time(n_grams <- lapply(my_tokens, function(x) {ngrams(x)}))
# 
# 
# library(qdap)
# #https://dl.dropboxusercontent.com/u/61803503/packages/qdap_vignette.html#tools
# system.time(word_counts <- word_list(data[1:10000]))
# system.time(freqs <- freq_list(data[1:10000]))
# system.time(freqs <- word_stats(data[1:10000]))
# system.time(stems <- stemmer(data[1:10000]))
# 
# 
# 
# 
# # New start
# library(snow)
# library(qdap)
# 
# twitter_file <- "data/twitter.txt"
# news_file    <- "data/news.txt"
# blogs_file   <- "data/blogs.txt"
# 
# # Reading the files
# twitter <- readLines(twitter_file)
# news    <- readLines(news_file)
# blogs   <- readLines(blogs_file)
# 
# 
# source("R/tokenizer.R")
# parallel_tokens <- function(entries) {
#   # Be sure to use the correct number of cores for your system.
#   cl <- makeCluster(16)
#   clusterEvalQ(cl, source("R/tokenizer.R"))
#   returned <- parSapply(cl, entries, function(x) {get_tokens(x)})  
#   stopCluster(cl)
#   return(returned)
# }  
# 
# Rprof()
# cl <- makeCluster(16)
# clusterEvalQ(cl, source("R/tokenizer.R"))
# returned <- parSapply(cl, twitter, function(x) {get_tokens(x)})
# stopCluster(cl)
# summaryRprof()
# 
# Rprof()
# twitter_words <- parallel_tokens(twitter)
# summaryRprof()
# 
# 
# cl <- makeCluster(16)
# #clusterEvalQ(cl, source("R/tokenizer.R"))
# returned <- parSapply(cl, twitter_lines, length)
# stopCluster(cl)
# 
# library(doSNOW)
# library(qdap)
# Rprof()
# cl <- makeCluster(16)
# registerDoSNOW(cl)
# freq <- freq_terms(returned)
# stopCluster(cl)
# summaryRprof()
# 
# 
# 
# #twitter_word_stats <- word_stats(words[[1]])
# twitter_freq <- freq_terms(words[[1]])
# plot(twitter_freq)