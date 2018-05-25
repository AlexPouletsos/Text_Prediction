setwd("~/Backup/Junk/Full_Text")

library(quanteda)
library(dplyr)
library(tidyr)

con <- file("en_US.blogs.txt", "r")
blog <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.news.txt", "r")
news <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.twitter.txt", "r")
twit <- readLines(con, skipNul = TRUE)
close(con)

set.seed(2187)
blogSamp <- sample(blog, size=length(blog)*0.01)
newsSamp <- sample(news, size=length(news)*0.01)
twitSamp <- sample(twit, size=length(twit)*0.01)

#Clean up text
all_text <- combine(blogSamp, newsSamp, twitSamp)
all_text <- iconv(all_text, "latin1", "ASCII", sub="")
dcorp <- corpus(all_text)
toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
toks <- tokens_tolower(toks)

rm(blog)
rm(news)
rm(twit)
rm(all_text)
rm(dcorp)

firsts <- NULL

for (i in 1:length(toks)) {
        word1 <- toks[i][[1]][1]
        firsts <- rbind(firsts, word1)
}

topfeatures(dfm(firsts))
