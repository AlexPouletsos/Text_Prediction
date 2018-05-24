#en_US.news, en_US.blogs, en_US.twitter must be in working directory
setwd("~/")

library(readtext)
library(quanteda)
library(tm)
library(stringi)
library(dplyr)

con <- file("en_US.blogs.txt", "r")
blog <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.news.txt", "r")
news <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.twitter.txt", "r")
twit <- readLines(con, skipNul = TRUE)
close(con)

wordIn <- "have a nice" #ENTER WORDS HERE
wordIn <- paste(wordIn,"*", sep = " ")
wordIn2 <- paste(gsub(" ", "_", wordIn), sep="")

blogLen <- 0
newsLen <- 0
twitLen <- 0
n <- 100
i <- 0
p <- 1

blog2 <- paste(wordIn, "abcxyzblog", sep = " ")
news2 <- paste(wordIn, "abcxyznews", sep = " ")
twit2 <- paste(wordIn, "abcxyztwit", sep = " ")

wordIn2 <- paste(gsub(" ", "_", wordIn), sep="")

all_text <- combine(blog2, news2, twit2)
all_text <- iconv(all_text, "latin1", "ASCII", sub="")
dcorp <- corpus(all_text)
toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
toks <- tokens_tolower(toks)
neg_bigram <- tokens_compound(toks, phrase(wordIn))
neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
dfmN <- dfm(neg_bigram)

while(p > .01) {
        
        blog2 <- blog[blogLen+1:(blogLen+length(blog)/n)]
        news2 <- news[newsLen+1:(newsLen+length(news)/n)]
        twit2 <- twit[twitLen+1:(twitLen+length(twit)/n)]
        
        blogLen <- blogLen + length(blog)/n
        newsLen <- newsLen + length(news)/n
        twitLen <- twitLen + length(twit)/n
        
        all_text <- combine(blog2, news2, twit2)
        all_text <- iconv(all_text, "latin1", "ASCII", sub="")
        
        dcorp <- corpus(all_text)
        toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
        toks <- tokens_tolower(toks)
        neg_bigram <- tokens_compound(toks, phrase(wordIn))
        neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
        toks_dfm <- dfm(neg_bigram)

        try(dfmN <- rbind(dfmN, toks_dfm), silent = TRUE)
        try(test <- as.data.frame(topfeatures(dfmN,ncol(dfmN))))
        try(p <- binom.test(topfeatures(dfmN)[[1]], sum(test))$p.value)
        
        i <- i + 1
}

print(topfeatures(dfmN, 10))

print(substr(names(topfeatures(dfmN)[1]),
       gregexpr("_",names(topfeatures(dfmN)[1]))[[1]][stri_count_regex(names(topfeatures(dfmN)[1]), "_")] + 1, 
       nchar(names(topfeatures(dfmN)[1]))))


