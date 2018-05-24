setwd("~/Backup/Junk/Full_Text")

library(readtext)
library(quanteda)
library(tm)
library(stringi)

con <- file("en_US.blogs.txt", "r")
blog <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.news.txt", "r")
news <- readLines(con, skipNul = TRUE)
close(con)

con <- file("en_US.twitter.txt", "r")
twit <- readLines(con, skipNul = TRUE)
close(con)

dir.create("./Sample2")

blogLen <- 0
newsLen <- 0
twitLen <- 0
n <- 100

wordIn <- "thank you" #Enter words
write(paste(wordIn, "abcxyzblog", sep = " "), file = "./Sample2/blog2.txt")
write(paste(wordIn, "abcxyznews", sep = " "), file = "./Sample2/news2.txt")
write(paste(wordIn, "abcxyztwit", sep = " "), file = "./Sample2/twit2.txt")

wordIn <- paste(wordIn,"*", sep = " ")
wordIn2 <- paste(gsub(" ", "_", wordIn), sep="")

all_text <- readtext(paste0("./", "./Sample2/*"), text_field = "speech")
all_text <- iconv(all_text, "latin1", "ASCII", sub="")
dcorp <- corpus(all_text)
toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
toks <- tokens_tolower(toks)
neg_bigram <- tokens_compound(toks, phrase(wordIn))
neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
dfmN <- dfm(neg_bigram)

while (topfeatures(dfmN)[[1]]<=10) {
        write(blog[blogLen+1:(blogLen+length(blog)/n)], file = "./Sample2/blog2.txt")
        write(news[newsLen+1:(newsLen+length(news)/n)], file = "./Sample2/news2.txt")
        write(twit[twitLen+1:(twitLen+length(twit)/n)], file = "./Sample2/twit2.txt")
        
        blogLen <- blogLen + length(blog)/n
        newsLen <- newsLen + length(news)/n
        twitLen <- twitLen + length(twit)/n
        
        all_text <- readtext(paste0("./", "./Sample2/*"), text_field = "speech")
        all_text <- iconv(all_text, "latin1", "ASCII", sub="")
        
        dcorp <- corpus(all_text)
        toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
        toks <- tokens_tolower(toks)
        neg_bigram <- tokens_compound(toks, phrase(wordIn))
        neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
        toks_dfm <- dfm(neg_bigram)
        
        try(dfmN <- rbind(dfmN, toks_dfm), silent = TRUE)
}

topfeatures(dfmN, 10)

substr(names(topfeatures(dfmN)[1]), 
       gregexpr("_",names(topfeatures(dfmN)[1]))[[1]][stri_count_regex(names(topfeatures(dfmN)[1]), "_")] + 1, 
       nchar(names(topfeatures(dfmN)[1])))  


