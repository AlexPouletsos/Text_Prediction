setwd("~/Backup/Junk/Full_Text")

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

#Read common ngram table to reference first
commontab <- read.csv("Common Word Table.csv", row.names = 1)

#First checks the common table
wordIn <- "a case of" #ENTER WORDS HERE
wordIn <- paste(wordIn,"*", sep = " ")
wordIn2 <- paste(gsub(" ", "_", wordIn), sep="")
neg_bigram <- tokens_compound(tokens(rownames(commontab)), phrase(wordIn))
neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
toks_dfm <- dfm(neg_bigram)
check <- topfeatures(toks_dfm)[1]

#If not found in table, then go through loop
ifelse(!is.na(check), check, {

#More accurate search

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

#&& (topfeatures(dfmN)[[1]]==topfeatures(dfmN)[[2]] || topfeatures(dfmN)[[1]]==1)) || i != 3

while(if_else(stri_count_regex(wordIn, " ")>=3, p > .1, p > .001)) {
        
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
})


###


if_else(p <= .001,
substr(names(topfeatures(dfmN)[1]),
       gregexpr("_",names(topfeatures(dfmN)[1]))[[1]][stri_count_regex(names(topfeatures(dfmN)[1]), "_")] + 1, 
        nchar(names(topfeatures(dfmN)[1])))
, "next step")

topfeatures(dfmN, 10)











while (p >= .05) {
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
        
        test <- as.data.frame(topfeatures(dfmN,ncol(dfmN)))
        p <- binom.test(topfeatures(dfmN)[[1]], sum(test))$p.value
}

topfeatures(dfmN, 10)
substr(names(topfeatures(dfmN)[1]), 
       gregexpr("_",names(topfeatures(dfmN)[1]))[[1]][stri_count_regex(names(topfeatures(dfmN)[1]), "_")] + 1, 
       nchar(names(topfeatures(dfmN)[1])))  

#if no match then...




blogLen <- 0
newsLen <- 0
twitLen <- 0
n <- 100

wordIn <- tokens_remove(tokens("president of the united states of"), stopwords("english"))[[1]] #Enter words
wordIn <- stri_paste(wordIn, collapse = ' ')
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
toks <- tokens_remove(toks, stopwords("english"))
neg_bigram <- tokens_compound(toks, phrase(wordIn))
neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
dfmN <- dfm(neg_bigram)

while (topfeatures(dfmN)[[1]]<=1) {
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
        toks <- tokens_remove(toks, stopwords("english"))
        neg_bigram <- tokens_compound(toks, phrase(wordIn))
        neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
        toks_dfm <- dfm(neg_bigram)
        
        try(dfmN <- rbind(dfmN, toks_dfm), silent = TRUE)
}

topfeatures(dfmN, 10)

substr(names(topfeatures(dfmN)[1]), 
       gregexpr("_",names(topfeatures(dfmN)[1]))[[1]][stri_count_regex(names(topfeatures(dfmN)[1]), "_")] + 1, 
       nchar(names(topfeatures(dfmN)[1])))  


test <- as.data.frame(topfeatures(dfmN,ncol(dfmN)))
prop.table(test[1])[1,]
x <- topfeatures(dfmN)[[1]]
n <- sum(test)
binom.test(x, n)$p.value

