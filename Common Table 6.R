#Building Common Table

setwd("~/Backup/Junk/Full_Text")

library(readtext)
library(quanteda)
library(tm)
library(stringi)
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
blogSamp <- sample(blog, size=length(blog)*0.1)
newsSamp <- sample(news, size=length(news)*0.1)
twitSamp <- sample(twit, size=length(twit)*0.1)

all_text <- combine(blogSamp, newsSamp, twitSamp)
all_text <- iconv(all_text, "latin1", "ASCII", sub="")
dcorp <- corpus(all_text)
toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
toks <- tokens_tolower(toks)

rm(blog)
rm(news)
rm(twit)
rm(blogSamp)
rm(newsSamp)
rm(twitSamp)
rm(all_text)
rm(dcorp)
gc()

#table of bigrams
toks2 <- tokens_ngrams(toks, n = 2)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test2 <- as.data.frame(topfeatures(dfmN, x))
test2 <- separate(as.data.frame(rownames(test2)), col = 1, into = c("base","pred"), sep = "_")

rm(toks2)
rm(dfmN)
gc()

#Table of trigrams
toks2 <- tokens_ngrams(toks, n = 3)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test3 <- as.data.frame(topfeatures(dfmN, x))
test3 <- separate(as.data.frame(rownames(test3)), col = 1, into = c("base1", "base2","pred"), sep = "_")
test3 <- unite(test3, "base", c("base1", "base2"), sep = " ")

rm(toks2)
rm(dfmN)
gc()

#Table of quadgrams
toks2 <- tokens_ngrams(toks, n = 4)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test4 <- as.data.frame(topfeatures(dfmN, x))
test4 <- separate(as.data.frame(rownames(test4)), col = 1, into = c("base1", "base2", "base3","pred"), sep = "_")
test4 <- unite(test4, "base", c("base1", "base2", "base3"), sep = " ")
timestamp()

rm(toks2)
rm(dfmN)
gc()

#Table of quintgrams
toks2 <- tokens_ngrams(toks, n = 5)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test5 <- as.data.frame(topfeatures(dfmN, x))
test5 <- separate(as.data.frame(rownames(test5)), col = 1, into = c("base1", "base2", "base3", "base4","pred"), sep = "_")
test5 <- unite(test5, "base", c("base1", "base2", "base3", "base4"), sep = " ")

rm(toks2)
rm(dfmN)
gc()

test <- rbind(test2, test3, test4, test5)

test <- test[!duplicated(test$base),]

testWstop1 <- test[1:(length(testWstop[,1])/3),]
testWstop2 <- test[(length(testWstop[,1])/3+1):(length(testWstop[,1])/3*2),]
testWstop3 <- test[(length(testWstop[,1])/3*2+1),length(testWstop[,1])]


write.csv(test, file = "Final Table.csv", row.names = FALSE)



#Finding Predicted word
test <- read.csv("Final Table.csv")

wordIn <- gsub("\\s+", " ", trimws("of the united states"))

as.character(test[test[,1]==wordIn, ][1,2])
###########


###Making table with stopwords removed###

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
blogSamp <- sample(blog, size=length(blog)*0.5)
newsSamp <- sample(news, size=length(news)*0.5)
twitSamp <- sample(twit, size=length(twit)*0.5)

all_text <- combine(blogSamp, newsSamp, twitSamp)
all_text <- iconv(all_text, "latin1", "ASCII", sub="")
dcorp <- corpus(all_text)
toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
toks <- tokens_tolower(toks)
toks <- tokens_remove(toks, stopwords("english"))

rm(blog)
rm(news)
rm(twit)
rm(blogSamp)
rm(newsSamp)
rm(twitSamp)
rm(all_text)
rm(dcorp)
gc()

#table of bigrams
toks2 <- tokens_ngrams(toks, n = 2)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test2 <- as.data.frame(topfeatures(dfmN, x))
test2 <- separate(as.data.frame(rownames(test2)), col = 1, into = c("base","pred"), sep = "_")

rm(toks2)
rm(dfmN)
gc()

#Table of trigrams
toks2 <- tokens_ngrams(toks, n = 3)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test3 <- as.data.frame(topfeatures(dfmN, x))
test3 <- separate(as.data.frame(rownames(test3)), col = 1, into = c("base1", "base2","pred"), sep = "_")
test3 <- unite(test3, "base", c("base1", "base2"), sep = " ")

rm(toks2)
rm(dfmN)
gc()

#Table of quadgrams
toks2 <- tokens_ngrams(toks, n = 4)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test4 <- as.data.frame(topfeatures(dfmN, x))
test4 <- separate(as.data.frame(rownames(test4)), col = 1, into = c("base1", "base2", "base3","pred"), sep = "_")
test4 <- unite(test4, "base", c("base1", "base2", "base3"), sep = " ")
timestamp()

rm(toks2)
rm(dfmN)
gc()

#Table of quintgrams
toks2 <- tokens_ngrams(toks, n = 5)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test5 <- as.data.frame(topfeatures(dfmN, x))
test5 <- separate(as.data.frame(rownames(test5)), col = 1, into = c("base1", "base2", "base3", "base4","pred"), sep = "_")
test5 <- unite(test5, "base", c("base1", "base2", "base3", "base4"), sep = " ")

rm(toks2)
rm(dfmN)
gc()

test <- rbind(test2, test3, test4, test5)
test <- test[!duplicated(test$base),]

write.csv(test, file = "Final Table x Stop.csv", row.names = FALSE)

#Finding Predicted word after removing stops
testXstop <- read.csv("Final Table x Stop.csv")

wordIn <- gsub("\\s+", " ", trimws(removeWords("united states of ", stopwords())))
as.character(test[test[,1]==wordIn, ][1,2])

#


testWstop <- read.csv("Final Table.csv") #762294
testWstop1 <- testWstop[1:(length(testWstop[,1])/4),]
testWstop2 <- testWstop[(length(testWstop[,1])/4+1):(length(testWstop[,1])/4*2),]
testWstop3 <- testWstop[((length(testWstop[,1])/4*2+1)+1):((length(testWstop[,1])/4*2)+(length(testWstop[,1])/4)),]
testWstop4 <- testWstop[((length(testWstop[,1])/4*2)+(length(testWstop[,1])/4)+1):length(testWstop[,1]),]

write.csv(testWstop1, "table1.csv")
write.csv(testWstop2, "table2.csv")
write.csv(testWstop3, "table3.csv")
write.csv(testWstop4, "table4.csv")



















test <- NULL
for (i in 1:x) {
        gram <- stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN, 10000)))[i]))
        nLetter <- nchar(stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN, 10000)))[i])))
        spaceLoc <- regexpr("_",stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN, 10000)))[i])))[1]
        
        primary <- substr(gram, 1, nLetter - spaceLoc-1)
        pred <- substr(gram, spaceLoc+1, nLetter)
        testadd <- cbind(primary, pred)
        test <- rbind(test, testadd)
}

write.csv(test, file = "Common Word Table 1.csv")

#Table of trigrams
toks2 <- tokens_ngrams(toks, n = 3)
dfmN <- dfm(toks2)
x <- length(featnames(dfmN))
test <- NULL
for (i in 1:x) {
        gram <- stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN)))[i]))
        nLetter <- nchar(stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN)))[i])))
        spaceLoc <- gregexpr("_",stri_extract_last_words(as.character(
                rownames(as.data.frame(topfeatures(dfmN)))[i])))[[1]][2]
        
        primary <- sub("_"," ",substr(gram, 1, spaceLoc-1))
        pred <- substr(gram, spaceLoc+1, nLetter)
        testadd <- cbind(primary, pred)
        test <- rbind(test, testadd)
}

write.csv(test, file = "Common Word Table 2.csv")


#Table of quadgrams
toks2 <- tokens_ngrams(toks, n = 4)
dfmN <- dfm(toks2)
x <- length(featnames(dfmN))
test <- NULL
for (i in 1:x) {
        gram <- stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN)))[i]))
        nLetter <- nchar(stri_extract_last_words(as.character(rownames(as.data.frame(topfeatures(dfmN)))[i])))
        spaceLoc <- gregexpr("_",stri_extract_last_words(as.character(
                rownames(as.data.frame(topfeatures(dfmN)))[i])))[[1]][3]
        
        primary <- sub("_"," ",substr(gram, 1, spaceLoc-1))
        pred <- substr(gram, spaceLoc+1, nLetter)
        testadd <- cbind(primary, pred)
        test <- rbind(test, testadd)
}

write.csv(test, file = "Common Word Table 3.csv")


#########














dfmN <- dfm_trim(dfmN, min_count = 4)
x <- length(featnames(dfmN))
dict <- names(topfeatures(dfmN, x))
test <- NULL
for (i in 1:x) {
        neg_bigram <- tokens_compound(toks, phrase(paste(gsub("_"," ",dict[i]),"*", sep = " ")))
        neg_bigram <- tokens_select(neg_bigram, phrase(paste(dict[i],"*", sep = "_")))
        toks_dfm <- dfm(neg_bigram)
        top <- as.data.frame(topfeatures(toks_dfm, 1))
        test <- rbind(test, top)
}

write.csv(test, file = "Common Word Table 2.csv")

gc()

toks2 <- tokens_ngrams(toks, n = 3)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
dict <- names(topfeatures(dfmN, x))
test <- NULL
for (i in 1:x) {
        neg_bigram <- tokens_compound(toks, phrase(paste(gsub("_"," ",dict[i]),"*", sep = " ")))
        neg_bigram <- tokens_select(neg_bigram, phrase(paste(dict[i],"*", sep = "_")))
        toks_dfm <- dfm(neg_bigram)
        top <- as.data.frame(topfeatures(toks_dfm, 1))
        test <- rbind(test, top)
}

write.csv(test, file = "Common Word Table 3.csv")

gc()

toks2 <- tokens_ngrams(toks, n = 4)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
dict <- names(topfeatures(dfmN, x))
test <- NULL
for (i in 1:x) {
        neg_bigram <- tokens_compound(toks, phrase(paste(gsub("_"," ",dict[i]),"*", sep = " ")))
        neg_bigram <- tokens_select(neg_bigram, phrase(paste(dict[i],"*", sep = "_")))
        toks_dfm <- dfm(neg_bigram)
        top <- as.data.frame(topfeatures(toks_dfm, 1))
        test <- rbind(test, top)
}

write.csv(test, file = "Common Word Table 4.csv")

gc()

toks2 <- tokens_ngrams(toks, n = 5)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
dict <- names(topfeatures(dfmN, x))
test <- NULL
for (i in 1:x) {
        neg_bigram <- tokens_compound(toks, phrase(paste(gsub("_"," ",dict[i]),"*", sep = " ")))
        neg_bigram <- tokens_select(neg_bigram, phrase(paste(dict[i],"*", sep = "_")))
        toks_dfm <- dfm(neg_bigram)
        top <- as.data.frame(topfeatures(toks_dfm, 1))
        test <- rbind(test, top)
}

write.csv(test, file = "Common Word Table 5.csv")

gc()

test1 <- read.csv("Common Word Table 1.csv", row.names = 1)
test2 <- read.csv("Common Word Table 2.csv", row.names = 1)
test3 <- read.csv("Common Word Table 3.csv", row.names = 1)
test4 <- read.csv("Common Word Table 4.csv", row.names = 1)
test5 <- read.csv("Common Word Table 5.csv", row.names = 1)

#group tests into one df
test <- rbind(test1, test2, test3, test4, test5)

#Finding the predicted word in "Common" Table
wordIn <- "the"
wordIn <- paste(wordIn,"*", sep = " ")
wordIn2 <- paste(gsub(" ", "_", wordIn), sep="")
neg_bigram <- tokens_compound(tokens(rownames(test)), phrase(wordIn))
neg_bigram <- tokens_select(neg_bigram, phrase(wordIn2))
toks_dfmT <- dfm(neg_bigram)
check <- topfeatures(toks_dfmT)[1]
####

ifelse(!is.na(check), check, "Next Step")




#Writing & Reading in the common table
write.csv(test, file = "Common Word Table.csv")
commontab <- read.csv("Common Word Table.csv", row.names = 1)


print(substr(names(check),
             gregexpr("_",names(check))[[1]][stri_count_regex(names(check), "_")] + 1, 
             nchar(names(check))))



