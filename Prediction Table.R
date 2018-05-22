#Building Prediction Table

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

#table of bigrams
toks2 <- tokens_ngrams(toks, n = 2)
dfmN <- dfm(toks2)
x <- length(featnames(dfmN))
test2 <- as.data.frame(topfeatures(dfmN, x))
names(test2) <- "Qty"
test <- separate(as.data.frame(rownames(test2)), col = 1, into = c("base","pred"), sep = "_")
test2 <- cbind(test, test2)

rm(toks2)
rm(dfmN)
rm(test)

#Shrinking table to top 3 predictions
test2 <- test2 %>%
        arrange(desc(Qty)) %>%
        group_by(base) %>%
        slice(1:3) %>%
        as.data.frame()

write.csv(test2, "PTable2.csv", row.names = FALSE)
rm(test2)

#Table of trigrams
toks2 <- tokens_ngrams(toks, n = 3)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test3 <- as.data.frame(topfeatures(dfmN, x))
names(test3) <- "Qty"
test <- separate(as.data.frame(rownames(test3)), col = 1, into = c("base1", "base2","pred"), sep = "_")
test <- unite(test, "base", c("base1", "base2"), sep = " ")
test3 <- cbind(test, test3)

rm(toks2)
rm(dfmN)
rm(test)

test3 <- test3 %>%
        arrange(desc(Qty)) %>%
        group_by(base) %>%
        slice(1:3) %>%
        as.data.frame()

write.csv(test3, "PTable3.csv", row.names = FALSE)
rm(test3)

#Table of quadgrams
toks2 <- tokens_ngrams(toks, n = 4)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test4 <- as.data.frame(topfeatures(dfmN, x))
names(test4) <- "Qty"
test <- separate(as.data.frame(rownames(test4)), col = 1, into = c("base1", "base2", "base3","pred"), sep = "_")
test <- unite(test, "base", c("base1", "base2", "base3"), sep = " ")
test4 <- cbind(test, test4)

rm(toks2)
rm(dfmN)
rm(test)

test4 <- test4 %>%
        arrange(desc(Qty)) %>%
        group_by(base) %>%
        slice(1:3) %>%
        as.data.frame()

write.csv(test4, "PTable4.csv", row.names = FALSE)
rm(test4)

#Table of quintgrams
toks2 <- tokens_ngrams(toks, n = 5)
dfmN <- dfm(toks2)
dfmN <- dfm_trim(dfmN, min_count = 2)
x <- length(featnames(dfmN))
test5 <- as.data.frame(topfeatures(dfmN, x))
names(test5) <- "Qty"
test <- separate(as.data.frame(rownames(test5)), col = 1, into = c("base1", "base2", "base3", "base4","pred"), sep = "_")
test <- unite(test, "base", c("base1", "base2", "base3", "base4"), sep = " ")
test5 <- cbind(test, test5)

rm(toks2)
rm(dfmN)
rm(test)

test5 <- test5 %>%
        arrange(desc(Qty)) %>%
        group_by(base) %>%
        slice(1:3) %>%
        as.data.frame()

write.csv(test5, "PTable5.csv", row.names = FALSE)
rm(test5)

PTable2 <- read.csv("PTable2.csv")
PTable3 <- read.csv("PTable3.csv")
PTable4 <- read.csv("PTable4.csv")
PTable5 <- read.csv("PTable5.csv")

PTable <- rbind(PTable2, PTable3, PTable4, PTable5)

write.csv(PTable, file = "Final_Prediction_Table.csv", row.names = FALSE)



#Manageable file chunks to load on GitHub
PTable <- read.csv("Final_Prediction_Table.csv")

chunk1 <- PTable[1:(nrow(PTable)/3),]
chunk2 <- PTable[(nrow(PTable)/3+1):(nrow(PTable)/3*2),]
chunk3 <- PTable[(nrow(PTable)/3*2+1),nrow(PTable)]

write.csv(chunk1, file = "PTable Chunk1.csv", row.names = FALSE)
write.csv(chunk2, file = "PTable Chunk2.csv", row.names = FALSE)
write.csv(chunk3, file = "PTable Chunk3.csv", row.names = FALSE)








##############################################################################################
test <- read.csv("Final_Prediction_Table.csv")

#Finding Predicted word
wordIn <- "follow me and make me the"
wordIn5 <- stri_c(tail(unlist(strsplit(tolower(wordIn), split = "\\s+")), 5), collapse = " ")
wordIn4 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 4), collapse = " ")
wordIn3 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 3), collapse = " ")
wordIn2 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 2), collapse = " ")
wordIn1 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 1), collapse = " ")

wordPred5 <- test[test[,1]==wordIn5,][1:3,][,2]
wordPred4 <- test[test[,1]==wordIn4,][1:3,][,2]
wordPred3 <- test[test[,1]==wordIn3,][1:3,][,2]
wordPred2 <- test[test[,1]==wordIn2,][1:3,][,2]
wordPred1 <- test[test[,1]==wordIn1,][1:3,][,2]
#wordTrim <- gsub("\\s+", " ", trimws(removeWords(wordIn5, stopwords())))
#wordStop <- as.character(testXstop[testXstop[,1]==wordTrim, ][1,2])

if_else(!is.na(wordPred5), wordPred5,
        if_else(!is.na(wordPred4), wordPred4,
                if_else(!is.na(wordPred3), wordPred3,
                        if_else(!is.na(wordPred2), wordPred2,
                                if_else(!is.na(wordPred2), wordPred1, "default the")))))


#if_else(!is.na(wordStop), wordStop, "default the"))))))
##############################################################################################




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