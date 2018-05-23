#############################
#Building a Prediction Table#
#############################

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

#Reduce data to 10% sample because of computer memory limitations
set.seed(2187)
blogSamp <- sample(blog, size=length(blog)*0.1)
newsSamp <- sample(news, size=length(news)*0.1)
twitSamp <- sample(twit, size=length(twit)*0.1)

#Clean up text
all_text <- combine(blogSamp, newsSamp, twitSamp)
all_text <- iconv(all_text, "latin1", "ASCII", sub="")
dcorp <- corpus(all_text)
toks <- tokens(dcorp, remove_punct=TRUE, remove_numbers=TRUE, remove_symbols=TRUE, remove_twitter=TRUE)
toks <- tokens_tolower(toks)

#Remove unnecessary variables to free up memory for ngram generation
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

        #Shrink table to top 3 predictions
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

        #Shrink table to top 3 predictions
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

        #Shrink table to top 3 predictions
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

        #Shrink table to top 3 predictions
test5 <- test5 %>%
        arrange(desc(Qty)) %>%
        group_by(base) %>%
        slice(1:3) %>%
        as.data.frame()

write.csv(test5, "PTable5.csv", row.names = FALSE)
rm(test5)

#Combine all ngram tables together
PTable2 <- read.csv("PTable2.csv")
PTable3 <- read.csv("PTable3.csv")
PTable4 <- read.csv("PTable4.csv")
PTable5 <- read.csv("PTable5.csv")
PTable <- rbind(PTable2, PTable3, PTable4, PTable5)

write.csv(PTable, file = "Final_Prediction_Table.csv", row.names = FALSE)
