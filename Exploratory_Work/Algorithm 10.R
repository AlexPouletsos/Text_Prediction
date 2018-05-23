
library(RCurl)
test <- read.csv(text=getURL("https://raw.githubusercontent.com/AlexPouletsos/Text_Prediction/master/Final_Prediction_Table.csv"), header=T)

library(stringi)
library(dplyr)
library(tm)

#test <- read.csv("Final_Prediction_Table.csv")

wordIn5 <- stri_c(tail(unlist(strsplit(tolower("is there any"), split = "\\s+")), 5), collapse = " ")
wordIn4 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 4), collapse = " ")
wordIn3 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 3), collapse = " ")
wordIn2 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 2), collapse = " ")
wordIn1 <- stri_c(tail(unlist(strsplit(tolower(wordIn5), split = "\\s+")), 1), collapse = " ")

#Finding Predicted word
wordPred5 <- test[test[,1]==wordIn5,][1:3,]
wordPred4 <- test[test[,1]==wordIn4,][1:3,]
wordPred3 <- test[test[,1]==wordIn3,][1:3,]
wordPred2 <- test[test[,1]==wordIn2,][1:3,]
wordPred1 <- test[test[,1]==wordIn1,][1:3,]
#wordTrim <- gsub("\\s+", " ", trimws(removeWords(wordIn5, stopwords())))
#wordStop <- as.character(testXstop[testXstop[,1]==wordTrim, ][1,2])
y <- NULL

for (i in 1:3) {
        
x <- if_else(!is.na(wordPred5[i,2]), as.character(wordPred5[i,2]),
        if_else(!is.na(wordPred4[i,2]), as.character(wordPred4[i,2]),
                if_else(!is.na(wordPred3[i,2]), as.character(wordPred3[i,2]),
                        if_else(!is.na(wordPred2[i,2]), as.character(wordPred2[i,2]),
                                if_else(!is.na(wordPred1[i,2]), as.character(wordPred1[i,2]),
                                        if_else(i==1, "of", if_else(i==2, "is", if_else(i==3, "the",""))))))))
y <- rbind(y,x)

}

y

#if_else(!is.na(wordStop), wordStop, "default the"))))))

