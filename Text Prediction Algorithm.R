library(RCurl)
library(stringi)
library(dplyr)

#Read in Prediction Table
test <- read.csv(text=getURL("https://raw.githubusercontent.com/AlexPouletsos/Text_Prediction/master/Final_Prediction_Table.csv"), 
                              header=T)

wordIn <- "i would like a cup of"

        wordIn4 <- stri_c(tail(unlist(strsplit(tolower(wordIn), split = "\\s+")), 4), collapse = " ")
        wordIn3 <- stri_c(tail(unlist(strsplit(tolower(wordIn4), split = "\\s+")), 3), collapse = " ")
        wordIn2 <- stri_c(tail(unlist(strsplit(tolower(wordIn4), split = "\\s+")), 2), collapse = " ")
        wordIn1 <- stri_c(tail(unlist(strsplit(tolower(wordIn4), split = "\\s+")), 1), collapse = " ")
        
        #Finding Predicted words
        wordPred4 <- test[test[,1]==wordIn4,][1:3,]
        wordPred3 <- test[test[,1]==wordIn3,][1:3,]
        wordPred2 <- test[test[,1]==wordIn2,][1:3,]
        wordPred1 <- test[test[,1]==wordIn1,][1:3,]
        
        #First
        x <- if_else(!is.na(wordPred5[1,2]), as.character(wordPred5[1,2]),
                     if_else(!is.na(wordPred4[1,2]), as.character(wordPred4[1,2]),
                             if_else(!is.na(wordPred3[1,2]), as.character(wordPred3[1,2]),
                                     if_else(!is.na(wordPred2[1,2]), as.character(wordPred2[1,2]),
                                             if_else(!is.na(wordPred1[1,2]), as.character(wordPred1[1,2]),"and")))))
        
        
        #Second
        y <- if_else(!is.na(wordPred5[2,2]) 
                     && as.character(wordPred5[2,2])!=x, 
                     as.character(wordPred5[2,2]),
                     if_else(!is.na(wordPred4[2,2]) 
                             && as.character(wordPred4[2,2])!=x, 
                             as.character(wordPred4[2,2]),
                             if_else(!is.na(wordPred3[2,2]) 
                                     && as.character(wordPred3[2,2])!=x, 
                                     as.character(wordPred3[2,2]),
                                     if_else(!is.na(wordPred2[2,2]) 
                                             && as.character(wordPred2[2,2])!=x,
                                             as.character(wordPred2[2,2]),
                                             if_else(!is.na(wordPred1[2,2]) 
                                                     && as.character(wordPred1[2,2])!=x, 
                                                     as.character(wordPred1[2,2]),"to")))))
        
        #Third
        z <- if_else(!is.na(wordPred5[3,2]) 
                     && as.character(wordPred5[3,2])!=x 
                     && as.character(wordPred5[3,2])!=y, 
                     as.character(wordPred5[3,2]),
                     if_else(!is.na(wordPred4[3,2]) 
                             && as.character(wordPred4[3,2])!=x 
                             && as.character(wordPred4[3,2])!=y, 
                             as.character(wordPred4[3,2]),
                             if_else(!is.na(wordPred3[3,2])
                                     && as.character(wordPred3[3,2])!=x
                                     && as.character(wordPred3[3,2])!=y,
                                     as.character(wordPred3[3,2]),
                                     if_else(!is.na(wordPred2[3,2])
                                             && as.character(wordPred2[3,2])!=x 
                                             && as.character(wordPred2[3,2])!=y,
                                             as.character(wordPred2[3,2]),
                                             if_else(!is.na(wordPred1[3,2])
                                                     && as.character(wordPred1[3,2])!=x
                                                     && as.character(wordPred1[3,2])!=y,
                                                     as.character(wordPred1[3,2]),"the")))))
        
        x
        y
        z
        