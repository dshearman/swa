library(tm)
data(crude)
tdm <- TermDocumentMatrix(crude, control = list(removePunctuation = TRUE,
                                                stopwords = stopwords(),
                                                removeNumbers = TRUE, tolower = TRUE))
m <- as.matrix(tdm)
freq <- sort(rowSums(m),decreasing=TRUE)
words = names(freq) 

library(animation)
source("mywordle.R")
mywordle(words,freq, pause=TRUE)