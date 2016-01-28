proc_col <- tolower(unskewed_data$title)
#proc_col <- gsub("rt", " ", proc_col)
proc_col <- gsub("@\\w+", "", proc_col)
proc_col <- gsub("[[:punct:]]", " ", proc_col)
proc_col<-gsub("  "," ",proc_col)
proc_col<-gsub("  "," ",proc_col)

proc_col <- gsub("http\\w+", " ", proc_col)
proc_col <- gsub("[ |\t]{2,}", " ", proc_col)
proc_col <- gsub("^ ", "", proc_col) 
#install.packages("tm")
library("tm")
proc_col.corpus <- Corpus(VectorSource(proc_col))
proc_col.corpus = tm_map(proc_col.corpus, removeNumbers)
proc_col.corpus = tm_map(proc_col.corpus, removeWords, stopwords("english"))
proc_col.corpus <- tm_map(proc_col.corpus, PlainTextDocument)
myDTM=DocumentTermMatrix(proc_col.corpus)
library(slam)
colTotals <-  col_sums(myDTM)
dtm2 <- myDTM[,which(colTotals >1)]

dim(dtm2)
colnames(dtm2)

#proc_col.corpus <- tm_map(proc_col.corpus, function(x)removeWords(x,stopwords()))
#dtm_gsub<-DocumentTermMatrix(proc_col.corpus)

#library(slam)
#colTotals <-  col_sums(dtm_gsub)
#colTotals
#dtm2 <- dtm_gsub[,which(colTotals >1)]

#dim(dtm2)
#write.csv()




#dtMatrix <- create_myhist_matrix(proc_col,removeStopwords=TRUE,language="english")