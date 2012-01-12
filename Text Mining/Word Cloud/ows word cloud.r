library(wordcloud)
library(tm)
require(RColorBrewer)
Tweets<-read.csv("D:/Micro-blog/Literature of micro-blog/Dropbox/tweets/data_delete after you download/OccupyWallstreetInterData.csv", 
      header = T,dec = ".")
d924<-subset(Tweets, Tweets$Day=="2011-09-24")
# Generate corps
d<-list(as.character(Tweets$Text))
corp<-Corpus(VectorSource(d))
# Clean the corpus
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, function(x)removeWords(x,stopwords()))
# Compute the corpus
ap.tdm <- TermDocumentMatrix(corp)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
freq<-as.data.frame(table(ap.d$freq))
write.csv(ap.d, "D:/Micro-blog/Literature of micro-blog/Dropbox/tweets/data_delete after you download/OccupyWallstreetInterDataWordcloud.csv")
plot(log(as.numeric(levels(freq[,1])[freq[,1]])),log(freq[,2]), xlab="Rank", ylab="Freq")
# Plot the word cloud
pal2 <- brewer.pal(8,"Dark2")
png("D:/github/Data-Mining-With-R/Text Mining/Word Cloud/wordcloud_ows_all_2.png", width=800,height=800)
wordcloud(ap.d$word,log(ap.d$freq+1), scale=c(8,.3),min.freq=0,
max.words=500, random.order=F, rot.per=.15,  colors=pal2)
dev.off()

comparison.cloud(term.matrix,max.words=300,random.order=FALSE)
commonality.cloud(term.matrix,random.order=FALSE)
attach(packages:wordcloud)

ap.d<-read.csv("D:/Micro-blog/Literature of micro-blog/Dropbox/tweets/data_delete after you download/OccupyWallstreetInterDataWordcloud.csv", 
    header = T,dec = ".")



install.packages("wordcloud")