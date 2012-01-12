# User's Comments on NYT
# RNYTimes package to access user-generate content on the NY Times
# and produce a wordcloud of 'today' comments on articles.
# Caveat: in order to use the RNYTimes package you need a API key 
# Refer to: http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html

require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
install.packages(packageName, repos = "http://www.omegahat.org/R", type = "source")
require(RNYTimes)
my.key <- "your API key here"
what= paste("by-date", format(Sys.time(), "%Y-%m-%d"),sep="/")
# what="recent"
recent.news <- community(what=what, key=my.key)
pagetree <- htmlTreeParse(recent.news, error=function(...){}, useInternalNodes = TRUE)
x <- xpathSApply(pagetree, "//*/body", xmlValue)
# do some clean up with regular expressions
x <- unlist(strsplit(x, "\n"))
x <- gsub("\t","",x)
x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
x <- x[!(x %in% c("", "|"))]
ap.corpus <- Corpus(DataframeSource(data.frame(as.character(x))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_NewYorkTimes_Community.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=2,
max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()