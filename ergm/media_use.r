# Globalization
# chengjun @ office, 20120218

# load data
re<-read.csv("D:/Micro-blog/Literature of micro-blog/Dropbox/globalization/data/re_.csv", header=T, sep=',', na.strings = "NA", colClasses = NA)
res<-subset(re, re$media!="NA") # keep only the non-missing values of re
write.csv(res, "D:/Micro-blog/Literature of micro-blog/Dropbox/globalization/data/res.csv")
length(na.omit(re$media))  # only60621 rows, 67176 missing rows.

city<-subset(re, re$City<=4) # subset the first 4 cities.
city_nonmissing<-subset(city, city$media!='NA')  # the same missing pattern: half values are missing.
# the media use frequency of each media
media_index<-as.data.frame(table(res$Index1))
# the matrix of media use
temp<-combn(as.character(media_index[,1]), 2)
temp[,1]  # there is 820 temp[,n]
comb<-as.data.frame(t(temp))
# function of finding edge
find_edge<-function(n){
      sub1<-subset(res$ID, res$Index1==as.character(comb[n,1]))
      sub2<-subset(res$ID, res$Index1==as.character(comb[n,2]))
      Edge<-length(intersect(sub1, sub2))
      return(Edge)
      }
edge<-sapply(c(1:820), find_edge)
comb$edge<-edge

write.csv(comb, "D:/Micro-blog/Literature of micro-blog/Dropbox/globalization/data/edgelist.csv")
write.csv(media_index, "D:/Micro-blog/Literature of micro-blog/Dropbox/globalization/data/nodesize.csv")


