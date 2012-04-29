'''
chengjun wang
'''

pr<-read.table("D:/Chengjun/tencent weibo KDD CUP/track1/track1/user_profile.txt",
           header=F, sep="\t" )

dim(pr)  # 2320895 people

names(pr)<-c("id", "year", "gender", "tweets", "tag")

table(pr$gender)
# Gender of 0, 1, or 2, which represents
# ¡°unknown¡±, ¡°male¡±, ¡°female¡±.
#      0       1       2        3 
#  24580 1159797 1136390      128 

age<-as.data.frame(table(pr$year))
age1<-age[c(27:length(age[,1])),]
age1[,1]<-as.numeric(levels(age1[,1])[age1[,1]])
age2<-subset(age1, age1[,1]>1900&age1[,1]<2012)
plot(age2)
subset(pr$tweets, pr$tweets<=10000)
hist(subset(pr$tweets, pr$tweets<=10000), breaks=100)

pr$tag_num<-as.numeric(lapply(strsplit(as.character(pr$tag), ";", fixed = TRUE), length))
