# Youtube dataset
# http://www.vod.dcc.ufmg.br/traces/youtime/data/
# chengjun wang
# 20120317


#~~~read and clean data~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
top<-read.csv("D:/Micro-blog/Youtube/TubeOverTime/top/dao/dao.csv", header=T, sep=",", stringsAsFactors=F)
# clean data
top<-subset(top, top$EVENT_TYPE!="[]")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# dim(top) 17127 18
data.frame(names(top))
# ID: The id of the video. This is a basic Youtube id with 11 characters, for example: abc_12g4567
# DAYS: The number of days of information available for the video.
# UPLOAD_DATE: The date the video was uploaded (in seconds since Jan 1 1970)
# FIRST_DATE: The initial date where any information was available, this will always be UPLOAD_DATE - 1
# LAST_DATE: The final date where any information was available, this can be also be viewed as the date which we collected the video
# TOTAL_VIEW: Total amount of views
# TOTAL_COMM: Total amount of comments
# TOTAL_FAVS: Total amount of favorites
# TOTAL_RATS: Total amount of ratings
# TOTAL_AVGR: Average rating of the video (we collected videos befor Youtube is like/dislike, when ratings were starts)
# VIEW_DATA: An array of the cumulative number of views, this will begin with 0 and end in TOTAL_VIEW
# COMM_DATA: An array of the cumulative number of comments, this will begin with 0 and end in TOTAL_COMM
# FAVS_DATA: An array of the cumulative number of favorites, this will begin with 0 and end in TOTAL_FAVS
# RATS_DATA: An array of the cumulative number of ratings, this will begin with 0 and end in TOTAL_RATS
# AVGR_DATA: An array of the average rating per point, this will begin with 0 and end in TOTAL_AVGR
# EVENT_TYPES: An array of the different referrers types which lead users to videos. For example, First referral from Google search
# EVENT_DATES: An array of the dates of each of the referrers above
# EVENT_VIEWS: An array of the amount of views of each of the referrers above 
head(top$X.ID)
head(top$DAYS)
head(top$UPLOAD_DATE)
head(top$FIRST_DATE)

head(top$TOTAL_VIEW)
plot(log(top$TOTAL_VIEW), log(top$TOTAL_COMM))
plot(log(top$TOTAL_VIEW), log(top$TOTAL_FAVS))
plot(log(top$TOTAL_VIEW), log(top$TOTAL_RATS))
plot(top[,6:10])
cov(top[,6:10])

head(top$VIEW_DATA)
head(top$COMM_DATA)

head(top$EVENT_TYPES)
head(top$EVENT_DATES)
head(top$EVENT_VIEWS)
class(top$EVENT_VIEWS)
#~~~~~~~~~Information Chanell: Referer analysis~~~~~~~~~~~~~~~~~~~~~~~~#
head(top$EVENT_TYPES)
# EXTERNAL
# First embedded view
# First embedded on 
# First referrer from
# FEATURED
# First view from ad
# First featured video view
# INTERNAL
# First referrer from YouTube
# First referrer from Related Video
# MOBILE 
# First view from a mobile device 
# SEARCH
# First referrer from Google
# First referrer from YouTube search 
# First referrer from Google Video
# SOCIAL
# First referrer from a subscriber
# First view on a channel page
# VIRAL 
# Other / Viral

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
getUniqueType<-function(n){
       # clean the referer type
       nt=nchar(top$EVENT_TYPES[n])
       type=substring(top$EVENT_TYPES[n], 2, nt-1)
       type=paste(unlist(strsplit(type,split=" ")), collapse="")
       type=unlist(strsplit(type, ","))
       return(type)
       }

unique(unlist(as.list(sapply(c(1:length(top[,1])),getUniqueType))))

 [1] "'Firstfeaturedvideoview'"             "'Firstviewonachannelpage'"            "'FirstreferralfromYouTube'"          
 [4] "'Firstreferralfromasubscribermodule'" "'Firstembeddedon'"                    "'Firstviewfromamobiledevice'"        
 [7] "'Other/Viral'"                        "'Firstreferralfrom'"                  "'Firstreferralfromrelatedvideo'"     
[10] "'FirstreferralfromYouTubesearch'"     "'Firstembeddedview'"                  "'FirstreferralfromGooglesearch'"     
[13] "'Firstviewfromad'"                    "'FirstreferralfromGoogleVideosearch'"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
getReferer<-function(n){
       # clean the referer type
       nt=nchar(top$EVENT_TYPES[n])
       type=substring(top$EVENT_TYPES[n], 2, nt-1)
       type=paste(unlist(strsplit(type,split=" ")), collapse="")
       type=unlist(strsplit(type, ","))
       # clean the view count
       nv=nchar(top$EVENT_VIEWS[n])
       view=substring(top$EVENT_VIEWS[n], 2, nv-1)
       view=as.numeric(unlist(strsplit(view, ",")))
       # aggregate the duplicate rows
       data=data.frame(type, view)
       data<-aggregate(view, list(type),  sum)
       names(data)<-c('type','view')
       rownames(data)<- data$type
       # merge by rownames
       type<-c("'Firstfeaturedvideoview'" ,  "'Firstviewonachannelpage'" ,"'FirstreferralfromYouTube'" ,         
               "'Firstreferralfromasubscribermodule'","'Firstembeddedon'" , "'Firstviewfromamobiledevice'" ,       
               "'Other/Viral'"  , "'Firstreferralfrom'"  , "'Firstreferralfromrelatedvideo'" ,   
               "'FirstreferralfromYouTubesearch'", "'Firstembeddedview'" , "'FirstreferralfromGooglesearch'",     
               "'Firstviewfromad'" , "'FirstreferralfromGoogleVideosearch'")
       view<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
       dat=data.frame(type,view)
       rownames(dat)<-dat$type     
       fast <- merge(dat, data,  all=T)  
       # aggregate to get the target    
       target<-aggregate(fast$view, list(fast$type),  sum)
       names(target)<-c('type','view')
       return(target)
       }
lapply(c(1), getReferer)
referer<-data.frame(sapply(c(1:length(top[,1])), getReferer))
referer<-as.data.frame(t(as.matrix(referer)))
rownames(referer)<-NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
unList<-function(n){
        l<-unlist(referer$view[n])
        return(l)
        }
test<-sapply(c(1:length(referer$view)), unList)
res<-as.data.frame(t(as.matrix(test)))
top<-data.frame(top, res)
# head(top[,19:32])
write.csv(top, "D:/Micro-blog/Youtube/TubeOverTime/top/dao/top.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# plot
pie(data$view,labels = data$type)
plot(top[,19:32])
cov(top[,19:32])

view<-top$TOTAL_VIEW/top$DAYS
plot(log(view), log(top[,19]/top$DAYS))
plot(view, top[,20]/top$DAYS)
plot(view, top[,21]/top$DAYS)
plot(view, top[,22]/top$DAYS)
plot(view, top[,23]/top$DAYS)


plot(log(top[,19]/top$DAYS),log(view),xlab="First embedded on")
layout(matrix(1:1,1,1))
plot(log(top[,23]/top$DAYS),log(view),xlab="First referal from a subscriber module")

#~~~~~~~~~~~~~~~~~~~~~~regression~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
reg<-lm(view/top$DAYS~feo+fev+ffvv+frf+frfsm+frfgs+frfgvs+frflv+frfy+frfys+fvfa+fvfmd+fvocp+ov)
summary(reg)
# calcuate the standard regression coefficient
b.x<-coef(reg)[2]
sd.y<-sd(view/top$DAYS)
sd.x<-sd(feo)
beta.x<-b.x*sd.x/sd.y

# model diagnosis
layout(matrix(1:4,2,2))
plot(reg)

feo<-top[,19]/top$DAYS
fev<-top[,20]/top$DAYS
ffvv<-top[,21]/top$DAYS
frf<-top[,22]/top$DAYS
frfsm<-top[,23]/top$DAYS
frfgs<-top[,24]/top$DAYS
frfgvs<-top[,25]/top$DAYS
frflv<-top[,26]/top$DAYS
frfy<-top[,27]/top$DAYS
frfys<-top[,28]/top$DAYS
fvfa<-top[,29]/top$DAYS
fvfmd<-top[,30]/top$DAYS
fvocp<-top[,31]/top$DAYS
ov<-top[,32]/top$DAYS

# output standard coefficient
library(QuantPsyc)# install.packages("QuantPsyc")
as.data.frame(lm.beta(reg))
# as.data.frame((as.data.frame(lapply(c(1), getReferer)))[,1])
# 1                               'Firstembeddedon'
# 2                             'Firstembeddedview'
# 3                        'Firstfeaturedvideoview'
# 4                             'Firstreferralfrom'
# 5            'Firstreferralfromasubscribermodule'
# 6                 'FirstreferralfromGooglesearch'
# 7            'FirstreferralfromGoogleVideosearch'
# 8                 'Firstreferralfromrelatedvideo'
# 9                      'FirstreferralfromYouTube'
# 10               'FirstreferralfromYouTubesearch'
# 11                              'Firstviewfromad'
# 12                   'Firstviewfromamobiledevice'
# 13                      'Firstviewonachannelpage'
# 14                                  'Other/Viral'

value<-mean(top[,19:32])
category<-c('Firstembeddedon' , 'Firstembeddedview', 'Firstfeaturedvideoview', 'Firstreferralfrom',
  'Firstreferralfromasubscribermodule','FirstreferralfromGooglesearch','FirstreferralfromGoogleVideosearch',
  'Firstreferralfromrelatedvideo','FirstreferralfromYouTube','FirstreferralfromYouTubesearch',
  'Firstviewfromad','Firstviewfromamobiledevice','Firstviewonachannelpage','Other/Viral')
forPie<-data.frame(category, value)
pie(forPie$value,labels = forPie$category)

