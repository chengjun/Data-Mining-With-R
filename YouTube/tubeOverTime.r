# Youtube dataset
# http://www.vod.dcc.ufmg.br/traces/youtime/data/
# chengjun wang
# 20120317


#~~~read and clean data~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# top<-read.csv("D:/Micro-blog/Youtube/TubeOverTime/top/dao/dao.csv", header=T, sep=",", stringsAsFactors=F)
# clean data
# top<-subset(top, top$EVENT_TYPE!="[]")
#~~~~~crawling category\author\duration information using python~~~~~~~~~~~~~~~~#
# top<-read.csv("D:/Micro-blog/Youtube/TubeOverTime/top/dao/top.csv", header=T, sep=",", stringsAsFactors=F)
# write.csv(id<-top[,2], "C:/Python27/weibo/youtube/id.csv", quote = F, row.names = F)
# write.csv(id<-top[2470:2480,2], "C:/Python27/weibo/youtube/id_test.csv", quote = F, row.names = F)
# cat<-read.csv( "C:/Python27/weibo/youtube/saveCat.csv", sep = ",", quote = "|", header = FALSE)
# names(cat)<-c("X.ID", "author","category", "duration")
# dim(TopData)  11386 24% are set as private or delete
# topcat<-merge(top, cat, by="X.ID",all = TRUE, incomparables=NA );dim(topcat)
# write.csv(topcat, "D:/Micro-blog/Youtube/TubeOverTime/top/dao/topcat.csv")
#~~~~~~~~~~~~~~~~LOAD DATA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
top<-read.csv("D:/google drive/Google Drive/Youtube/Data/topCatPeak.csv", header = T, sep=',', stringsAsFactors=F)
# write.csv(top, "D:/Research/Youtube/TubeOverTime/top/dao/topCatPeak.csv")

top$feo<-top[,21]
top$fev<-top[,22]
top$ffvv<-top[,23]
top$frf<-top[,24]
top$frfsm<-top[,25]
top$frfgs<-top[,26]
top$frfgvs<-top[,27]
top$frflv<-top[,28]
top$frfy<-top[,29]
top$frfys<-top[,30]
top$fvfa<-top[,31]
top$fvfmd<-top[,32]
top$fvocp<-top[,33]
top$ov<-top[,34]
df<-head(top)
top<-top[ -c(21:34) ] #delete columns

#~~~~~~~~~~~~~~~~~~~~~~Introduction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#~~~~~~~~~~~~~~~~~~endogenous_subcritical~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
topp<-subset(top, top$peak<=0.05)
summary(topp$DAYS)
summary(topp$TOTAL_VIEW)
hist(log(topp$TOTAL_VIEW/topp$DAYS), main='', breaks=30, xlab= 'Average Views  (Log)')
hist(log(topp$TOTAL_VIEW/topp$DAYS), main='', breaks=30, xlab= 'Average Views  (Log)')

par(mfrow=c(1,1))
plot(log(topp$peak/topp$DAYS)~log(topp$DAYS), col='grey', ylab='Peak Fraction Normalized by Lifetime (Log)', xlab='Lifetime  (Log)')
plot((topp$peak/topp$DAYS)~(topp$DAYS), col='grey', ylab='Peak Fraction Normalized by Lifetime', xlab='Lifetime')

 top$cat<-top$peak
 top$cat[top$cat>0.8]='red'
 top$cat[top$cat<=0.8&top$cat>0.2]='green'
 top$cat[top$cat<=0.2&top$cat>0.05]='yellow'
 top$cat[top$cat<=0.05]='blue'
plot(log(top$peak/top$DAYS)~log(top$DAYS), col=top$cat, ylab='Peak Fraction Normalized by Lifetime (Log)', xlab='Lifetime  (Log)')
plot((top$peak/top$DAYS)~(top$DAYS), col=top$cat, ylab='Peak Fraction Normalized by Lifetime', xlab='Lifetime')
plot((top$peak)~(top$DAYS), col=top$cat, ylab='Peak Fraction', xlab='Lifetime')
plot(log(top$peak)~log(top$DAYS), col=top$cat, ylab='Peak Fraction  (Log)', xlab='Lifetime (Log)')

#
plot((top$peak)~(top$DAYS), col=log(top$TOTAL_VIEW)/2, ylab='Peak Fraction', xlab='Lifetime')

plot(log(top$TOTAL_VIEW)~log(top$DAYS), xlab='Lifetime (Log)', ylab='Total Views (Log)', col=top$cat, main='')

plot(log(top$TOTAL_VIEW)~log(top$DAYS), xlab='Lifetime (Log)', ylab='Total Views (Log)', col=top$peak*7, main='')

#
plot(log(top$peak)~log(top$TOTAL_VIEW), col=top$cat, ylab='Peak Fraction Normalized by Lifetime (Log)', xlab='Lifetime  (Log)')
plot(log(top$peak/top$DAYS)~log(top$TOTAL_VIEW), col=top$cat, ylab='Peak Fraction Normalized by Lifetime (Log)', xlab='Lifetime  (Log)')

#~~~~~~~~~~~Plot the distribution using log scale on both axes~~~~~~~~~~~~~~~~~~~~#
m <- hist(top$M, plot=F, breaks=1000)
plot(m$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Predicted Total Views", ylab="Probability") 
p <- hist(top$P, plot=F, breaks=1000)

hist(log(top$P),  breaks=1000)
hist((top$Q),  breaks=1000)

> dim(subset(top, top$DAYS<10))
[1] 3530   40
> dim(subset(top, top$DAYS<9))
[1] 3056   40
> dim(subset(top, top$DAYS<8))
[1] 2524   40
> dim(subset(top, top$DAYS<7))
[1] 2128   40



plot(p$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Innovation Parameter", ylab="Probability") 
q <- hist(top$Q, plot=F, breaks=1000)
plot(q$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Imitation Parameter", ylab="Probability") 


v <- hist(top$TOTAL_VIEW, plot=F, breaks=1000)
c <- hist(top$TOTAL_COMM, plot=F, breaks=1000)
f <- hist(top$TOTAL_FAVS, plot=F, breaks=1000)
r <- hist(top$TOTAL_RATS, plot=F, breaks=1000)
d <- hist(top$DAYS, plot=F, breaks=1000)
par(mfrow=c(2,3))
plot(v$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Total Views", ylab="Probability") 
plot(c$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Total Comments", ylab="Probability") 
plot(f$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Total Favorites", ylab="Probability") 
plot(r$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Total Rates", ylab="Probability") 
plot(d$density, log="xy", pch=20, col="blue", 
    main=" ", 
    xlab="Lifetime", ylab="Probability") 
par(mfrow=c(1,1))
hist(top$peak, freq = F, breaks=40, ylim = range(0,3.5),
   xlab='Peak Fraction', main = paste("Histogram of" , 'Peak Fraction'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
plot(top$TOTAL_VIEW, top$M)
layout(matrix(1:2,1,2))
plot((top$TOTAL_VIEW), (top$TOTAL_COMM))
plot(log(top$TOTAL_VIEW), log(top$TOTAL_COMM))
cor(top[,6:7], use="complete.obs", method="kendall") 

cor((top$TOTAL_VIEW), (top$TOTAL_COMM))
cor(log(top$TOTAL_VIEW+1), log(top$TOTAL_COMM+1))



viewLog<-log(top$TOTAL_VIEW)
commentLog<-log(top$TOTAL_COMM)
dvc<-data.frame(viewLog, commentLog)
cor(dvc, use="complete.obs", method="kendall") 

plot(log(top$TOTAL_VIEW), log(top$TOTAL_RATS))
plot(log(top$TOTAL_VIEW), log(top$TOTAL_FAVS))

plot(top[,6:10])
cor(top[,6:10], use="complete.obs", method="kendall") 

library(corrgram)
corrgram(top[,6:10], order=TRUE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main=" ")

cov(top[,6:10])

head(top$VIEW_DATA)
head(top$COMM_DATA)

head(top$EVENT_TYPES)
head(top$EVENT_DATES)
head(top$EVENT_VIEWS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~get the growth curve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
topp<-subset(top, top$DAYS>=100) #  3549

findGrowth<-function(n){
       time <- 1:top$DAYS[n]
       # Tdelt <- (1:100) / 10
       nt=nchar(top$VIEW_DATA[n])
       growth=substring(top$VIEW_DATA[n], 2, nt-1)
       growth=paste(unlist(strsplit(growth,split=" ")), collapse="")
       cumGrowth=as.numeric(unlist(strsplit(growth, ",")))     
       # difGrowth <- diff(cumGrowth, lag = 1, differences = 1)
	 id=rep(top$X.ID[n], top$DAYS[n])
       growthLine<-data.frame(cbind(time, cumGrowth, as.character(id)))
       return(growthLine)}

gcc <- do.call(rbind, lapply(c(1:5),findGrowth))

as.data.frame(do.call(rbind, your_list))


gc <- do.call(rbind, lapply(c(1:length(top$X.ID)),findGrowth))
gc$cumGrowth<-as.numeric(levels(gc$cumGrowth)[gc$cumGrowth])
gc$time<-as.numeric(levels(gc$time)[gc$time])
write.csv(gc, "D:/Research/Youtube/TubeOverTime/top/dao/growthCurve.csv", row.names=F)

plot(as.numeric(levels(gc$cumGrowth)[gc$cumGrowth]), as.numeric(levels(gc$time)[gc$time]))
re<-lm(as.numeric(levels(gc$cumGrowth)[gc$cumGrowth])~as.numeric(levels(gc$time)[gc$time]))
summary(re)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot cdf~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
topf<-subset(top, top$M!=0)

topz<-subset(top, top$M==0)
zero<-data.frame(topz$DAYS, topz$M)

# the largest growth http://www.youtube.com/watch?v=_OBlgSz8sSM&feature=youtu.be
time <- if( top$DAYS[392] <= 100) 1:top$DAYS[392] else seq(1, top$DAYS[392],by= (top$DAYS[392]-1)/(100-1))
t=time/top$DAY[392]
nt=nchar(top$VIEW_DATA[392])
growth=substring(top$VIEW_DATA[392], 2, nt-1)
growth=paste(unlist(strsplit(growth,split=" ")), collapse="")

cumGrowth=as.numeric(unlist(strsplit(growth, ","))) 
cf=cumGrowth/max(cumGrowth)

difGrowth <- c(0, diff(cumGrowth, lag = 1, differences = 1)) 
df=difGrowth/max(cumGrowth)

peak=max(difGrowth)/max(cumGrowth)
dat = data.frame(t, difGrowth)
pt<-subset(dat$t, dat$difGrowth==max(difGrowth))


plot(pt,peak,col="blue",pch=20,xlab="time",ylab="normalized peak views", type='b', xlim=c(0, 1), ylim=c(0, 1)) 
# plot(t,cf,col="blue",pch=20,xlab="cumulative time",ylab="Growth Curve") 
# plot(t,df,col="red",pch=20,xlab="cumulative time",ylab="PDF", xlim=c(0, 1), ylim=c(0, 1)) 
# text(0.5, 1, 'Days < 10')

# plot(0 , 0,col="blue",pch=20,xlab="time",ylab="normalized peak views", type='b', xlim=c(0, 1), ylim=c(0, 0.2)) 
# text(0.5, 0.1, 'Peak Faction < 0.1')

# plot(time,cumGrowth,col="blue",pch=20,xlab="cumulative time",ylab="cumulative frequency") 

# only 100 cf 326 ct
# top1<-subset(top, top$DAYS > 0)  

# top1<-subset(top, top$peak < 0.2&top$peak > 0.1 )  
   top1<-subset(top, top$peak <  0.1 )  

plotGrowthCurves=function(n){
	 time <- if( top1$DAYS[n] <= 100) 1:top1$DAYS[n] else seq(1, top1$DAYS[n],by= (top1$DAYS[n]-1)/(100-1))
       t=time/top1$DAY[n]
       nt=nchar(top1$VIEW_DATA[n])
       growth=substring(top1$VIEW_DATA[n], 2, nt-1)
       growth=paste(unlist(strsplit(growth,split=" ")), collapse="")
       cumGrowth=as.numeric(unlist(strsplit(growth, ","))) 
       cf=cumGrowth/max(cumGrowth) 

	 difGrowth <- c(0, diff(cumGrowth, lag = 1, differences = 1)) 
	 df=difGrowth/max(cumGrowth)

	 peak=max(difGrowth)/max(cumGrowth)
	 dat = data.frame(t, difGrowth)
	 pt<-subset(dat$t, dat$difGrowth==max(difGrowth))[1]
       # return(pt)
       try(suppressWarnings(     
		# points( t, df, col=sample(c(1:151),1),pch=20, type='b')  ))  # b
       	points( pt,peak,col=sample(c(1:1000),1),pch=20, type='p') ))
}

sapply(list<-sample(c(1:length(top1$DAY)), 100), plotGrowthCurves)
sapply(c(1:length(top1$DAYS)),plotGrowthCurves)	# length(top$DAYS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~threshold of time series~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tsDyn)

# the largest growth http://www.youtube.com/watch?v=_OBlgSz8sSM&feature=youtu.be
time <- if( top$DAYS[392] <= 100) 1:top$DAYS[392] else seq(1, top$DAYS[392],by= (top$DAYS[392]-1)/(100-1))
t=time/top$DAY[392]
nt=nchar(top$VIEW_DATA[392])
growth=substring(top$VIEW_DATA[392], 2, nt-1)
growth=paste(unlist(strsplit(growth,split=" ")), collapse="")

cumGrowth=as.numeric(unlist(strsplit(growth, ","))) 
cf=cumGrowth/max(cumGrowth)

difGrowth <- c(0, diff(cumGrowth, lag = 1, differences = 1)) 
df=difGrowth/max(cumGrowth)

peak=max(difGrowth)/max(cumGrowth)
dat = data.frame(t, difGrowth)
pt<-subset(dat$t, dat$difGrowth==max(difGrowth))

summary(star(df, mTh=c(0,1), control=list(maxit=3000)))[0]

setarTest(df, m=1, thDelay = 0, nboot=10, trim=0.1, test=c("1vs", "2vs3"), hpc=c("none", "foreach"),check=T)

library(tseries)
terasvirta.test(as.ts(df), lag = 1, type = c("Chisq","F"),scale = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~bursts demise along time~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
plot(top$peak~top$DAYS, col='grey', ylab='Peak Fraction', xlab='Days')
par(mfrow=c(1,2))
plot(log(top$peak)~log(top$DAYS), col='grey', ylab='Peak Fraction  (Log)', xlab='Days  (Log)')
plot(log(top$peak)~log(top$TOTAL_VIEW), col='grey', ylab='Peak Fraction  (Log)', xlab='Total Views  (Log)')

par(mfrow=c(1,2))
plot(log(top$peak/top$DAYS)~log(top$DAYS), col='grey', ylab='Peak Fraction Normalized by Lifetime (Log)', xlab='Lifetime  (Log)')
plot((top$peak/top$DAYS)~(top$DAYS), col='grey', ylab='Peak Fraction Normalized by Lifetime', xlab='Lifetime')


smoothScatter(log(top$peak)~log(top$TOTAL_VIEW), col = densCols(top$peak), ylab='Peak Fraction', xlab='Total Views')
smoothScatter(log(top$peak)~log(top$DAYS), col = densCols(top$peak), ylab='Peak Fraction', xlab='Lifetime')

r<-lm(log(top$peak)~log(top$DAYS)+log(top$TOTAL_VIEW)+log(top$DAYS)*log(top$TOTAL_VIEW)) # +top$duration
summary(r)
library(car)
vif(r)

r<-lm(log(top$peak/top$DAYS)~log(top$DAYS)+log(top$TOTAL_VIEW)) # +top$duration
summary(r)
library(car)
vif(r)

p <- ggplot(top,
  aes(x=DAYS, y=peak, colour=log(TOTAL_VIEW)*2))
p + geom_point()

(d <- qplot(log(DAYS), log(peak), data=top, colour=log(TOTAL_VIEW)))

# Change scale label
d + scale_colour_gradient(limits=c(5, 10))


(d <- qplot(TOTAL_VIEW, peak, data=top, colour=log(DAYS)))
# Change scale label
d + scale_colour_gradient(limits=c(5, 10))




pg <- ggplot(top, aes(DAYS, peak)) + geom_point() +
    geom_smooth(method = "loess", se = FALSE) + xlab("Days") +
    ylab("Peak Fraction")
print(pg)


library(ggplot2)
p <- ggplot(top,aes(DAYS,peak))
   p + geom_point()
p + stat_bin2d(bins = 100)

p + stat_density2d(aes(DAYS,peak), geom="polygon") +
     coord_cartesian(xlim = c(0, 1),ylim=c(0,max(top$DAYS)))+
     scale_fill_continuous(high='red2',low='blue4')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~fit growth curve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
findGrowth<-function(n){
	 time <- if( top$DAYS[n] <= 100) 1:top$DAYS[n] else seq(1, top$DAYS[n],by= (top$DAYS[n]-1)/(100-1))
       nt=nchar(top$VIEW_DATA[n])
       growth=substring(top$VIEW_DATA[n], 2, nt-1)
       growth=paste(unlist(strsplit(growth,split=" ")), collapse="")
       cumGrowth=as.numeric(unlist(strsplit(growth, ",")))     
       # difGrowth <- c(0, diff(cumGrowth, lag = 1, differences = 1))
	 out=try(suppressWarnings(
	 nls(cumGrowth ~ (M *  (1-exp(-(P+Q) * time) ) /(1+(Q/P)*exp(-(P+Q)*time))),
                   control = nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/4096,printEval = FALSE, warnOnly = FALSE),
                   start = list(M= max(cumGrowth), P=0.03, Q=0.38))
	  ),  silent=T)
	  if (class(out)=="try-error"){return(c(0,0,0))}
	  else {
	  coeffs=as.data.frame(coef(out))[,1]
	  # rsquare=cor(cumGrowth,(coeffs[1]*(1-exp(-(coeffs[2]+coeffs[3])*time) )/(1+(coeffs[1]/coeffs[2])*exp(-(coeffs[2]+coeffs[3])*time))))^2
	  # n= length(cumGrowth)
	  # adjustedRsquare=1-(1-rsquare)*(n-1)/(n-2) 
	  coees=(c(coeffs)) # ,n, adjustedRsquare))
	return(coees)}  }

bass <- data.frame(do.call(rbind, lapply(c(1:length(top$X.ID)),findGrowth)))  # length(top$X.ID)

test <- data.frame(do.call(rbind, lapply(c(1:10),findGrowth)))  # length(top$X.ID)
n=6
time <- if( top$DAYS[n] <= 100) 1:top$DAYS[n] else seq(1, top$DAYS[n],by= (top$DAYS[n]-1)/(100-1))
       nt=nchar(top$VIEW_DATA[n])
       growth=substring(top$VIEW_DATA[n], 2, nt-1)
       growth=paste(unlist(strsplit(growth,split=" ")), collapse="")
       cumGrowth=as.numeric(unlist(strsplit(growth, ",")))     
       difGrowth <- c(0,diff(cumGrowth, lag = 1, differences = 1))

 nls(cumGrowth ~ (M *  (1-exp(-(P+Q) * time) ) /(1+(Q/P)*exp(-(P+Q)*time))),
                   control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/4096e1000,printEval = F, warnOnly = F),
                   start = list(M= max(cumGrowth), P=0.003, Q=0.38), trace=F)  # plinear, alg = "port"

# http://en.wikipedia.org/wiki/Shifted_Gompertz_distribution
 nls(cumGrowth ~ (M *  (1-exp(-B * time) )*exp(-N*exp(-B*time))),
                   control = nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/4096e1000,printEval = F, warnOnly = F),
                   start = list(M= max(cumGrowth), B=0.003, N=0.001), trace=F)  # plinear, alg = "port"


## Initial values are in fact the converged values
Run<-rep(1, length(time))
dat<-data.frame(Run, time, difGrowth)

Asym <- 4.5; b2 <- 2.3; b3 <- 0.7
SSgompertz(log(DNase.1$conc), Asym, b2, b3) # response and gradient
h<-getInitial(difGrowth ~ SSgompertz(time, Asym, b2, b3), data = dat)

fm2 <- nls(data$difGrowth ~ SSgompertz(log(conc), Asym, b2, b3),
           data = dat)
summary(fm2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Chick.1 <- ChickWeight[ChickWeight$Chick == 1, ]
Asym <- 368; xmid <- 14; scal <- 6
g<-getInitial(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
## Initial values are in fact the converged values
fm2 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
summary(fm2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# http://en.wikipedia.org/wiki/Logistic_distribution
 nls(cumGrowth ~ (M ) /(1+exp(-(time-U)/S)),
                   control = nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/4096e1000,printEval = F, warnOnly = F),
                   start = list(M= max(cumGrowth), U=2.7, S=0.1), trace=F)  # plinear, alg = "port"




fm1DNase1 <- nls(cumGrowth ~ (M *  (1-exp(-(P+Q) * time) ) /(1+(Q/P)*exp(-(P+Q)*time))), dat)

summary(fm1DNase1)



names(bass)<-c('M', 'P', 'Q', 'n')
top$M<-bass$M
top$P<-bass$P
top$Q<-bass$Q
write.csv(top, "D:/Research/Youtube/TubeOverTime/top/dao/topcat.csv", row.names=F)

dim(subset(bass, bass$M!=0))

plot((top$P), (top$Q), xlab='Innovation', ylab='Imitation', col='green')
plot(bass)
plot(top$TOTAL_VIEW, top$M, col='purple', xlab='Total Views', ylab='Total Views Predicted by Bass Model') 
text(1.0e+08, 6e+07,"8433 Correctly Predicted Values")
text(1.3e+08, 1e+07,"6545 Incorrectly Predicted Values")

# dim(subset(top, top$M==0))  # [1] 5275   40
# out<-nls(difGrowth ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * time) ) /(1+(Q/P)*exp(-(P+Q)*time))^2,
#   	  control = nls.control(maxiter = 10000, tol = 1e-05, minFactor = 1/1000000000,printEval = FALSE, warnOnly = FALSE),
#  	  start = list(M= max(difGrowth), P=0.03, Q=0.38))

install.packages('nlstools')
library(nlstools)
data(growthcurve4) 
#~~~~~~~~~~~~~~identify insufficient growth curve~~~~~~~~~~~~~~~~~~~~~~~~~~#
summary(topf$M-topf$TOTAL_VIEW)

#---------cluster analysis:Hierarchical Agglomerative----------------------#

tc3=topf[1:100, 38:40] 

tc3<- na.omit(tc3) # listwise deletion of missing
tc4 <- scale(tc3) # standardize variables 

# Ward Hierarchical Clustering
d <- dist(tc4, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit,xlab="Video IDs",ylab="Ward's similarity") # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red") 

#~~~~~~~~addTheoreticalCDFs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ids=subset(cbind(c(1:110),tc)[,1],cbind(c(1:110),tc)[,2]!=0)
clusterIds=cbind(ids,groups)

addTheoreticalCDFs=function(n){
	if (tc[n,1]!=0){
	t1=subset(t[,1:3],t[,1]==n)
	ct=t1[,3]/max(t1[,3])
	cf=t1[,2]/max(t1[,2])
	c=sample(c(1:151),1)
	points( ct,cf,col=c,pch=20)
      lines( ct,tc[n,2]/(tc[n,2]+1/(exp(tc[n,1]*ct))),col=c)
	}
}

ids1=subset(clusterIds[,1],clusterIds[,2]==1)
ids2=subset(clusterIds[,1],clusterIds[,2]==2)
ids3=subset(clusterIds[,1],clusterIds[,2]==3)

t1=subset(t[,1:3],t[,1]==1)
ct=t1[,3]/max(t1[,3])
cf=t1[,2]/max(t1[,2])
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type1")
sapply(ids1,addTheoreticalCDFs)
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type2")
sapply(ids2,addTheoreticalCDFs)
plot(ct,cf,col="blue",pch=20,xlab="cumulative time",
ylab="cumulative frequency",type="n",main="type2")
sapply(ids3,addTheoreticalCDFs)

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


plot(log(top[,21]/top$DAYS),log(top$TOTAL_VIEW),xlab="First embedded on")
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

feo<-top[,21]/top$DAYS
fev<-top[,22]/top$DAYS
ffvv<-top[,23]/top$DAYS
frf<-top[,24]/top$DAYS
frfsm<-top[,25]/top$DAYS
frfgs<-top[,26]/top$DAYS
frfgvs<-top[,27]/top$DAYS
frflv<-top[,28]/top$DAYS
frfy<-top[,29]/top$DAYS
frfys<-top[,30]/top$DAYS
fvfa<-top[,31]/top$DAYS
fvfmd<-top[,32]/top$DAYS
fvocp<-top[,33]/top$DAYS
ov<-top[,34]/top$DAYS

# output standard coefficient
library(QuantPsyc)# install.packages("QuantPsyc")
as.data.frame(lm.beta(reg))

#~~~~~~~~~~~~~~~~~~~~~popularity of comment~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
comment<-top$TOTAL_COMM
regc<-lm((comment/top$DAYS)~(feo)+(fev)+(ffvv)+(frf)+(frfsm)+(frfgs)+(frfgvs)+(frflv)+(frfy)+(frfys)+(fvfa)+(fvfmd)+(fvocp)+(ov))
summary(regc)
as.data.frame(lm.beta(regc))


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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
favs<-top$TOTAL_FAVS
regf<-lm((favs/top$DAYS)~(feo)+(fev)+(ffvv)+(frf)+(frfsm)+(frfgs)+(frfgvs)+(frflv)+(frfy)+(frfys)+(fvfa)+(fvfmd)+(fvocp)+(ov))
summary(regf)
as.data.frame(lm.beta(regf))

value<-mean(top[,19:32])
category<-c('Firstembeddedon' , 'Firstembeddedview', 'Firstfeaturedvideoview', 'Firstreferralfrom',
  'Firstreferralfromasubscribermodule','FirstreferralfromGooglesearch','FirstreferralfromGoogleVideosearch',
  'Firstreferralfromrelatedvideo','FirstreferralfromYouTube','FirstreferralfromYouTubesearch',
  'Firstviewfromad','Firstviewfromamobiledevice','Firstviewonachannelpage','Other/Viral')
forPie<-data.frame(category, value)
pie(forPie$value,labels = forPie$category)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rats<-top$TOTAL_RATS
regr<-lm((rats/top$DAYS)~(feo)+(fev)+(ffvv)+(frf)+(frfsm)+(frfgs)+(frfgvs)+(frflv)+(frfy)+(frfys)+(fvfa)+(fvfmd)+(fvocp)+(ov))
summary(regr)
as.data.frame(lm.beta(regr))

