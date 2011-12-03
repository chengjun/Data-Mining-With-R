###################################################
### Loading the Data into R
###################################################
library(DMwR)
head(algae)


algae <- read.table('Analysis.txt',
          header=F,
          dec='.',
          col.names=c('season','size','speed','mxPH','mnO2','Cl',
          'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
          'a5','a6','a7'),
          na.strings=c('XXXXXXX'))


###################################################
### Data Visualization and Summarization
###################################################
summary(algae)

hist(algae$mxPH, prob=T)


library(car)
par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab='',
      main='Histogram of maximum pH value',ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH,main='Normal QQ plot of maximum pH')
par(mfrow=c(1,1))




boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
rug(jitter(algae$oPO4),side=2)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)




plot(algae$NH4,xlab='')
abline(h=mean(algae$NH4,na.rm=T),lty=1)
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm=T),lty=3)
identify(algae$NH4)


plot(algae$NH4,xlab='')
clicked.lines <- identify(algae$NH4)
algae[clicked.lines,]


algae[algae$NH4 > 19000,]


library(lattice)
print(bwplot(size ~ a1, data=algae,ylab='River Size',xlab='Algal A1'))


library(Hmisc)
print(bwplot(size ~ a1, data=algae,panel=panel.bpplot, 
        probs=seq(.01,.49,by=.01), datadensity=TRUE,
        ylab='River Size',xlab='Algal A1'))


minO2 <- equal.count(na.omit(algae$mnO2),
                     number=4,overlap=1/5)
stripplot(season ~ a3|minO2,
          data=algae[!is.na(algae$mnO2),])


###################################################
### Unkwnon Values
###################################################
library(DMwR)
data(algae)


algae[!complete.cases(algae),]


nrow(algae[!complete.cases(algae),])


algae <- na.omit(algae)


algae <- algae[-c(62,199),]


apply(algae,1,function(x) sum(is.na(x)))


data(algae)
manyNAs(algae,0.2)


algae <- algae[-manyNAs(algae),]


algae[48,'mxPH'] <- mean(algae$mxPH,na.rm=T)


algae[is.na(algae$Chla),'Chla'] <- median(algae$Chla,na.rm=T)


data(algae)
algae <- algae[-manyNAs(algae),]
algae <- centralImputation(algae)


cor(algae[,4:18],use="complete.obs")


data(algae)
algae <- algae[-manyNAs(algae),]
lm(PO4 ~ oPO4,data=algae)


algae[28,'PO4'] <- 42.897 + 1.293 * algae[28,'oPO4']


data(algae)
algae <- algae[-manyNAs(algae),]
fillPO4 <- function(oP) {
   if (is.na(oP)) return(NA)
   else return(42.897 + 1.293 * oP)
}
algae[is.na(algae$PO4),'PO4'] <- 
    sapply(algae[is.na(algae$PO4),'oPO4'],fillPO4)


histogram(~ mxPH | season,data=algae)


algae$season <- factor(algae$season,levels=c('spring','summer','autumn','winter'))


histogram(~ mxPH | size*speed,data=algae) 


stripplot(size ~ mxPH | speed, data=algae, jitter=T)


data(algae)
algae <- algae[-manyNAs(algae),]


algae <- knnImputation(algae,k=10)


algae <- knnImputation(algae,k=10,meth='median')



###################################################
### Multiple Linear Regression
###################################################
data(algae)
algae <- algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k = 10)


lm.a1 <- lm(a1 ~ .,data=clean.algae[,1:12])


summary(lm.a1)


anova(lm.a1)


lm2.a1 <- update(lm.a1, . ~ . - season)


summary(lm2.a1)


anova(lm.a1,lm2.a1)


final.lm <- step(lm.a1)


summary(final.lm)



###################################################
### Regression Trees
###################################################
library(rpart)
data(algae)
algae <- algae[-manyNAs(algae), ]
rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])


rt.a1


prettyTree(rt.a1)


printcp(rt.a1)


rt2.a1 <- prune(rt.a1,cp=0.08)
rt2.a1


set.seed(1234) # Just to ensure  same results as in the book
(rt.a1 <- rpartXse(a1 ~ .,data=algae[,1:12]))


first.tree <- rpart(a1 ~ .,data=algae[,1:12])
snip.rpart(first.tree,c(4,7))



prettyTree(first.tree)
snip.rpart(first.tree)


###################################################
### Model Evaluation and Selection
###################################################
lm.predictions.a1 <- predict(final.lm,clean.algae)
rt.predictions.a1 <- predict(rt.a1,algae)


(mae.a1.lm <- mean(abs(lm.predictions.a1-algae[,'a1'])))
(mae.a1.rt <- mean(abs(rt.predictions.a1-algae[,'a1'])))


(mse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2))
(mse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2))


(nmse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))
(nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))


regr.eval(algae[,'a1'],rt.predictions.a1,train.y=algae[,'a1'])


old.par <- par(mfrow=c(1,2))
plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
plot(rt.predictions.a1,algae[,'a1'],main="Regression Tree",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
par(old.par)



plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
     xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
algae[identify(lm.predictions.a1,algae[,'a1']),]



sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0,0,lm.predictions.a1)
regr.eval(algae[,'a1'],lm.predictions.a1,stats=c('mae','mse'))
regr.eval(algae[,'a1'],sensible.lm.predictions.a1,stats=c('mae','mse'))


cv.rpart <- function(form,train,test,...) {
  m <- rpartXse(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
cv.lm <- function(form,train,test,...) {
  m <- lm(form,train,...)
  p <- predict(m,test)
  p <- ifelse(p < 0,0,p)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


res <- experimentalComparison(
            c(dataset(a1 ~ .,clean.algae[,1:12],'a1')),
            c(variants('cv.lm'), 
              variants('cv.rpart',se=c(0,0.5,1))),
            cvSettings(3,10,1234))


summary(res)


plot(res)


getVariant('cv.rpart.v1',res)


DSs <- sapply(names(clean.algae)[12:18],
         function(x,names.attrs) { 
           f <- as.formula(paste(x,"~ ."))
           dataset(f,clean.algae[,c(names.attrs,x)],x) 
         },
         names(clean.algae)[1:11])

res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',se=c(0,0.5,1))
                   ),
                  cvSettings(5,10,1234))


plot(res.all)


bestScores(res.all)


library(randomForest)
cv.rf <- function(form,train,test,...) {
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
res.all <- experimentalComparison(
                  DSs,
                  c(variants('cv.lm'),
                    variants('cv.rpart',se=c(0,0.5,1)),
                    variants('cv.rf',ntree=c(200,500,700))
                   ),
                  cvSettings(5,10,1234))


bestScores(res.all)


compAnalysis(res.all,against='cv.rf.v3',
               datasets=c('a1','a2','a4','a6'))



###################################################
### Predictions for the 7 algae
###################################################
bestModelsNames <- sapply(bestScores(res.all),
                          function(x) x['nmse','system'])
learners <- c(rf='randomForest',rpart='rpartXse') 
funcs <- learners[sapply(strsplit(bestModelsNames,'\\.'),
                        function(x) x[2])]
parSetts <- lapply(bestModelsNames,
                   function(x) getVariant(x,res.all)@pars)

bestModels <- list()
for(a in 1:7) {
  form <- as.formula(paste(names(clean.algae)[11+a],'~ .'))
  bestModels[[a]] <- do.call(funcs[a],
          c(list(form,clean.algae[,c(1:11,11+a)]),parSetts[[a]]))
}


clean.test.algae <- knnImputation(test.algae,k=10,distData=algae[,1:11])


preds <- matrix(ncol=7,nrow=140)
for(i in 1:nrow(clean.test.algae)) 
  preds[i,] <- sapply(1:7,
                      function(x) 
                        predict(bestModels[[x]],clean.test.algae[i,])
                     )


avg.preds <- apply(algae[,12:18],2,mean)
apply( ((algae.sols-preds)^2),           2,mean) / 
apply( (scale(algae.sols,avg.preds,F)^2),2,mean)


