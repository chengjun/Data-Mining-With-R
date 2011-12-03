###################################################
### Loading the data into R
###################################################
load('sales.Rdata')


library(DMwR)
data(sales)


head(sales)




###################################################
### Exploring the data set
###################################################
summary(sales)



nlevels(sales$ID)
nlevels(sales$Prod)


length(which(is.na(sales$Quant) & is.na(sales$Val)))


sum(is.na(sales$Quant) & is.na(sales$Val))


table(sales$Insp)/nrow(sales)*100


totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS,main='Transactions per salespeople',names.arg='',xlab='Salespeople',ylab='Amount')
barplot(totP,main='Transactions per product',names.arg='',xlab='Products',ylab='Amount')


sales$Uprice <- sales$Val/sales$Quant


summary(sales$Uprice)


attach(sales)
upp <- aggregate(Uprice,list(Prod),median,na.rm=T)
topP <- sapply(c(T,F),function(o) 
               upp[order(upp[,2],decreasing=o)[1:5],1])
colnames(topP) <- c('Expensive','Cheap')
topP


tops <- sales[Prod %in% topP[1,],c('Prod','Uprice')]
tops$Prod <- factor(tops$Prod)
boxplot(Uprice ~ Prod,data=tops,ylab='Uprice',log="y")


vs <- aggregate(Val,list(ID),sum,na.rm=T)
scoresSs <- sapply(c(T,F),function(o) 
                   vs[order(vs$x,decreasing=o)[1:5],1])
colnames(scoresSs) <- c('Most','Least')
scoresSs


sum(vs[order(vs$x,decreasing=T)[1:100],2])/sum(Val,na.rm=T)*100
sum(vs[order(vs$x,decreasing=F)[1:2000],2])/sum(Val,na.rm=T)*100


qs <- aggregate(Quant,list(Prod),sum,na.rm=T)
scoresPs <- sapply(c(T,F),function(o) 
                   qs[order(qs$x,decreasing=o)[1:5],1])
colnames(scoresPs) <- c('Most','Least')
scoresPs
sum(as.double(qs[order(qs$x,decreasing=T)[1:100],2]))/
  sum(as.double(Quant),na.rm=T)*100
sum(as.double(qs[order(qs$x,decreasing=F)[1:4000],2]))/
  sum(as.double(Quant),na.rm=T)*100


out <- tapply(Uprice,list(Prod=Prod),
              function(x) length(boxplot.stats(x)$out))


out[order(out,decreasing=T)[1:10]]


sum(out)
sum(out)/nrow(sales)*100



###################################################
### Data problems
###################################################
totS <- table(ID)
totP <- table(Prod)


nas <- sales[which(is.na(Quant) & is.na(Val)),c('ID','Prod')]


propS <- 100*table(nas$ID)/totS
propS[order(propS,decreasing=T)[1:10]]


propP <- 100*table(nas$Prod)/totP
propP[order(propP,decreasing=T)[1:10]]


detach(sales)
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]


nnasQp <- tapply(sales$Quant,list(sales$Prod),
                 function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]


sales <- sales[!sales$Prod %in% c('p2442','p2443'),]


nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)


nnasQs <- tapply(sales$Quant,list(sales$ID),function(x) sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs,decreasing=T)[1:10]]


nnasVp <- tapply(sales$Val,list(sales$Prod),
                 function(x) sum(is.na(x)))
propNAsVp <- nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing=T)[1:10]]


nnasVs <- tapply(sales$Val,list(sales$ID),function(x) sum(is.na(x)))
propNAsVs <- nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs,decreasing=T)[1:10]]


tPrice <- tapply(sales[sales$Insp != 'fraud','Uprice'],list(sales[sales$Insp != 'fraud','Prod']),median,na.rm=T)


noQuant <- which(is.na(sales$Quant))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /
                                  tPrice[sales[noQuant,'Prod']])
noVal <- which(is.na(sales$Val))
sales[noVal,'Val'] <- sales[noVal,'Quant'] *
                      tPrice[sales[noVal,'Prod']]


sales$Uprice <- sales$Val/sales$Quant


save(sales,file='salesClean.Rdata')


attach(sales)
notF <- which(Insp != 'fraud')
ms <- tapply(Uprice[notF],list(Prod=Prod[notF]),function(x) {
     bp <- boxplot.stats(x)$stats
     c(median=bp[3],iqr=bp[4]-bp[2])
   })
ms <- matrix(unlist(ms),
             length(ms),2,
             byrow=T,dimnames=list(names(ms),c('median','iqr')))
head(ms)


par(mfrow=c(1,2))
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='')
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='',col='grey',log="xy")
smalls <- which(table(Prod) < 20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch='+')


dms <- scale(ms)
smalls <- which(table(Prod) < 20)
prods <- tapply(sales$Uprice,sales$Prod,list)
similar <- matrix(NA,length(smalls),7,dimnames=list(names(smalls),c('Simil','ks.stat','ks.p','medP','iqrP','medS','iqrS')))

for(i in seq(along=smalls)) {
  d <- scale(dms,dms[smalls[i],],FALSE)
  d <- sqrt(drop(d^2 %*% rep(1,ncol(d))))
  stat <- ks.test(prods[[smalls[i]]],prods[[order(d)[2]]])
  similar[i,] <- c(order(d)[2],stat$statistic,stat$p.value,ms[smalls[i],],ms[order(d)[2],])
}


head(similar)


levels(Prod)[similar[1,1]]


nrow(similar[similar[,'ks.p'] >= 0.9,])


sum(similar[,'ks.p'] >= 0.9)


save(similar,file='similarProducts.Rdata')




###################################################
### Evaluation criteria
###################################################
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance(pred,'prec','rec')
plot(perf)


PRcurve <- function(preds,trues,...) {
  require(ROCR,quietly=T)
  pd <- prediction(preds,trues)
  pf <- performance(pd,'prec','rec')
  pf@y.values <- lapply(pf@y.values,function(x) rev(cummax(rev(x))))
  plot(pf,...)
}


PRcurve(ROCR.simple$predictions, ROCR.simple$labels )




pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance(pred,'lift','rpp')
plot(perf,main='Lift Chart')


CRchart <- function(preds,trues,...) {
  require(ROCR,quietly=T)
  pd <- prediction(preds,trues)
  pf <- performance(pd,'rec','rpp')
  plot(pf,...)
}  


CRchart(ROCR.simple$predictions, ROCR.simple$labels, 
        main='Cumulative Recall Chart')




avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats)) 
    stop('Provide either the training data or the product stats')
  if (missing(stats)) {
    notF <- which(train$Insp != 'fraud')
    stats <- tapply(train$Uprice[notF],
                    list(Prod=train$Prod[notF]),
                    function(x) {
                      bp <- boxplot.stats(x)$stats
                      c(median=bp[3],iqr=bp[4]-bp[2])
                    })
    stats <- matrix(unlist(stats),
                    length(stats),2,byrow=T,
                    dimnames=list(names(stats),c('median','iqr')))
    stats[which(stats[,'iqr']==0),'iqr'] <- 
        stats[which(stats[,'iqr']==0),'median']
  }

  mdtp <- mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
               stats[toInsp$Prod,'iqr'])
  return(mdtp)
}


###################################################
### Experimental Methodology
###################################################
evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds) {
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}


###################################################
### The modified box plot rule
###################################################
BPrule <- function(train,test) {
  notF <- which(train$Insp != 'fraud')
  ms <- tapply(train$Uprice[notF],list(Prod=train$Prod[notF]),
               function(x) {
                 bp <- boxplot.stats(x)$stats
                 c(median=bp[3],iqr=bp[4]-bp[2])
               })
  ms <- matrix(unlist(ms),length(ms),2,byrow=T,
               dimnames=list(names(ms),c('median','iqr')))
  ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
  ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
             ms[test$Prod,'iqr']
  return(list(rankOrder=order(ORscore,decreasing=T),
              rankScore=ORscore))
}


notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF],
                      list(Prod=sales$Prod[notF]),
                      function(x) {
                        bp <- boxplot.stats(x)$stats
                        c(median=bp[3],iqr=bp[4]-bp[2])
                      })
globalStats <- matrix(unlist(globalStats),
                length(globalStats),2,byrow=T,
                dimnames=list(names(globalStats),c('median','iqr')))
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
   globalStats[which(globalStats[,'iqr']==0),'median']


ho.BPrule <- function(form, train, test, ...) {
  res <- BPrule(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


bp.res <- holdOut(learner('ho.BPrule',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(bp.res)


par(mfrow=c(1,2))
info <- attr(bp.res,'itsInfo')
PTs.bp <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2)
               )
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',avg='vertical')
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',avg='vertical')




###################################################
### Local outlier factors (LOF)
###################################################
ho.LOF <- function(form, train, test, k, ...) {
  require(dprep,quietly=T)
  ntr <- nrow(train)
  all <- rbind(train,test)
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
                 lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
              else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
              else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],
                                           decreasing=T),...),
            itInfo=list(preds=all[(ntr+1):N,'lof'],
                        trues=ifelse(test$Insp=='fraud',1,0))
           )
}


lof.res <- holdOut(learner('ho.LOF',
                          pars=list(k=7,Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(lof.res)


par(mfrow=c(1,2))
info <- attr(lof.res,'itsInfo')
PTs.lof <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))



###################################################
### Clustering-based outlier rankings (OR_h)
###################################################
ho.ORh <- function(form, train, test, ...) {
  require(dprep,quietly=T)
  ntr <- nrow(train)
  all <- rbind(train,test)
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
                 outliers.ranking(ups[[u]])$prob.outliers 
              else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
              else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],
                                           decreasing=T),...),
            itInfo=list(preds=all[(ntr+1):N,'lof'],
                        trues=ifelse(test$Insp=='fraud',1,0))
           )
}


orh.res <- holdOut(learner('ho.ORh',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(orh.res)


par(mfrow=c(1,2))
info <- attr(orh.res,'itsInfo')
PTs.orh <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('topright',c('BPrule','LOF','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))


###################################################
### The class imbalance problem
###################################################
data(iris)
data <- iris[,c(1,2,5)]
data$Species <- factor(ifelse(data$Species == 'setosa','rare','common'))
newData <- SMOTE(Species ~ .,data,perc.over=600)
table(newData$Species)


par(mfrow=c(1,2))
plot(data[,1],data[,2],pch=19+as.integer(data[,3]),main='Original Data')
plot(newData[,1],newData[,2],pch=19+as.integer(newData[,3]),main="SMOTE'd Data")



###################################################
### Naive Bayes
###################################################
nb <- function(train,test) {
  require(e1071,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- naiveBayes(Insp ~ .,data)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}


ho.nb <- function(form, train, test, ...) {
  res <- nb(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


nb.res <- holdOut(learner('ho.nb',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(nb.res)


par(mfrow=c(1,2))
info <- attr(nb.res,'itsInfo')
PTs.nb <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('topright',c('NaiveBayes','ORh'),
       lty=1,col=c('black','grey'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh'),
       lty=1,col=c('black','grey'))


nb.s <- function(train,test) {
  require(e1071,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  newData <- SMOTE(Insp ~ .,data,perc.over=700)
  model <- naiveBayes(Insp ~ .,newData)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}


ho.nbs <- function(form, train, test, ...) {
  res <- nb.s(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


nbs.res <- holdOut(learner('ho.nbs',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(nbs.res)


par(mfrow=c(1,2))
info <- attr(nbs.res,'itsInfo')
PTs.nbs <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.nbs[,,1],PTs.nbs[,,2],
        add=T,lty=2,
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('topright',c('NaiveBayes','smoteNaiveBayes','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.nbs[,,1],PTs.nbs[,,2],
        add=T,lty=2,
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('bottomright',c('NaiveBayes','smoteNaiveBayes','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))



###################################################
### AdaBoost
###################################################
library(RWeka)
WOW(AdaBoostM1)


data(iris)
idx <- sample(150,100)
model <- AdaBoostM1(Species ~ .,iris[idx,],
                    control=Weka_control(I=100))
preds <- predict(model,iris[-idx,])
head(preds)
table(preds,iris[-idx,'Species'])
prob.preds <- predict(model,iris[-idx,],type='probability')
head(prob.preds)


ab <- function(train,test) {
  require(RWeka,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- AdaBoostM1(Insp ~ .,data,
                      control=Weka_control(I=100))
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}


ho.ab <- function(form, train, test, ...) {
  res <- ab(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


ab.res <- holdOut(learner('ho.ab',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(ab.res)


par(mfrow=c(1,2))
info <- attr(ab.res,'itsInfo')
PTs.ab <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2)
                )
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.ab[,,1],PTs.ab[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','black'))



###################################################
### Semi-supervised approaches
###################################################
set.seed(1234) # Just to ensrure you get the same results as in the book
library(DMwR)
library(e1071)
data(iris)
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]
nb <- naiveBayes(Species ~ .,tr)
table(predict(nb,ts),ts$Species)
trST <- tr
nas <- sample(100,90)
trST[nas,'Species'] <- NA
func <- function(m,d) {
   p <- predict(m,d,type='raw')
   data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
}
nbSTbase <- naiveBayes(Species ~ .,trST[-nas,])
table(predict(nbSTbase,ts),ts$Species)
nbST <- SelfTrain(Species ~ .,trST,learner('naiveBayes',list()),'func')
table(predict(nbST,ts),ts$Species)


pred.nb <- function(m,d) {
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
             )
}
nb.st <- function(train,test) {
  require(e1071,quietly=T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(Insp ~ .,train,
                     learner('naiveBayes',list()),'pred.nb')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}
ho.nb.st <- function(form, train, test, ...) {
  res <- nb.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


nb.st.res <- holdOut(learner('ho.nb.st',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(nb.st.res)


par(mfrow=c(1,2))
info <- attr(nb.st.res,'itsInfo')
PTs.nb.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.nb.st[,,1],PTs.nb.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.nb.st[,,1],PTs.nb.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))


pred.ada <- function(m,d) {
  p <- predict(m,d,type='probability')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
             )
}
ab.st <- function(train,test) {
  require(RWeka,quietly=T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(Insp ~ .,train,
                     learner('AdaBoostM1',
                             list(control=Weka_control(I=100))),
                     'pred.ada')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}
ho.ab.st <- function(form, train, test, ...) {
  res <- ab.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}
ab.st.res <- holdOut(learner('ho.ab.st',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(ab.st.res)


par(mfrow=c(1,2))
info <- attr(ab.st.res,'itsInfo')
PTs.ab.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )
PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.ab.st[,,1],PTs.ab.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('AdaBoostM1','ORh','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.ab[,,1],PTs.ab[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.ab.st[,,1],PTs.ab.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('AdaBoostM1','ORh','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))


