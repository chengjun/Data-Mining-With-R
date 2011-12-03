###################################################
### The Available Data
###################################################
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("ALL")


library(Biobase)
library(ALL)
data(ALL)


ALL


pD <- phenoData(ALL)
varMetadata(pD)


table(ALL$BT)
table(ALL$mol.biol)
table(ALL$BT,ALL$mol.bio)


featureNames(ALL)[1:10]
sampleNames(ALL)[1:5]


tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & 
                   ALL$mol.bio %in% levels(ALL$mol.bio)[1:4])
ALLb <- ALL[,tgt.cases]
ALLb


ALLb$BT <- factor(ALLb$BT)
ALLb$mol.bio <- factor(ALLb$mol.bio)


###################################################
### Exploring the data set
###################################################
es <- exprs(ALLb)
dim(es)


summary(as.vector(es))


source("http://bioconductor.org/biocLite.R")
biocLite("genefilter")


library(genefilter)
hist(as.vector(es),breaks=80,prob=T,
     xlab='Expression Levels',
     main='Histogram of Overall Expression Levels')
abline(v=c(median(as.vector(es)),
           shorth(as.vector(es)),
           quantile(as.vector(es),c(0.25,0.75))),
       lty=2,col=c(2,3,4,4))
legend('topright',c('Median','Shorth','1stQ','3rdQ'),
       lty=2,col=c(2,3,4,4))


sapply(levels(ALLb$mol.bio),function(x) summary(as.vector(es[,which(ALLb$mol.bio == x)])))



###################################################
### Gene (Feature) Selection
###################################################
rowIQRs <- function(em) 
  rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))
plot(rowMedians(es),rowIQRs(es),
     xlab='Median expression level',
     ylab='IQR expression level',
     main='Main Characteristics of Genes Expression Levels')


library(genefilter)
ALLb <- nsFilter(ALLb,
                 var.func=IQR,
                 var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
ALLb


ALLb <- ALLb$eset
es <- exprs(ALLb)
dim(es)


f <- Anova(ALLb$mol.bio,p=0.01)
ff <- filterfun(f)
selGenes <- genefilter(exprs(ALLb),ff)


sum(selGenes) 
ALLb <- ALLb[selGenes,]
ALLb


es <- exprs(ALLb)
plot(rowMedians(es),rowIQRs(es),
     xlab='Median expression level',
     ylab='IQR expression level',
     main='Distribution Properties of the Selected Genes')


featureNames(ALLb) <- make.names(featureNames(ALLb))
es <- exprs(ALLb)


library(randomForest)
dt <- data.frame(t(es),Mut=ALLb$mol.bio)
rf <- randomForest(Mut ~  .,dt,importance=T)
imp <- importance(rf)
imp <- imp[,ncol(imp)-1]
rf.genes <- names(imp)[order(imp,decreasing=T)[1:30]]


sapply(rf.genes,function(g) tapply(dt[,g],dt$Mut,median))


library(lattice)
ordMut <- order(dt$Mut)
levelplot(as.matrix(dt[ordMut,rf.genes]),
          aspect='fill', xlab='', ylab='',
          scales=list(
            x=list(
              labels=c('+','-','*','|')[as.integer(dt$Mut[ordMut])],
              cex=0.7,
              tck=0)
            ),
          main=paste(paste(c('"+"','"-"','"*"','"|"'),
                           levels(dt$Mut)
                          ),
                     collapse='; '),
          col.regions=colorRampPalette(c('white','orange','blue'))
          )


library(Hmisc)
vc <- varclus(t(es))
clus30 <- cutree(vc$hclust,30)
table(clus30)


getVarsSet <- function(cluster,nvars=30,seed=NULL,verb=F) 
{
  if (!is.null(seed)) set.seed(seed)

  cls <- cutree(cluster,nvars)
  tots <- table(cls)
  vars <- c()
  vars <- sapply(1:nvars,function(clID)
    {
      if (!length(tots[clID])) stop('Empty cluster! (',clID,')')
      x <- sample(1:tots[clID],1)
      names(cls[cls==clID])[x]
    })
  if (verb)  structure(vars,clusMemb=cls,clusTots=tots)
  else       vars
}
getVarsSet(vc$hclust)
getVarsSet(vc$hclust)



###################################################
### Predicting Cytogenetic Abnormalities
###################################################
data(iris)
rpart.loocv <- function(form,train,test,...) {
  require(rpart,quietly=T)
  m <- rpart(form,train,...)
  p <- predict(m,test,type='class')
  c(accuracy=ifelse(p == resp(form,test),100,0))
}
exp <- loocv(learner('rpart.loocv',list()),
             dataset(Species~.,iris),
             loocvSettings(seed=1234,verbose=F))


summary(exp)


library(class)
data(iris)
idx <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
tr <- iris[idx,]
ts <- iris[-idx,]
preds <- knn(tr[,-5],ts[,-5],tr[,5],k=3)
table(preds,ts[,5])


kNN <- function(form,train,test,norm=T,norm.stats=NULL,...) {
  require(class,quietly=TRUE)
  tgtCol <- which(colnames(train)==as.character(form[[2]]))
  if (norm) {
    if (is.null(norm.stats)) tmp <- scale(train[,-tgtCol],center=T,scale=T)
    else tmp <- scale(train[,-tgtCol],center=norm.stats[[1]],scale=norm.stats[[2]])
    train[,-tgtCol] <- tmp
    ms <- attr(tmp,"scaled:center")
    ss <- attr(tmp,"scaled:scale")
    test[,-tgtCol] <- scale(test[,-tgtCol],center=ms,scale=ss)
  }
  knn(train[,-tgtCol],test[,-tgtCol],train[,tgtCol],...)
}
preds.norm <- kNN(Species ~ .,tr,ts,k=3)
table(preds.norm,ts[,5])
preds.notNorm <- kNN(Species ~ .,tr,ts,norm=F,k=3)
table(preds.notNorm,ts[,5])


vars <- list()
vars$randomForest <- list(ntree=c(500,750,100),
                          mtry=c(5,15,30),
                          fs.meth=list(list('all'),
                                       list('rf',30),
                                       list('varclus',30,50)))
vars$svm <- list(cost=c(1,100,500),
                 gamma=c(0.01,0.001,0.0001),
                 fs.meth=list(list('all'),
                              list('rf',30),
                              list('varclus',30,50)))
vars$knn <- list(k=c(3,5,7,11),
                 norm=c(T,F),
                 fs.meth=list(list('all'),
                              list('rf',30),
                              list('varclus',30,50)))


varsEnsembles <- function(tgt,train,test,
                          varsSets,
                          baseLearner,blPars,
                          verb=F)
{
  preds <- matrix(NA,ncol=length(varsSets),nrow=NROW(test))
  for(v in seq(along=varsSets)) {
    if (baseLearner=='knn')
      preds[,v] <- knn(train[,-which(colnames(train)==tgt)],
                       test[,-which(colnames(train)==tgt)],
                       train[,tgt],blPars)
    else {
      m <- do.call(baseLearner,
                   c(list(as.formula(paste(tgt,
                                           paste(varsSets[[v]],
                                                 collapse='+'),
                                           sep='~')),
                          train[,c(tgt,varsSets[[v]])]),
                     blPars)
                   )
      if (baseLearner == 'randomForest')
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])],
                                  type='response'))
      else
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])]))
    }
  }
  ps <- apply(preds,1,function(x)
                 levels(factor(x))[which.max(table(factor(x)))])
  ps <- factor(ps,
               levels=1:nlevels(train[,tgt]),
               labels=levels(train[,tgt]))
  if (verb) structure(ps,ensemblePreds=preds) else ps
}


genericModel <- function(form,train,test,
                         learner,
                         fs.meth,
                         ...)
  {
    cat('=')
    tgt <- as.character(form[[2]])
    tgtCol <- which(colnames(train)==tgt)

    # Anova filtering  
    f <- Anova(train[,tgt],p=0.01)
    ff <- filterfun(f)
    genes <- genefilter(t(train[,-tgtCol]),ff)
    genes <- names(genes)[genes]
    train <- train[,c(tgt,genes)]
    test <- test[,c(tgt,genes)]
    tgtCol <- 1

    # Specific filtering 
    if (fs.meth[[1]]=='varclus') {
      require(Hmisc,quietly=T)
      v <- varclus(as.matrix(train[,-tgtCol]))
      VSs <- lapply(1:fs.meth[[3]],function(x)
                    getVarsSet(v$hclust,nvars=fs.meth[[2]]))
      pred <- varsEnsembles(tgt,train,test,VSs,learner,list(...))

    } else {
      if (fs.meth[[1]]=='rf') {
        require(randomForest,quietly=T)
        rf <- randomForest(form,train,importance=T)
        imp <- importance(rf)
        imp <- imp[,ncol(imp)-1]
        rf.genes <- names(imp)[order(imp,decreasing=T)[1:fs.meth[[2]]]]
        train <- train[,c(tgt,rf.genes)]
        test <- test[,c(tgt,rf.genes)]
      }

      if (learner == 'knn') 
        pred <- kNN(form,
             train,
             test,
             norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                             rowIQRs(t(as.matrix(train[,-tgtCol])))),
             ...)
      else {
        model <- do.call(learner,c(list(form,train),list(...)))
        pred <- if (learner != 'randomForest') predict(model,test)
                else predict(model,test,type='response')
      }

    }

    c(accuracy=ifelse(pred == resp(form,test),100,0))
  }


require(class,quietly=TRUE)
require(randomForest,quietly=TRUE)
require(e1071,quietly=TRUE)
load('myALL.Rdata')
es <- exprs(ALLb)
# simple filtering
ALLb <- nsFilter(ALLb,
                 var.func=IQR,var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
ALLb <- ALLb$eset
# the data set
featureNames(ALLb) <- make.names(featureNames(ALLb))
dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)
DSs <- list(dataset(Mut ~ .,dt,'ALL'))
# The learners to evaluate
TODO <- c('knn','svm','randomForest')
for(td in TODO) {
  assign(td,
         experimentalComparison(
              DSs,
              c(
                do.call('variants',
                        c(list('genericModel',learner=td),
                          vars[[td]],
                          varsRootName=td))
                ),
               loocvSettings(seed=1234,verbose=F)
                                )
         )
  save(list=td,file=paste(td,'Rdata',sep='.'))
}


load('knn.Rdata')
load('svm.Rdata')
load('randomForest.Rdata')


rankSystems(svm,max=T)


all.trials <- join(svm,knn,randomForest,by='variants')


rankSystems(all.trials,top=10,max=T)


getVariant('knn.v2',all.trials)


bestknn.loocv <- function(form,train,test,...) {
  require(Biobase,quietly=T)
  require(randomForest,quietly=T)
  cat('=')
  tgt <- as.character(form[[2]])
  tgtCol <- which(colnames(train)==tgt)
  # Anova filtering
  f <- Anova(train[,tgt],p=0.01)
  ff <- filterfun(f)
  genes <- genefilter(t(train[,-tgtCol]),ff)
  genes <- names(genes)[genes]
  train <- train[,c(tgt,genes)]
  test <- test[,c(tgt,genes)]
  tgtCol <- 1
  # Random Forest filtering
  rf <- randomForest(form,train,importance=T)
  imp <- importance(rf)
  imp <- imp[,ncol(imp)-1]
  rf.genes <- names(imp)[order(imp,decreasing=T)[1:30]]
  train <- train[,c(tgt,rf.genes)]
  test <- test[,c(tgt,rf.genes)]
  # knn prediction
  ps <- kNN(form,train,test,norm=T, 
            norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                            rowIQRs(t(as.matrix(train[,-tgtCol])))),
            k=5,...)
  structure(c(accuracy=ifelse(ps == resp(form,test),100,0)),
            itInfo=list(ps)
           )
}
resTop <- loocv(learner('bestknn.loocv',pars=list()),
                dataset(Mut~.,dt),
                loocvSettings(seed=1234,verbose=F),
                itsInfo=T)


attr(resTop,'itsInfo')[1:4]


dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)


preds <- unlist(attr(resTop,'itsInfo'))
table(preds,dt$Mut)



