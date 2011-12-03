###################################################
### The Available Data
###################################################
library(DMwR)
data(GSPC)



###################################################
### Handling time dependent data in R
###################################################
library(xts)
x1 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01"),len=100,by="day"))
x1[1:5]
x2 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01 13:00"),len=100,by="min"))
x2[1:4]
x3 <- xts(rnorm(3),as.Date(c('2005-01-01','2005-01-10','2005-01-12')))
x3


x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]
x1["20000105"]
x1["2000-04"]
x1["2000-03-27/"]
x1["2000-02-26/2000-03-03"]
x1["/20000103"]


mts.vals <- matrix(round(rnorm(25),2),5,5)
colnames(mts.vals) <- paste('ts',1:5,sep='')
mts <- xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04',
                    '2003-01-05','2003-01-06','2003-02-16')))
mts
mts["2003-01",c("ts2","ts5")]
index(mts)
coredata(mts)


###################################################
### Reading the data from the CSV file
###################################################
GSPC <- as.xts(read.zoo('sp500.csv',header=T))


###################################################
### Getting the data from the Web
###################################################
library(tseries)
GSPC <- as.xts(get.hist.quote("^GSPC",start="1970-01-02",
          quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
head(GSPC)
GSPC <- as.xts(get.hist.quote("^GSPC",
          start="1970-01-02",end='2009-09-15',
          quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))



library(quantmod)
getSymbols('^GSPC')



getSymbols('^GSPC',from='1970-01-01',to='2009-09-15')
colnames(GSPC) <- c("Open", "High", "Low", "Close","Volume","AdjClose")


setSymbolLookup(IBM=list(name='IBM',src='yahoo'),
                USDEUR=list(name='USD/EUR',src='oanda',
                            from=as.Date('2009-01-01')))
getSymbols(c('IBM','USDEUR'))
head(IBM)
head(USDEUR)



###################################################
### Reading the data from a MySQL database
###################################################
library(RODBC)
ch <- odbcConnect("QuotesDSN",uid="myusername",pwd="mypassword")
allQuotes <- sqlFetch(ch,"gspc")
GSPC <- xts(allQuotes[,-1],order.by=as.Date(allQuotes[,1]))
head(GSPC)
odbcClose(ch)



library(DBI)
library(RMySQL)
drv <- dbDriver("MySQL")
ch <- dbConnect(drv,dbname="Quotes","myusername","mypassword")
allQuotes <- dbGetQuery(ch,"select * from gspc")
GSPC <- xts(allQuotes[,-1],order.by=as.Date(allQuotes[,1]))
head(GSPC)
dbDisconnect(ch)
dbUnloadDriver(drv)


setSymbolLookup(GSPC=list(name='gspc',src='mysql',
         db.fields=c('Index','Open','High','Low','Close','Volume','AdjClose'),
         user='xpto',password='ypto',dbname='Quotes'))
getSymbols('GSPC')


                
###################################################
### Defining the Prediction Tasks
###################################################
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)

  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x)

  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}


candleChart(last(GSPC,'3 months'),theme='white',TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()



myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]


library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
       myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
       myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
       CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
       myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
       mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
             training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
             ntree=50, importance=T)


ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM),k=1:3))
data <- modelData(ex.model,data.window=c('2009-01-01','2009-08-10'))




varImpPlot(rf@fitted.model,type=1)


imp <- importance(rf@fitted.model,type=1)
rownames(imp)[which(imp > 10)]


data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1) + myATR(GSPC) + myADX(GSPC) +    myEMV(GSPC) + myVolat(GSPC)  + myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)) )


Tdata.train <- as.data.frame(modelData(data.model,
                       data.window=c('1970-01-02','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                       data.window=c('2000-01-01','2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~ .')



###################################################
### The Prediction Models
###################################################
set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,linout=T,trace=F)
norm.preds <- predict(nn,norm.data[1001:2000,])
preds <- unscale(norm.preds,norm.data)


sigs.nn <- trading.signals(preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.nn,true.sigs)


set.seed(1234)
library(nnet)
signals <- trading.signals(Tdata.train[,'T.ind.GSPC'],0.1,-0.1)
norm.data <- data.frame(signals=signals,scale(Tdata.train[,-1]))
nn <- nnet(signals ~ .,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,trace=F)
preds <- predict(nn,norm.data[1001:2000,],type='class')


sigs.PR(preds,norm.data[1001:2000,1])


library(e1071)
sv <- svm(Tform,Tdata.train[1:1000,],gamma=0.001,cost=100)
s.preds <- predict(sv,Tdata.train[1001:2000,])
sigs.svm <- trading.signals(s.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.svm,true.sigs)


library(kernlab)
data <- cbind(signals=signals,Tdata.train[,-1])
ksv <- ksvm(signals ~ .,data[1:1000,],C=10)
ks.preds <- predict(ksv,data[1001:2000,])
sigs.PR(ks.preds,data[1001:2000,1])


library(earth)
e <- earth(Tform,Tdata.train[1:1000,])
e.preds <- predict(e,Tdata.train[1001:2000,])
sigs.e <- trading.signals(e.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.e,true.sigs)



###################################################
### From Predictions into Actions
###################################################
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
                     )
  {
    d <- NROW(market) # this is the ID of today
    orders <- NULL
    nOs <- NROW(opened.pos)
    # nothing to do!
    if (!nOs && signals[d] == 'h') return(orders)

    # First lets check if we can open new positions
    # i) long positions
    if (signals[d] == 'b' && !nOs) {
      quant <- round(bet*money/market[d,'Close'],0)
      if (quant > 0) 
        orders <- rbind(orders,
              data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1+exp.prof),
                                 market[d,'Close']*(1-max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )

    # ii) short positions  
    } else if (signals[d] == 's' && !nOs) {
      # this is the nr of stocks we already need to buy 
      # because of currently opened short positions
      need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                                 "N.stocks"])*market[d,'Close']
      quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
      if (quant > 0)
        orders <- rbind(orders,
              data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1-exp.prof),
                                 market[d,'Close']*(1+max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )
    }
    
    # Now lets check if we need to close positions
    # because their holding time is over
    if (nOs) 
      for(i in 1:nOs) {
        if (d - opened.pos[i,'Odate'] >= hold.time)
          orders <- rbind(orders,
                data.frame(order=-opened.pos[i,'pos.type'],
                           order.type=1,
                           val = NA,
                           action = 'close',
                           posID = rownames(opened.pos)[i]
                          )
                         )
      }

    orders
  }



policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
                    )
  {
    d <- NROW(market) # this is the ID of today
    orders <- NULL
    nOs <- NROW(opened.pos)
    # nothing to do!
    if (!nOs && signals[d] == 'h') return(orders)

    # First lets check if we can open new positions
    # i) long positions
    if (signals[d] == 'b') {
      quant <- round(bet*money/market[d,'Close'],0)
      if (quant > 0) 
        orders <- rbind(orders,
              data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1+exp.prof),
                                 market[d,'Close']*(1-max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )

    # ii) short positions  
    } else if (signals[d] == 's') {
      # this is the money already committed to buy stocks
      # because of currently opened short positions
      need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                                 "N.stocks"])*market[d,'Close']
      quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
      if (quant > 0)
        orders <- rbind(orders,
              data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                         val = c(quant,
                                 market[d,'Close']*(1-exp.prof),
                                 market[d,'Close']*(1+max.loss)
                                ),
                         action = c('open','close','close'),
                         posID = c(NA,NA,NA)
                        )
                       )
    }

    orders
  }



# Train and test periods
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)

# getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date,'/',sep='')][1:len.ts]

# learning the model and obtaining its signal predictions
library(e1071)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)

# now using the simulated trader
t1 <- trading.simulator(market,sig,
             'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))



t1
summary(t1)


tradingEvaluation(t1)


plot(t1,market,theme='white',name='SP500')



t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)


start <- 2000
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)




###################################################
### Model Evaluation and Selection
###################################################
MC.svmR <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(e1071)
  t <- svm(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.svmC <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[,tgtName] <- trading.signals(train[,tgtName],b.t,s.t)
  t <- svm(form,train,...)
  p <- predict(t,test)
  factor(p,levels=c('s','h','b'))
}
MC.nnetR <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(nnet)
  t <- nnet(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.nnetC <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(nnet)
  tgtName <- all.vars(form)[1]
  train[,tgtName] <- trading.signals(train[,tgtName],b.t,s.t)
  t <- nnet(form,train,...)
  p <- predict(t,test,type='class')
  factor(p,levels=c('s','h','b'))
}
MC.earth <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(earth)
  t <- earth(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
singleModel <- function(form,train,test,learner,policy.func,...) {
  p <- do.call(paste('MC',learner,sep='.'),list(form,train,test,...))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
slide <- function(form,train,test,learner,relearn.step,policy.func,...) {
  real.learner <- learner(paste('MC',learner,sep='.'),pars=list(...))
  p <- slidingWindowTest(real.learner,form,train,test,relearn.step)
  p <- factor(p,levels=1:3,labels=c('s','h','b'))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
grow <- function(form,train,test,learner,relearn.step,policy.func,...) {
  real.learner <- learner(paste('MC',learner,sep='.'),pars=list(...))
  p <- growingWindowTest(real.learner,form,train,test,relearn.step)
  p <- factor(p,levels=1:3,labels=c('s','h','b'))
  eval.stats(form,train,test,p,policy.func=policy.func)
}


eval.stats <- function(form,train,test,preds,b.t=0.1,s.t=-0.1,...) {
  # Signals evaluation
  tgtName <- all.vars(form)[1]
  test[,tgtName] <- trading.signals(test[,tgtName],b.t,s.t)
  st <- sigs.PR(preds,test[,tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec','rec'),each=3),
                     c('s','b','sb'),sep='.')
  
  # Trading evaluation
  date <- rownames(test)[1]
  market <- GSPC[paste(date,"/",sep='')][1:length(preds),]
  trade.res <- trading.simulator(market,preds,...)

  c(st,tradingEvaluation(trade.res))
}


pol1 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.025,max.loss=0.05,hold.time=10)

pol2 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.05,max.loss=0.05,hold.time=20)

pol3 <- function(signals,market,op,money)
  policy.2(signals,market,op,money,
           bet=0.5,exp.prof=0.05,max.loss=0.05)


# The list of learners we will use
TODO <- c('svmR','svmC','earth','nnetR','nnetC')

# The data sets used in the comparison
DSs <- list(dataset(Tform,Tdata.train,'SP500'))

# Monte Carlo (MC) settings used
MCsetts <- mcSettings(20,     # 20 repetitions of the MC exps
                      2540,   # ~ 10 years for training
                      1270,   # ~ 5 years for testing
                      1234)   # random number generator seed

# Variants to try for all learners
VARS <- list()
VARS$svmR   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$svmC   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$earth <- list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001),
                   policy.func=c('pol1','pol2','pol3'))
VARS$nnetR  <- list(linout=T,maxit=750,size=c(5,10),
                    decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))
VARS$nnetC  <- list(maxit=750,size=c(5,10),decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))

# main loop
for(td in TODO) {
  assign(td,
         experimentalComparison(
           DSs,         
           c(
             do.call('variants',
                     c(list('singleModel',learner=td),VARS[[td]],
                       varsRootName=paste('single',td,sep='.'))),
             do.call('variants',
                     c(list('slide',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('slide',td,sep='.'))),
             do.call('variants',
                     c(list('grow',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('grow',td,sep='.')))
             ),
            MCsetts)
         )

  # save the results
  save(list=td,file=paste(td,'Rdata',sep='.'))
}


load('svmR.Rdata')
load('svmC.Rdata')
load('earth.Rdata')
load('nnetR.Rdata')
load('nnetC.Rdata')



tgtStats <- c('prec.sb','Ret','PercProf',
              'MaxDD','SharpeRatio')
allSysRes <- join(subset(svmR,stats=tgtStats),
                  subset(svmC,stats=tgtStats),
                  subset(nnetR,stats=tgtStats),
                  subset(nnetC,stats=tgtStats),
                  subset(earth,stats=tgtStats),
                  by = 'variants')
rankSystems(allSysRes,5,maxs=c(T,T,T,F,T))


summary(subset(svmC,
               stats=c('Ret','RetOverBH','PercProf','NTrades'),
               vars=c('slide.svmC.v5','slide.svmC.v6')))


fullResults <- join(svmR,svmC,earth,nnetC,nnetR,by='variants')
nt <- statScores(fullResults,'NTrades')[[1]]
rt <- statScores(fullResults,'Ret')[[1]]
pp <- statScores(fullResults,'PercProf')[[1]]
s1 <- names(nt)[which(nt > 20)]
s2 <- names(rt)[which(rt > 0.5)]
s3 <- names(pp)[which(pp > 40)]
namesBest <- intersect(intersect(s1,s2),s3)


compAnalysis(subset(fullResults,
                    stats=tgtStats,
                    vars=namesBest))


plot(subset(fullResults,
            stats=c('Ret','PercProf','MaxDD'),
            vars=namesBest))


getVariant('single.nnetR.v12',nnetR)







###################################################
### The Trading System
###################################################
data <- tail(Tdata.train,2540)
results <- list()
for(name in namesBest) {
  sys <- getVariant(name,fullResults)
  results[[name]] <- runLearner(sys,Tform,data,Tdata.eval)
}
results <- t(as.data.frame(results))




results[,c('Ret','RetOverBH','MaxDD','SharpeRatio','NTrades','PercProf')]


getVariant('grow.nnetR.v12',fullResults)


model <- learner('MC.nnetR',list(maxit=750,linout=T,trace=F,size=10,decay=0.001))
preds <- growingWindowTest(model,Tform,data,Tdata.eval,relearn.step=120)
signals <- factor(preds,levels=1:3,labels=c('s','h','b'))
date <- rownames(Tdata.eval)[1]
market <- GSPC[paste(date,"/",sep='')][1:length(signals),]
trade.res <- trading.simulator(market,signals,policy.func='pol2')


plot(trade.res,market,theme='white',name='SP500 - final test')


library(PerformanceAnalytics)
rets <- Return.calculate(trade.res@trading$Equity)


chart.CumReturns(rets,main='Cumulative returns of the strategy',ylab='returns')


yearlyReturn(trade.res@trading$Equity)


plot(100*yearlyReturn(trade.res@trading$Equity),
     main='Yearly percentage returns of the trading system')
abline(h=0,lty=2)


table.CalendarReturns(rets)


table.DownsideRisk(rets)


