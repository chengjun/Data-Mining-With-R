# goodness of fit for ergm
# 20120208

#~~~~~~~~~~~~~~~~~~~~~load data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dat<-read.table("d:/download/firmb.txt", sep="		", header=T)

#~~~~~~~~~~~~~~~~~sort the id~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# sort the values and assigns a unique id for each ID number
code=sort(unique(c(dat[,1],dat[,2]))) 
# match the IDs of column 1 in the edgelist to the unique IDs 
dat[,1]=match(dat[,1],code) 
# match the IDs of column 2 in the edgelist to the unique IDs 
dat[,2]=match(dat[,2],code) 
#~~~~~~~~~~~~~~~~ergm~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# library("statnet")
n=network(dat,vertex.attr=NULL, vertex.attrnames=NULL,matrix.type="edgelist", directed=F) 
# plot the network
plot(n) 
# creates an edgelist using the unique IDs
network.vertex.names(n)=code 
# Assigns the actual ID numbers as vertex names###
summary(n) # see the basic info of the network

m1<-ergm(n~edges+triangle)
summary(m1)

gest <- logLik(m1, add=TRUE,parallel=10)
# Deviances, AIC, and BIC now shown:
summary(gest)
mcmc.diagnostics(m1)
# show the exp() for the ERGM coefficients
data.frame(lapply(m1[1],exp))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~simulate~~~~~~~~~~~~~~~~~~~~~~~~#
m1.sim<-simulate(m1,nsim=10)#simulate 10 networks according to m1
m1.sim$networks[[1]];  m1.sim$networks[[2]]  
par(mfrow=c(1,2))
plot(n, main="real network")
plot(m1.sim$networks[[1]], main="simulated network")# select one of the network 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~goodness of fit~~~~~~~~~~~~~~~~~#
m1.gof<-gof(m1,GOF=~degree+distance+espartners+triadcensus,
 verbose=TRUE, interval=5e+4, seed=111)
par(mfrow=c(2,2))
plot(m1.gof)

## runing 100 simulaton to plot the gof, very time-consuming.
firmbgof <- gof(m1~degree)
plot(firmbgof)


