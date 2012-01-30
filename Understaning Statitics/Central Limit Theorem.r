#~~~~~~~~~~~~~~sample mean from normal distribution~~~~~~~~~~~~~~~~~~~~~#
mu=100; sigma=10
n=10
xbar=rep(0,500)
for (i in 1:500) {xbar[i]=mean(rnorm(n,mean=mu,sd=sigma))}
hist(xbar,prob=TRUE,breaks=12,xlim=c(90,110),ylim=c(0,0.2))
#~~~~~~~~~~~~~~~~~~sample mean from an arbitary distribution~~~~~~~~~~~#
x = c(1,3,5)
px = c(.6,.3,.1)
draws1 = sample(x,size=500,replace=TRUE,prob=px)

hist(draws1,breaks=seq(1,5,by=.25),main="1000 discrete draws")
var(draws1)

draws2 = sample(x,size=4*500,replace=TRUE,prob=px)
draws2 = matrix(draws2,4)
drawmeans2 = apply(draws2,2,mean)
hist(drawmeans2,breaks=seq(1,5,by=.25),main="1000 means of 4 draws")

drawmeans3 =
apply(matrix(sample(x,size=16*500,replace=TRUE,prob=px),16),2,mean)
hist(drawmeans3,breaks=seq(1,5,by=.25),main="1000 means of 16 draws")


draws3 = matrix(rnorm(1000*6,0,3),6)
drawvar3 = apply(draws3,2,var)

draws3 =   5 * drawvar3 / 9
hist(draws3,breaks=20,prob=TRUE,main="standard distribution for sample variance")
v = seq(0,max(draws3),length=200)
lines(v,dchisq(v,5),lty = 2,lwd = 2)

n = 18
pop.var = 90
value = 160
pchisq((n-1)*value/pop.var,n-1)


pop.var = 0.0016
n = 9
prob = 0.99
pop.var * qchisq(prob,n-1)/(n-1)

#~~~~~~~~~~~~~~~~big number law~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
numsim = 100
matx = matrix(rnorm(numsim^2), nrow=numsim)

runavg = function(x) { cumsum(x)/(1:length(x)) }
# runavg calculates the running average of a vector 
ramatx = t(apply(matx, 1, runavg))
# apply() we can get the running average of each row.

plot(x=1:numsim, y = ramatx[,1], type="n",
  xlab="number of observations", ylab="running mean")

rapoints = function(x) points(x~seq(1:length(x)), pch=20, cex=0.2)

apply(ramatx,1,rapoints)

plot(x=1:numsim, y = ramatx[,1], type="n",
  xlab="number of observations", ylab="running mean")
ralines = function(x) lines(x~seq(1:length(x)))
apply(ramatx, 1, ralines)

#~~~~~illustration of the Central Limit Theorem by convolution~~~~~~~~~~~#
H<- function(x) {  ifelse(x>0,1,0) }
H2 <- convolve( H(x), rev(H(x)),        type = "open"   )
H4 <- convolve(H2, rev(H2),   type = "open"   )
H8 <- convolve(H4, rev(H4),   type = "open"   )
H16 <- convolve(H8, rev(H8),   type = "open"   )
par(mfrow=c(1,1))
plot(H2)
plot(H4)
plot(H8)
plot(H16)


