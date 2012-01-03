# Visualizing distribution in R
# chengjun @ lab
# 20120103

#~~~~~~~~~~~~~~~~Distributions in R~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~Distribution~~~~Base name~Parameters
# beta	      beta	shape1, shape2
# binomial	      binom	size, prob
# Cauchy	      cauchy	location, scale
# chi-squared	chisq	df
# exponential	exp	rate
# F	            f	df1, df2
# gamma	      gamma	shape, rate
# geometric	      geom	p
# hypergeometric	hyper	m, n, k
# log-normal	lnorm	meanlog, sdlog
# logistic	      logis	location, scale
# negative        binomial	nbinom	size, prob
# normal	      norm	mean, sd
# Poisson	      pois	lambda
# Student-t	      t	df
# uniform	      unif	min, max
# Weibull	      weibull	shape, scale
#~~~~~~~~~~~~~~~4. The uniform distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
x=seq(1,5,length=200)
y=rep(1/4,200) # y=dunif(x,min=1,max=5)
plot(x,y,type="l",xlim=c(0,6),ylim=c(0,0.5),lwd=2,col="red",ylab="p")
polygon(c(1,x,5),c(0,y,0),col="lightgray",border=NA)
lines(x,y,type="l",lwd=2,col="red")
#~~~~~~~~~~~~1. Normal distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# refer to http://msenux.redwoods.edu/math/R/normal.php
x=seq(-8,8,length=500)
y=1/sqrt(2*pi)*exp(-x^2/2) # normal distribution function with mean=0 & sd=1
plot(x,y,type="l",lwd=2,col="red")

u=0; 考=2
y1=1/(考*sqrt(2*pi))*exp(-(x-u)^2/(2*考^2)) 
lines(x,y1,type="l",lwd=2,col="green")

u=0; 考=3
y2=1/(考*sqrt(2*pi))*exp(-(x-u)^2/(2*考^2)) 
lines(x,y2,type="l",lwd=2,col="blue")

# actually, we can use the built-in function of R.
x=seq(-8,8,length=500)
y1=dnorm(x,mean=0,sd=1)
plot(x,y1,type="l",lwd=2,col="red")
y2=dnorm(x,mean=0,sd=2)
lines(x,y2,type="l",lwd=2,col="green")
y3=dnorm(x,mean=0,sd=3)
lines(x,y3,type="l",lwd=2,col="blue")


x=seq(0,100,length=100)
y=dnorm(x,mean=50,sd=10)
plot(x,y,type="l",lwd=2,col="red")
x=seq(60,80,length=100)
y=dnorm(x,mean=50,sd=10)
polygon(c(60,x,80),c(0,y,0),col="lightgray")

#~~~~~~~~~~~~~~~5. exponential distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
x=seq(0,4,length=200)
y=dexp(x,rate=1)
# 竹=1 ; y1=竹*exp(-竹*x)
plot(x,y,type="l",lwd=8,col="red",ylab="p")
lines(x,y1,type="l",lwd=2,col="blue")
#~~~~~~~~~~~~gamma distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 忙(x) = integral_0^Inf t^(x-1) exp(-t) dt  
curve(dgamma(x, scale=1.5, shape=2),from=0, to=15, main="Gamma distribution")

## gamma has 1st order poles at 0, -1, -2, ...
## this will generate loss of precision warnings, so turn off
op <- options("warn")
options(warn = -1)
x <- sort(c(seq(-3,4, length.out=201), outer(0:-3, (-1:1)*1e-6, "+")))
plot(x, gamma(x), ylim=c(-20,20), col="red", type="l", lwd=2,
     main=expression(Gamma(x)))
abline(h=0, v=-3:0, lty=3, col="midnightblue")
options(op)

#~~~~~~~~~~~~1. beta distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# B(a,b) = 忙(a)忙(b)/忙(a+b) # beta distribution is defined based on gamma distribution
#~~~~~~~~~~~~~~cauchy distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
plot(dcauchy(-1:40), type='l')

#~~~~~~~~~~~~~~~2. Bournulli Distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
x=c(0,1)
y=c(0.3,0.7)
plot(x,y,type="h",xlim=c(-1,2),ylim=c(0,1),lwd=2,col="blue",ylab="p")
points(x,y,pch=16,cex=2,col="dark red")
#~~~~~~~~~~~~~~~3. Binomial Distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
n=15 # n is the size, the "number of trials," e.g. the number of tosses.
p=1/3 # prob is the probability of success, e.g. probability of obtaining "heads".
x=0:15 # x is the value of the random variable, e.g. the number of heads.

p=dbinom(x,size=n,prob=p)
plot(x,p,type="h",xlim=c(-1,11),ylim=c(0,0.5),lwd=2,col="blue",ylab="p")
points(x,p,pch=16,cex=2,col="dark red")
#~~~~~~~~weibull distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# f(x) = (a/b) (x/b)^(a-1) exp(- (x/b)^a)
curve(dweibull(x, scale=2.5, shape=1.5),from=0, to=15, main="Weibull distribution")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hist(x=rnorm(500,mean=100,sd=10),prob=TRUE,ylim=c(0,0.04))
curve(dnorm(x,mean=100,sd=10),70,130,add=TRUE,lwd=2,col="red")

mu=100; sigma=10
n=10
xbar=rep(0,500)
for (i in 1:500) {xbar[i]=mean(rnorm(n,mean=mu,sd=sigma))}
hist(xbar,prob=TRUE,breaks=12,xlim=c(70,130),ylim=c(0,0.1))
# What if the Parent Distribution is not Normal?
lambda=1
n=10
xbar=rep(0,500)
for (i in 1:500) { xbar[i]=mean(rexp(n,rate=1))}
hist(xbar,prob=TRUE,breaks=12)
# 

x=c(1,2,3,4,5,6)
p=c(0.1,0.1,0.1,0.1,0.2,0.4)
plot(x,p,type="h",lwd=2,col="red",ylim=c(0,0.5))
points(x,p,pch=16,cex=2,col="black")

n=5
xbar=rep(0,500)
for (i in 1:500) {
	xbar[i]=mean(sample(x,size=n,replace=TRUE,prob=p))
}
hist(xbar,prob=TRUE,breaks=12)


?ks.test
x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)

























