# Draw cdf by sorting
# Chengjun Wang & Lingfei Wu
# @ Hall 8
#~~~cdf and pdf~~~~#
x<-rnorm(1000,0, 1)
x<-as.data.frame(x)
x$x<-sort(x$x)
x$p<-1/1000
x$cp<-cumsum(x$p)
par(mfrow=c(3,1))
hist(x$x, xlab="x", main = paste("Histogram of" , "x"))
plot(x$x, x$p, xlab="x", ylab="PDF", main="Figure 1")
plot(x$x, x$cp, xlab="x", ylab="CDF", main="Figure 2")

