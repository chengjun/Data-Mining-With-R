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

hist(x$x,
   probability = TRUE, # In stead of frequency
   breaks = "FD",      # For more breaks than the default
   col = "darkslategray4", border = "seashell3",
   xlab="x", main = paste("Histogram of" , "x, The real PDF"))
lines(density(x$x - 0.5),   # Add the kernel density estimate (-.5 fix for the bins)
   col = "firebrick2", lwd = 3)
plot(x$x, x$p, xlab="x", ylab="PDF", main="Figure of Fake PDF")
plot(x$x, x$cp, xlab="x", ylab="CDF", main="CDF")

