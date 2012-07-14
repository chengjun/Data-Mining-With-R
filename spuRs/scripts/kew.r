kew <- read.table("../data/kew.txt", col.names=c("year", "jan", "feb",
    "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
kew[,2:13] <- kew[,2:13]/10
kew.mean <- apply(kew[-1], 2, mean)
kew.var <- apply(kew[-1], 2, var)
lambda.mm <- kew.mean/kew.var
m.mm <- kew.mean^2/kew.var
hist(kew$jul, breaks=20, freq=FALSE, xlab="rainfall (mm)",
    ylab="density", main="July rainfall at Kew, 1697 to 1999")
t <- seq(0, 200, 0.5)
lines(t, dgamma(t, m.mm[7], lambda.mm[7]), lty=2)
x <- kew$jul
x[x == 0] <- 0.1
newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100, ...) {
  # find a root of ftn(x, ...) near x0 using Newton-Raphson
  # initialise
  x <- x0
  fx <- ftn(x, ...)
  iter <-  0
  # continue iterating until stopping conditions are met
  while ((abs(fx[1]) > tol) & (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    fx <- ftn(x, ...)
    iter <-  iter + 1
  }
  # output depends on success of algorithm
  if (abs(fx[1]) > tol) {
    stop("Algorithm failed to converge\n")
  } else {
    return(x)
  }
}
dl <- function(m, a) {
  return(c(log(m) - digamma(m) - a, 1/m - trigamma(m)))
}
m.ml <- newtonraphson(dl, m.mm[7], a = log(mean(x)) - mean(log(x)))
lambda.ml <- m.ml/mean(x)
lines(t, dgamma(t, m.ml, lambda.ml))
