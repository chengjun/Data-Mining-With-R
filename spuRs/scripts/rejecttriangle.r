# program spuRs/resources/scripts/rejecttriangle.r

rejectionK <- function(fx, a, b, K) {
  # simulates from the pdf fx using the rejection algorithm
  # assumes fx is 0 outside [a, b] and bounded by K
  # note that we exit the infinite loop using the return statement
  while (TRUE) {
    x <- runif(1, a, b)
    y <- runif(1, 0, K)
    if (y < fx(x)) return(x)
  }
}

fx<-function(x){
  # triangular density
  if ((0<x) && (x<1)) {
    return(x)
  } else if ((1<x) && (x<2)) {
    return(2-x)
  } else {
    return(0)
  }
}

# generate a sample
set.seed(21)
nreps <- 3000
Observations <- rep(0, nreps)
for(i in 1:nreps)   {
  Observations[i] <- rejectionK(fx, 0, 2, 1)
}

# plot a scaled histogram of the sample and the density on top
hist(Observations, breaks = seq(0, 2, by=0.1), freq = FALSE,
     ylim=c(0, 1.05), main="")
lines(c(0, 1, 2), c(0, 1, 0))
