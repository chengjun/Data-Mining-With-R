# program spuRs/resources/scripts/seed-test.r

# set up two plots side-by-side
par(las=1, mfrow=c(1,2), mar=c(4,5,0,2))

# graph f_R and f_T on the LHS plot
curve(dgamma(x, shape=2, rate=1/2), from=0, to=20,
      ylim=c(0, dexp(0, rate=1/2)), lty=2,
      xlab="r", ylab=expression(paste(f[T](r), " and ", f[R](r))))
curve(dexp(x, rate=1/2), add=TRUE)
abline(h=0, col="grey")

# generate T, U, and S samples for case 1
T <- rexp(1000000, rate=1/2)
U <- runif(1000000, min=0, max=20)
S <- T[T > U]

# graph estimate of f_S and f_R on the RHS plot
hist(S, breaks=seq(0, max(S)+0.5, 0.5), freq=FALSE,
     xlim=c(0,20), ylim=c(0, dexp(0, rate=1/2)),
     main="", xlab="r",
     ylab=expression(paste(f[R](r), " and ", hat(f)[S](r))),
     col="lightgrey", border="darkgrey")
curve(dgamma(x, shape=2, rate=1/2), add=TRUE)
