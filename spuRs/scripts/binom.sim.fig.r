# inputs
N <- 10000      # sample size
n <- 10         # rv parameters
p <- 0.7
set.seed(100)   # seed for RNG

# generate sample and estimate p
X <- rep(0, N)
for (i in 1:N) X[i] <- binom.sim(n, p)
phat <- rep(0, n+1)
for (i in 0:n) phat[i+1] <- sum(X == i)/N
phat.CI <- 1.96*sqrt(phat*(1-phat)/N)

# plot output
plot(0:n, dbinom(0:n, n, p), type="h", xlab="x", ylab="p(x)")
points(0:n, dbinom(0:n, n, p), pch=19)
points(0:n, phat, pch=3)
points(0:n, phat+phat.CI, pch=3)
points(0:n, phat-phat.CI, pch=3)
