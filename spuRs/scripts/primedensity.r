# spuRs/resources/scripts/primedensity.r
# estimate the density of primes (using a very inefficient algorithm)

# clear the workspace
rm(list=ls())

prime <- function(n) {
    # returns TRUE if n is prime
    # assumes n is a positive integer
    if (n == 1) {
        is.prime <- FALSE
    } else if (n == 2) {
        is.prime <- TRUE
    } else {
        is.prime <- TRUE
        for (m in 2:(n/2)) {
            if (n %% m == 0) is.prime <- FALSE
        }
    }
    return(is.prime)
}

# input
# we consider primes <= n
n <- 1000

# calculate the number of primes <= m for m in 2:n
# num.primes[i] == number of primes <= i+1
m.vec <- 2:n
primes <- sapply(m.vec, prime)
num.primes <- cumsum(primes)

# output
# plot the actual prime density against the theoretical limit
par(mfrow = c(1, 2))
plot(m.vec, num.primes/m.vec, type = "l",
    main = "prime density", xlab = "n", ylab = "")
lines(m.vec, 1/log(m.vec), col = "red")

plot(m.vec, num.primes/m.vec*log(m.vec), type = "l",
    main = "prime density * log(n)", xlab = "n", ylab = "")
par(mfrow = c(1, 1))
