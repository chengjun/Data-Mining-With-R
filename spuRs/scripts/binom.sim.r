# program spuRs/resources/scripts/binom.sim.r

binom.sim <- function(n, p) {
  X <- 0
  px <- (1-p)^n
  Fx <- px
  U <- runif(1)
  while (Fx < U) {
    X <- X + 1
    px <- px*p/(1-p)*(n-X+1)/X
    Fx <- Fx + px
  }
  return(X)
}

