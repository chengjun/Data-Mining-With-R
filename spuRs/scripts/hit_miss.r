# program spuRs/resources/scripts/hit_miss.r

hit_miss <- function(ftn, a, b, f.min, f.max, n) {
  # Monte-Carlo integration using the hit and miss method
  # ftn is a function of one variable
  # [a, b] is the range of integration
  # f.min and f.max are bounds on ftn over the range [a, b]
  # that is f.min <= ftn(x) <= f.max for all x in [a, b]
  # n is the number of samples used in the estimation
  # that is the number of calls made to the function ftn
  Z.sum <- 0
  for (i in 1:n) {
    X <- runif(1, a, b)
    Y <- runif(1, f.min, f.max)
    Z <- (ftn(X) >= Y)
    Z.sum <- Z.sum + Z
    # cat("X =", X, "Y =", Y, "Z =", Z, "Z.sum =", Z.sum, "\n")
  }
  I <- (b - a)*f.min + (Z.sum/n)*(b - a)*(f.max - f.min)
  return(I)
}


