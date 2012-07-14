hit_miss2 <- function(ftn, a, b, c, d, n) {
  # Monte-Carlo integration using the hit & miss method
  # vectorised version
  X <- runif(n, a, b)
  Y <- runif(n, c, d)
  Z <- (Y <= sapply(X, ftn))
  I <- (b - a)*c + (cumsum(Z)/(1:n))*(b - a)*(d - c)
  plot(1:n, I, type = "l")
  return(I[n])
}
