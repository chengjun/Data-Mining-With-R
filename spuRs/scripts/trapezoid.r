# program spuRs/resources/scripts/trapezoid.r

trapezoid <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using the trapezoid rule with n subdivisions
  #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive integer
  
  h <- (b-a)/n
  x.vec <- seq(a, b, by = h)
  f.vec <- sapply(x.vec, ftn)
  T <- h*(f.vec[1]/2 + sum(f.vec[2:n]) + f.vec[n+1]/2)
  return(T)
}
