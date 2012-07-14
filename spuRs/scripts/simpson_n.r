#program spuRs/resources/scripts/simpson_n.r

simpson_n <- function(ftn, a, b, n = 100) {
  # numerical integral of ftn from a to b
  # using Simpson's rule with n subdivisions
  #
  # ftn is a function of a single variable
  # we assume a < b and n is a positive even integer

  n <- max(c(2*(n %/% 2), 4))
  h <- (b-a)/n
  x.vec1 <- seq(a+h, b-h, by = 2*h)
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
  f.vec1 <- sapply(x.vec1, ftn)
  f.vec2 <- sapply(x.vec2, ftn)
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2))
  return(S)
}
