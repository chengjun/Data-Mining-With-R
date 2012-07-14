# program spuRs/resources/scripts/f3.r

f3 <- function(x) {
  a <- x[1]^2/2 - x[2]^2/4
  b <- 2*x[1] - exp(x[2])
  f <- sin(a)*cos(b)
  f1 <- cos(a)*cos(b)*x[1] - sin(a)*sin(b)*2
  f2 <- -cos(a)*cos(b)*x[2]/2 + sin(a)*sin(b)*exp(x[2])
  f11 <- -sin(a)*cos(b)*(4 + x[1]^2) + cos(a)*cos(b) -
      cos(a)*sin(b)*4*x[1]
  f12 <- sin(a)*cos(b)*(x[1]*x[2]/2 + 2*exp(x[2])) +
      cos(a)*sin(b)*(x[1]*exp(x[2]) + x[2])
  f22 <- -sin(a)*cos(b)*(x[2]^2/4 + exp(2*x[2])) - cos(a)*cos(b)/2 -
      cos(a)*sin(b)*x[2]*exp(x[2]) + sin(a)*sin(b)*exp(x[2])
  return(list(f, c(f1, f2), matrix(c(f11, f12, f12, f22), 2, 2)))
}

