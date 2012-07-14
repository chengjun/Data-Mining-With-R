# program spuRs/resources/scripts/bisection.r
# loadable spuRs function

bisection <- function(ftn, x.l, x.r, tol = 1e-9) {
  # applies the bisection algorithm to find x such that ftn(x) == 0
  # we assume that ftn is a function of a single variable
  #
  # x.l and x.r must bracket the fixed point, that is
  # x.l < x.r and ftn(x.l) * ftn(x.r) < 0
  #
  # the algorithm iteratively refines x.l and x.r and terminates when
  # x.r - x.l <= tol

  # check inputs
  if (x.l >= x.r) {
    cat("error: x.l >= x.r \n")
    return(NULL)
  } 
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  if (f.l == 0) {
    return(x.l)
  } else if (f.r == 0) {
    return(x.r)
  } else if (f.l * f.r > 0) {
    cat("error: ftn(x.l) * ftn(x.r) > 0 \n")
    return(NULL)
  }

  # successively refine x.l and x.r
  n <- 0
  while ((x.r - x.l) > tol) {
    x.m <- (x.l + x.r)/2
    f.m <- ftn(x.m)
    if (f.m == 0) {
      return(x.m)
    } else if (f.l * f.m < 0) {
      x.r <- x.m
      f.r <- f.m
    } else {
      x.l <- x.m
      f.l <- f.m
    }
    n <- n + 1
    cat("at iteration", n, "the root lies between", x.l, "and", x.r, "\n")
  }

  # return (approximate) root
  return((x.l + x.r)/2)
}
