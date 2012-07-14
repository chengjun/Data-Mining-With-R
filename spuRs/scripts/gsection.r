# Program spuRs/resources/scripts/gsection.r

gsection <- function(ftn, x.l, x.r, x.m, tol = 1e-9) {
  # applies the golden-section algorithm to maximise ftn
  # we assume that ftn is a function of a single variable
  # and that x.l < x.m < x.r and ftn(x.l), ftn(x.r) <= ftn(x.m)
  #
  # the algorithm iteratively refines x.l, x.r, and x.m and terminates
  # when x.r - x.l <= tol, then returns x.m

  # golden ratio plus one
  gr1 <- 1 + (1 + sqrt(5))/2

  # successively refine x.l, x.r, and x.m
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  f.m <- ftn(x.m)
  while ((x.r - x.l) > tol) {
    if ((x.r - x.m) > (x.m - x.l)) {
      y <- x.m + (x.r - x.m)/gr1
      f.y <- ftn(y)
      if (f.y >= f.m) {
        x.l <- x.m
        f.l <- f.m
        x.m <- y
        f.m <- f.y
      } else {
        x.r <- y
        f.r <- f.y
      }
    } else {
      y <- x.m - (x.m - x.l)/gr1
      f.y <- ftn(y)
      if (f.y >= f.m) {
        x.r <- x.m
        f.r <- f.m
        x.m <- y
        f.m <- f.y
      } else {
        x.l <- y
        f.l <- f.y
      }
    }
  }
  return(x.m)
}
