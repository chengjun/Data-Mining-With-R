# Program spuRs/resources/scripts/change.r

change <- function(x, y.vec = c()) {
  # finds possible ways of making up amount x using Australian coins
  # x is given in cents and we assume it is divisible by 5
  # y.vec are coins already used (so total amount is x + sum(y.vec))
  if (x == 0) {
    cat(y.vec, "\n")
  } else {
    coins <- c(200, 100, 50, 20, 10, 5)
    new.x <- x - coins
    new.x <- new.x[new.x >= 0]
    for (z in new.x) {
      y.tmp <- c(y.vec, x - z)
      if (identical(y.tmp, sort(y.tmp))) {
        change(z, y.tmp)
      }
    }
  }
  return(invisible(NULL))
}
