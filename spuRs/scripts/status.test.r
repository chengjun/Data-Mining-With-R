# Program spuRs/resources/scripts/status.test.r

status.test <- function(s.ftn) {
  x.vec <- (-1):11
  y.vec <- (-1):11
  plot(x.vec, y.vec, type = "n", xlab = "x", ylab = "y")
  for (x in x.vec) {
    for (y in y.vec) {
      s <- s.ftn(x, y)
      if (s == "impossible") text(x, y, "X", col = "red")
      else if (s == "unfinished") text(x, y, "?", col = "blue")
      else if (s == "player 1 win") text(x, y, "1", col = "green")
      else if (s == "player 2 win") text(x, y, "2", col = "green")
    }
  }
  return(invisible(NULL))
}
