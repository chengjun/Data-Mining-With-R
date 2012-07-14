# program: spuRs/resources/scripts/threexplus1.r
x <- 3
for (i in 1:3) {
  show(x)
  cat("i = ", i, "\n")
  if (x %% 2 == 0) {
    x <- x/2
  } else {
    x <- 3*x + 1
  }
}
show(x)
