# program spuRs/resources/scripts/swap.r

swap <- function(x) {
  # swap values of x[1] and x[2]
  y <- x[2]
  x[2] <- x[1]
  x[1] <- y
  return(x)
}

x <- c(7, 8, 9)
x[1:2] <- swap(x[1:2])
x[2:3] <- swap(x[2:3])
