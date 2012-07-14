# program spuRs/resources/scripts/newtonraphson_show.r
# loadable spuRs function

newtonraphson_show <- function(ftn, x0, xmin = x0-1, xmax = x0+1) {
  # applies Newton-Raphson to find x such that ftn(x)[1] == 0
  # x0 is the starting point
  # subsequent iterations are plotted in the range [xmin, xmax]

  # plot the function
  x <- seq(xmin, xmax, (xmax - xmin)/200)
  fx <- c()
  for (i in 1:length(x)) {
    fx[i] <- ftn(x[i])[1]
  }
  plot(x, fx, type = "l", xlab = "x", ylab = "f(x)",
    main = "zero f(x) = 0", col = "blue", lwd = 2)
  lines(c(xmin, xmax), c(0, 0), col = "blue")

  # do first iteration
  xold <- x0
  f.xold <- ftn(xold)
  xnew <- xold - f.xold[1]/f.xold[2]
  lines(c(xold, xold, xnew), c(0, f.xold[1], 0), col = "red")

  # continue iterating while user types "y"
  cat("last x value", xnew, " ")
  continue <- readline("continue (y or n)? ") == "y"
  while (continue) {
    xold <- xnew;
    f.xold <- ftn(xold)
    xnew <- xold - f.xold[1]/f.xold[2]
    lines(c(xold, xold, xnew), c(0, f.xold[1], 0), col = "red")
    cat("last x value", xnew, " ")
    continue <- readline("continue (y or n)? ") == "y"
  }

  return(xnew)
}
