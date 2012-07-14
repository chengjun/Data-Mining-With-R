# program spuRs/resources/scripts/fixedpoint.r
# loadable spuRs function

fixedpoint <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  # applies the fixed-point algorithm to find x such that ftn(x) == x
  # we assume that ftn is a function of a single variable
  #
  # x0 is the initial guess at the fixed point
  # the algorithm terminates when successive iterations are
  # within distance tol of each other,
  # or the number of iterations exceeds max.iter

  # do first iteration
  xold <- x0
  xnew <- ftn(xold)
  iter <- 1
  cat("At iteration 1 value of x is:", xnew, "\n")

  # continue iterating until stopping conditions are met
  while ((abs(xnew-xold) > tol) && (iter < max.iter)) {
    xold <- xnew;
    xnew <- ftn(xold);
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", xnew, "\n")
  }

  # output depends on success of algorithm
  if (abs(xnew-xold) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(xnew)
  }
}
