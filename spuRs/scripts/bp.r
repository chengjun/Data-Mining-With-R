# Program spuRs/resources/scripts/bp.r
# branching process simulation

bp <- function(gen, rv.sim, ...) {
  # population of a branching process from generation 0 to gen
  # rv.sim(n, ...) simulates n rv's from the offspring distribution
  # Z[i] is population at generation i-1; Z[1] = 1
  Z <- rep(0, gen+1)
  Z[1] <- 1
  for (i in 1:gen) {
    if (Z[i] > 0) {
      Z[i+1] <- sum(rv.sim(Z[i], ...))
    }
  }
  return(Z)
}

bp.plot <- function(gen, rv.sim, ..., reps = 1, logplot = TRUE) {
  # simulates and plots the population of a branching process
  # from generation 0 to gen; rv.sim(n, ...) simulates n rv's
  # from the offspring distribution
  # the plot is repeated reps times
  # if logplot = TRUE then the population is plotted on a log scale
  # Z[i,j] is population at generation j-1 in the i-th repeat
  Z <- matrix(0, nrow = reps, ncol = gen+1)
  for (i in 1:reps) {
    Z[i,] <- bp(gen, rv.sim, ...)
  }
  if (logplot) {
    Z <- log(Z)
  }
  plot(c(0, gen), c(0, max(Z)), type = "n", xlab = "generation",
    ylab = if (logplot) "log population" else "population")
  for (i in 1:reps) {
    lines(0:gen, Z[i,])
  }
  return(invisible(Z))
}