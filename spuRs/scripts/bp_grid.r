# program spuRs/resources/scripts/bp_grid.r

bp.sim <- function(gen, rv.sim, ...) {
  # population of a branching process at generation gen
  # rv.sim(n, ...) simulates n rv's from the offspring distribution
  Z <- 1
  for (i in 1:gen) {
    if (Z > 0) {
      Z <- sum(rv.sim(Z, ...))
    }
  }
  return(Z)
}


# set parameter values
gen <- 50
size <- 2
prob <- seq(0.3, 0.6, by = 0.01)
n.reps <- 100 # sample size for estimating E Z

# estimate E Z for each value of prob
mu <- rep(0, length(prob))
Z.mean <- rep(0, length(prob))
for (i in 1:length(prob)) {
  Z.sum <- 0
  for (k in 1:n.reps) {
    Z.sum <- Z.sum + bp.sim(gen, rbinom, size, prob[i])
  }
  mu[i] <- size*prob[i]
  Z.mean[i] <- Z.sum/n.reps
}

# plot estimates
# note that values of log(0) (= -infinity) are not plotted
plot(mu, log(Z.mean), type = "o",
     xlab = "E family size", ylab = paste("log pop at gen", gen))
