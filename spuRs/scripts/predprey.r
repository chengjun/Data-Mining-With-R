# program spuRs/resources/scripts/predprey.r
# Lotka-Volterra predator-prey equations
br <- 0.04   # growth rate of rabbits
dr <- 0.0005 # death rate of rabbits due to predation
df <- 0.2    # death rate of foxes
bf <- 0.1    # efficiency of turning predated rabbits into foxes
x <- 4000
y <- 100
while (x > 3900) {
  # cat("x =", x, " y =", y, "\n")
  x.new <- (1+br)*x - dr*x*y
  y.new <- (1-df)*y + bf*dr*x*y
  x <- x.new
  y <- y.new
}
