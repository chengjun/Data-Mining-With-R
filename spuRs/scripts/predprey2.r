# programme: spuRs/resources/scripts/predprey2.r

T <- 500
a <- 0.04 # natural growth rate of rabbits in the absence of predation,
c <- 0.2 # natural death rate of foxes in the absence of food (rabbits),
b <- 0.0005 # death rate per encounter of rabbits due to predation,
e <- 0.1 # efficiency of turning predated rabbits into foxes.
x <- 4000
y <- 100
for (i in 1:T) {
  # cat("x =", x[i], " y =", y[i], "\n")
  x.new <- (1+a)*x[i] - b*x[i]*y[i]
  y.new <- (1-c)*y[i] + e*b*x[i]*y[i]
  x[i+1] <- x.new
  y[i+1] <- y.new
}
plot(x, y, type="l")
# text(x, y, 1:T)
