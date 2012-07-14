# program: spuRs/resources/scripts/discrete_queue.r
# Discrete Queue Simulation

# inputs
lambda <- 1     # arrival rate
mu <- 1.1       # service rate
t.end <- 100    # duration of simulation
t.step <- 0.05  # time step
rand.seed <- 99 # seed for random number generator

# simulation
set.seed(rand.seed)
queue <- rep(0, t.end/t.step + 1)
for (i in 2:length(queue)) {
  if (runif(1) < lambda*t.step) { # arrival
    queue[i] <- queue[i-1] + 1
  } else if (runif(1) < mu*t.step) { # potential departure
    queue[i] <- max(0, queue[i-1] - 1)
  } else { # nothing happens
    queue[i] <- queue[i-1]
  }
}

# output
plot(seq(from=0, to=t.end, by=t.step), queue, type='l', 
    xlab='time', ylab='queue size')
title(paste('Queuing Simulation. Arrival rate:', lambda,
    'Service rate:', mu))
