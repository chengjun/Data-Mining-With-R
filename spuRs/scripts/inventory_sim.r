# program: spuRs/resources/scripts/inventory_sim.r

rm(list=ls())
set.seed(1939)
source("../scripts/add_event.r")

# inputs
# system parameters
D <- 1000
L <- 0.1
K <- 1000
p <- 100
h <- 100
s <- 200
# control parameters
q <- 146
r <- 115

# initialise system and event list
n <- 0  # number of events so far
t <- 0  # time
stock <- r
costs <- 0
event.list <- list(list(type = "purchase", time = rexp(1, rate = D)))
event.list <- add_event(event.list, list(type = "new stock", time = L))
# initialise stopping condition
time.to.stop <- FALSE
# simulation
while (!time.to.stop) {
  # get next event
  current.event <- event.list[[1]]
  event.list <- event.list[-1]
  n <- n + 1
  # update state and event list according to type of current event
  if (current.event$type == "purchase") {
    # update system state
    t[n+1] <- current.event$time
    if (stock[n] > 0) {  # reduce inventory, update holding costs
      costs[n+1] <- costs[n] + h*stock[n]*(t[n+1] - t[n])
      stock[n+1] <- stock[n] - 1
    } else {             # lost sale
      costs[n+1] <- costs[n] + s
      stock[n+1] <- stock[n]
    }
    # generate next purchase
    new.event <- list(type = "purchase", time = t[n+1] + rexp(1, rate = D))
    event.list <- add_event(event.list, new.event)
    # check for end of cycle
    if (stock[n+1] == r) {
      # order more stock
      new.event <- list(type = "new stock", time = t[n+1] + L)
      event.list <- add_event(event.list, new.event)
      costs[n+1] <- costs[n+1] + K + q*p
    }
  } else if (current.event$type == "new stock") {
    # update system state
    t[n+1] <- current.event$time
    costs[n+1] <- costs[n] + h*stock[n]*(t[n+1] - t[n])
    stock[n+1] <- stock[n] + q
  }
  # check stopping condition
  if (stock[n+1] == r) time.to.stop <- TRUE
}
