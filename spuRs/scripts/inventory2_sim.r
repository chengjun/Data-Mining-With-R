# program spuRs/resources/scripts/inventory2_sim.r

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
t <- 0
stock <- r
costs <- 0
event.list <- list(list(type = "purchase", time = rexp(1, rate = D)))
event.list <- add_event(event.list, list(type = "new stock", time = L))
# initialise data collection
total.cycles <- 1000
n.cycles <- 0
cost.per.unit.time <- rep(0, total.cycles)
t.cycle.start <- 0
costs.cycle.start <- 0
# simulation
while (n.cycles < total.cycles) {
  # get next event
  t.old <- t
  current.event <- event.list[[1]]
  event.list <- event.list[-1]
  # update state and event list according to type of current event
  if (current.event$type == "purchase") {
    # update system state
    t <- current.event$time
    if (stock > 0) {
      costs <- costs + h*stock*(t - t.old)
      stock <- stock - 1
    } else {
      costs <- costs + s
    }
    # generate next purchase
    new.event <- list(type = "purchase", time = t + rexp(1, rate = D))
    event.list <- add_event(event.list, new.event)
    # check for end of cycle
    if (stock == r) {
      # order more stock
      new.event <- list(type = "new stock", time = t + L)
      event.list <- add_event(event.list, new.event)
      costs <- costs + K + q*p
      # data collection
      n.cycles <- n.cycles + 1
      cost.per.unit.time[n.cycles] <- (costs - costs.cycle.start)/(t - t.cycle.start)
      t.cycle.start <- t
      costs.cycle.start <- costs
    }
  } else if (current.event$type == "new stock") {
    # update system state
    t <- current.event$time
    costs <- costs + h*stock*(t - t.old)
    stock <- stock + q
  }
}

show(mean(cost.per.unit.time))
show(mean(cost.per.unit.time) - 1.96*sd(cost.per.unit.time)/sqrt(total.cycles))
show(mean(cost.per.unit.time) + 1.96*sd(cost.per.unit.time)/sqrt(total.cycles))
