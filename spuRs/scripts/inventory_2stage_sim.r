# program spuRs/resources/scripts/inventory_2stage_sim.r

rm(list=ls())
set.seed(1939)

add.event <- function(event.list, new.event) {
  # add new.event to event.list
  N <- length(event.list)
  if (N == 0) return(list(new.event))
  # find position n of new.event
  n <- 1
  while ((n <= N) && (new.event$time > event.list[[n]]$time)) {
    n <- n + 1
  }
  # insert new.event
  if (n == 1) {
    event.list <- c(list(new.event), event.list)
  } else if (n == N + 1) {
    event.list <- c(event.list, list(new.event))
  } else {
    event.list <- c(event.list[1:(n-1)], list(new.event), event.list[n:N])
  }
  return(event.list)
}

# inputs
# system parameters
D <- 1000
L1 <- 0.01
L2 <- 0.1
K1 <- 100
K2 <- 1000
p <- 10
h1 <- 500
h2 <- 10
s <- 200
# control parameters
q1 <- 30
r1 <- 15
q2 <- 10*q1
r2 <- 3*q1
b <- 0.005

# initialise system and event list
n <- 0  # number of events so far
t <- 0  # time
stock.store <- r1
stock.depot <- r2
costs <- 0
event.list <- list(list(type = "purchase", time = rexp(1, rate = D)))
event.list <- add.event(event.list, list(type = "new stock store", time = L1))
event.list <- add.event(event.list, list(type = "new stock depot", time = L2))
# simulation
while (t[n+1] < 1) {
  # get next event
  current.event <- event.list[[1]]
  event.list <- event.list[-1]
  n <- n + 1
  # update state and event list according to type of current event
  if (current.event$type == "purchase") {
    # update system state
    t[n+1] <- current.event$time
    costs[n+1] <- costs[n] + h1*stock.store[n]*(t[n+1] - t[n])
                           + h2*stock.depot[n]*(t[n+1] - t[n])
    if (stock.store[n] > 0) {
      stock.store[n+1] <- stock.store[n] - 1
    } else {
      costs[n+1] <- costs[n+1] + s
      stock.store[n+1] <- stock.store[n]
    }
    stock.depot[n+1] <- stock.depot[n]
    # generate next purchase
    new.event <- list(type = "purchase", time = t[n+1] + rexp(1, rate = D))
    event.list <- add.event(event.list, new.event)
    # check stock levels
    if (stock.store[n+1] == r1) {
      # order more stock for store
      if (stock.depot[n+1] >= q1) {
        new.event <- list(type = "new stock store", time = t[n+1] + L1)
        event.list <- add.event(event.list, new.event)
        costs[n+1] <- costs[n+1] + K1
        stock.depot[n+1] <- stock.depot[n+1] - q1
        # order more stock for depot
        if (stock.depot[n+1] == r2) {
          new.event <- list(type = "new stock depot", time = t[n+1] + L2)
          event.list <- add.event(event.list, new.event)
          costs[n+1] <- costs[n+1] + K2 + p*q2
        }
      # depot out of stock
      } else {
        new.event <- list(type = "backlogged order", time = t[n+1] + b)
        event.list <- add.event(event.list, new.event)
      }
    }
  } else if (current.event$type == "new stock store") {
    # update system state
    t[n+1] <- current.event$time
    costs[n+1] <- costs[n] + h1*stock.store[n]*(t[n+1] - t[n])
                           + h2*stock.depot[n]*(t[n+1] - t[n])
    stock.store[n+1] <- stock.store[n] + q1
    stock.depot[n+1] <- stock.depot[n]
  } else if (current.event$type == "new stock depot") {
    # update system state
    t[n+1] <- current.event$time
    costs[n+1] <- costs[n] + h1*stock.store[n]*(t[n+1] - t[n])
                           + h2*stock.depot[n]*(t[n+1] - t[n])
    stock.store[n+1] <- stock.store[n]
    stock.depot[n+1] <- stock.depot[n] + q2
  } else if (current.event$type == "backlogged order") {
    # update system state
    t[n+1] <- current.event$time
    costs[n+1] <- costs[n] + h1*stock.store[n]*(t[n+1] - t[n])
                           + h2*stock.depot[n]*(t[n+1] - t[n])
    stock.store[n+1] <- stock.store[n]
    stock.depot[n+1] <- stock.depot[n]
    # order more stock for store
    if (stock.depot[n+1] >= q1) {
      new.event <- list(type = "new stock store", time = t[n+1] + L1)
      event.list <- add.event(event.list, new.event)
      costs[n+1] <- costs[n+1] + K1
      stock.depot[n+1] <- stock.depot[n+1] - q1
      # order more stock for depot
      if (stock.depot[n+1] == r2) {
        new.event <- list(type = "new stock depot", time = t[n+1] + L2)
        event.list <- add.event(event.list, new.event)
        costs[n+1] <- costs[n+1] + K2 + p*q2
      }
    # depot out of stock
    } else {
      new.event <- list(type = "backlogged order", time = t[n+1] + b)
      event.list <- add.event(event.list, new.event)
    }
  }
}

opar <- par(mfrow = c(3, 1), mar=c(4, 4, 1, 4), oma=c(0,0,4,0))
plot(t, stock.store, type = "s", ylim = c(0, max(stock.store)), ylab="Stock (Store)")
plot(t, stock.depot, type = "s", ylim = c(0, max(stock.depot)), ylab="Stock (Depot)")
plot(t, costs, type = "l", ylab="Costs")
par <- opar
