add_event <- function(event.list, new.event) {
  # add new.event to event.list
  N <- length(event.list)
  if (N == 0) return(list(new.event))
  # find position n of new.event
  n <- 1
  while ((n <= N) && (new.event$time > event.list[[n]]$time)) {
    n <- n + 1
  }
  # add new.event to event.list
  if (n == 1) {
    event.list <- c(list(new.event), event.list)
  } else if (n == N + 1) {
    event.list <- c(event.list, list(new.event))
  } else {
    event.list <- c(event.list[1:(n-1)], list(new.event), event.list[n:N])
  }
  return(event.list)
}
