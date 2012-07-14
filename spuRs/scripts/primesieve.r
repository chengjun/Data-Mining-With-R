# program spuRs/resources/scripts/primesieve.r
# loadable spuRs function

primesieve <- function(sieved, unsieved) {
  # finds primes using the Sieve of Eratosthenes
  # sieved: sorted vector of sieved numbers
  # unsieved: sorted vector of unsieved numbers

  # cat("sieved", sieved, "\n")
  # cat("unsieved", unsieved, "\n")
  p <- unsieved[1]
  n <- unsieved[length(unsieved)]
  if (p^2 > n) {
      return(c(sieved, unsieved))
  } else {
      unsieved <- unsieved[unsieved %% p != 0]
      sieved <- c(sieved, p)
      return(primesieve(sieved, unsieved))
  }
}
