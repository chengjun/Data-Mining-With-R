# program spuRs/resources/scripts/SIRsim.r

SIRsim <- function(a, b, N, T) {
  # Simulate an SIR epidemic
  # a is infection rate, b is removal rate
  # N initial susceptibles, 1 initial infected, simulation length T
  # returns a matrix size (T+1)*3 with columns S, I, R respectively
  S <- rep(0, T+1)
  I <- rep(0, T+1)
  R <- rep(0, T+1)
  S[1] <- N
  I[1] <- 1
  R[1] <- 0
  for (i in 1:T) {
    S[i+1] <- rbinom(1, S[i], (1 - a)^I[i])
    R[i+1] <- R[i] + rbinom(1, I[i], b)
    I[i+1] <- N + 1 - R[i+1] - S[i+1]
  }
  return(matrix(c(S, I, R), ncol = 3))
}
