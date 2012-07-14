# program spuRs/resources/scripts/SIR_grid.r
# discrete SIR epidemic model
#
# initial susceptible population N
# initial infected population 1
# infection probability a
# removal probability b
#
# estimates expected final population size for different values of
# the infection probability a and removal probability b
# we observe a change in behaviour about the line Na = b
# (Na is the expected number of new infected at time 1 and
# b is the expected number of infected who are removed at time 1)

SIR <- function(a, b, N, T) {
  # simulates SIR epidemic model from time 0 to T
  # returns number of susceptibles, infected and removed at time T
  S <- N
  I <- 1
  R <- 0
  for (i in 1:T) {
    S <- rbinom(1, S, (1 - a)^I)
    R <- R + rbinom(1, I, b)
    I <- N + 1 - S - R
  }
  return(c(S, I, R))
}

# set parameter values
N <- 1000
T <- 100
a <- seq(0.0001, 0.001, by = 0.0001)
b <- seq(0.1, 0.5, by = 0.05)

n.reps <- 400 # sample size for estimating E S[T]
f.name <- "SIR_grid.dat" # file to save simulation results

# estimate E S[T] for each combination of a and b
write(c("a", "b", "S_T"), file = f.name, ncolumns = 3)
for (i in 1:length(a)) {
  for (j in 1:length(b)) {
    S.sum <- 0
    for (k in 1:n.reps) {
      S.sum <- S.sum + SIR(a[i], b[j], N, T)[1]
    }
    write(c(a[i], b[j], S.sum/n.reps), file = f.name, 
      ncolumns = 3, append = TRUE)
  }
}

# plot estimates in 3D
g <- read.table(f.name, header = TRUE)
library(lattice)
print(wireframe(S_T ~ a*b, data = g, scales = list(arrows = FALSE),
                aspect = c(.5, 1), drape = TRUE,
                xlab = "a", ylab = "b", zlab = "E S[T]"))


