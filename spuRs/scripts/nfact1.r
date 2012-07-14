# program: spuRs/resources/scripts/nfact1.r
# Calculate n factorial

# clear the workspace
rm(list=ls())

# Input
n <- 6

# Calculation
n_factorial <- 1
for (i in 1:n) {
    n_factorial <- n_factorial * i
}

# Output
show(n_factorial)
