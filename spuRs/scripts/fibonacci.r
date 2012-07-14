# program: spuRs/resources/scripts/fibonacci.r
# calculate the first Fibonacci number greater than 100

# clear the workspace
rm(list=ls())

# initialise variables
F <- c(1, 1) # list of Fibonacci numbers
n <- 2       # length of F

# iteratively calculate new Fibonacci numbers
while (F[n] <= 100) {
    # cat("n =", n, " F[n] =", F[n], "\n")
    n <- n + 1
    F[n] <- F[n-1] + F[n-2]
}

# output
cat("The first Fibonacci number > 100 is F(", n, ") =", F[n], "\n")
