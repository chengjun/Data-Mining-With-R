# program: spuRs/resources/scripts/pension.r
# Forecast pension growth under compound interest

# clear the workspace
rm(list=ls())

# Inputs
r <- 0.11             # Annual interest rate
term <- 10            # Forecast duration (in years)
period <- 1/12        # Time between payments (in years)
payments <- 100       # Amount deposited each period

# Calculations
n <- floor(term/period)  # Number of payments
pension <- 0
for (i in 1:n) {
    pension[i+1] <- pension[i]*(1 + r*period) + payments
}
time <- (0:n)*period

# Output
plot(time, pension)
