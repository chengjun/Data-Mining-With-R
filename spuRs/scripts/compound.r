# program: spuRs/resources/scripts/compound.r
# Duration of a loan under compound interest

# clear the workspace
rm(list=ls())

# Inputs
r <- 0.11             # Annual interest rate
period <- 1/12        # Time between repayments (in years)
debt_initial <- 1000  # Amount borrowed
repayments <- 12      # Amount repaid each period

# Calculations
time <- 0
debt <- debt_initial
while (debt > 0) {
    time <- time + period
    debt <- debt*(1 + r*period) - repayments
}

# Output
cat('Loan will be repaid in', time, 'years\n')
