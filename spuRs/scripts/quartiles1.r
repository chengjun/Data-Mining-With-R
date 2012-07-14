# program: spuRs/resources/scripts/quartiles1.r
# Calculate median and quartiles.

# Clear the workspace
rm(list=ls())

# Input
# We assume that the file file_name consists of numeric values
# separated by spaces and/or newlines
file_name = "../data/data1.txt"

# Read from file
data <- scan(file = file_name)

# Calculations
n <- length(data)
data.sort <- sort(data)
data.1qrt <- data.sort[ceiling(n/4)]
data.med <- data.sort[ceiling(n/2)]
data.3qrt <- data.sort[ceiling(3*n/4)]

# Output
cat("1st Quartile:", data.1qrt, "\n")
cat("Median:      ", data.med, "\n")
cat("3rd Quartile:", data.3qrt, "\n")
