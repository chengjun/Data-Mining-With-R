# program spuRs/resources/scripts/powers.r
# display powers 1 to n of x

# input
x <- 7
n <- 5

# display powers
cat("Powers of", x, "\n")
cat("exponent    result\n\n")

result <- 1
for (i in 1:n) {
  result <- result * x
  cat(format(i, width = 8),
      format(result, width = 10),
      "\n", sep = "")
}
