# lab 2 task 1
# display a table of squares and cubes of the numbers 1 to n

# input
n <- 7

# display table
cat("  number  square    cube\n\n")
for (i in 1:n) {
	cat(format(i, width = 8), format(i^2, width = 8), format(i^3, width = 8), "\n", sep = "")
}
