# program: spuRs/resources/scripts/quad1.r
# find the zeros of a2*x^2 + a1*x + a0 = 0

# clear the workspace
rm(list=ls())

# input
a2 <- 1
a1 <- 4
a0 <- 2

# calculation
root1 <- (-a1 + sqrt(a1^2 - 4*a2*a0))/(2*a2)
root2 <- (-a1 - sqrt(a1^2 - 4*a2*a0))/(2*a2)

# output
show(c(root1, root2))
