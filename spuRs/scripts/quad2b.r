# program spuRs/resources/scripts/quad2b.r
# find the zeros of a2*x^2 + a1*x + a0 = 0

# clear the workspace
rm(list=ls())

# input
cat("find the zeros of a2*x^2 + a1*x + a0 = 0\n")
a2 <- as.numeric(readline("a2 = "))
a1 <- as.numeric(readline("a1 = "))
a0 <- as.numeric(readline("a0 = "))

# calculate the discriminant
discrim <- a1^2 - 4*a2*a0
# calculate the roots depending on the value of the discriminant
if (discrim > 0) {
    roots <- (-a1 + c(1,-1) * sqrt(a1^2 - 4*a2*a0))/(2*a2)
} else {
    if (discrim == 0) {
        roots <- -a1/(2*a2)
    } else {
        roots <- c()
    }
}

# output
if (length(roots) == 0) {
    cat("no roots\n")
} else if (length(roots) == 1) {
    cat("single root at", roots, "\n")
} else {
    cat("roots at", roots[1], "and", roots[2], "\n")
}
