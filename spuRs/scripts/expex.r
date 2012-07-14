# program: spuRs/resources/scripts/expex.r
#
# calculating the mean of a discrete rv X

x <- 1/(1000:1)              # possible values for X
pX <- x^1.5                  # probability mass ftn
pX <- pX/sum(pX)             # must have sum(pX) == 1
muX <- sum(x*pX)             # mean

# plot the pmf and mean
par(las=1)
plot(c(0, 1), c(0, max(pX)), type="n", xlab="x", ylab="p(x)")
lines(x, pX, type="h")
points(muX, 0, pch=19)
text(muX, 0, "mean", pos=4)
