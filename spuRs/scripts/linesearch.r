# Program spuRs/resources/scripts/linesearch.r

source("../scripts/gsection.r")

line.search <- function(f, x, y, tol = 1e-9, a.max = 2^5) {
    # f is a real function that takes a vector of length d
    # x and y are vectors of length d
    # line.search uses gsection to find a >= 0 such that
    #   g(a) = f(x + a*y) has a local maximum at a,
    #   within a tolerance of tol
    # if no local max is found then we use 0 or a.max for a
    # the value returned is x + a*y

    if (sum(abs(y)) == 0) return(x) # g(a) constant

    g <- function(a) return(f(x + a*y))

    # find a triple a.l < a.m < a.r such that
    # g(a.l) <= g(a.m) and g(a.m) >= g(a.r)
    # a.l
    a.l <- 0
    g.l <- g(a.l)
    # a.m
    a.m <- 1
    g.m <- g(a.m)
    while ((g.m < g.l) & (a.m > tol)) {
        a.m <- a.m/2
        g.m <- g(a.m)
    }
    # if a suitable a.m was not found then use 0 for a
    if ((a.m <= tol) & (g.m < g.l)) return(x)
    # a.r
    a.r <- 2*a.m
    g.r <- g(a.r)
    while ((g.m < g.r) & (a.r < a.max)) {
        a.m <- a.r
        g.m <- g.r
        a.r <- 2*a.m
        g.r <- g(a.r)
    }
    # if a suitable a.r was not found then use a.max for a
    if ((a.r >= a.max) & (g.m < g.r)) return(x + a.max*y)

    # apply golden-section algorithm to g to find a
    a <- gsection(g, a.l, a.r, a.m)
    return(x + a*y)
}
