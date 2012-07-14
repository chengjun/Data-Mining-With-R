# Code spuRs/resources/scripts/newton_gamma.r

newton <- function(f3, x0, tol = 1e-9, n.max = 100) {
    # Newton's method for optimisation, starting at x0
    # f3 is a function that given x returns the vector
    # (f(x), f'(x), f''(x)), for some f

    x <- x0
    f3.x <- f3(x)
    n <- 0
    while ((abs(f3.x[2]) > tol) & (n < n.max)) {
        x <- x - f3.x[2]/f3.x[3]
        f3.x <- f3(x)
        n <- n + 1
    }
    if (n == n.max) {
        cat('newton failed to converge\n')
    } else {
        return(x)
    }
}

gamma.2.3 <- function(x) {
    # gamma(2,3) density
    if (x < 0) return(c(0, 0, 0))
    if (x == 0) return(c(0, 0, NaN))
    y <- exp(-2*x)
    return(c(4*x^2*y, 8*x*(1-x)*y, 8*(1-2*x^2)*y))
}


