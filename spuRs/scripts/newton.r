# program spuRs/resources/scripts/newton.r

newton <- function(f3, x0, tol = 1e-9, n.max = 100) {
    # Newton's method for optimisation, starting at x0
    # f3 is a function that given x returns the list
    # {f(x), grad f(x), Hessian f(x)}, for some f

    x <- x0
    f3.x <- f3(x)
    n <- 0
    while ((max(abs(f3.x[[2]])) > tol) & (n < n.max)) {
        x <- x - solve(f3.x[[3]], f3.x[[2]])
        f3.x <- f3(x)
        n <- n + 1
    }
    if (n == n.max) {
        cat('newton failed to converge\n')
    } else {
        return(x)
    }
}
