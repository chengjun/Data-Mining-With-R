# Program spuRs/resources/scripts/ascent.r

source("../scripts/linesearch.r")

ascent <- function(f, grad.f, x0, tol = 1e-9, n.max = 100) {
    # steepest ascent algorithm
    # find a local max of f starting at x0
    # function grad.f is the gradient of f
    
    x <- x0
    x.old <- x
    x <- line.search(f, x, grad.f(x))
    n <- 1
    while ((f(x) - f(x.old) > tol) & (n < n.max)) {
        x.old <- x
        x <- line.search(f, x, grad.f(x))
        n <- n + 1
    }
    return(x)
}
