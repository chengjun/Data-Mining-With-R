# program spuRs/resources/scripts/wmean.r

wmean <- function(x, k) {
    # calculate the k-th Windsorised mean of the vector x
    x <- sort(x)
    n <- length(x)
    x[1:k] <- x[k+1]
    x[(n-k+1):n] <- x[n-k]
    return(mean(x))
}
