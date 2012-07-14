# program spuRs/resources/scripts/n_choose_r.r

n_factorial <- function(n) {
    # Calculate n factorial
    n_fact <- prod(1:n)
    return(n_fact)
}

n_choose_r <- function(n, r) {
    # Calculate n choose r
    n_ch_r <- n_factorial(n)/n_factorial(r)/n_factorial(n-r)
    return(n_ch_r)
}
