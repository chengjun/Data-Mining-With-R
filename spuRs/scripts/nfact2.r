# function spuRs/resources/scripts/nfact2.r

nfact2 <- function(n) {
    # calculate n factorial
    if (n == 1) {
        cat("called nfact2(1)\n")
        return(1)
    } else {
        cat("called nfact2(", n, ")\n", sep = "")
        return(n*nfact2(n-1))
    }
}
