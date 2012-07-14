# program spuRs/resources/scripts/life.r

neighbours <- function(A, i, j, n) {
    # A is an n*n 0-1 matrix
    # calculate number of neighbours of A[i,j]
    .
    .
    .
}

# grid size
n <- 50

# initialise lattice
A <- matrix(round(runif(n^2)), n, n)

finished <- FALSE
while (!finished) {
    # plot
    plot(c(1,n), c(1,n), type = "n", xlab = "", ylab = "")
    for (i in 1:n) {
        for (j in 1:n) {
            if (A[i,j] == 1) {
                points(i, j)
            }
        }
    }
    
    # update
    B <- A
    for (i in 1:n) {
        for (j in 1:n) {
            nbrs <- neighbours(A, i, j, n)
            if (A[i,j] == 1) {
                if ((nbrs == 2) | (nbrs == 3)) {
                    B[i,j] <- 1
                } else {
                    B[i,j] <- 0
                }
            } else {
                if (nbrs == 3) {
                    B[i,j] <- 1
                } else {
                    B[i,j] <- 0
                }
            }
        }
    }
    A <- B
    
    ## continue?
    #input <- readline("stop? ")
    #if (input == "y") finished <- TRUE
}
