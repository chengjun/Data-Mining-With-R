# program: spuRs/resources/scripts/forest_fire.r
# forest fire simulation
rm(list = ls())

neighbours <- function(A, i, j) {
  # calculate number of neighbours of A[i,j] that are infected
  # we have to check for the edge of the grid
  nbrs <- 0
  # sum across row i - 1
  if (i > 1) {
    if (j > 1) nbrs <- nbrs + (A[i-1, j-1] == 1)
    nbrs <- nbrs + (A[i-1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i-1, j+1] == 1)
  }
  # sum across row i
  if (j > 1) nbrs <- nbrs + (A[i, j-1] == 1)
  nbrs <- nbrs + (A[i, j] == 1)
  if (j < ncol(A)) nbrs <- nbrs + (A[i, j+1] == 1)
  # sum across row i + 1
  if (i < nrow(A)) {
    if (j > 1) nbrs <- nbrs + (A[i+1, j-1] == 1)
    nbrs <- nbrs + (A[i+1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i+1, j+1] == 1)
  }
  return(nbrs)
}

forest.fire.plot <- function(X) {
  # plot infected and removed individuals
  for (i in 1:nrow(X)) {
    for (j in 1:ncol(X)) {
      if (X[i,j] == 1) points(i, j, col = "red", pch = 19)
      else if (X[i,j] == 0) points(i, j, col = "grey", pch = 19)
    }
  }
}

forest.fire <- function(X, a, b, pausing = FALSE) {
  # simulate forest fire epidemic model
  # X[i, j] = 2 for susceptible; 1 for infected; 0 for removed
  
  # set up plot
  plot(c(1,nrow(X)), c(1,ncol(X)), type = "n", xlab = "", ylab = "")
  forest.fire.plot(X)

  # main loop
  burning <- TRUE
  while (burning) {
    burning <- FALSE
    # check if pausing between updates
    if (pausing) {
      input <- readline("hit any key to continue")
    }

    # update
    B <- X
    for (i in 1:nrow(X)) {
      for (j in 1:ncol(X)) {
        if (X[i, j] == 2) {
          if (runif(1) > (1 - a)^neighbours(X, i, j)) {
            B[i, j] <- 1
          }
        } else if (X[i, j] == 1) {
          burning <- TRUE
          if (runif(1) < b) {
            B[i, j] <- 0
          }
        }
      }
    }
    X <- B
    
    # plot
    forest.fire.plot(X)
  }
  
  return(X)
}

# spark
set.seed(3)
X <- matrix(2, 21, 21)
X[11, 11] <- 1
# big fires
#X <- forest.fire(X, .1, .2, TRUE)
X <- forest.fire(X, .2, .4, TRUE)
# medium fires
#X <- forest.fire(X, .07, .2, TRUE)
#X <- forest.fire(X, .1, .4, TRUE)
# small fires
#X <- forest.fire(X, .05, .2, TRUE)
#X <- forest.fire(X, .07, .4, TRUE)
