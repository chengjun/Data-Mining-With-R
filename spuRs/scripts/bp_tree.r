###############################################################################
#
# plotting the branching process as a tree
#
###############################################################################
#
# in the following the population is represented by a list Z
# each element of Z is a vector representing a line of descent
# a line that has died out ends with -1
# in a line of descent, an individual at generation k has a label
# which distinguishes it from other individuals in the same family
# the children in a family are labelled -(n-1)/(2*n) to (n+1)/(2*n)
# in steps of 1/n, where n is the family size
#
# add.gen adds a new generation to an existing population Z
# bp.gen generates the first gen generations of a branching process
# bp.tree generates and plots the first gen generations of a branching process

last <- function(x) return(x[length(x)])

add.gen <- function(Z, rv.sim, ...) {
  # adds a new generation to an existing population Z
  # rv.sim(1, ...) simulates a single family size
  newZ <- list()
  k <- 1 # current line of descent in the updated population
  for (i in 1:length(Z)) {
    if (last(Z[[i]]) == -1) {
      newZ[[k]] <- Z[[i]]
      k <- k + 1
    } else {
      n <- rv.sim(1, ...) # number of offspring in new generation
      if (n == 0) {
        newZ[[k]] <- c(Z[[i]], -1)
        k <- k + 1
      } else {
        for (j in seq(-(n-1)/2, (n-1)/2)) {
          newZ[[k]] <- c(Z[[i]], j/n)
          k <- k + 1
        }
      }
    }
  }
  return(newZ)
}

bp.gen <- function(gen, rv.sim, ...) {
  # generates the first gen generations of a branching process
  # rv.sim(1, ...) simulates a single family size
  Z <- list(0) # start with a single ancestor
  for (i in 1:gen) {
    Z <- add.gen(Z, rv.sim, ...)
  }
  return(Z)
}

bp.tree <- function(gen, rv.sim, ..., sf = 2) {
  # generates and plots the first gen generations of a branching process
  # rv.sim(1, ...) simulates a single family size
  # sf is a scale factor used to separate lines of descent
  # choose a value slightly larger than the mean family size
  # (lines of descent may still overlap when plotted)
  #
  # The population list is transformed into a list of positions Zpos
  # in generation j a child with label r is offset r*sf^(gen-j+1) from its parent
  Z <- bp.gen(gen, rv.sim, ...)
  sfs <- sf^(gen:0) # scale factors for generations 0 to gen
  Zpos <- list()
  for (i in 1:length(Z)) {
    pos <- 0 # position of initial ancestor
    for (j in 2:length(Z[[i]])) {
      r <- Z[[i]][j]
      if (r != -1) {
        pos <- c(pos, last(pos) + r*sfs[j]) # add position of generation j individual
      }
    }
    Zpos[[i]] <- pos
  }
  m1 <- min(sapply(Zpos, min)) # for setting up plot
  m2 <- max(sapply(Zpos, max)) # for setting up plot
  plot(c(0, gen), c(m1, m2), type = "n", xlab = "generation", ylab = "", yaxt = "n")
  for (i in 1:length(Zpos)) {
    lines(0:(length(Zpos[[i]])-1), Zpos[[i]])
    points(0:(length(Zpos[[i]])-1), Zpos[[i]])
  }
  return(invisible(Z))
}

set.seed(6)
bp.tree(7, rbinom, 12, .1)
