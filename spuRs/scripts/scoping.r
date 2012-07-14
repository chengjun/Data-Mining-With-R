# program spuRs/resources/scripts/scoping.r
# Script to demonstrate the difference between passing and copying
# arguments.
# We use an artificial example with no real-world utility.

require(nlme)

fm1 <- lme(distance ~ age, data = Orthodont)
fm1$numIter <- 1

fm2 <- fm1

nochange <- function(x) {
  2 * x$numIter
  return(x)
}

change <- function(x) {
  x$numIter <- integer(2)
  return(x)
}


