# Program spuRs/resources/scripts/moverings.r

# Tower of Hanoi

moverings <- function(numrings, frompole, topole) {
  if (numrings == 1) {
    cat("move ring 1 from pole", frompole,
        "to pole", topole, "\n")
  } else {
    sparepole <- 6 - frompole - topole # clever
    moverings(numrings - 1, frompole, sparepole)
    cat("move ring", numrings, "from pole", frompole,
        "to pole", topole, "\n")
    moverings(numrings - 1, sparepole, topole)
  }
  return(invisible(NULL))
}
