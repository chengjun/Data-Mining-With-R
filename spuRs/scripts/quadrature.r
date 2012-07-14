# program spuRs/resources/scripts/quadrature.r
# numerical integration using adaptive quadrature

quadrature <- function(ftn, a, b, tol = 1e-8, trace = FALSE) {
  # numerical integral of ftn from a to b
  # ftn is a function of one variable
  # the partition used is recursively refined until the
  # estimate on successive partitions differs by at most tol
  # if trace is TRUE then intermediate results are printed
  #
  # the main purpose of this function is to call function q.recursion
  #
  # the function returns a vector of length 2 whose first element
  # is the integral and whose second element is the number of
  # function evaluations required

  c = (a + b)/2
  fa <- ftn(a)
  fb <- ftn(b)
  fc <- ftn(c)
  h <- (b - a)/2
  I.start <- h*(fa + 4*fc + fb)/3 # Simpson's rule
  q.out <- q.recursion(ftn,a,b,c,fa,fb,fc,I.start,tol,1,trace)
  q.out[2] <- q.out[2] + 3
  if (trace) {
    cat("final value is", q.out[1], "in",
        q.out[2], "function evaluations\n")
  }
  return(q.out)
}

q.recursion <- function(ftn,a,b,c,fa,fb,fc,I.old,tol,level,trace) {
  # refinement of the numerical integral of ftn from a to b
  # ftn is a function of one variable
  # the current partition is [a, c, b]
  # fi == ftn(i)
  # I.old is the value of the integral I using the current partition
  # if trace is TRUE then intermediate results are printed
  # level is the current level of refinement/nesting
  #
  # the function returns a vector of length 2 whose first element
  # is the integral and whose second element is the number of
  # function evaluations required
  #
  # I.left and I.right are estimates of I over [a, c] and [c, b]
  # if |I.old - I.left - I.right| <= tol then we are done, otherwise 
  # I.left and I.right are recursively refined

  level.max <- 64
  if (level > level.max) {
    cat("recursion limit reached: singularity likely\n")
    return(NULL)
  } else {
    h <- (b - a)/4
    f1 <- ftn(a + h)
    f2 <- ftn(b - h)
    I.left <- h*(fa + 4*f1 + fc)/3  # Simpson's rule for left half
    I.right <- h*(fc + 4*f2 + fb)/3 # Simpson's rule for right half
    I.new <- I.left + I.right       # new estimate for the integral
    f.count <- 2

    if (abs(I.new - I.old) > tol) { # I.new not accurate enough
      q.left <- q.recursion(ftn, a, c, a + h, fa, fc, f1, I.left,
                            tol/2, level + 1, trace)
      q.right <- q.recursion(ftn, c, b, b - h, fc, fb, f2, I.right,
                             tol/2, level + 1, trace)
      I.new <- q.left[1] + q.right[1]
      f.count <-  f.count + q.left[2] + q.right[2];
    } else { # we have achieved the desired tolerance
      if (trace) {
        cat("integral over [", a, ", ", b, "] is ", I.new,
            " (at level ", level, ")\n", sep = "")
      }
    }

    return(c(I.new, f.count))
  }
}
