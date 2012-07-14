# program: spuRs/resources/scripts/ppoint.r

phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))

ppoint <- function(p, pdf = phi, z.min = -10, tol = 1e-9) {
  # calculate a percentage point
  #
  # p is assumed to be between 0 and 1
  # pdf is assumed to be a probability density function
  #
  # let F(x) be the integral of pdf from -infinity to x
  # we apply the Newton-Raphson algorithm to find z_p such that F(z_p) = p
  # that is, to find z_p such that F(z_p) - p = 0
  # note that the derivative of F(z) - p is just pdf(z)
  #
  # we approximate -infinity by z.min (that is we assume that the integral
  # of pdf from -infinity to z.min is negligible)

  # do first iteration
  x <- 0
  f.x <- simpson_n(pdf, z.min, x) - p
  # continue iterating until stopping conditions are met
  while (abs(f.x) > tol) {
    x <- x - f.x/pdf(x)
    f.x <- simpson_n(pdf, z.min, x) - p
  }
  return(x)
}
