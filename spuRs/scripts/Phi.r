# program spuRs/resources/scripts/Phi.r
# estimate and plot the normal cdf Phi

rm(list = ls()) # clear the workspace
source("../scripts/simpson_n.r")
phi <- function(x) return(exp(-x^2/2)/sqrt(2*pi))
Phi <- function(z) {
  if (z < 0) {
    return(0.5 - simpson_n(phi, z, 0))
  } else {
    return(0.5 + simpson_n(phi, 0, z))
  }
}

z <- seq(-5, 5, by = 0.1)
phi.z <- sapply(z, phi)
Phi.z <- sapply(z, Phi)
plot(z, Phi.z, type = "l", ylab = "", main = "phi(z) and Phi(z)")
lines(z, phi.z)
