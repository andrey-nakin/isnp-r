# GNEIS neutron spectrum approximation function
# E - energy in MeV
# a - fitting parameter
# Result - intensity in n/(cm^2 * s * MeV)
calc.gneis.spectrum.log.approx <- function(E, a = 0) {
  res <- 4.281 * exp(9.7999 + 0.5557 * (log(E)) - 1.4006 * (log(E))^2 + 0.3706 * (log(E))^3 - 0.0312 * (log(E))^4 )
  return (a + log10(res))
}
