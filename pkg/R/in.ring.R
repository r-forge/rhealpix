in.ring <- function(nside, ring.number,
                    phi0 = 0.0, delta.phi = pi,
                    nest = FALSE)
{
  if(nest)
    warning("NESTED scheme not supported yet")

  npix <- 12*nside^2
  twopi <- 2*pi

  phi.low <- (phi0 - delta.phi) %% twopi
  if(phi.low < 0) phi.low <- phi.low + twopi

  phi.hi <- (phi0 + delta.phi) %% twopi
  if(phi.hi < 0) phi.hi <- phi.hi + twopi
  take.all <- abs(delta.phi - pi) < 1e-6

  if((ring.number >= nside) && (ring.number <= 3*nside)) {
    ir <- ring.number - nside + 1
    ipix1 <- 2 * nside * (nside - 1) + 4 * nside * (ir - 1)
    ipix2 <- ipix1 + 4 * nside - 1
    kshift <- ir %% 2
    nr <- nside * 4
  } else {
    if(ring.number < nside) {
      ir <- ring.number
      ipix1 <- 2 * ir * (ir - 1)
      ipix2 <- ipix1 + 4 * ir - 1
    } else {
      ir <- 4*nside - ring.number
      ipix1 <- npix - 2 * ir * (ir + 1)
      ipix2 <- ipix1 + 4 * ir - 1
    }

    nr <- ir * 4
    kshift <- 1
  }

  if(take.all) {
    nir <- ipix2 - ipix1 + 1
    return(ipix1:ipix2)
  } else {
    shift <- kshift * .5
    ip.low <- round(nr * phi.low / twopi - shift) %% nr
    ip.hi  <- round(nr * phi.hi  / twopi - shift) %% nr

    to.top <- ip.low > ip.hi

    ip.low <- ip.low + ipix1
    ip.hi  <- ip.hi  + ipix1

    if(to.top) {
      nir1 <- ipix2 - ip.low + 1
      nir2 <- ip.hi - ipix1  + 1
      nir <- nir1 + nir2

      if((nir1 > 0) && (nir2 > 0)) {
        return(c(ip.low + 1:nir1,
                 ipix1  + 1:nir2))
      } else {
        if (nir1 == 0)
          return(ipix1 + 1:nir2)
        if (nir2 == 0)
          return(ip.low + 1:nir1)
      }
    } else {
      nir <- ip.hi - ip.low + 1
      return(ip.low + 1:nir)
    }
  }

  return(NA)
}
