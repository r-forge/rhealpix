ring.num <- function(nside, z) {
  iring <- round(nside * (2.0 - 1.5*z))

  three.times.z <- 3*z
  if(three.times.z > 2){
    my.iring <- round(nside * sqrt(3.0 * (1 - z)))
    return(if(my.iring < 1) 1 else my.iring)
  } else if(three.times.z < -2) {
    my.iring <- round(nside * sqrt(3.0 * (1 + z)))
    return(4*nside - my.iring)
  } else {
    return(iring)
  }
}
