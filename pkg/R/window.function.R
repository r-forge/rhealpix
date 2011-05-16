window.function <-
function (t, B) {
   val <- ndl.phi (t / B, B) - ndl.phi (t, B)
   val[val < 0.0] <- 0.0
   sqrt(val)
}

