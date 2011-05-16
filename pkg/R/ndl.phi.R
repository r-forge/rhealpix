ndl.phi <-
function (t, B) {
   u <- 1.0 - 2*B / (B - 1.0) * (t - 1.0/B);
   ndl.psi (u)
}
