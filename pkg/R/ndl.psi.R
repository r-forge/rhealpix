ndl.psi <-
function (u) {
    neg_u <- -abs(u)
    neg_u[neg_u < -1.0] <- -1.0;
    result <- ndl.psi.splinefunc (neg_u);
    invert.mask <- u > 0.0;
    result[invert.mask] <- 1 - result[invert.mask];
    result
}
