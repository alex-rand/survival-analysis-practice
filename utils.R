# This function taken from UCLA's IDRE and Solomon Kurz's tutorial from Singer and Willett. 
kernel_smoothed_hazard <- function(width, time, survive) { 
  
  n   <- length(time)
  lo  <- time[1] + width
  hi  <- time[n] - width
  npt <- 50
  inc <- (hi - lo) / npt
  
  s <- t(lo + t(c(1:npt)) * inc)
  
  slag <- c(1, survive[1:n - 1])
  h    <- 1 - survive / slag
  x1   <- as.vector(rep(1, npt)) %*% (t(time))
  x2   <- s %*% as.vector(rep(1, n))
  x    <- (x1 - x2) / width
  k    <- .75 * (1 - x * x) * (abs(x) <= 1)
  
  lambda <- (k %*% h) / width
  
  smoothed <- data.frame(x = s, y = lambda)
  
  return(smoothed)
}
