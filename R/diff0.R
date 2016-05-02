perc_change <- function(prev, current) {
  r <- 100*(current - prev) / prev
  r
}

change_fun <- function(series, lag=1, fun=perc_change) {
  n <- length(series)
  result <- numeric(n-1)
  for(i in 1:(n-1)) {
    result[i] <- fun(series[i], series[i+1])
  }
  result
}

diff0 <- function(x, ...) {
  if (!is.vector(x)) stop("Muste be vetor")
  return(c(0,diff(x , ...)))
}