perc_change <- function(prev, current) {
  r <- 100*(current - prev) / prev
  r
}

change_fun <- function(series, lag=1, fun=perc_change) {
  n <- length(series)
  result <- numeric(n-lag)
  for(i in 1:(n-lag)) {
    result[i] <- fun(series[i], series[i+lag])
  }
  result
}

diff0 <- function(x, ...) {
  if (!is.vector(x)) stop("Muste be vetor")
  return(c(0,diff(x , ...)))
}