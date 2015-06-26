diff0 <- function(x, ...) {
  if (!is.vector(x)) stop("Muste be vetor")
  return(c(0,diff(x , ...)))
}