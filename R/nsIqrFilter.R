require(Biobase)
nsIqrFilter <- function(eset, cutoff=0.5) {
  IQRs <- apply(exprs(eset), 1, IQR)
  selected <- IQRs > cutoff
  return(eset[selected,])   
}