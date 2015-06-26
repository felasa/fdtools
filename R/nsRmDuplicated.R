require(Biobase)
nsRmDuplicated <- function(eset, geneId=featureData(eset)$geneId) {
  IQRs <- apply(exprs(eset), 1, IQR)  
  maxs <- tapply(IQRs, factor(geneId), max) 
  selected <-  sapply(seq_along(IQRs), function(x) IQRs[x] >= maxs[[geneId[x]]])
  return(eset[selected,])
}