require(Biobase)
## Particular
nsEntrezFilter <- function(eset, geneId=featureData(eset)$geneId) {
  selected <- geneId != "N/A"
  return(eset[selected,])
}