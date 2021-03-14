#' Build morphological space
#'
#' Build a morphological space by performing dimensionality reduction on a table of morphological descriptors.
#'
#' @param x data.frame or matrix of morphological descriptors. Should be all numeric and as close to normally-distributed as possible.
#' @param weights vector of objects weights, with no weights by default. Should have as many elements as rows in `x` but they do not need to sum to 1.
#' @param method dimensionality reduction method. Currently only "PCA" is implemented (with [FactoMineR::PCA()]).
#' @param ... passed to the dimensionality reduction function.
#'
#' @return An object of class `morphospace`.
#'
#' @export
#' @examples
#' s <- morphospace(plank[,-(1:2)], weights=plank$conc)
#' plot(s, choix="ind")
#' plot(s, choix="var")
morphospace <- function(x, weights=NULL, method="PCA", ...) {
  if (method == "PCA") {
    s <- FactoMineR::PCA(as.matrix(x), row.w=as.numeric(weights), graph=FALSE, ...)
  }
  class(s) <- c("morphospace", class(s))
  return(s)
}
