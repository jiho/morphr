#' Build morphological space
#'
#' Build a morphological space by computing a Principal Component Analysis on a table of morphological descriptors.
#'
#' @param x data.frame or matrix of morphological descriptors. Should be all numeric and as close to normally-distributed as possible.
#' @param weights vector of objects weights, with no weights by default. Should have as many elements as `x` has rows but does not need to sum to 1.
#' @param ... passed to [FactoMineR::PCA()].
#'
#' @return An object of class `PCA`, see [FactoMineR::PCA()] for more information.
#'
#' @export
#' @examples
#' s <- morpho_space(plank[,-(1:2)], weights=plank$w)
#' plot(s, choix="ind")
#' plot(s, choix="var")
morpho_space <- function(x, weights=NULL, ...) {
  FactoMineR::PCA(as.matrix(x), row.w=as.numeric(weights), graph=FALSE, ...)
}
