#' Replace extreme values by NA
#'
#' Compute quantiles of the input data and replace data beyond a given quantile by NA.
#'
#' @param x a numeric vector.
#' @param percent a vector of length 2 containing the percentage of data to replace by NA at the low end and at the high end of x. If percent contains only one number, this percentage is removed at both ends.
#'
#' @return The input vector with NA where the extreme were.
#' @export
#' @examples
#' x <- rgamma(1000, 1, 1)
#' hist(x)
#' # remove 0.5% at both ends
#' hist(mask_extreme(x))
#' # remove at the high end (shorten the long tail)
#' hist(mask_extreme(x, c(0,1)))
#' # remove at the low end (little change)
#' hist(mask_extreme(x, c(1,0)))
mask_extreme <- function(x, percent=c(0.5,0.5)) {
  if (length(percent) == 1) {
    percent <- c(percent, percent)
  }
  if (length(percent) != 2) {
    stop("percent should contain one or two values")
  }
  x[x < quantile(x, percent[1]/100, na.rm=TRUE)] <- NA
  x[x > quantile(x, 1 - (percent[2]/100), na.rm=TRUE)] <- NA
  return(x)
}
