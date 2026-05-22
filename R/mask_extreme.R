#' @importFrom stats quantile
mask_values <- function(x, percent=c(0.5,0.5), winsorize=FALSE) {
  if (length(percent) == 1) {
    percent <- c(percent, percent)
  }
  if (length(percent) != 2) {
    stop("percent should contain one or two values")
  }
  if (!all(percent == 0)) {
    low_quant <- quantile(x, percent[1]/100, na.rm=TRUE)
    x[x < low_quant] <- if (winsorize) { low_quant } else { NA }
    high_quant <- quantile(x, 1 - (percent[2]/100), na.rm=TRUE)
    x[x > high_quant] <- if (winsorize) { high_quant } else { NA }
  }
  return(x)
}


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
  mask_values(x, percent, winsorize=FALSE)
}


#' Crop extreme values and replace them by the bounds
#'
#' Compute quantiles of the input data and replace data beyond a given quantile by the quantile value.
#'
#' @inheritParams mask_extreme
#'
#' @return The input vector with the extreme values cropped.
#' @export
#' @examples
#' x <- rgamma(1000, 1, 1)
#' hist(x)
#' # remove 0.5% at both ends
#' hist(winsorize(x))
#' # remove at the high end (shorten the long tail)
#' hist(winsorize(x, c(0,1)))
#' # remove at the low end (little change)
#' hist(winsorize(x, c(1,0)))
winsorize <- function(x, percent=c(0.5,0.5)) {
  mask_values(x, percent, winsorize=TRUE)
}

