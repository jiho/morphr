#' Display an array of pixel as a greyscale image
#'
#' @param x a greyscale image, with grey levels coded in `[0, 1]`. This can be either a 2D matrix of pixels or a 4D [imager::cimg()] object.
#'
#' @return The input image, invisibly (this allows to use img_show() within a pipe chain).
#' @export
#' @examples
#' # check that the overall grey level is reflected in the plot
#' img_show(matrix(runif(100, 0, 0.5), nrow=10))
#' img_show(matrix(runif(100, 0.5, 1), nrow=10))
#'
#' # display an image
#' path <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- img_read(path)
#' img_show(x)
img_show <- function(x) {
  # if x is a cimg object:
  # - keep only depth 1
  # - convert the rest in a numeric array to control colour scale etc.
  if (inherits(x, "imager_array")) {
    xm <- x[,,1,]
    xm <- aperm(xm, perm=if(length(dim(xm))==2) { c(2,1) } else { c(2,1,3) })
  } else {
    xm <- x
  }
  grid::grid.newpage()
  # draw a neutral grey background
  grid::grid.rect(gp=grid::gpar(fill="grey50", col=NA))
  # draw the image
  grid::grid.raster(xm, interpolate=FALSE)
  # TODO allow to plot over it, maybe revert ot plot then...

  return(invisible(x))
}
