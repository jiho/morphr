#' Display an array of pixel as a greyscale image
#'
#' @param x input greyscale image, with grey levels coded in `[0, 1]`. This can be either a 2D matrix or a 4D [imager::cimg()] object.
#'
#' @return The (x, y, z) list used to plot the image, which is also suitable for contour, persp, etc. returned invisibly.
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
  if (inherits(x, "cimg")) {
    x <- x[,,1,]
    x <- aperm(x, perm=if(length(dim(x))==2) { c(2,1) } else { c(2,1,3) })
  }
  grid::grid.newpage()
  # draw a neutral grey background
  grid::grid.rect(gp=grid::gpar(fill="grey50", col=NA))
  # draw the image
  grid::grid.raster(x, interpolate=FALSE)
}
