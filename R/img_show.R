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
  if (inherits(x,"cimg")) {
    # drop the extra dimensions
    x <- x[,,1,1]
  }

  # reverse the y axis for the image to show the in correct orientation
  x <- x[,ncol(x):1]

  # compute an x, y, z list to force aspect ratio
  i <- list(
    x=1:nrow(x),
    y=1:ncol(x),
    z=x
  )
  # set a plot window with a grey background and no margins?grid>>
  pars <- graphics::par(no.readonly=TRUE)
  graphics::par(bg="grey50", mai=c(0,0,0,0))

  # plot the image
  graphics::image(i,
    # map intensity in [0,255] to grey levels
    col=grDevices::grey(0:254/254), breaks=(0:255)/255,
    # for 1:1 aspect ratio
    asp=1,
    # suppress decorations
    xaxt="n", yaxt="n", bty="n"
  )
  # reset graphical parameters to their default
  graphics::par(pars)

  return(invisible(i))
}
