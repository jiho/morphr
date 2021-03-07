#' Display an array of pixel as a greyscale image
#'
#' @param x input array of pixels, with grey levels coded in `[0, 1]`.
#'
#' @return The (x, y, z) list used to plot the image, which is also suitable for contour, persp, etc. returned invisibly.
#' @export
#' @examples
#' # check that the overall grey level is reflected in the plot
#' imshow(matrix(runif(100, 0, 0.5), nrow=10))
#' imshow(matrix(runif(100, 0.5, 1), nrow=10))
#'
#' # display an image
#' path <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- img_read(path)
#' img_show(x)
img_show <- function(x) {
  # compute an x, y, z list to force aspect ratio
  i <- list(
    x=1:nrow(x),
    y=1:ncol(x),
    # reverse the y axis for the image to show the in correct orientation
    z=x[,ncol(x):1]
  )
  # set a plot window with a grey background and no margins
  pars <- graphics::par(no.readonly=TRUE)
  graphics::par(bg="grey50", mai=c(0,0,0,0))
  graphics::image(i,
    # map intensity in [0,255] to grey levels
    col=grDevices::grey(0:254/254), breaks=(0:255)/255,
    # for 1:1 aspect ratio
    asp=1,
    # suppress decorations
    xaxt="n", yaxt="n", bty="n"
  )
  graphics::par(pars)
  return(invisible(i))
}
