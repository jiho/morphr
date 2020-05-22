#' Display an array of pixel as a greyscale image
#'
#' @param x input array of pixels, with grey levels coded in `[0, 255]`
#' @return The (x, y, z) list used to plot the image, which is also suitable for contour, persp, etc. returned invisibly.
#' @export
#' @examples
#' # check that the overall grey level is reflected in the plot
#' imshow(matrix(runif(100, 0, 100), nrow=10))
#' imshow(matrix(runif(100, 150, 250), nrow=10))
#'
#' # read an image through python
#' io <- reticulate::import("skimage.io", as="io")
#' img <- io$imread(
#'   system.file("extdata", "amphipoda", "33463695.jpg", package="morphr"),
#'   as_gray=TRUE
#' )
#' # display it
#' imshow(img)
imshow <- function(x) {
  # compute an x, y, z list to force aspect ratio
  i <- list(
    x=1:ncol(x),
    y=1:nrow(x),
    z=t(x)[,nrow(x):1]
  )
  # set a plot window with a grey background and no margins
  pars <- graphics::par(no.readonly=TRUE)
  graphics::par(bg="grey50", mai=c(0,0,0,0))
  graphics::image(i,
    # map intensity in [0,255] to grey levels
    col=grDevices::grey(0:254/254), breaks=0:255,
    # for 1:1 aspect ratio
    asp=1,
    # suppress decorations
    xaxt="n", yaxt="n", bty="n"
  )
  graphics::par(pars)
  return(invisible(i))
}
