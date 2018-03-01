#' Display an array of pixel as a greyscale image
#'
#' @param x input array of pixels
#' @return The (x, y, z) list used to plot the image, which is also suitable for contour, persp, etc. returned invisibly.
#' @export
#' @examples
#' # read an image through python
#' io <- reticulate::import("skimage.io", as="io")
#' img <- io$imread(
#'   system.file("extdata", "amphipoda", "33463695.jpg", package="morphr"),
#'   as_grey=TRUE
#' )
#' # display it
#' imshow(img)
imshow <- function(x) {
  # compute an x, y, z list to force aspect ratio
  i <- list(
    x=1:ncol(x),
    y=1:nrow(x),
    z=t(x)
  )
  graphics::image(i,
    # map intensity to grey levels
    col=grDevices::grey(0:254/254),
    # for 1:1 aspect ratio
    asp=1,
    # suppress decorations
    xaxt="n", yaxt="n", bty="n"
  )
  return(invisible(i))
}