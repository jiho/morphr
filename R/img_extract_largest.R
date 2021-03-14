#' Extract largest object from a greyscale image
#'
#' Extract the largest object from the image background.
#'
#' Threshold the input image, detect connected particles, and keep only the largest one. Note that, for ease of use, when the image has an obviously white/light background (based on [img_guess_background()]), the image is automatically inverted. Indeed, thresholding select pixels whose value is *above* the threshold = it supposes that the object of interest has values higher (i.e. lighter) than the background, which is therefore supposed to be dark.
#'
#' @inheritParams img_make_transparent
#' @param threshold grey value threshold to detect the object over the background. Either a number in `[0,1]` where `0` is pure black and `1` is pure white, or a string with a grey level quantile value (e.g. "50%" to keep 50% of the greys in the foreground).
#'
#' @return The extracted object, on a pure white background, as a [imager::cimg()] object.
#'
#' @export
#' @importFrom dplyr %>%
#' @examples
#' # get an image with lots of background space, several blobs and a legend
#' # NB: invert it so that the background is black
#' x <- img_read(system.file("extdata", "blob.jpg", package="morphr"), invert=TRUE)
#' img_show(x)
#' # extract its largest object
#' img_extract_largest(x) %>% img_show()
#'
#' # now leave the image in its original dark-on-light aspect
#' x <- img_read(system.file("extdata", "blob.jpg", package="morphr"))
#' img_show(x)
#' # and extraction still works because the image is automatically inverted
#' img_extract_largest(x) %>% img_show()
#' # NB: use quiet=TRUE to suppress the message
#'
#' # if we want to be sure to pick up the object itself and not the legend,
#' # we can start by chopping the legend away
#' x %>% img_chop(b=31) %>% img_extract_largest(quiet=TRUE) %>% img_show()
img_extract_largest <- function(x, threshold=2/255, quiet=FALSE) {
  # guess background colour
  back <- img_guess_background(x)
  # if the background is ~ white, invert the image
  if ( back > 0.95 ) {
    if (!quiet) { message("The image has a light background, inverting it for computation.") }
    x <- 1 - x
  }

  # segment image into particles
  particles <- x %>%
    imager::threshold(thr=threshold) %>%
    imager::split_connected()

  # find the largest one
  largest <- particles[[sapply(particles, sum) %>% which.max()]]

  # mask-out the outside of the object, which becomes pure black
  # (largest is a mask, with 0 outside of the object)
  xmasked <- (x * largest)
  # then we crop to the limits of the object
  xcropped <- imager::autocrop(xmasked)

  # re-invert the image if it was inverted automatically above
  if ( back > 0.95 ) {
    xcropped <- 1 - xcropped
  }

  return(xcropped)
}
