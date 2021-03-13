#' Extract largest object from a greyscale image
#'
#' Extract the largest dark object over a light image background.
#'
#' @inheritParams img_make_transparent
#' @param threshold grey value threshold to detect the object over the background, in `[0,1]` where `0` is pure black and `1` is pure white. The default threshold value of `1-2/255` keeps almost all pixels that are not pure white in an image that was originally coded as 8-bit (i.e. in `[0,255]`).
#'
#' @return The extracted object, on a pure white background, as a [imager::cimg()] object.
#'
#' @export
#' @importFrom dplyr %>%
#' @examples
#' # get an input image with lots of white space, several blobs and a legend
#' path <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- img_read(path)
#' img_show(x)
#'
#' # extract its largest object
#' img_extract_largest(x) %>% img_show()
#'
#' # if we want to be sure to pick up the object itself and not the legend,
#' # we can start by chopping the legend away
#' x %>% img_chop(b=31) %>% img_extract_largest() %>% img_show()
img_extract_largest <- function(x, threshold=1-2/255) {
  # we start by inverting the image so that thresholding keeps the object, not the background
  xinv <- (1-x)

  # segment image into particles
  particles <- xinv %>%
    imager::threshold(thr=1-threshold) %>%
    imager::split_connected()

  # find the largest one
  largest <- particles[[sapply(particles, sum) %>% which.max()]]

  # get the corresponding object from the original image, on a pure white background

  # we mask out the outside of the object, which becomes pure black
  xmasked <- (xinv * largest)
  # then we crop to the limits of the object
  xcropped <- imager::autocrop(xmasked)
  # and we re-invert the image to get it into grey on white
  x_largest <- 1 - xcropped

  return(x_largest)
}
