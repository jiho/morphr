#' Read a greyscale image
#'
#' @param file path to the input image file.
#' @param invert when TRUE, invert the input image (useful for further processing functions such as [img_moments()] that reaquire light-on-dark input images to make sense.)
#'
#' @return A [imager::cimg()] object, which is a four-dimensional numeric array having x,y,z,c coordinates, where x,y are image pixel dimensions, z is depth (for a sequence of images), and c is colour channels (1 for greyscale images, 3 for RGB images, 4 for RGBA images).
#' @export
#' @examples
#' path <- system.file("extdata", "blob.jpg", package="morphr")
#' img_read(path) %>% img_show()
#' img_read(path, invert=TRUE) %>% img_show()
#' # or shorter
#' img_read(path, i=T) %>% img_show()
img_read <- function(file, invert=FALSE) {
  im <- imager::load.image(file)
  # keep only one colour channel
  # TODO support colour images
  im <- im[,,,1,drop=FALSE]
  if (invert) {im <- 1-im}
  return(im)
}