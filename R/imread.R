#' Read an image
#'
#' @param path path to the input image file.
#'
#' @return A matrix with the pixel values of the image.
#' @export
#' @examples
#' img_file  <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- imread(img_file)
#' imshow(x)
imread <- function(path) {
  io <- reticulate::import("skimage.io", as="io")
  io$imread(path, as_gray=TRUE)*255
}