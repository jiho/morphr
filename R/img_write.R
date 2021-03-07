#' Write a grayscale image
#'
#' @param x image data matrix: a 2D array of values in `[0,1]`.
#' @param file path to the output file; the extension determines the format.
#' @param quality image quality, for JPG output only.
#'
#' @return The path to the written file, invisibly (assign it to a variable to capture it).
#' @export
#' @examples
#' path <- system.file("extdata", "16195419.jpg", package="morphr")
#' x <- img_read(path)
#' out <- img_write(x, file=tempfile(fileext=".jpg"))
#' out
#' out <- img_write(x, file=tempfile(fileext=".png"))
#' out
img_write <- function(x, file, quality=0.9) {
  im <- imager::as.cimg(x)
  imager::save.image(im, file, quality=quality)
  return(invisible(file))
}