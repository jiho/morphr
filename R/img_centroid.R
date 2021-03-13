#' Centroid of object on image
#'
#' Compute the coordinates of the centroid of the object based on image moments. The centroid is based on shape if the image is binary and is weighted by the pixel values if it is not (in which case it is usually called the centre of mass).
#'
#' @inheritParams img_make_transparent
#'
#' @return A vector with the x and y coordinates of the centroid.
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "plank/16199658.jpg", package="morphr"))
#' X_binary <- x < 1
#' X_inverted <- 1 - x
#' img_centroid(X_binary)
#' img_centroid(X_inverted)
# TODO better example with some plotting
img_centroid <- function(x) {
  m <- img_moments(x, order=1)
  # NB: 1-based indexing in R vs 0-based indexing for moments
  c( m[2,1]/m[1,1], m[1,2]/m[1,1] )
}
