#' Compute the coordinates of points on an ellipse
#'
#' @param major,minor length of the major and minor axis
#' @param alpha angle, in trigonometric convention (from the horizontal, positive counter-clockwise, in radians).
#' @param x,y coordinates of the center of the ellipse (around which the rotation is computed if alpha is not 0).
#' @param n number of points to compute on the ellipse.
#'
#' @export
#' @examples
#' plot(ellipse(10, 5), asp=1)
#' plot(ellipse(10, 5, alpha=pi/4), asp=1)
#' plot(ellipse(10, 5, alpha=pi/4, x=3, y=4, n=200), asp=1)
ellipse <- function(major, minor, alpha=0, x=0, y=0, n=100) {
  # https://math.stackexchange.com/questions/426150/what-is-the-general-equation-of-the-ellipse-that-is-not-in-the-origin-and-rotate
  t <- seq(0, 2*pi, length=n)
  X <- x + major*cos(t)*cos(alpha) + minor*sin(t)*(-sin(alpha))
  Y <- y + major*cos(t)*sin(alpha) + minor*sin(t)*( cos(alpha))
  return(data.frame(x=X, y=Y))
}
