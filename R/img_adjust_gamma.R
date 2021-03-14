#' Perform gamma correction
#'
#' Transorm the input image according to the equation x^gamma.
#'
#' @inheritParams img_write
#' @param gamma gamma value, in \[0,+Inf\[ where 1 means no change, <1 make the image lighter, >1 makes the image darker.
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "amphipoda/33463695.jpg",
#'              package="morphr")) %>% img_show()
#' img_adjust_gamma(x, 0.2) %>% img_show()
#' img_adjust_gamma(x, 2) %>% img_show()
img_adjust_gamma <- function(x, gamma=1) {
  A <- 1
  x <- A*x^gamma
  return(x)
}