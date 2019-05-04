#' Make a greyscale image transparent
#'
#' Turn white into fully transparent and black into fully opaque in a greyscale image.
#'
#' @param x matrix representing a grayscale image (in [0,1] or [0,255], where the maximum is white).
#'
#' @export
#' @examples
#' # create an image
#' amph <- list.files(
#'           system.file("extdata", "amphipoda", package="morphr"),
#'           full.names=TRUE
#'        )
#' img <- morph(amph)
#' dim(img)
#'
#' # add transparency = turn it into RGBA
#' imga <- make_transparent(img)
#' dim(imga)
#'
#' # show the difference between the two
#' grid::grid.newpage()
#' grid::grid.rect(gp=gpar(fill="dodgerblue"))
#' grid::grid.raster(img/255)
#'
#' grid::grid.newpage()
#' grid::grid.rect(gp=gpar(fill="dodgerblue"))
#' grid::grid.raster(imga)
make_transparent <- function(x) {
  # force it into [0,1]
  if (max(x) > 1) {
    x <- x / 255
  }
  # make an array of black
  xa <- array(0, dim=c(dim(x), 4))
  # turn the input image into a transparency mask
  xa[,,4] <- 1 - x
  return(xa)
}
