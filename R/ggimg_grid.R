#' Plot a grid of images
#'
#' @param imgs vector of paths to images
#' @param scale scaling factor used to display the images; from pixels to plot dimensions (in \[0,1\]).
#' @param fun function to apply to each image, as a pre-processing step (a typical one is [img_chop()] to remove a scale bar added to the image).
#' @param ... passed to `fun`.
#'
#' @export
#' @importFrom rlang .data
#' @examples
#' paths <- list.files(system.file("extdata", "plank",
#'                     package="morphr"), full.names=TRUE)
#' ggimg_grid(imgs=sample(paths, 15), scale=0.002)
#' ggimg_grid(imgs=sample(paths, 56), scale=0.001)
ggimg_grid <- function(imgs, scale=0.001, fun=NULL, ...) {
  n <- length(imgs)
  if (n > 100) {
    stop("That's a lot of images, you won't see anything")
  }
  # prepare a grid
  nx <- round(sqrt(n))
  ny <- ceiling(n/nx)
  X <- dplyr::as_tibble(expand.grid(x=seq(0, 1, length=nx), y=seq(0, 1, length=ny)))
  X <- X[1:n,]

  # read all images
  X$img <- lapply(imgs, img_read)
  # optionnally process them
  if (!is.null(fun)) {
    X$img <- lapply(X$img, fun, ...)
  }

  # compute width and height
  X <- X %>% dplyr::rowwise() %>% dplyr::mutate(w=ncol(.data$img), h=nrow(.data$img))

  # plot
  p <- ggplot2::ggplot() +
    # set coordinates
    ggplot2::coord_fixed(xlim=c(-0.1,1.1), ylim=c(-0.1,1.1)) +
    # remove decoration
    ggplot2::theme_void()

  # plot each image
  for (i in 1:nrow(X)) {
    Xi <- X[i,]
    p <- p + ggplot2::annotation_custom(
      grid::rasterGrob(img_make_transparent(Xi$img[[1]]) %>% as_rgba_raster()),
      xmin=Xi$x-Xi$w*scale, xmax=Xi$x+Xi$w*scale,
      ymin=Xi$y-Xi$h*scale, ymax=Xi$y+Xi$h*scale
      # TODO scale by power law or something, to see better the small ones
    )
  }

  p
}
