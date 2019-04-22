#' Tile morphs within a morphological space
#'
#' @param space morphological space object created by [morpho_space()].
#' @param imgs vector of paths to images; should have as many elements as are in the morphological space
# TODO specify that at the time of construction of the mopho space
#' @param dimensions couple of dimensions to plot.
#' @param steps number of steps along each dimension that define the tiles.
#' @param n_imgs number of images to randomly select and morph in each tile.
#' @inheritParams morph
#' @param scale scaling factor used to display the images; from pixels to morphological space dimensions units.
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @examples
#' space <- morpho_space(plank[,-(1:2)], weights=plank$conc)
#' img_root <- system.file("extdata", "plank", package="morphr")
#' imgs <- file.path(img_root, paste0(plank$id, ".jpg"))
#' ggmorph_tile(space, imgs)
#' ggmorph_tile(space, imgs, step=8, scale=0.005)
#' ggmorph_tile(space, imgs, dim=c(3,4), scale=0.004)
ggmorph_tile <- function(space, imgs, dimensions=c(1,2), steps=5, n_imgs=5, adjust_grey=TRUE, scale=0.01) {
  # get objects in the full space that are close to the plane defined by the selected dimensions
  X <- ggmorph_get(space, dimensions)

  # sanity check
  if (steps > 50) {
    stop("Too many steps")
  }

  # define tiles in x and y
  X$xbin <- cut(X$x, breaks=steps, labels=FALSE)
  X$ybin <- cut(X$y, breaks=steps, labels=FALSE)
  # and define a single identifier for each tile
  X$bin <- interaction(X$xbin, X$ybin)

  # construct the morph in each tile
  Xm <- ggmorph_morph(X, imgs, n_imgs, adjust_grey)

  # and plot all morphs
  ggmorph_plot(Xm, scale)
}


#' Display morphs radially within a morphological space
#'
#' @inheritParams ggmorph_tile
#' @param directions number of radial directions along which to display morphs.
#' @param steps number of steps in each direction at which to display a morph (a morph is always displayed in the middle of the space).
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @examples
#' space <- morpho_space(plank[,-(1:2)], weights=plank$conc)
#' img_root <- system.file("extdata", "plank", package="morphr")
#' imgs <- file.path(img_root, paste0(plank$id, ".jpg"))
#' set.seed(1)
#' ggmorph_radial(space, imgs)
#' ggmorph_radial(space, imgs, directions=10, steps=4, scale=0.005)
#' ggmorph_radial(space, imgs, dim=c(3,4), scale=0.004)
ggmorph_radial <- function(space, imgs, dimensions=c(1,2), directions=4, steps=2, n_imgs=5, adjust_grey=TRUE, scale=0.01) {
  # get objects in the full space that are close to the plane defined by the selected dimensions
  X <- ggmorph_get(space, dimensions)

  # sanity checks
  if (directions > 16*2) {
    stop("directions is too big")
  }
  if (steps > 10) {
    stop("steps is too big")
  }
  if (steps < 1) {
    stop("steps should be at least 1")
  }

  # convert to polar coordinates
  X$a <- atan2(X$y, X$x)
  X$a[X$a < 0] <- pi + (pi + X$a[X$a < 0])
  X$r <- sqrt(X$x^2+X$y^2)

  # define angle bins
  astep <- pi / directions
  acuts <- seq(from=astep, to=2*pi, by=astep*2)
  # divide space in wedges
  X$abin <- cut(X$a, breaks=acuts, labels=FALSE)
  # combine first and last wedges
  X$abin[is.na(X$abin)] <- 0

  # define radial bins
  rstep <- 1 / (steps*2)
  rcuts <- c(seq(from=rstep, to=1, by=rstep*2), 1)
  X <- X %>% dplyr::group_by(.data$abin) %>%
    dplyr::mutate(rbin=cut(.data$r, stats::quantile(.data$r, probs=rcuts), labels=FALSE)) %>%
    dplyr::ungroup()

  # combine all the center objects
  X[is.na(X$rbin),c("abin", "rbin")] <- 0

  # and define a single identifier for each bin
  X$bin <- interaction(X$abin, X$rbin)

  # construct the morph in each tile
  Xm <- ggmorph_morph(X, imgs, n_imgs, adjust_grey)

  # and plot all morphs
  ggmorph_plot(Xm, scale)
}


# Get objects in the full space that are close to the plane defined by the selected dimensions
ggmorph_get <- function(space, dimensions) {
  if (length(dimensions) != 2) {
    stop("dimensions should be a vector of length 2")
  }

  # get coordinates
  X <- as.data.frame(space$ind$coord)

  # detect points close to the plane of interest
  # = in the middle of the other dimensions
  middle <- X[,-c(dimensions)] %>%
    sapply(function(x) {dplyr::between(x, 0-stats::sd(x), 0+stats::sd(x))}) %>%
    apply(1, all)

  # reformat X
  X <- X[, dimensions]
  names(X) <- c("x", "y")
  X$i <- 1:nrow(X)
  # and get the objects of interest
  X <- X[middle,]

  X
}


# Build morphs in each bin
ggmorph_morph <- function(X, imgs, n_imgs, adjust_grey) {
  if (n_imgs > 100) {
    stop("n_imgs is too big")
  }
  X %>% dplyr::group_by(.data$bin) %>%
    # pick at most n_imgs in each bin
    dplyr::sample_n(size=min(n_imgs, dplyr::n())) %>%
    dplyr::summarise(
      # compute the actual position of the bin's center
      x=mean(.data$x), y=mean(.data$y),
      # and morph the n_imgs
      img=list(morph(imgs[.data$i], adjust_grey=adjust_grey)/255)
    ) %>%
    # measure the dimensions of each image
    dplyr::group_by(.data$bin) %>%
    dplyr::mutate(w=ncol(.data$img[[1]]), h=nrow(.data$img[[1]])) %>%
    dplyr::ungroup()
}


# Plot morphed images
ggmorph_plot <- function(Xm, scale) {
  # prepare the plot space
  p <- ggplot2::ggplot() +
    ggplot2::coord_fixed(xlim=range(Xm$x), ylim=range(Xm$y)) +
    # TODO: maybe expand the range a bit
    ggplot2::theme_void() +
    ggplot2::geom_hline(ggplot2::aes(yintercept=0), colour="grey80") +
    ggplot2::geom_vline(ggplot2::aes(xintercept=0), colour="grey80")

  # plot each morphed image
  for (i in 1:nrow(Xm)) {
    Xi <- Xm[i,]
    p <- p + ggplot2::annotation_custom(
      grid::rasterGrob(Xi$img[[1]]),
      xmin=Xi$x-Xi$w*scale, xmax=Xi$x+Xi$w*scale,
      ymin=Xi$y-Xi$h*scale, ymax=Xi$y+Xi$h*scale
    )
  }

  p
}
