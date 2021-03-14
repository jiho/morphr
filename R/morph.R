#' Morph several images into one
#'
#' Given a set of images, rotate them to the same orientation (with [img_make_horizontal()]), align them to their centroid (with [img_center()]), average them, and, optionally, adjust the grey levels of the resulting, morphed, image (see `adjust_grey`).
#'
#' @param x list of images, of type [imager::cimg()], or list/vector of file paths that are then read with [img_read()].
#' @param adjust_grey whether to adjust the mean grey level of the morphed image to match that of the original images it was constructed with. Usually, computing the average (i.e. the morphing) results in lighter images; this compensates it a little. Note that it requires more intense computation and is therefore switched off by default.
#' @param threshold grey level threshold used to separate the foreground object from the background for the adjustment of grey level. Ignored when `adjust_grey` is FALSE.
#' @param invert passed to [img_read()] when `x` is a list/vector of file paths: whether to invert the images when reading them. If images are inverted upon being read, the morphed result is inverted back to be produced with the same aspect as the input files.
#'
#' @return The morphed image, as a [imager::cimg()] object.
#'
#' @export
#' @examples
#' # get a list of images to morph
#' amph <- list.files(system.file("extdata", "amphipoda",
#'                    package="morphr"), full.names=TRUE)
#' # read and plot a couple
#' img_read(amph[1]) %>% img_show()
#' img_read(amph[3]) %>% img_show()
#' # now morph them all
#' morph(amph) %>% img_show()
#' morph(amph, adjust_grey=TRUE) %>% img_show()
#'
#' cres <- list.files(system.file("extdata", "creseidae",
#'                    package="morphr"), full.names=TRUE)
#' img_read(cres[1]) %>% img_show()
#' img_read(cres[2]) %>% img_show()
#' morph(cres) %>% img_show()
#' morph(cres, adjust_grey=TRUE) %>% img_show()
morph <- function(x, adjust_grey=FALSE, threshold=2/255, invert=TRUE) {
  # read all images if x are paths
  if (is.character(x)) {
    ximg <- lapply(x, img_read, invert=invert)
  } else if ( inherits(x, "imager-array") ){
    ximg <- x
  } else {
    stop("x needs to be character or an image of class imager::cimg")
  }

  # rotate all images horizontally
  xhoriz <- lapply(ximg, img_make_horizontal)

  # center all images
  widths  <- sapply(xhoriz, imager::width)
  heights <- sapply(xhoriz, imager::height)
  w <- max(widths)  * 1.5 %>% round()
  h <- max(heights) * 1.5 %>% round()
  xcentred <- lapply(xhoriz, img_center, w=w, h=h, col=0)

  # combine in a single array
  xarray <- imager::as.imlist(xcentred) %>% imager::imappend("z")
  # crop to largest content
  xcropped <- imager::autocrop(xarray, color=0)

  # average (i.e. morph)
  xavg <- apply(xcropped[,,,1], 1:2, mean) %>% imager::as.cimg()

  if (adjust_grey) {
    # compute the average of the average greys of the input image
    target_grey <- sapply(ximg, function(img, thr=threshold) { mean(img[img>thr]) }) %>% mean()
    # make a function that adjusts the gamma and computes the difference in greys
    match_grey <- function(gamma, img, thr, target) {
      img_adjusted <- img_adjust_gamma(img, gamma=gamma)
      abs(target - mean(img_adjusted[img_adjusted>thr]))
    }
    # use numerical optimization to find the gamma that minimizes the difference in greys
    best_iter <- optim(
      # parameter to optimise: the gamma
      par=1, fn=match_grey,
      # optimisation method, its bounds, its tolerance
      # tolerance is factr*.Machine$double.eps => 1e14 gives ~ 0.02
      method="L-BFGS-B", lower=0.1, upper=2, control=list(factr=1e14),
      # further arguments to match_grey()
      img=xavg, thr=threshold, target=target_grey)
    # adjust the image with the best gamma value
    xadjusted <- img_adjust_gamma(xavg, best_iter$par)
  } else {
    xadjusted <- xavg
  }

  # if the images were inverted when they were read, reinvert the result
  if (is.character(x) & invert) {
    xadjusted <- 1 - xadjusted
  }

  return(xadjusted)
}
