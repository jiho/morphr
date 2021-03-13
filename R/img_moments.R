#' Image moments
#'
#' Compute raw, central, or central normalised moments of a greyscale image.
#'
#' @inheritParams img_make_transparent
#' @param order maximal order of the moments to compute.
#'
#' @details Black pixels have a value of 0 and therefore do not contribute to the moments. To compute the moments based on the shape of the object only, compute a mask where the object is white (1) and the background black (0). To compute the moments of an object based on the grey scale intensities in the object, make the background black (i.e. invert it when the image is dark on white).
#'
#' @return A square matrix of dimension `order`+1. The usual notation of moments uses 0-based indexing (M\[0,0\] is the first moment) while R uses 1-based indexing (M\[1,1\] is the first moment). To facilitate the extraction of moments from those matrices, we provide the utility function [get_moment()] which does the 0-based indexing.
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "16195419.jpg", package="morphr"))
#' img_show(x)
#' # extract the (largest) object from the image
#' X <- img_extract_largest(x)
#' img_show(X)
#' # convert it into a binary mask
#' X_binary <- X<1
#' img_show(X_binary)
#' # and into an inverted greyscale image
#' X_intensity <- 1-X
#' img_show(X_intensity)
#'
#' # compute the moments
#' # of the binary image
#' m <- img_moments(X_binary, 2)
#' mu <- img_moments_central(X_binary, 2)
#' nu <- img_moments_normalised(X_binary, 2)
#' # of the intensity image
#' m_i <- img_moments(X_intensity, 2)
#' mu_i <- img_moments_central(X_intensity, 2)
#' nu_i <- img_moments_normalised(X_intensity, 2)
#'
#' # from the (binary) moments, other properties can be derived
#' # http://raphael.candelier.fr/?blog=Image%20Moments
#' area <- gm(m, 0, 0)
#' centroid <- c(
#'   gm(m,1,0)/gm(m,0,0),
#'   gm(m,0,1)/gm(m,0,0)
#' )
#' centre_of_mass <- c(
#'   gm(m_i,1,0)/gm(m_i,0,0),
#'   gm(m_i,0,1)/gm(m_i,0,0)
#' )
#' orientation <- 0.5 * atan(2 * gm(mu,1,1) / (gm(mu,2,0) - gm(mu,0,2))) + pi/2
#'
#' # further normalise by area
#' mu20_n <- gm(mu,2,0)/gm(mu,0,0)
#' mu02_n <- gm(mu,0,2)/gm(mu,0,0)
#' mu11_n <- gm(mu,1,1)/gm(mu,0,0)
#' major_axis <- 0.5 * sqrt(8 * (mu20_n + mu02_n + sqrt(4* mu11_n^2 + (mu20_n - mu02_n)^2)))
#' minor_axis <- 0.5 * sqrt(8 * (mu20_n + mu02_n - sqrt(4* mu11_n^2 + (mu20_n - mu02_n)^2)))
#'
#' # which allows to identify remarquable features of the image
#' image(0:18, 0:27, X_intensity[,,1,1], asp=1, ylim=c(27,0), col=grey(seq(1,0,length=255)))
#' points(centroid[1], centroid[2], col="red")
#' points(centre_of_mass[1], centre_of_mass[2], pch=3, col="red")
#' lines(ellipse(major_axis, minor_axis, orientation, centroid[1], centroid[2]), col="blue")
#' # and rotate it to a horizontal orientation
#' (1- imager::imrotate(1-X, -orientation*180/pi, boundary=0)) %>% img_show()
#'
#' # all this is implemented as functions of this package
img_moments <- function(x, order=3) {
  M <- x[,,1,1]

  # dimensions
  h <- nrow(M)
  w <- ncol(M)

  # matrices of indexes
  # NB: computation assumes 0-based indexing
  x <- matrix(rep(0:(w-1), times=h), nrow=h, byrow=TRUE)
  y <- matrix(rep(0:(h-1), times=w), nrow=h)

  # prepare storage for moments
  m <- matrix(NA, nrow=order+1, ncol=order+1)
  # compute moments
  for (i in 0:order) {
    for (j in 0:order) {
      m[i+1,j+1] <- sum(x^i*y^j*M)
    }
  }

  # transpose the matrix to match scikit-image row-col convention
  # this makes it
  m <- t(m)
  return(m)
}

#' @rdname img_moments
#' @export
img_moments_central <- function(x, order=3) {
  M <- x[,,1,1]

  # dimensions
  h <- nrow(M)
  w <- ncol(M)

  # centroid
  m <- img_moments(x, order=1)
  x_bar <- m[1,2]/m[1,1]
  y_bar <- m[2,1]/m[1,1]

  # matrices of indexes, relative to the centroid
  # NB: computation assumes 0-based indexing
  x <- matrix(rep(0:(w-1)-x_bar, times=h), nrow=h, byrow=TRUE)
  y <- matrix(rep(0:(h-1)-y_bar, times=w), nrow=h)

  # prepare storage for moments
  mu <- matrix(NA, nrow=order+1, ncol=order+1)
  # compute moments
  for (i in 0:order) {
    for (j in 0:order) {
      mu[i+1,j+1] <- sum(x^i*y^j*M)
    }
  }

  mu <- t(mu)
  return(mu)
}

#' @rdname img_moments
#' @export
img_moments_normalised <- function(x, order=3) {
  # checks
  if (inherits(x, "imager_array")) {
    x <- img_moments_central(x, order=order)
  } else if (inherits(x, "matrix")) {
    # TODO document the fact that X can be a moments matrix also
    if (nrow(x) != ncol(x)) {
      stop("x should be a square moments matrix; it is now square")
    }
    max_order <- nrow(x) - 1
    if (max_order < order) {
      warning("Cannot computes normalised moments of order ", order, " from central moments of order ", max_order, "; reducing to order ", order)
      order <- max_order
    }
  } else {
    stop("x should be an imager object or a moments matrix")
  }
  nu <- matrix(NA, nrow=order+1, ncol=order+1)
  for (i in 0:order) {
    for (j in 0:order) {
      if (i+j >= 2) {
        nu[i+1,j+1] <- x[i+1,j+1] / (x[1,1]^(1+(i+j)/2))
      }
    }
  }

  return(nu)
}

# TODO implement Hu moments

#' Get an image moment
#'
#' Extract an image moment from a square moments matrix using 0-based indexing.
#'
#' @param x a moments matrix generated by of the [img_moments()] functions
#' @param i,j the 0-based indexes of the matrix
#'
#' @export
#' @examples
#' x <- img_read(system.file("extdata", "plank/16199658.jpg", package="morphr"))
#' X_binary <- x < 1
#' m <- img_moments(X_binary, 3)
#' m
#' get_moment(m, 0,0)
#' gm(m, 0,0)
get_moment <- function(x, i=0, j=0) {
  x[i+1,j+1]
}
#' @rdname get_moment
#' @export
gm <- get_moment
