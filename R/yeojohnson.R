# Most code inspired by https://github.com/petersonR/bestNormalize/blob/master/R/yeojohnson.R

#' Yeo-Johnson transformation
#'
#' Perform Yeo-Johnson transformation to attempt normalization.
#'
#' @param x a numeric vector of values to make normal.
#' @param lambda the value of the lambda parameter. When NULL, an appropriate value is estimated from the data.
#' @param eps the tolerance value under which lamba is considered to be 0 and that is used to choose the appropriate formula.
#'
#' @details The Yeo-Johnson is similar to the Box-Cox method, however it allows for the transformation of non-positive data as well.
#'
#' @return A vector of transformed values, with an attribute to store the lambda value. The object if of class 'yeojohnson' and can be used with [predict.yeojohnson()].
#'
#' @references Yeo, I. K., & Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. Biometrika.
#'
#' @seealso [predict.yeojohnson()] to apply the same transformation to a new dataset.
#'
#' @export
#' @examples
#' # simulate non-normal data
#' x <- rgamma(100, 1, 1)
#' hist(x)
#' # and make it more normal looking
#' hist(yeojohnson(x))
yeojohnson <- function(x, lambda=NULL, eps=0.001) {
  stopifnot(is.numeric(x))

  # estimate the value of lambda if needed
  if (is.null(lambda)) {
    lambda <- estimate_yeojohnson_lambda(x, eps=eps)
  }

  # transform non-missing data
  x_t <- x
  na_idx <- is.na(x)
  x_t[!na_idx] <- yeojohnson_trans(x[!na_idx], lambda=lambda, eps=eps)

  # prepare output
  attr(x_t, "lambda") <- lambda
  class(x_t) <- c("yeojohnson", class(x_t))
  return(x_t)
}

#' Transform new data using an already fitted Yeo-Johnson object
#'
#' @param object an object of class 'yeojohnson'
#' @param newdata a numeric vector of data to transform.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' # fit the Yeo=-Johnson transfomration on non-normal data
#' x <- rgamma(100, 1, 1)
#' hist(x)
#' xt <- yeojohnson(x)
#' # apply the same transformation to new data
#' x2 <- rgamma(100, 1, 1)
#' hist(x2)
#' hist(predict(xt, newdata=x2))
predict.yeojohnson <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) {
    # just output the already transformed data
    newdata <- object
  } else {
    # run the transformation
    na_idx <- is.na(newdata)
    newdata[!na_idx] <- yeojohnson_trans(newdata[!na_idx], attr(object, "lambda"))
  }
  return(newdata)
}

# Helper function that estimates the lambda parameter
#' @importFrom stats var optimize
estimate_yeojohnson_lambda <- function(x, lower=-5, upper=5, eps=0.001) {
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]

  # See Yeo & Johnson Biometrika (2000)
  yj_loglik <- function(lambda) {
    x_t <- yeojohnson_trans(x, lambda, eps)
    x_t_bar <- mean(x_t)
    x_t_var <- var(x_t) * (n - 1) / n
    constant <- sum(sign(x) * log(abs(x) + 1))
    -0.5 * n * log(x_t_var) + (lambda - 1) * constant
  }

  results <- optimize(yj_loglik, lower=lower,
                      upper=upper, maximum=TRUE,
                      tol=.0001)

  return(results$maximum)
}

# Workhouse function that performs the transformation
yeojohnson_trans <- function(x, lambda, eps=0.001) {
  pos_idx <- x >= 0
  neg_idx <- x < 0

  # Transform positive (non-negative) values
  if (any(pos_idx)) {
    if (abs(lambda) < eps) {
      x[pos_idx] <- log(x[pos_idx] + 1)
    } else {
      x[pos_idx] <- ((x[pos_idx] + 1) ^ lambda - 1) / lambda
    }
  }

  # Transform negative values
  if (any(neg_idx)){
    if (abs(lambda - 2) < eps) {
      x[neg_idx] <- - log(-x[neg_idx] + 1)
    } else {
      x[neg_idx] <- - ((-x[neg_idx] + 1) ^ (2 - lambda) - 1) / (2 - lambda)
    }
  }

  return(x)
}
