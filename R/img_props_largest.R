#' Measure properties of the largest object in an image
#'
#' @param paths paths to the input image files.
#' @param properties vector of at least *two* image properties to measure. See \href{https://scikit-image.org/docs/dev/api/skimage.measure.html#skimage.measure.regionprops}{scikit-image's documentation} for a list of possible properties.
#' @inheritParams img_crop_largest
#'
#' @details Each input image is read, inverted and thresholded. Then, thresholded sections are measured and only the largest one is kept. Finally the properties of this largest region are recorded.
#'
#' @return A data.frame of image properties.
#' @export
#' @examples
#' # measure a batch of clean images
#' input  <- system.file("extdata", "amphipoda", package="morphr")
#' img_files <- list.files(input, full.names=TRUE)
#' img_props_largest(img_files, properties=c("area", "perimeter", "mean_intensity"), threshold=2/255)
#'
#' # measure one image with extra stuff on it
#' img_file  <- system.file("extdata", "16195419.jpg", package="morphr")
#' imshow(imread(img_file))
#' img_props_largest(img_file, properties=c("area", "perimeter", "mean_intensity"),
#'                   bottom=31, threshold=2/255)
img_props_largest <- function(paths, properties=c("area", "mean_intensity"),
                              top=0, right=0, bottom=0, left=0, threshold=0) {
  # measure each image
  out <- lapply(paths, function(p) {
    pymorph$img_props_largest(
      p, properties,
      as.integer(top), as.integer(right), as.integer(bottom), as.integer(left),
      threshold
    )
  })
  # get a nice data.frame as output
  out <- data.frame(do.call(rbind, out))
  return(out)
}
