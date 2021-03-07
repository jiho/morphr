#' Morphology of planktonic organisms
#'
#' A dataset containing link to images, concentration, and morphological descriptors of planktonic organisms.
#'
#' @format A data.frame of 500 rows and 10 variables
#' \describe{
#'   \item{id}{object identifier, which links to the corresponding image in the package data.}
#'   \item{w}{concentration in situ, which is to be considered as weights for the morphological space construction.}
#'   \item{area:mean}{morphological descriptors of each organism: area, major dimension length, minor dimension length (in pixels), elongation (=major/minor), fractal characteristic of the perimeter, circularity, average grey level, and coefficient of variation of the grey levels.}
#' }
#'
#' @source Underwater Vision Profiler samples from the GreenEdge 2016 cruise in Baffin Bay.
"plank"
