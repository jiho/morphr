% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morph.R
\name{morph}
\alias{morph}
\title{Morph several images into one}
\usage{
morph(paths, dest = "", adjust_grey = FALSE)
}
\arguments{
\item{paths}{paths to image files to morph together}

\item{dest}{path to a new file in which to write the morphed image. When empty (the default), the morphed image is returned as a matrix. When not empty, the image is written to disk and returned invisibly by the function.}

\item{adjust_grey}{whether to adjust the mean grey level of the morphed image to match that of the orignal images it was constructed with. Usually, computing the average(i.e. the morphing) results in lighter images; this compensates it a little. Note that it requires more intense computatin and is therefore switched off by default.}
}
\value{
A matrix containing the pixel values for the morphed image, in \code{[0,255]} (black to white).
}
\description{
Morph several images into one
}
\examples{
amph <- list.files(
          system.file("extdata", "amphipoda", package="morphr"),
          full.names=TRUE
       )
img <- morph(amph)
imshow(img)
img <- morph(amph, adjust_grey=TRUE)
imshow(img)
\dontrun{
img <- morph(files, dest=path.expand("~/amphipoda_morphed.jpg"))
}
cres <- list.files(
          system.file("extdata", "creseidae", package="morphr"),
          full.names=TRUE
       )
img <- morph(cres)
imshow(img)
img <- morph(cres, adjust_grey=TRUE)
imshow(img)
}
