% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morph_dir.R
\name{morph_dir}
\alias{morph_dir}
\title{Morph images from a directory into one}
\usage{
morph_dir(path, n = 15, write = FALSE, adjust_grey = FALSE)
}
\arguments{
\item{path}{path to the directory containing image files to morph together}

\item{n}{how many images to randomly select within the directory. Set this to a very high number to process all}

\item{write}{boolean, whether to write the image next to the directory in which the files are read. When FALSE (the default), the morphed image is returned, as a matrix. When TRUE, the image is written to disk and returned invisibly by the function.}

\item{adjust_grey}{whether to adjust the mean grey level of the morphed image to match that of the orignal images it was constructed with. Usually, computing the average(i.e. the morphing) results in lighter images; this compensates it a little. Note that it requires more intense computatin and is therefore switched off by default.}
}
\value{
A matrix containing the pixel values for the morphed image, in \code{[0,255]} (black to white).
}
\description{
Morph images from a directory into one
}
\examples{
amph <- system.file("extdata", "amphipoda", package="morphr")
imshow(morph_dir(amph))
imshow(morph_dir(amph, n=3))
imshow(morph_dir(amph, n=10, adjust_grey=TRUE))
}
