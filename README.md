# morphr

R (+ python) package to describe the morphological variations and create "morphotypes" from greyscale images

## Installation

`morphr` is not on CRAN yet, install it from this github repository with

```
remotes::install_github("jiho/morphr")
```

`morphr` relies on Python packages for much of its functionality. To intall them, use the function `install_py_deps()`.

## Usage

Use `imread()` and `imshow()` to read and display images.

Use `img_crop_largest()`, `img_props_largest()` extract and measure the largest object in an images. Use `make_transparent()` to ... make the background transparent.

Use `morpho_space()` to create a morphological space, i.e. a description of the morphological features of the images on a n-dimensional space, through a Principal Component Analysis.

Use `morph()` and `morph_dir()` to "morph" (i.e. fuse) several images into one, which is representative of the original batch of images.

Use `ggimg_grid()`, `ggmorph_radial()`, `ggmorph_tile()` to display several images and describe a morphological space by displaying morphs at regular locations in that space.
