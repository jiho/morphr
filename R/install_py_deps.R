#' Install python dependencies
#'
#' Tests whether os, numpy and skimage are available. Install them if they are not.
#' @examples
#' \dontrun{
#' # check which python version is detected and install python modules
#' library("reticulate")
#' py_config()
#' install_py_deps()
#'
#' # use python3 (works only in a new R session)
#' library("reticulate")
#' use_python(system("which python3", intern=TRUE))
#' py_config()
#' install_py_deps()
#' }
#' @export
install_py_deps <- function() {
  test_module("os")
  test_module("pillow", "PIL")
  test_module("scikit-image", "skimage")
  test_module("numpy")
  message("All done")
}

#' Test whether a python module is installed
#'
#' @param module name of the module to test
#' @param import_name name under which the module should be imported (when different from the module name); for e.g. scikit-image which is imported as skimage
test_module <- function(module, import_name=module) {
  tryCatch(
    reticulate::import(import_name),
    error=function(e) {
      message(e$message)
      message("Installing...")
      pip(module)
    }
  )
}

#' Install a python module with pip
#'
#' @param module name of the module to install
pip <- function(module) {
  # find python version
  conf <- reticulate::py_discover_config()
  v <- as.numeric(conf$version)
  # determine which version of pip to run depending on the python version
  pip <- paste0("pip", floor(v))
  # run the install command
  system2(pip, paste("install", module))
}
