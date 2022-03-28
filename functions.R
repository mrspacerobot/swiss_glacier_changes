# This script contains self-written R functions used in the project

# Function that checks whether a package is already installed.
# If not, the package is installed from CRAN.
install_pkgs <- function(pkgs){
  install_one_pkg <- function(pkg){
    found <- tryCatch({ find.package(pkg); 0L },
                      error = function(e) 1L)
    if(found == 1L){
      message("Trying to install package \"", pkg, "\" from CRAN.")
      utils::install.packages(pkg)
      found <- tryCatch({ find.package(pkg); 0L },
                        error = function(e) 1L)
    }
    if(found == 1L) message("Package \"", x, "\" could not be installed from CRAN.")
    found
  }
  vapply(pkgs, install_one_pkg, integer(1L), USE.NAMES = TRUE)
}
