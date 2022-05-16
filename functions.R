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

# This function takes a character vector and constructs factor labels
# that can be used for nicer titles and annotations in ggplots 
# by using the syntax used by bquote().
make_labels <- function(names){
  levels <- unique(names)
  labels <- gsub("(\\w)(\\d+)", "*\\1[\\2]*", levels)
  labels <- gsub("\\s", "~", labels)
  labels <- gsub("^(\\*|~)", "", labels)
  labels <- gsub("(\\*|~)$", "", labels)
  labels <- gsub(",", "*symbol(',')*", labels)
  labels <- gsub("\\*\\*", "\\*", labels)
  labels
}

# This function calculates the deviation of each element of the vector x
# from the closest multiple the number passed as argument "multiple"
get_deviation <- function(x, multiple){
  stopifnot(length(multiple) == 1L)
  times <- floor(x / multiple)
  lower <- (times * multiple) - x
  upper <- multiple - abs(lower)
  ifelse(abs(lower) < upper, lower, upper)
}

