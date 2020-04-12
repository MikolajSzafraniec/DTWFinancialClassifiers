# This script is intendent to create package with C++ functions

require(Rcpp)
require(devtools)

Rcpp.package.skeleton(name = "RcppShapeDTW",
                      path = ".", 
                      cpp_files = paste(
                        "src",
                        list.files(path = "src"),
                        sep = "/"))


