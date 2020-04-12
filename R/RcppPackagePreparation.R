# This script is intendent to create package with C++ functions

require(Rcpp)
require(devtools)
library(tools)

Rcpp.package.skeleton(name = "RcppShapeDTW",
                      path = ".", 
                      cpp_files = paste(
                        "src",
                        list.files(path = "src"),
                        sep = "/"))
compileAttributes(pkgdir = "RcppShapeDTW", verbose = T)
package_native_routine_registration_skeleton(dir = "RcppShapeDTW",
                                             character_only = F)
