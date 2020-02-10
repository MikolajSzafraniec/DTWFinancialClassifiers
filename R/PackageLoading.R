# This script is responsible for installing (if they're not already installed)
# and loading necessary packages

checkInstalled <- function(necessaryPackages, cranMirror = getOption("repos")){
  whichNotInstalled <- which(!(necessaryPackages %in% installed.packages()))
  
  if(length(whichNotInstalled))
    lapply(necessaryPackages[whichNotInstalled], FUN = install.packages,
           repos = cranMirror)
}

checkRequired <- function(necessaryPackages){
  whichNotRequired <- which(!(necessaryPackages %in% loadedNamespaces()))
  
  if(length(whichNotRequired))
    lapply(necessaryPackages[whichNotRequired], FUN = require, character.only = TRUE)
}

necessaryPackages <- necPack()

checkInstalled(necessaryPackages)
checkRequired(necessaryPackages)
