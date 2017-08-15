

library(snowfall)
library(parallel)
library(ggplot2)
library(reshape2)  # we need the function “melt”
library(devtools)  # not necessary
library(plyr)      # we need the ddply function in bootstrap_trend() operation
library(deSolve)   # we need the function “lsoda”
library(psych)     # we need the function “skew”, which has notified in the source file.
library(forecast)
library(Kendall)
library(smoother)  # Gaussian smoot


## If you want to source() a bunch of files, something like
## the following may be useful:
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}


#sourceDir("earlywarning_toolbox/R")

