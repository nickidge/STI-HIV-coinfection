### Main line of code: run this to run the whole model ###

# # On first run, make sure to run the following line of code to install necessary packages
# install.packages(c("plyr", "readxl", "abind", "lambda.tools", "dfoptim", "gplots", "ggplot2", "reshape2",
#                    "directlabels", "ggpubr", "scales", "gridExtra", "lemon", "tables", "gdata", "tidyr"))

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages
library(plyr)
library(base)
library(readxl)
library(abind)
library(lambda.tools)
library(dfoptim)
library(gplots)
library(ggplot2)
library(reshape2)
library(directlabels)
library(ggpubr)
library(scales)
library(gridExtra)
library(lemon)
library(tables)
library(gdata)
library(tidyr)
library(dplyr)
library(minpack.lm)
library(grid)

label_years = unique(c(plot_years, seq(2000, 2050, by=5)))

options(stringsAsFactors = FALSE)

makearray = function(dimnames){
  return(array(0, dim=lengths(dimnames), dimnames=dimnames))
}

DIM <- function( ... ){
  args <- list(...)
  lapply( args , function(x) { if( is.null( dim(x) ) )
    return( length(x) )
    dim(x) } )
}