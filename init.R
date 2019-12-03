### Main line of code: run this to run the whole model ###

# # On first run, make sure to run the following line of code to install necessary packages
# install.packages(c("plyr", "readxl", "abind", "lambda.tools", "dfoptim", "gplots", "ggplot2", "reshape2",
#                    "directlabels", "ggpubr", "scales", "gridExtra", "lemon", "tables", "gdata", "tidyr"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

# record start time. This is to record how long the model takes to run
ptm <- proc.time()

fixnan = function(x){
  x[is.nan(x)] = 0
  return(x)
}

makearray = function(dimnames){
  return(array(0, dim=lengths(dimnames), dimnames=dimnames))
}

options(stringsAsFactors = FALSE)
