# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-30
#
# Script Name:
#
# Script Description:
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")



# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "stringr", "readxl") # list of packages to load
n_packages <- length(packages) # count how many packages are required

new.pkg <- packages[!(packages %in% installed.packages())] # determine which packages aren't installed

# install missing packages
if(length(new.pkg)){
  install.packages(new.pkg)
  }

# load all requried libraries
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
  eval(parse(text = lib_load)) # evaluate the string to load the library
  }

# LOAD FUNCTIONS ------------------------------------
# space reserved for your functions
# 


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------