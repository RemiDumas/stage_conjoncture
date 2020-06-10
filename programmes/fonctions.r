# Installation et chargement de packages
install_and_load <- function(P) {
  Pi <- P[!(P %in% installed.packages()[,"Package"])]
  
  if (length(Pi)>0) install.packages(Pi)
  for(i in P) library(i,character.only = TRUE)
}

packages <-
  c("downloader",
    "glue",
    "hms",
    "httr",
    "lubridate"  ,
    "readxl",
    "rio"   ,
    "rlang"  ,
    "tidyverse"
  )

install_and_load(packages)