#Script to clean DURIN WP4 data
#Install and load packages
packages <- c("readr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
library(packages)

#set wd
wd <- getwd()
setwd(wd)

#Vegetation measurements in control and removal plots
#Uploading Excel files
vegetation_data #<- read.csv('raw_data/.... . csv')#open datasheet