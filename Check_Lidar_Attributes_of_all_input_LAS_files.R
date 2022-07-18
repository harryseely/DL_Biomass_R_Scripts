#Purpose of script:
  #Check Lidar Attributes of all input LAS files

#Get packages
library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#Get data

#Plot data
plots <- read.csv("D:/Sync/Data/Model_Input/model_input_plot_biomass_data.csv")

#Lidar data
las_fns <- list.files("D:/Sync/Data/Model_Input/lidar_data")

#Subset las_fns to plot data
plot_id_match <- paste(plots$PlotID, ".las", sep = "")

#Match las_fns to plot IDs
table(las_fns %in% plot_id_match)
las_fns <- las_fns[las_fns %in% plot_id_match]
rm(plot_id_match)

#Retrieve attributes for each LAS -----

#Set wd
setwd("D:/Sync/Data/Model_Input/lidar_data")

#Loop through each plot LAS and get attributes
  #Classification
  #Return number

#Create empty vectors
classification <- list(length = length(las_fns))
return_num <- list(length = length(las_fns))
num_points <- vector(length = length(las_fns))
PlotIDs = vector("numeric", length = length(las_fns))

#Read lidar data into high biomass las list
for (i in 1:length(las_fns)){
  #Load the las if its in the list
  las_i <- readLAS(file = las_fns[i], select = "xyzicrn")
  #Get plot ID from las filename
  PlotID_i = gsub('.las', '', las_fns[i])
  PlotIDs[i] = gsub('./', '', PlotID_i)
  #Get the classification
  classification[[i]] <- las_i@data$Classification
  #Get the return number
  return_num[[i]] <- las_i@data$ReturnNumber
  #Get the number of points
  num_points[i] <- length(las_i@data$ReturnNumber)
  #Print progress
  print(paste(i, "out of", length(las_fns)))
}

#Name lists by dataset ID derived from plotID
names(classification) <- substr(PlotIDs, 1, 2)
names(return_num) <- substr(PlotIDs, 1, 2)
names(num_points) <- substr(PlotIDs, 1, 2)

#Check the total number of points per plot
hist(num_points[names(num_points) == "BC"])
hist(num_points[names(num_points) == "PF"])
hist(num_points[names(num_points) == "RM"])


#Check to see which classifications each dataset has
table(unlist(classification[names(classification) == "BC"]))
table(unlist(classification[names(classification) == "RM"]))
table(unlist(classification[names(classification) == "PF"]))

#Check to see which return numbers each dataset has
table(unlist(return_num[names(return_num) == "BC"]))
table(unlist(return_num[names(return_num) == "RM"]))
table(unlist(return_num[names(return_num) == "PF"]))
