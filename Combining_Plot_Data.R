#The purpose of this script is to combine all the ground data for plot
  #... biomass into a single CSV

#Get packages 

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#Get data 

#Plot biomass data
bc_gov <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs/BC_biomass_by_plot.csv")
petawawa <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Petawawa/Outputs/petawawa_plots_w_biomass.csv")
romeo <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Romeo_Malette/Outputs/romeo_plots_w_biomass.csv")

#Combine
plots <- rbind(bc_gov, petawawa, romeo)

#Add a dataset source var
plots$dataset <- substr(plots$PlotID, 1, 2)
table(plots$dataset)
round(table(plots$dataset)/nrow(plots)*100, 0)

#Export
write.csv(plots, "D:/Sync/Data/Model_Input/model_input_plot_biomass_data.csv")

