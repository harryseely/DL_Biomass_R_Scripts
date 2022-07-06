#Purpose of this script is to divide all LAS files clipped to ground plots into training and test sets
  #From a central folder with all the LAS files, a subset of training, testing, and validation data
  #... will be evenly sampled using stratified sampling based on total biomass
  #Data will be divided based on 70% training, 15% testing, 15% validation

#Get packages -----
library(sf)
library(lidR)
library(tidyverse)

#Get data -------

#Set directory
setwd("D:/Sync/Data/Model_Input")

#Get plot data
plots <- read.csv("model_input_plot_biomass_data.csv")

#Get list of las files
las_flist <- list.files("lidar_data")

#Check that all LAS files align with plot data
las_match <- paste(plots$PlotID, ".las", sep = "")
table(las_flist %in% las_match)

#Subset las_flist to those that have available plot data
las_flist <- las_flist[las_flist %in% las_match]

#Visualize biomass distribution after sampling----

#Distribution of all data
hist(plots$total_AGB)

#Randomly sample pop for training (70%), testing (15%), and validation (15%)
set.seed(99); idx <- sample(seq(1, 3), size = nrow(plots), replace = T, prob = c(.7, .15, .15)); idx
train_IDs <- plots$PlotID[idx == 1]
test_IDs <- plots$PlotID[idx == 2]
val_IDs <- plots$PlotID[idx == 3]

#Add sample ID to plots df
plots$sample_ID <- factor(idx)

#Plot the resulting distributions'
ggplot(plots, aes(x = total_AGB, fill = sample_ID)) +
  geom_histogram(position = "dodge") + 
  scale_fill_manual(values=c("green", "red", "blue"))

#Check that train/test/val sets evenly include datasets (BC, Romeo, Petawawa) -----

#Check even spread of source datasets among train/val/test sets
ggplot(plots, aes(x = total_AGB, fill = dataset)) +
  geom_histogram(position = "dodge") + 
  scale_fill_manual(values=c("green", "red", "blue"))

table(is.na(plots$sample_ID))



#Copy LAS files to new folders -----

#First, clear the three target folders of current las files
do.call(file.remove, list(list.files("train", full.names = TRUE)))
do.call(file.remove, list(list.files("test", full.names = TRUE)))
do.call(file.remove, list(list.files("val", full.names = TRUE)))

#Copy the files to their respective folders 
for (i in 1:length(las_flist)){
  
  lasfile_i <- las_flist[i]
  plot_ID_i = gsub('.las', '', las_flist[i])
  plot_ID_i = gsub('./', '', plot_ID_i)
  sample_ID_i = plots$sample_ID[plots$PlotID == plot_ID_i]
  
  if(plots$sample_ID[i] == 1){
    file.copy(paste("D:/Sync/Data/Model_Input/lidar_data/", plot_ID_i, ".las", sep = ""), 
              "D:/Sync/Data/Model_Input/train")
    
    } else if(plots$sample_ID[i] == 2){
      file.copy(paste("D:/Sync/Data/Model_Input/lidar_data/", plot_ID_i, ".las", sep = ""), 
                "D:/Sync/Data/Model_Input/test")
      
    } else{
      file.copy(paste("D:/Sync/Data/Model_Input/lidar_data/", plot_ID_i, ".las", sep = ""), 
                "D:/Sync/Data/Model_Input/val")
        
    }
  }



