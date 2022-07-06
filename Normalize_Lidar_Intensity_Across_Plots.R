#The purpose of this script is to achieve comparable intensity values across all plots used in modelling

#Get packages-----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#Get data ----

#Get las filenames
las_fns <- list.files("D:/Sync/Data/Model_Input/lidar_data")
las_folder <- "D:/Sync/Data/Model_Input/lidar_data"

#Check intensity vals and normalize ----

#Get range of intensity values for each plot
intensity_vals <- list()

#Retrieve set of intensity values from each las
for(i in 1:length(las_fns)){
  
  #Load las
  las_i <- readLAS(paste(las_folder, las_fns[i], sep = "/"))
  
  #Extract plotID
  plotID_i <- gsub(".las", "", las_fns[i])
  
  #Print plotID i
  print(paste("Plot:", plotID_i, "for iteration:", i))
  
  #Get point density in plot
  intensity_i <- las_i@data$Intensity
  
  #Store in list
  intensity_vals[[i]] <- intensity_i
  
  #Add name
  names(intensity_vals)[i] <- plotID_i
  
}


#Get min and max values of intensity for each plot
intensity_mins <- unname(unlist(lapply(intensity_vals, min)))
intensity_maxs <- unname(unlist(lapply(intensity_vals, max)))
names <- names(intensity_vals)

#Convert to dfs
intensity_df <- data.frame(PlotID = names,
                           i_min = intensity_mins,
                           i_max = intensity_maxs)

#Calculate difference between min and max intensity for each plot
intensity_df$i_diff <- intensity_df$i_max - intensity_df$i_min

#Sort by i_diff
intensity_df <- intensity_df[order(intensity_df$i_diff, decreasing = T),]

head(intensity_df)

#Normalize intensity
get_normalized_i <- function(i){
  i_norm = (i - min(i)) / (max(i) - min(i))
  return(i_norm)
}

range(get_normalized_i(intensity_vals[[1]]))*20 #want intensity to have a similar range compared to biomass values

intensity_vals_norm <- lapply(intensity_vals, get_normalized_i)

#Get BC lidar sensor info ----

#Load lidar project info
lidar_info <- st_drop_geometry(st_read("D:/Sync/Data/Lidar_Coverage/BC_Lidar_Coverage/BC_Lidar_Tiles.shp"))

#Get lidar aquisition project names
projects <- str_trim(unlist(strsplit(unique(plots$ldr_prj),",")))
projects

#Subset lidar info to relevant projects
lidar_info <- lidar_info[lidar_info$oper_name %in% projects,]

#Remove duplicates
lidar_info <- lidar_info[!duplicated(lidar_info$oper_name),]
lidar_info <- lidar_info %>% relocate(s3RptUrl, .after = oper_name)

#Add sensor types to plots df




