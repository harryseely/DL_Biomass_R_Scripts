#The purpose of this collection of scripts is prepare identify which lidar to download for the available ground plots
#..., to prepare and clean the lidar data, and to aggregate tree data to get biomass estimates for each plot


#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#     CLEAN LIDAR DATA ---------------------------------------------------------------------------------------
#********* LIDAR SECTION HAS LONG PROCESSING TIMES!!!! ---------------
#Clean lidar data ----

#Load plots and tree level data
plots_poly <- st_read("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_Plots_polygons_w_disturbance_n_lidar_data.shp")
tree_level <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Sample_Plot_Full_Dataset_Raw/faib_ground_samples_tree_detail.csv")

#Load lidar intersection with plots
target_lidar <- st_read("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_Plots_subset_intersect_w_lidar_tiles.shp")
target_lidar <- target_lidar %>% rename("OG_PlID" = "PlotID")

#Add updated plot ID to target lidar
target_lidar <- left_join(target_lidar, 
                          st_drop_geometry(plots_poly[,c("PlotID", "OG_PlID")]), 
                          by ="OG_PlID")

#Reconfigure 
target_lidar <- target_lidar %>% relocate(PlotID, .before = clstr_d)
target_lidar <- target_lidar %>% subset(select = -OG_PlID)

#Summarize lidar attributes for each plot by PlotID
lidar_summary <- target_lidar %>% 
  group_by(PlotID) %>%
  summarize(num_tiles = n(),
            filenames = toString(unique(filenam)),
            crs = toString(unique(projctn)),
            year = toString(unique(lidr_yr)))

#Check out lidar summary
table(lidar_summary$num_tiles)
table(lidar_summary$crs)

#There is 1 plot that has two tiles with differing CRS, remove this plot
plotID_rm1 <- lidar_summary$PlotID[lidar_summary$crs == "utm10, utm09"]
#Remove the weird plot that has the BC albers crs
plotID_rm2 <- lidar_summary$PlotID[lidar_summary$crs == "bcalbers"]
#Remove plots from polygons and lidar summary
plots_poly <- plots_poly[!plots_poly$PlotID %in% c(plotID_rm1, plotID_rm2),]
lidar_summary <- lidar_summary[!lidar_summary$PlotID %in% c(plotID_rm1, plotID_rm2),]

#Join lidar summary and plots poly
plots_poly <- left_join(plots_poly, 
                        st_drop_geometry(lidar_summary), 
                        by = "PlotID")

#Set directories
out_dir = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_clipped_to_plots/"
in_dir = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_tiles_raw/"

#Check height of tallest tree in dataset in order to set the maximum height allowance after normalization
max(tree_level$height, na.rm = T)
max(tree_level$ht_calc, na.rm = T)
max(tree_level$ht_est, na.rm = T)
max(tree_level$ht_meas_orig, na.rm = T)

#Create a field to note if a LAS tile does not contain a plot
plots_poly$missing_las <- "N"

#Loop through each plot and load the required tile(s) as a LAScatalog
for(i in 1:nrow(plots_poly)){
  #1) Select plot_i
  plot_i <- plots_poly[i, ]
  #2) Get PlotID
  plotID_i <- plot_i$PlotID
  #3) Get filename(s)
  las_fns <- plot_i$filenames
  las_fns <- unlist(strsplit(las_fns, ", "))
  print(paste("Working on", plotID_i, "with", las_fns, "file(s)."))
  #4) Load files as las catalog
  ctg <- readLAScatalog(paste(in_dir, las_fns, sep = ""))
  opt_progress(ctg) <- FALSE #Turn off progress reporting
  #5) Reproject Plot_i to the crs of las catalog
  target_crs <- st_crs(ctg)
  if(!is.na(target_crs)){
    plot_i <- st_transform(plot_i, crs = target_crs)
    #6) Clip LAS catalog to the plot_i polygon
    plot(ctg, main = paste(plotID_i)); plot(st_geometry(plot_i), lwd =8,  add = T) #Note: plot size exaggerated for viewing
    las <- clip_roi(ctg, st_geometry(plot_i))
    #6) If the tile contains the plot, process, if not, go to next iteration
    if(length(las$Z) > 0){ #Check num Z coords, if zero, move on
      #7) Filter duplicate points
      las = filter_duplicates(las)
      #8) Perform ground classification and height normalization of the clipped las
      dtm <- grid_terrain(las, 1, knnidw()) #Create dtm
      las <- normalize_height(las, dtm, method = "bilinear") #Normalize height
      #9) Remove outlier points below 0m and over 70m (see maximum tree height above)
      las <- filter_poi(las, Z < 70 & Z > 0)
      #10) export the cleaned and normalized las to designated folder
      writeLAS(las, file = paste(out_dir, plotID_i, ".las", sep = ""))
      print(paste("Done las ", i, "out of ", nrow(plots_poly)))
    } else{
      plots_poly$missing_las[i] <- "Y" #Note that plot is missing LAS data
      print(paste("Plot", plotID_i, "is not located within", las_fns))
      print("Moving on to next file...")
    } 
  } else{
    plots_poly$missing_las[i] <- "Y" #Note that plot is missing LAS data
    print(paste("Plot", plotID_i, "does not have a CRS for", las_fns))
    print("Moving on to next file...")
  }
}

#Check that las clipped properly to plot
#las_check_flist <- list.files(out_dir)
#for(i in seq(from = 4,to = length(las_check_flist), by = 4)){
#  las <- readLAS(paste(out_dir, las_check_flist[i], sep = ""))
#  plot(las)}


