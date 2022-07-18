#The purpose of this collection of scripts is prepare identify which lidar to download for the available ground plots
#..., to prepare and clean the lidar data, and to aggregate tree data to get biomass estimates for each plot

#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#     CHECK LIDAR COVERAGE FOR EACH PLOT --------------------------------------------------
#Get data ----

#Set directory
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC")

#Set mapview settings 
mapviewOptions(fgb = FALSE)

#Load plot data 
plots <- st_read("Outputs/BC_plots_paired_with_lidar_cleaned.shp")

#Load a subset of the lidar data 
las_fns <- list.files("BC_Lidar_clipped_to_plots")
las_folder <- paste(getwd(), "BC_Lidar_clipped_to_plots", sep = "/")

#Test for a single LAS ----

i = 5

#Load las
las <- readLAS(paste(las_folder, las_fns[i], sep = "/"))

#Extract plotID
plotID <- gsub(".las", "", las_fns[i])

#Select associated plot
plot <- plots[plots$PlotID == plotID,]

#Convert las into 2D point data (sf object)
lascov <- st_as_sf(las)



#Convert points into a polygon hull
hull <- lascov %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull() %>% st_transform(crs = st_crs(lascov))

#Reproject hull to same CRS as plots
hull <- st_transform(hull, crs = st_crs(plots))

#Add plot ID to hull object
hull$PlotID <- plotID

#Visualize plot with lidar coverage
mapview(hull, col.region = "red") + 
  mapview(plots[plots$PlotID == plotID,], col.region = "green")

#Retrieve the coverage of all LAS files ----

#Set las_hull object to store all the polygons of las coverage
las_hull <- st_sf(id = 1, 
            geometry = st_sfc(lapply(1, 
                                     function(x) st_polygon())),
                                      crs = st_crs(plots))

#Add PlotID col
las_hull$PlotID <- NA

#Relocate PlotID and remove other ID
las_hull <- las_hull %>% relocate(PlotID, .before = id)
las_hull <- las_hull %>% subset(select = -id)

#Remove first row of empty df
las_hull <- las_hull[0,]

for(i in 1:length(las_fns)){
  
  #Load las
  las_i <- readLAS(paste(las_folder, las_fns[i], sep = "/"))
  
  #Extract plotID
  plotID_i <- gsub(".las", "", las_fns[i])
  
  #Select associated plot
  plot_i <- plots[plots$PlotID == plotID_i,]
  
  #Convert las into 2D point data (sf object)
  lascov_i <- st_as_sf(las_i)
  
  #Convert points into a polygon hull
  hull_i <- lascov_i %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_convex_hull() %>% st_transform(crs = st_crs(lascov_i))
  
  #Reproject hull to same CRS as plots
  hull_i <- st_transform(hull_i, crs = st_crs(plot_i))
  
  #Add plot ID to hull object
  hull_i$PlotID <- plotID_i
  
  #relocate plotID
  hull_i <- hull_i %>% relocate(PlotID, .before = geometry)
  
  #Append hull i to the las_hull object
  las_hull <- rbind(las_hull, hull_i)
  
  print(paste("Completed", i, "out of", length(las_fns), "las files!"))

}

#Drop z geometry for las_hull
las_hull <- st_zm(las_hull)

#Visualize plots and las coverage ----

mapview(plots, col.region = "green") +
  mapview(las_hull, col.region = "red", alpha.region = 0, color = "red", lwd = 1)

#Verify las coverage for each plot ----

#Sort the las hull object so that it matches the plots
table(las_hull$PlotID[order(match(las_hull$PlotID, plots$PlotID))] == plots$PlotID)
las_hull <- las_hull[order(match(las_hull$PlotID, plots$PlotID)),]

#Create overlap field in plots
plots$overlap_m_sqrd <- NA

for(i in 1:nrow(plots)){
  #For each plot, check the overlap with its las hull counterpart
  plots$overlap_m_sqrd[i] <- st_intersection(las_hull[i,], plots[i,]) %>% 
    mutate(overlap_m_sqrd = st_area(.)) %>%   # create new column with shape area
    dplyr::select(overlap_m_sqrd) %>%   # only select column needed
    st_drop_geometry()  # drop geometry as we don't need it
}

#Get each plot area
plots$area_m_sqrd <- plots %>% st_area(.)

#Unlist coverage vector
plots$overlap_m_sqrd <- unlist(plots$overlap_m_sqrd)


#Drop units
plots$area_m_sqrd <- drop_units(plots$area_m_sqrd)

#Get the overlap percentage
plots$overlap_percent <- plots$overlap_m_sqrd/plots$area_m_sqrd

#Sort plots based on overlap
plots <- plots[order(plots$overlap_percent, decreasing = T),]

#It appears that one plot is barely covered by lidar
problem_plot <- plots %>% filter(overlap_percent < 0.5)
problem_plot_ID <- problem_plot$PlotID


#Visualize plot(s) with poor coverage
mapview(plots[plots$PlotID %in% problem_plot_ID,], col.region = "green") +
  mapview(las_hull[las_hull$PlotID %in% problem_plot_ID,], col.region = NA, alpha.region = 0, color = "red", lwd = 1)

#Plot the las file with issues
problem_las <- readLAS(paste(las_folder, "/", problem_plot_ID, ".las", sep = ""))

#Check
plot(problem_las)

#REMOVE PROBLEMATIC PLOT(S) -----

#Remove from las filename list
las_fns <- las_fns[!las_fns %in% paste(problem_plot_ID, ".las", sep = "")]

#Remove problem plot(s) from df
plots <- plots %>% filter(!plots$PlotID %in% problem_plot_ID)

#Check final plot coverage
range(plots$overlap_percent)
mean(plots$overlap_percent)

#Visually inspect some point clouds ----

for(i in seq(1, nrow(plots), 20)){
  
  #Load las
  las_i <- readLAS(paste(las_folder, las_fns[i], sep = "/"))
  
  #Extract plotID
  plotID_i <- gsub(".las", "", las_fns[i])

  #Print plotID i
  print(paste("Plot:", plotID_i, "for iteration:", i))
  
  #Plot las
  plot(las_i)
}

#Check point density of each plot ----

#Create number of points and point density field
plots$n_poi <- NA
plots$pd <- NA

#Retrieve numbver of points and point density from each las
for(i in 1:length(las_fns)){
  
  #Load las
  las_i <- readLAS(paste(las_folder, las_fns[i], sep = "/"))
  
  #Extract plotID
  plotID_i <- gsub(".las", "", las_fns[i])
  
  #Print plotID i
  print(paste("Plot:", plotID_i, "for iteration:", i))
  
  #Get number of points in plot
  n_poi_i <- length(las@data$NumberOfReturns)
  plots$n_poi[plots$PlotID == plotID_i] <- n_poi_i
  
  #Get point density in plot
  plots$pd[plots$PlotID == plotID_i] <- density(las_i)
  
}

#Remove plots that have a point density lower than 8 points/m^2
low_density_plots <- plots %>% filter(pd < 8)
low_density_plot_ids <- low_density_plots$PlotID

#REMOVE THE PLOTS WITH LOW POINT DENSITY ----

#Remove plots with low point density
plots <- plots %>% filter(!plots$PlotID %in% low_density_plot_ids)

#Check final plot coverage
range(plots$pd)
mean(plots$pd)

#Remove from las filename list
las_fns <- las_fns[!las_fns %in% paste(low_density_plot_ids, ".las", sep = "")]

#Export finalized plots df and the lidar hull df ----

#Rename stand age
plots  <- plots %>% rename(std_age = stand_g)

st_write(plots,
         dsn = "Outputs//BC_PLOTS_FINAL.shp",
         append = F)

#Reduce LAS hull to plots with problematic lidar files removed
las_hull <- las_hull %>% filter(PlotID %in% plots$PlotID)

#Export LAS hull object 
st_write(las_hull, 
         dsn = "Outputs//las_files_hull_coverage.shp",
         append = F)

#Clean and copy LAS files without issues to model input LAS folder ----

in_dir = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_clipped_to_plots/"
out_dir = "D:/Sync/Data/Model_Input/lidar_data/"

for (i in 1:length(las_fns)){
  #Get las file
  lasfile_i <- las_fns[i]
  las_i <- readLAS(paste(in_dir, lasfile_i, sep = ""))
  #Change points that are labeled as being greater than the 4th return as 4th return
  las_i@data$ReturnNumber[las_i@data$ReturnNumber > 4] = 4
  #Convert ReturnNumber to integer
  las_i@data$ReturnNumber <- as.integer(las_i@data$ReturnNumber)
  #Save cleaned LAS
  writeLAS(las_i, paste(out_dir, lasfile_i, sep = ""))
  print(paste("Copying file:", las_fns[i]))
}


