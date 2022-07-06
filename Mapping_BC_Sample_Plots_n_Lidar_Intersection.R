#The purpose of this script is to evaluate how many BC gov plots align with freely available lidar data
  #If there are a substantial number of plots (i.e., >100) biomass will be calculated for each one

#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)

#Get data ----

#BC government lidar coverage
lidar_cov <- st_read("D:/Sync/Data/Lidar_Coverage/BC_Lidar_Coverage/BC_Lidar_Tiles.shp")

#Ground plot data 
plots <- read.csv("D:/Sync/Data/Provincial_Sample_Plot_Data/BC/faib_open_data_ground_samples_compiled_summaries.csv")
tree_level <- read.csv("D:/Sync/Data/Provincial_Sample_Plot_Data/BC/faib_open_data_ground_samples_stand_stock_tables.csv")


#Clean lidar coverage data ----

#Check the types of plots
table(plots$sampletype)

#Check UTM zones coverage
table(plots$utm_zone)

#Divide the plots into UTM zones
plots8 <- plots[plots$utm_zone == 8,]
plots9 <- plots[plots$utm_zone == 9,]
plots10 <- plots[plots$utm_zone == 10,]
plots11 <- plots[plots$utm_zone == 11,]

#Convert each plot to a geographic point using appropriate UTM CRS

#UTM Zone 8 first: NAD83 / UTM zone 8N uses the EPSG code: 26908 
plots8 <- st_as_sf(x = plots8, coords = c("utm_easting", "utm_northing"), crs = "26908")
st_crs(plots8) <- 26908 #Set CRS

#UTM Zone 9 first: NAD83 / UTM zone 9N uses the EPSG code: 26909 
plots9 <- st_as_sf(x = plots9, coords = c("utm_easting", "utm_northing"), crs = "26909")
st_crs(plots9) <- 26909 #Set CRS

#UTM Zone 10 first: NAD83 / UTM zone 10N uses the EPSG code: 26910 
plots10 <- st_as_sf(x = plots10, coords = c("utm_easting", "utm_northing"), crs = "26910")
st_crs(plots10) <- 26910 #Set CRS

#UTM Zone 11 second: NAD83 / UTM zone 11N EPSG code: 26911
plots11 <- st_as_sf(x = plots11, coords = c("utm_easting", "utm_northing"), crs = "26911")
st_crs(plots11) <- 26911 #Set CRS

#Convert all UTM zones to common CRS (NAD83 BC Environmental Albers EPSG:3005)
plots8 <- st_transform(plots8, crs = 3005)
plots9 <- st_transform(plots9, crs = 3005)
plots10 <- st_transform(plots10, crs = 3005)
plots11 <- st_transform(plots11, crs = 3005)

#Combine all plots into single sf object
plots <- rbind(plots8, plots9, plots10, plots11)

#Now visualize all the plots
plot(plots["sampletype"], axes = T, cex = 0.5, main = "Sample Plots", key.pos = 1)

#Perform intersect with lidar coverage and plots ----

#Ensure CRS is the same
st_crs(plots) == st_crs(lidar_cov)

#Dissolve the lidar coverage into a single polygon
lidar_cov_dis <- lidar_cov %>% group_by() %>% summarise()

#Plot the lidar coverage and the sample plots
plot(st_geometry(lidar_cov_dis), bg = NA, axes = T)
plot(plots["sampletype"], cex = 0.5, key.pos = 1 , add = T)

#Intersect with lidar data and plots
plots_w_cov <- st_intersection(plots, lidar_cov)

#Check plot with coverage
plot(plots_w_cov["sampletype"], cex = 0.5, main = "Sample Plots with lidar", key.pos = 1, axes = T)

#Analyze sample plots with lidar data ----

#Check earliest year for lidar aquisition
min(plots_w_cov$year)

#Remove plots collected earlier than 2016
plots_w_cov <- plots_w_cov %>% filter(meas_yr >= 2016)

#Check time distribution of sample plots
hist(plots_w_cov$meas_yr)
table(plots_w_cov$meas_yr)

#Lots of overlap between multiple tiles, some plots have multiple sample times
nrow(plots_w_cov) 

#Check for number of unique plots
length(unique(plots_w_cov$SAMP_ID))

#Organize a df for rene request of plots by SAMP_ID and clstr_id
df_request <- plots_w_cov

#Now, remove all rows with duplicate CLSTR ID
length(unique(df_request$clstr_id))
length(unique(df_request$SAMP_ID))
length(unique(df_request$filename))
length(unique(df_request$tsa))
table(df_request$no_meas)

#Summarize each SAMP_ID by years collected
SAMP_ID_req <- df_request %>% group_by(SAMP_ID) %>% 
                                  summarize(count = n())



#Visualize plots with coverage
mapview(df_request)



#Export data ----

st_write(plots, dsn = "D:/Sync/Data/Provincial_Sample_Plot_Data/BC/Sample_Plot_Locations_Cleaned/sample_plots_locations_cleaned.shp",
         append = F)

st_write(plots_w_cov, dsn = "D:/Sync/Data/Provincial_Sample_Plot_Data/BC/Sample_Plot_Locations_Cleaned/sample_plot_locations_lidar_coverage.shp",
         append = F)

#Export csv without geometry
write.csv(st_drop_geometry(SAMP_ID_req), 
          file = "D:/Sync/Data/Provincial_Sample_Plot_Data/BC/Sample_Plot_Locations_Cleaned/SAMP_ID_req.csv")

#
