#The purpose of this collection of scripts is prepare identify which lidar to download for the available ground plots
  #..., to prepare and clean the lidar data, and to aggregate tree data to get biomass estimates for each plot


#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#     S.1 CLEAN PLOT DATA ---------------------------------------------------------------------------------------
#Get data ----

#BC government lidar coverage
lidar_cov <- st_read("D:/Sync/Data/Lidar_Coverage/BC_Lidar_Coverage/BC_Lidar_Tiles.shp")

#Ground plot data 
plots <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Sample_Plot_Full_Dataset_Raw/sample_bymsmt.csv")
tree_level <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Sample_Plot_Full_Dataset_Raw/faib_ground_samples_tree_detail.csv")



#Clean plot data -----

#Need to remove duplicate rows (rows with repeated info for multiple DBH classes, shown in "util" col)

#First, remove unnecessary cols where year and SAMP_ID are duplicated
plots_filt <- plots[,-c(45:ncol(plots))]

#Filter duplicate rows
plots_filt <- plots[!duplicated(plots[,1:4]),]

#Check that # of SAMP_IDs (unique plot locations) is the same
length(unique(plots_filt$SAMP_ID))

#Filter plots that were sampled before 2013 (3 years before the earliest lidar aquisition)
  #... we are allowing for a buffer of +/- 3 years between lidar aquisition and ground sampling
plots_filt <- plots_filt %>% filter(meas_yr >= 2013)

#Check the types of plots (fixed radius versus variable radius)
table(plots_filt$plot_typ)

#Remove variable radius plots
plots_filt <- plots_filt %>% filter(plot_typ == "F")

#Check sampling programs
table(plots_filt$sampletype)

#Check that all SAMP_IDs have only 1 plot
table(plots_filt$no_plots)

#Only retain circular plots
table(plots_filt$shp_pm)
plots_filt <- plots_filt %>% filter(shp_pm == "C")

#Check the reliability of the GPS measurement for each plot
#... DGPS=digitally corrected GPS at IPC +/-10m accuracy (anything else is removed)
table(plots_filt$utm_source)
plots_filt <- plots_filt %>% filter(utm_source == "DGPS")

#Check that all plots have a provided radius
table(!is.na(plots_filt$rad_pm))
table(plots_filt$rad_pm)

#Check that clstr_id and SAMP_ID are same length
length(unique(plots_filt$SAMP_ID))
length(unique(plots_filt$clstr_id))

#Check the distribution of stand age
hist(plots_filt$tot_stand_age)
boxplot(plots_filt$tot_stand_age)

#Subset df to relevant fields
plots_filt <- plots_filt %>% subset(select = c("clstr_id",
                                               "SAMP_ID",
                                               "sampletype",
                                               "meas_yr",
                                               "meas_dt",
                                               "utm_zone",
                                               "utm_easting",
                                               "utm_northing",
                                               "rad_pm",
                                               "tot_stand_age"
                                               ))

#Assign each plot a new unique ID for further analysis
plots_filt <-  tibble::rowid_to_column(plots_filt, "PlotID")
plots_filt$PlotID <- paste("BCGOV", plots_filt$PlotID, sep = "")

#Convert plot locations into polygon sf objects ----

#Convert each plot to a geographic point using the BC Environmental Albers CRS

#Check UTM zones coverage
table(plots_filt$utm_zone)

#Divide the plots into UTM zones
plots9 <- plots_filt[plots_filt$utm_zone == 9,]
plots10 <- plots_filt[plots_filt$utm_zone == 10,]
plots11 <- plots_filt[plots_filt$utm_zone == 11,]

#Convert each plot to a geographic point using appropriate UTM CRS

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
plots9 <- st_transform(plots9, crs = 3005)
plots10 <- st_transform(plots10, crs = 3005)
plots11 <- st_transform(plots11, crs = 3005)

#Combine all plots into single sf object
plots_filt <- rbind(plots9, plots10, plots11)

#Now visualize all the plots as point locations
plot(plots_filt["sampletype"], axes = T, cex = 0.5, main = "Sample Plots", key.pos = 1)

#Check units for BC Env. Albers
st_crs(plots_filt)$units

#Convert plot radius to a units object
plots_filt$rad_pm <- set_units(plots_filt$rad_pm, m)

#Now, create a circular polygon from each plot using the radius as a buffer
plots_poly <- st_buffer(plots_filt, 
                        dist = plots_filt$rad_pm,
                        nQuadSegs = 30)

#Identify BC Lidar tiles overlap with plots ----

#Check that CRS match
st_crs(plots_poly) == st_crs(lidar_cov)

#Rename lidar aquisition year field
lidar_cov <- lidar_cov %>% rename("lidar_yr" = "year")

#Clip the BC lidar data to each plot
target_lidar <- st_intersection(plots_poly, lidar_cov)

#Get the number of tiles
length(unique(target_lidar$filename))

#Subset original lidar df to target files
target_lidar_full <- lidar_cov[lidar_cov$filename %in% unique(target_lidar$filename),]

#Check out the target tiles
mapview(target_lidar_full, zcol = "lidar_yr")

#Remove rows from the plots_poly df where the difference between the 
  #...lidar aquisition year and ground year is greater than 3
target_lidar$diff_yrs <- abs(target_lidar$meas_yr - target_lidar$lidar_yr)
table(target_lidar$diff_yrs)
target_lidar <- target_lidar %>% filter(diff_yrs <= 3)

#Get plot IDs from target lidar
target_plot_IDs <- unique(target_lidar$PlotID)

#Subset plot polygons to target plots
plots_poly <- plots_poly[plots_poly$PlotID %in% target_plot_IDs,]

#Dissolve the lidar coverage into a single polygon
lidar_cov_dis <- lidar_cov %>% group_by() %>% summarise(); plot(st_geometry(lidar_cov_dis))

#Check to see if plots are all contained within lidar tiles
containment <- st_covered_by(st_geometry(plots_poly), 
                             lidar_cov_dis, 
                             sparse = FALSE)
table(containment)

#Visually inspect containment
mapview(lidar_cov_dis, alpha.region = 0.1) + 
  mapview(plots_poly, col.regions = "red", color = "red", lwd = 4)


#Export the info for target lidar tiles
target_lidar_nogeo <- st_drop_geometry(target_lidar_full)
write.csv(target_lidar_nogeo, 
          file = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs/BC_Gov_target_lidar_info.csv")

#Check for overlap between plots and pest infestation/fire ----

#Read fire polygon
fire <- st_read("D:/Sync/Data/BC_Disturbance/Historical_fire_perimeters/H_FIRE_PLY_polygon.shp")
#Read pest infestation polygon
pest <- st_read("D:/Sync/Data/BC_Disturbance/Historical_Pest_infestation/pest_infestation_poly.shp")

#Clip fire and pests to the plots to check if there has been 
  #...disturbance between ground sampling and lidar aquisition
fire_clip <- st_intersection(plots_poly, fire)
pest_clip <- st_intersection(plots_poly, pest)

#Remove fire and pests to save space
rm(fire, pest)

#Filter polygons that have a capture year less earlier than 2012
fire_clip <- fire_clip %>% filter(FIRE_YEAR >= 2012 & FIRE_YEAR <= 2019)
pest_clip <- pest_clip %>% filter(CAPTURE_YE >= 2012 & CAPTURE_YE <= 2019)

#Examine fires that occured between 2012 and lidar aquisition
burned_plots <- plots_poly[unique(fire_clip$PlotID),]

#Check lidar acquisition with regard to burned plots
fire_clip$meas_yr
fire_clip$FIRE_YEAR
target_lidar$lidar_yr[target_lidar$PlotID %in% unique(fire_clip$PlotID)]

#Remove plots that burned between sampling and lidar aquisition
plots_poly <- plots_poly[!plots_poly$PlotID %in% unique(fire_clip$PlotID),]

#Filter plots from pest clip that have already been remode due to fire
pest_clip <- pest_clip[!pest_clip$PlotID %in% unique(fire_clip$PlotID),]

#Filter infestations that are classified as trace or light
pest_clip <- pest_clip %>% filter(PEST_SEVER != "T" & PEST_SEVER != "L")

#Summarize severity rating
table(pest_clip$PEST_SEVER)

#Check species
table(pest_clip$PEST_SPECI)

#Summarize the years
table(pest_clip$CAPTURE_YE)

#Check number of affected plots
length(unique(pest_clip$PlotID))

#Summarize pest disturbance by each plot
pest_summary <- pest_clip %>% 
                group_by(PlotID) %>%
                summarise(num_infestations = n(),
                          severity = toString(unique(PEST_SEVER)),
                          infest_yrs = toString(unique(CAPTURE_YE)),
                          ground_meas_yr = round(mean(meas_yr), 0)
                          )

#Add lidar aquisition year
pest_summary <- left_join(pest_summary, 
               st_drop_geometry(target_lidar[,c("PlotID", "lidar_yr")]),
               by = "PlotID")

#Remove duplicates (multiple plots in a single lidar tile)
pest_summary <- pest_summary[!duplicated(pest_summary$PlotID),]

#Export pest summary
write.csv(st_drop_geometry(pest_summary),
          file = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//pest_disturbance_summary.csv")

#Include plots with pest infestation history, 
  #...but label using a different prefix for PlotID

#Add flag for infestation
plots_poly$infestation <- NA
plots_poly$infestation[plots_poly$PlotID %in% pest_summary$PlotID] <- "Y"
plots_poly$infestation[is.na(plots_poly$infestation)] <- "N"

#Relocate infestation var
plots_poly <- plots_poly %>% relocate(infestation, .after = tot_stand_age)

#Join with infestation data
plots_poly <- left_join(plots_poly, st_drop_geometry(pest_summary), by = "PlotID")

#Remove lidar_yr and ground measurement year fields that transfered from pest df join
plots_poly <- plots_poly %>% subset(select = -c(lidar_yr, ground_meas_yr))

#Subset target lidar var so that it has the right plots
target_lidar <- target_lidar[target_lidar$PlotID %in% plots_poly$PlotID,]

#Summarize target lidar 
target_lidar_summary <- target_lidar %>% 
  group_by(PlotID) %>%
  summarise(n_tiles = n(),
            lidr_yr = toString(unique(lidar_yr)),
            project = toString(unique(oper_name)),
            diff_yrs = toString(unique(diff_yrs)),
            crs = toString(unique(projection)),
              ) %>%
  st_drop_geometry()

#Join with plot data 
plots_poly <- left_join(plots_poly, target_lidar_summary, by = "PlotID")

#Rename some cols
plots_poly <- plots_poly %>% rename("lidr_crs" = "crs",
                                    "lidr_project" = "project",
                                    "std_age" = "tot_stand_age"
                                    )

#Export plot data ----

#Export target lidar
st_write(target_lidar, 
         dsn = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_Plots_subset_intersect_w_lidar_tiles.shp",
         append = F)

#Export target lidar summary
write.csv(target_lidar_summary, 
         "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_Plots_target_lidar_summary.csv")

#Retain original PlotID
plots_poly$OG_PlotID <- plots_poly$PlotID

#Change PlotID prefix of infested plots to "BCGOV_P"
plots_poly$PlotID[plots_poly$infestation == "Y"] <-
  gsub(pattern = "BCGOV", 
       replacement = "BCGOV_P", 
       x = plots_poly$PlotID[plots_poly$infestation == "Y"])

#Export plots poly
st_write(plots_poly, 
         dsn = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_Plots_polygons_w_disturbance_n_lidar_data.shp",
         append = F)

#Load and subset plot data to available lidar data ----

#Read plot shp
plots_poly <- st_read("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_Plots_polygons_w_disturbance_n_lidar_data.shp")

#Load filenames of plots that had lidar coverage
las_fns <- list.files("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_clipped_to_plots")

#Fix las filenames names
las_fns <- gsub(".las", "", las_fns)

#Check which plots are available for which las filenames
table(plots_poly$PlotID %in% las_fns)

#Reduce plots to only those that have available LAS files
plots_red <- plots_poly[plots_poly$PlotID %in% las_fns,]

#Export reduced plots df
st_write(plots_red, 
         dsn = "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_plots_paired_with_lidar_cleaned.shp",
         append = F)



