#The purpose of this script is to prepare the Romeo lidar and plots data for deep learning biomass modelling

#Get packages-----
library(sf)
library(lidR)
library(tidyverse)
library(stringr)
library(mapview)

#Get data -----

#Set working directory
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Romeo_Malette")

#Load data
trees <- read_csv("romeo_individual_tree_measurements.csv")
sp_codes <- read_csv("romeo_species_codes.csv")
plots <- st_read("Plots/MasterPlotLocationUTM_December6.shp")
plot_info <- read_csv("Plots/tblPlot.csv")

#Join the species info with the trees df
trees <- left_join(trees, sp_codes, by = "SpeciesCode")

#Join plot info with plots and trees
trees <- left_join(trees, plot_info, by = "PlotKey")

#check for NAs in plot Key or plot ID
table(is.na(trees$PlotKey))
table(is.na(trees$PlotName))

#Rename PlotName as PlotID
trees <- trees %>% rename(PlotID = PlotName)
  
#Load allometric equation parameters
eqs <- read.csv("D:/Sync/Data/Ung_Lambert_Individual_Tree_Biomass_Parameters/allometric_eqs_clean.csv")

#Remove reference col
eqs <- eqs %>% subset(select = -Reference)

#Load parameters for DBH only equations
eqs_DBH_only <- read.csv("D:/Sync/Data/Ung_Lambert_Individual_Tree_Biomass_Parameters/allometric_eqs_clean_DBH_only.csv")

#Change parameter names from a and b to d anb e for easy data merging
eqs_DBH_only <- eqs_DBH_only %>% rename(d = a, e = b)

#ASSIGN A UNIQUE TREE ID across all plots/trees
trees <- tibble::rowid_to_column(trees, "tree_ID")

#Clean tree data and species names  ----

#What percent of measured trees are dead?
round(table(trees$StatusCode)/nrow(trees)*100,1)

#Remove dead trees (only estimating live biomass for now)
trees <- trees %>% filter(StatusCode == "L"| StatusCode == "V")

#Remove unwanted cols from tree df
trees <- trees[,-c(2:9, 11:15, 17:27, 28, 30, 31, 33:51)]

#Rename vars
trees <- trees %>% rename(species = Common,
                          ht_meas = TotalHeight,
                          dbh = DBH)

#Check the number of plots
length(unique(trees$PlotID))

#Get species in plots
table(trees$species)

#Rename tree species that have multiple names:

    #Paper Birch as White Birch 
    trees$species[trees$species == "Paper Birch"] = "White Birch"
    
    #American Larch as Tamarack Larch
    trees$species[trees$species == "American Larch"] = "Tamarack Larch"
    
    #Bitternut Hickory as Hickory
    trees$species[trees$species == "Bitternut Hickory"] = "Hickory"
    
    #Pitch Pine as Coniferous (since there is only 1 across entire dataset, 
      #...not worth gathering exact allometric equation parameters for)
    trees$species[trees$species == "Pitch Pine"] = "Conifers"
    
    #Eastern Red Cedar as Coniferous (since there is only 1 across entire dataset, 
    #...not worth gathering exact allometric equation parameters for)
    trees$species[trees$species == "Eastern Red Cedar"] = "Conifers"
    
#Assign trees without ID species var to "All" to use generic biomass eq
trees$species[is.na(trees$species)] <- "All"
    
#Add "All" to list of sp names that are included in Canadian biomass eqs
sp_w_eqs <- append(sp_w_eqs, "All")
    
#Also add "Conifers" for generic conifer instances
sp_w_eqs <- append(sp_w_eqs, "Conifers")
    
#Check to see which species are in plots 
sp_names <- unique(trees$species); sp_names

#Capitalize allometric eq species names for both lists of parameters
eqs$Species_en <- str_to_title(eqs$Species_en)
eqs_DBH_only$Species_en <- str_to_title(eqs_DBH_only$Species_en)

#Get species that have allometric equations available
sp_w_eqs <- unique(eqs$Species_en)
sp_w_eqs

#Check which sp in Romeo dataset do not have available eqs
print("These species have eqs:"); sp_names[sp_names %in% sp_w_eqs]

#Check height and DBH ----

#Check for NA values in height and DBH cols
table(is.na(trees$dbh))
table(is.na(trees$ht_meas)) #Some trees have NA for height

#Remove trees that have no DBH measurement
trees <- trees %>% filter(!is.na(dbh))

#Calculate biomass by component for each tree ----

#NOTE: biomass is calculated in kilograms (kg)

#Need to create different dfs depending on available measurements
  #DBH
  #Height
  #Species

#Need different dfs depending on whether or not allometric eqs are available
  #Some sp do not have developed allometric eqs and must use generic eqs
      #Generic conifer or deciduous eqs

#Create function to get biomass using height and DBH
get_biomass_DBH_n_H <- function(a, b, c, dbh, height){ #DBH in cm, height in meters
  Biomass_kg = a * dbh^b * height^c
  return(Biomass_kg)
}

#Create function to get biomass using DBH only
get_biomass_DBH_only <- function(d, e, dbh){ #DBH in cm
  Biomass_kg = d * (dbh^e)
  return(Biomass_kg)
}

get_component_biomass <- function(species, dbh, height, component){
  
  #Check to see if species is included in Canadian Biomass Eqs
  if(species %in% sp_w_eqs){
  
     #Subset to individuals that have height + DBH measurements
     if(!is.na(height)){
       
        #Assign eq parameters for wood and for given species
        a = eqs$a[eqs$Species_en == species & eqs$Component_en == component]
        b = eqs$b[eqs$Species_en == species & eqs$Component_en == component]
        c = eqs$c[eqs$Species_en == species & eqs$Component_en == component]
  
        #Calculate component biomass given parameters and height + DBH
        return(get_biomass_DBH_n_H(a, b, c, dbh, height))}
        
        #For individuals that do not have a height measurement   
        else{
        
        #Assign eq parameters for wood and for given species
        d = eqs_DBH_only$d[eqs_DBH_only$Species_en == species & eqs_DBH_only$Component_en == component]
        e = eqs_DBH_only$e[eqs_DBH_only$Species_en == species & eqs_DBH_only$Component_en == component]
        
        #Calculate component biomass using only DBH
        return(get_biomass_DBH_only(d, e, dbh))
        
      }
      
    } else{return(NA)}
}

#Create component biomass variables for trees df
trees$bark_biomass <- NA
trees$wood_biomass <- NA
trees$foliage_biomass <- NA
trees$branches_biomass <- NA

#Loop through each tree to calculate component biomass

#Bark
for(i in 1:nrow(trees)){
  trees$bark_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$ht_meas[i],
                                                 component = "Bark")}

#Wood
for(i in 1:nrow(trees)){
  trees$wood_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$ht_meas[i],
                                                 component = "Wood")}

#Foliage
for(i in 1:nrow(trees)){
  trees$foliage_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$ht_meas[i],
                                                 component = "Foliage")}

#Branches
for(i in 1:nrow(trees)){
  trees$branches_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$ht_meas[i],
                                                 component = "Branches")}

#Get total aboveground  biomass (AGB)
trees$tree_AGB <- trees$wood_biomass +
  trees$bark_biomass +
  trees$branches_biomass + 
  trees$foliage_biomass


#Verify correct biomass calculation for bark ----

#First, check if calculation was correct for species with height + DBH 

    #Extract random tree where height is known, and it is in Canadian eqs
    sample_tree_ID <- sample(trees$tree_ID[!is.na(trees$ht_meas)] , size = 1)
    trees$species[trees$tree_ID == sample_tree_ID]; trees$ht_meas[trees$tree_ID == sample_tree_ID]
    #Extract estimated bark biomass
    estimated_bark_biomass <- trees$bark_biomass[trees$tree_ID == sample_tree_ID]
    #Perform manual biomass estimation using correct eq parameters
    a = eqs$a[eqs$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
    b = eqs$b[eqs$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
    c = eqs$c[eqs$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
    dbh = trees$dbh[trees$tree_ID == sample_tree_ID]
    height = trees$ht_meas[trees$tree_ID == sample_tree_ID]
    #Check whether estimated DBH in function is correct
    get_biomass_DBH_n_H(a, b, c, dbh, height) == estimated_bark_biomass
    
#Now check for species without height measurement

    #Extract random tree where height is NOT known, and it is in Canadian eqs
    sample_tree_ID <- sample(trees$tree_ID[is.na(trees$ht_meas)] , size = 1)
    trees$species[trees$tree_ID == sample_tree_ID]; trees$ht_meas[trees$tree_ID == sample_tree_ID]
    #Extract estimated bark biomass
    estimated_bark_biomass <- trees$bark_biomass[trees$tree_ID == sample_tree_ID]
    #Perform manual biomass estimation using correct eq parameters
    d = eqs_DBH_only$d[eqs_DBH_only$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
    e = eqs_DBH_only$e[eqs_DBH_only$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
    dbh = trees$dbh[trees$tree_ID == sample_tree_ID]
    #Check whether estimated DBH in function is correct
    get_biomass_DBH_only(d, e, dbh) == estimated_bark_biomass
  
#Now check for NAs
    table(is.na(trees$bark_biomass))
    table(is.na(trees$wood_biomass))
    table(is.na(trees$foliage_biomass))
    table(is.na(trees$branches_biomass))
    
#Aggregate biomass calculations for each plot ----

#How many plots have tree data?
length(unique(trees$PlotID))
    
#Check for NA values in vars
table(is.na(trees$PlotID))

#Get plot IDs
unique(trees$PlotID)

#Summarize biomass by component for each plot
biomass_by_plot <- trees %>% group_by(PlotID) %>% summarise(
  bark_total = sum(bark_biomass),
  branch_total = sum(branches_biomass),
  foliage_total = sum(foliage_biomass),
  wood_total = sum(wood_biomass),
  total_AGB = sum(tree_AGB))

#Join with the psp df
plots <- left_join(plots, biomass_by_plot, by = "PlotID" )

#Clean plot data ----

#Remove unwated vars
plots <- plots[,-c(1:19, 21:24)]

#Remove plots with NA vals for biomass
plots <- plots %>% filter(!is.na(total_AGB))

#check CRS of plot data
st_crs(plots)

#Order the plots by increasing plot ID
plots <- plots[order(plots$PlotID, decreasing = F),]

#Convert biomass to tonnes (1t == 1000 kg)
plots$total_AGB = plots$total_AGB/1000
plots$branch_total = plots$branch_total/1000
plots$foliage_total = plots$foliage_total/1000
plots$bark_total = plots$bark_total/1000
plots$wood_total = plots$wood_total/1000

#Fix plotID so that it represents Romeo Malette Research Forest (RMFR)
plots$PlotID <- paste("RMRF", plots$PlotID, sep = "")

#Add plot buffer ----

#Each plot has a radius of 11.28m (see RMF_TechnicalSpecifications pdf)

#The current shp for plots is a point feature

#Let's add a 12.28m radius to create a polygon, 
  #...since we are in a UTM CRS, the units are meters
plots_buf <- st_buffer(plots, dist = 12.28)




#Load lidar data ----

#Read lidar data as catalog
ctg <- readLAScatalog("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Romeo_Malette/Lidar")

#reproject the plots shp to the lidar data crs
plots_buf <- st_transform(plots_buf, crs = lidR::crs(ctg))

#Check
st_crs(plots_buf) == st_crs(ctg)

#Plot lidar coverage
las_coverage <- st_as_sf(ctg)
plot(ctg)
plot(st_centroid(st_geometry(plots_buf)), axes = T)

#Clip lidar data to plots and export ----

#Each las has a 5m buffer around plot, clip las to exact plot area and save to drive
for(i in seq(nrow(plots_buf))){
  las = clip_roi(ctg, plots_buf[i,])
  plotID <- plots_buf$PlotID[i]
  writeLAS(las,
           file = paste("D:/Sync/Data/Model_Input/lidar_data/", plotID, ".las", 
                        sep = ""))
  print(paste("Done LAS", i, "of", nrow(plots_buf), sep = " "))
}


#Visually inspect point clouds with high/low biomass ----

#Setwd
setwd("D:/Sync/Data/Model_Input/lidar_data")

#Extract top 5 plots with greatest biomass
highest_AGB <- plots[order(plots$total_AGB, decreasing = T)[1:5],]

#Extract 5 plots with lowest biomass
lowest_AGB <- plots[order(plots$total_AGB, decreasing = F)[1:5],]

#Get LAS file list
RMRF_las_flist = list.files(full.names = TRUE)

#Reduce file list to only those that begin with RMRF
subset_bool <- vector()
for(i in 1:length(RMRF_las_flist)){
  if(substr(RMRF_las_flist[i], 3, 6) == "RMRF"){
    subset_bool[i] = TRUE}
  else{
    subset_bool[i] = FALSE}}

#Subset flist
RMRF_las_flist <- RMRF_las_flist[subset_bool]

#Create empty las list for high and low biomass values
high_biomass_laslist <- list()
low_biomass_laslist <- list()

#Read lidar data into high biomass las list
for (i in 1:nrow(highest_AGB)){
  #Extract LAS ID (Plot ID)
  las_fn <- paste("./", highest_AGB$PlotID[i], ".las", sep = "")
  #Load the las if its in the list
  high_biomass_laslist[[i]] = readLAS(file = las_fn, select = "xyzicrn")
}

#Label las list
names(high_biomass_laslist) <- highest_AGB$PlotID

#Read lidar data into low biomass las list
for (i in 1:nrow(lowest_AGB)){
  #Extract LAS ID (Plot ID)
  las_fn <- paste("./", lowest_AGB$PlotID[i], ".las", sep = "")
  #Load the las if its in the list
  low_biomass_laslist[[i]] = readLAS(file = las_fn, select = "xyzicrn")
}

#Label las list
names(low_biomass_laslist) <- lowest_AGB$PlotID

#Check labels
names(high_biomass_laslist)
names(low_biomass_laslist)

#Visualize the high biomass plots
for(i in 1:length(high_biomass_laslist)){
  plot(high_biomass_laslist[[i]])
}

#Visualize the low biomass plots
for(i in 1:length(low_biomass_laslist)){
  plot(low_biomass_laslist[[i]])
}

#Export data plot and tree data ----

#Reset wd
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Romeo_Malette")

#Export trees
write.csv(trees, "Outputs//romeo_individual_trees_w_biomass.csv")

#Export plots
st_write(plots_buf, 
         dsn = "Outputs//romeo_plots_w_biomass.shp",
         append = F)

#Export plots in csv format
plots_nogeo <- st_drop_geometry(plots)
write.csv(plots_nogeo, "Outputs//romeo_plots_w_biomass.csv")

#Checking point density range and mean -----

#Reset wd
setwd("D:/Sync/Data/Model_Input/lidar_data")

#Get LAS file list
RMRF_las_flist

#Create empty vectors
point_density = vector("numeric", length = length(RMRF_las_flist))
point_count = vector("numeric", length = length(RMRF_las_flist))
PlotIDs = vector("numeric", length = length(RMRF_las_flist))

#Read lidar data into high biomass las list
for (i in 1:length(RMRF_las_flist)){
  #Load the las if its in the list
  las_i <- readLAS(file = RMRF_las_flist[i], select = "xyzicrn")
  #Get plot ID from las filename
  PlotID = gsub('.las', '', RMRF_las_flist[i])
  PlotIDs[i] = gsub('./', '', PlotID)
  #Get the point count
  point_count[i] <- length(las_i$NumberOfReturns)
  #Get the point density
  point_density[i] <- length(las_i$NumberOfReturns)/lidR::area(las_i)
  #Print progress
  print(paste(i, "out of", length(RMRF_las_flist)))
}

#Create a df that has pd with plot ID
point_info <- data.frame(PlotID = PlotIDs, 
                         point_density = point_density,
                         point_count)

#Boxplot of point density
boxplot(point_info$point_density)
boxplot(point_info$point_count)



