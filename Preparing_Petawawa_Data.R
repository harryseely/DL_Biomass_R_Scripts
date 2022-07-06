#The purpose of this script is to prepare the Petawawa plots and lidar data for deep learning biomass modelling

#Get packages -----

#Get packages
library(sf)
library(lidR)
library(tidyverse)
library(stringr)

#Get data -----

#Set working directory
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Petawawa")

#individual tree data
trees <- read_csv("petawawa_2018_tree_data.csv")
sp_codes <- read_csv("species_dict.csv")

#Join the species name with the trees df
trees <- left_join(trees, sp_codes, by = c("tree_spec"="SpeciesCode"))

#Load allometric equation parameters
eqs <- read.csv("D:/Sync/Data/Ung_Lambert_Individual_Tree_Biomass_Parameters/allometric_eqs_clean.csv")

#Remove reference col
eqs <- eqs %>% subset(select = -Reference)

#Load parameters for DBH only equations
eqs_DBH_only <- read.csv("D:/Sync/Data/Ung_Lambert_Individual_Tree_Biomass_Parameters/allometric_eqs_clean_DBH_only.csv")

#Change parameter names from a and b to d anb e for easy data merging
eqs_DBH_only <- eqs_DBH_only %>% rename(d = a, e = b)

#Sort eq dfs by species name alphabetically
eqs <- eqs[order(eqs$Species_en),]
eqs_DBH_only <- eqs_DBH_only[order(eqs_DBH_only$Species_en),]

#Clean tree data ----

#check for NAs in Plot Name
table(is.na(trees$PlotName))

#Rename PlotName as PlotID
trees <- trees %>% rename(PlotID = PlotName)

#ASSIGN A UNIQUE TREE ID across all plots/trees and remove the existing one
trees <- trees %>% subset(select = -TreeID)
trees <- tibble::rowid_to_column(trees, "tree_ID")

#What percent of measured trees are dead?
round(table(trees$StatusCode)/nrow(trees)*100,1)

#Remove dead trees (only estimating live biomass for now)
trees <- trees %>% filter(Status == "L")

#Remove unwanted cols from tree df
trees <- trees[,-c(2,4:6, 8:10, 12:27, 29)]

#Rename vars
trees <- trees %>% rename(species = Name,
                          ht_meas = Ht,
                          dbh = DBH)

#Sort trees by species alphabteically
trees <- trees[order(trees$species),]

#Capitalize all the species names in trees df and equations dfs
eqs$Species_en <- str_to_title(eqs$Species_en)
eqs_DBH_only$Species_en <- str_to_title(eqs_DBH_only$Species_en)
trees$species <- str_to_title(trees$species)

#Check the number of plots
length(unique(trees$PlotID))

#Check for height and DBH measurements for each tree
table(is.na(trees$ht_meas))
table(is.na(trees$dbh))

#Get species in plots ===
table(trees$species)
unique(trees$species)
#...and sp with eqs
unique(eqs$Species_en)

#Get species in plots without eqs
unique(trees$species)[!unique(trees$species) %in% unique(eqs$Species_en)]

#Rename tree species that have multiple names (and adjust mismatched names)

  #American elm as White elm
  trees$species[trees$species == "American Elm"] = "White Elm"
  
  #Ironwood (Ostrya virginiana) as Hop-Hornbeam
  trees$species[trees$species == "Ironwood"] = "Hop-Hornbeam"
  
  #Northern white cedar as Eastern white cedar
  trees$species[trees$species == "Northern White Cedar"] = "Eastern White Cedar"
  
  #Red (soft) maple as Red Maple
  trees$species[trees$species == "Red (Soft) Maple"] = "Red Maple"
  
  #Tamarack as Tamarack larch
  trees$species[trees$species == "Tamarack"] = "Tamarack Larch"
  
  #White pine as Eastern white pine
  trees$species[trees$species == "White Pine"] = "Eastern White Pine"
  
  #Assign trees without ID species var to "All" to use generic biomass eq
  trees$species[is.na(trees$species)] <- "All"

#Check that all species names have been aligned
unique(trees$species)[!unique(trees$species) %in% unique(eqs$Species_en)]

#Add species with eqs to a list
sp_w_eqs = unique(trees$species)[unique(trees$species) %in% unique(eqs$Species_en)]

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

#Get total aboveground  biomass (Tree AGB)
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

#Convert biomass to tonnes (1t == 1000 kg)
biomass_by_plot$total_AGB = biomass_by_plot$total_AGB/1000
biomass_by_plot$branch_total = biomass_by_plot$branch_total/1000
biomass_by_plot$foliage_total = biomass_by_plot$foliage_total/1000
biomass_by_plot$bark_total = biomass_by_plot$bark_total/1000
biomass_by_plot$wood_total = biomass_by_plot$wood_total/1000

#Load lidar data and convert from LAZ to LAS, then move to main lidar folder ----

#Set output folder
output_path = "D:/Sync/Data/Model_Input/lidar_data//"

#Set new wd
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Petawawa/lidar_clipped_to_plots_raw")

#Get list of laz filenames
las_fns <- list.files()

#Subset list of laz fn's to those that have matching plot data
plotid_match <- paste(biomass_by_plot$PlotID, ".laz", sep = "")

#Check match with laz files
table(las_fns %in% plotid_match) 
#Some laz files do not have matching plot data

#Subset laz file list to those that have plot data
las_fns <- las_fns[las_fns %in% plotid_match]

#Read each LAZ in which corresponds to a plot, convert it to a LAS, and then move it to lidar folder
for (i in 1:length(las_fns)){
  #Load the LAZ if its in the list
  laz_i = readLAS(file = las_fns[i], select = "xyzicrn")
  #Remove "R" from name (if there is one at end)
  laz_i_fn <- gsub("R", "", las_fns[i])
  #Swap laz for las
  laz_i_fn <- gsub("laz", "las", laz_i_fn)
  #Save the LAZ as a LAS
  writeLAS(laz_i, paste(output_path, laz_i_fn , sep = ""))
  print(paste("Done las:",laz_i_fn, "number", i, "out of", nrow(biomass_by_plot)))
}

#EXPORT PLOT DATA -----

#Remove "R" at end of some plot names
biomass_by_plot$PlotID <- gsub("R", "", biomass_by_plot$PlotID)

#Check for correct number of plots
unique(biomass_by_plot$PlotID)
length(unique(biomass_by_plot$PlotID))

#Set working directory
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Petawawa/Outputs")
write.csv(biomass_by_plot, "petawawa_plots_w_biomass.csv")



