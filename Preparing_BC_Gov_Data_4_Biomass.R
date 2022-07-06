#The purpose of this collection of scripts is prepare identify which lidar to download for the available ground plots
#..., to prepare and clean the lidar data, and to aggregate tree data to get biomass estimates for each plot


#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)


#     CLEAN INDIVIDUAL TREE DATA ---------------------------------------------------------------------------------------
#Load tree data and pair with plot data -----

#Load plot data
plots <- st_read("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs/BC_PLOTS_FINAL.shp")
#Remove geometry
plots <- plots %>% st_drop_geometry()

#Load tree data
trees <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Sample_Plot_Full_Dataset_Raw/faib_ground_samples_tree_detail.csv")

#Filter tree data to relevant vars
trees <- trees[,-c(3, 5:26, 28:32, 34:39, 41:76, 80:104, 106:113)]

#Remove trees that were sampled prior to 2016
trees <- trees %>% filter(meas_yr >= 2016)

#Check num plot locations (unique SAMP_ID) and resampled plots (unique clstr_id)
length(unique(trees$clstr_id))
length(unique(trees$SAMP_ID))

#Join trees to plots based on SAMP_ID
trees <- left_join(trees, plots, by = "SAMP_ID")

#Remove trees without a PlotID
trees <- trees %>% filter(!is.na(PlotID))

#Check that no trees in plots are from different sampling years
trees_check <- trees %>% 
  group_by(SAMP_ID) %>% 
  summarize(year = toString(unique(meas_yr.x)))
unique(trees_check$year)
unique(plots$meas_yr)

#Double check alignment between tree and plot measurement year
table(trees$meas_yr.x == trees$meas_yr.y)

#Remove duplicated measurement year var
trees <- trees %>% 
  subset(select = -meas_yr.y) %>% 
  rename("meas_yr" = "meas_yr.x")

#ASSIGN A UNIQUE TREE ID across all plots/trees
trees <- tibble::rowid_to_column(trees, "tree_ID")

#Load allometric eqs and tree names legend, clean tree data ----

#Load tree codes legend
codes <- read.csv("D:/Sync/Data/BC_Tree_Species_Codes/bc_tree_codes.csv")

#Load allometric equation parameters
eqs <- read.csv("D:/Sync/Data/Ung_Lambert_Individual_Tree_Biomass_Parameters/allometric_eqs_clean.csv")

#Remove reference col
eqs <- eqs %>% subset(select = -Reference)

#Load parameters for DBH only equations
eqs_DBH_only <- read.csv("D:/Sync/Data/Ung_Lambert_Individual_Tree_Biomass_Parameters/allometric_eqs_clean_DBH_only.csv")

#Change parameter names from a and b to d anb e for easy data merging
eqs_DBH_only <- eqs_DBH_only %>% rename(d = a, e = b)

#Check DBH, Height, and remove dead trees ----

#Check NA for measured ground height
table(is.na(trees$height))
#Note: some trees do not have height values, for these, DBH eqs will be used instead

#Check NA for DBH
table(is.na(trees$dbh))
#Remove one tree that does not have DBH
trees <- trees %>% filter(!is.na(dbh))

#Check dead trees
table(trees$dead_standing_or_down)

#Remove dead trees
trees <- trees %>% filter(dead_standing_or_down == "")

#Get additional biomass eqs from various sources -----

add_eqs = read.csv("D:/Sync/Data/Additional_Biomass_Equations/additional_biomass_eqs.csv")

#Note: Added eqs have different formats
unique(add_eqs$eq_format)

#Note: DBH must be input to the eq in centimeters (cm)

#Adapt sp names from BC codes and adjust full names to match eqs ----

#Check for NA values for species
table(is.na(trees$species))

#Check species found in plots
table(trees$species)

#Convert all codes to lower case letters
codes$TreeCode <- tolower(codes$TreeCode)
trees$species <- tolower(trees$species)

#Check each set of codes
sort(unique(trees$species))
sort(unique(codes$TreeCode))

#Check to see if species are represented in code legend
unique(trees$species) %in% unique(codes$TreeCode)
#Check which species do not have codes that align
unique(trees$species)[!unique(trees$species) %in% unique(codes$TreeCode)]

#Remove whitespace at the end of sp names
eqs$Species_en <- str_trim(eqs$Species_en)
eqs_DBH_only$Species_en <- str_trim(eqs_DBH_only$Species_en)

#Join tree codes with tree species
trees <- left_join(trees, codes, by = c("species" = "TreeCode"))

#Capitalize all the species names in trees df and equations dfs
eqs$Species_en <- str_to_title(eqs$Species_en)
eqs_DBH_only$Species_en <- str_to_title(eqs_DBH_only$Species_en)
trees$EnglishName <- str_to_title(trees$EnglishName)

#Fix names based on BC tree species codes 
# See:(https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm)
trees$EnglishName[trees$species == "fd"] = "Douglas Fir"
trees$EnglishName[trees$species == "jr"] = "Rocky Mountain Juniper"
trees$EnglishName[trees$species == "ep"] = "Paper Birch"
trees$EnglishName[trees$species == "pl"] = "Lodgepole Pine"
trees$EnglishName[trees$species == "w"] = "Willow"
trees$EnglishName[trees$species == "ac"] = "Poplar"
trees$EnglishName[trees$species == "xc"] = "Conifers" #xc code denotes unknown conifer
trees$EnglishName[trees$species == "s"] = "Spruce"
trees$EnglishName[trees$species == "sx"] = "Spruce" #sx denotes spruce hybrid

#Fix additional names such that tree list names align with eq names
trees$EnglishName[trees$EnglishName == "Amabilis Fir"] = "Pacific Silver Fir"
trees$EnglishName[trees$EnglishName == "Red Alder"] = "Red Alder And Black Cottonwood"
trees$EnglishName[trees$EnglishName == "Western Redcedar"] = "Western Red Cedar"
trees$EnglishName[trees$EnglishName == "Paper Birch"] = "White Birch"

#Now check to see if long names align with those in biomass eqs
sort(unique(trees$EnglishName))
sort(unique(eqs$Species_en))
sort(unique(trees$EnglishName)[!unique(trees$EnglishName) %in% c(unique(eqs$Species_en), unique(add_eqs$Species))  ])

#Check species without exact biomass eqs-----

#Which are missing
missing_sp <- sort(unique(trees$EnglishName)[!unique(trees$EnglishName) %in% c(unique(eqs$Species_en), unique(add_eqs$Species))  ])

#Check out missing sp stats
missing_sp <- trees %>% filter(EnglishName %in% missing_sp) %>% group_by(EnglishName) %>% 
  summarise(n = n(),
            mean_dbh = mean(dbh),
            mean_ht = mean(height))

#Add proportion
missing_sp$prop <- round(missing_sp$n/nrow(missing_sp), 0)

#Sort missing sp by count
missing_sp <- missing_sp[order(missing_sp$n, decreasing = T),]
missing_sp

#Trees without biomass eqs make up less then 5% of all trees in plots, 
#...and are generally quite small (DBH < 20)
sum(missing_sp$n)/nrow(trees)*100 

#Make assumptions about sp without exact biomass eqs -----

#Species that require assumptions about biomass eq
sort(unique(trees$EnglishName)[!unique(trees$EnglishName) %in% c(unique(eqs$Species_en), unique(add_eqs$Species))  ])

#Species that are assumed to be similar in terms of biomass eqs:
trees$EnglishName[trees$EnglishName == "Spruce"] = "White Spruce" #Assume that generic spruce ID is white spruce because this is most common spruce in dataset
trees$EnglishName[trees$EnglishName == "Pacific Yew"] = "Deciduous" #Treat pacific yew as a generic deciduous tree
trees$EnglishName[trees$EnglishName == "Western Larch"] = "Tamarack Larch" #Assume Tamrack larch is Western larch
trees$EnglishName[trees$EnglishName == "Columbia Ponderosa Pine"] = "Ponderosa Pine" #Use generic ponderosa pine eq
trees$EnglishName[trees$EnglishName == "Rocky Mountain Ponderosa Pine"] = "Ponderosa Pine" #Use generic ponderosa pine eq
trees$EnglishName[trees$EnglishName == "Western White Pine"] = "Eastern White Pine" #Assume Western White Pine is eastern white pine
trees$EnglishName[trees$EnglishName == "Grand Fir"] = "Douglas Fir" #Assume Grand-Fir is douglas fir
trees$EnglishName[trees$EnglishName == "Mountain Hemlock"] = "Western Hemlock" #Assume moutain hemlock and western hemlock are same species
trees$EnglishName[trees$EnglishName == "Bitter Cherry"] = "Black Cherry" #Assume Bitter Cherry and Black Cherry are same species
trees$EnglishName[trees$EnglishName == "Cascara Buckthorn"] = "Deciduous" #Use generic deciduous eq for Cascara Buckthorn 
trees$EnglishName[trees$EnglishName == "Eastern Cottonwood"] = "Red Alder And Black Cottonwood" #Assume that Eastern Cottonwood is the same as black Cottonwood
trees$EnglishName[trees$EnglishName == "Plains Eastern Cottonwood"] = "Red Alder And Black Cottonwood" #Assume that Eastern Cottonwood is the same as black Cottonwood
trees$EnglishName[trees$EnglishName == "Rocky Mountain Juniper"] = "Deciduous" #Use generic deciduous eq for Rocky Mountain Juniper
trees$EnglishName[trees$EnglishName == "Shore Pine"] = "Conifers" #Use generic conifer eq for Shore Pine
trees$EnglishName[trees$EnglishName == "Poplar"] = "Balsam Poplar" #Use generic conifer eq for Shore Pine
trees$EnglishName[trees$EnglishName == "Willow"] = "Deciduous" #Use generic deciduous eq for willow


#Quality check for species labelling ----

sp_summary <- trees %>% group_by(EnglishName) %>% summarise(n = n(),
                                                            ScientificName = toString(unique(ScientificName)),
                                                            sp_codes = toString(unique(species)))

sp_summary <- sp_summary[order(sp_summary$n, decreasing = T),]
sp_summary

#Final cleaning of trees df----

trees <- trees %>% rename("sp_code" = species,
                          "species" = "EnglishName")

trees <- trees %>% subset(select = -c(TaxonLevel, 
                                      ScientificName, 
                                      dead_standing_or_down,
                                      clstr_id, 
                                      EnglishName_Old))

#Check that all trees have species and DBH assigned
table(is.na(trees$species))
table(is.na(trees$dbh))

#Remove duplicates
trees <- trees[!duplicated(trees$tree_ID),]

#Create biomass by component equations ----

#NOTE: biomass is calculated in kilograms (kg)

#Need to create different dfs depending on available measurements
#DBH
#Height
#Species

#For Bigleaf Maple, Lodgepole Pine, and Yellow Cedar, use additional eqs
add_eq_sp <- unique(add_eqs$Species); add_eq_sp

#Create function to get biomass using height and DBH (Ung)
get_biomass_DBH_n_H <- function(a, b, c, dbh, height){ #DBH in cm, height in meters
  Biomass_kg = a * dbh^b * height^c
  return(Biomass_kg)
}

#Create function to get biomass using DBH only (Ung)
get_biomass_DBH_only <- function(d, e, dbh){ #DBH in cm
  Biomass_kg = d * (dbh^e)
  return(Biomass_kg)
}

#Create function to get biomass using additional biomass eqs
get_biomass_add <- function(a, b, dbh){
  Biomass_kg = a*dbh^b
  return(Biomass_kg)
}

#Equation to compute biomass for each component
get_component_biomass <- function(species, dbh, height, component){
  
  #Check to see if species is included in Canadian Biomass Eqs
  if(!species %in% add_eq_sp){
    
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
    
  } else{
    
    print("Using additional biomass equations...")
    
    a = add_eqs$a[add_eqs$Species == species & add_eqs$Component == component]
    b = add_eqs$b[add_eqs$Species == species & add_eqs$Component == component]
    return(get_biomass_add(a, b, dbh))
    
  }
}

#Implement biomass equations -----

#Create component biomass variables for trees df
trees$bark_biomass <- NA
trees$wood_biomass <- NA
trees$foliage_biomass <- NA
trees$branches_biomass <- NA

#Loop through each tree to calculate component biomass


for(i in 1:nrow(trees)){
  
  #Bark
  trees$bark_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$height[i],
                                                 component = "Bark")
  
  #Wood
  trees$wood_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$height[i],
                                                 component = "Wood")
  
  #Foliage
  trees$foliage_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                    dbh = trees$dbh[i],
                                                    height = trees$height[i],
                                                    component = "Foliage")
  
  #Branches
  trees$branches_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                     dbh = trees$dbh[i],
                                                     height = trees$height[i],
                                                     component = "Branches")
  
  print(paste("Complete tree", i, "out of", nrow(trees)))
  
}

#Get total aboveground  biomass (Tree AGB)
trees$tree_AGB <- trees$wood_biomass +
  trees$bark_biomass +
  trees$branches_biomass + 
  trees$foliage_biomass  

#Check for NA values
table(is.na(trees$bark_biomass))
table(is.na(trees$wood_biomass))
table(is.na(trees$foliage_biomass))
table(is.na(trees$branches_biomass))
table(is.na(trees$tree_AGB))

#Verify correct biomass calculation ----

#First, check if calculation was correct for species with height + DBH using Ung

#Extract random tree where height is known, and it is in Ung eqs
sample_tree_ID <- sample(trees$tree_ID[!is.na(trees$height)] , size = 1)
trees$species[trees$tree_ID == sample_tree_ID]; trees$height[trees$tree_ID == sample_tree_ID]
#Extract estimated bark biomass
estimated_bark_biomass <- trees$bark_biomass[trees$tree_ID == sample_tree_ID]
#Perform manual biomass estimation using correct eq parameters
a = eqs$a[eqs$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
b = eqs$b[eqs$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
c = eqs$c[eqs$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs$Component_en == "Bark"] 
dbh = trees$dbh[trees$tree_ID == sample_tree_ID]
height = trees$height[trees$tree_ID == sample_tree_ID]
#Check whether estimated DBH in function is correct
get_biomass_DBH_n_H(a, b, c, dbh, height) == estimated_bark_biomass

#Next, check if calculation was correct for species with only DBH using Ung

#Extract random tree where height is NOT known, and it is in Ung eqs
sample_tree_ID <- sample(trees$tree_ID[is.na(trees$height)] , size = 1)
trees$species[trees$tree_ID == sample_tree_ID]; trees$height[trees$tree_ID == sample_tree_ID]
#Extract estimated bark biomass
estimated_bark_biomass <- trees$bark_biomass[trees$tree_ID == sample_tree_ID]
#Perform manual biomass estimation using correct eq parameters
d = eqs_DBH_only$d[eqs_DBH_only$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs_DBH_only$Component_en == "Bark"] 
e = eqs_DBH_only$e[eqs_DBH_only$Species_en == trees$species[trees$tree_ID == sample_tree_ID] & eqs_DBH_only$Component_en == "Bark"] 
dbh = trees$dbh[trees$tree_ID == sample_tree_ID]
#Check whether estimated DBH in function is correct
get_biomass_DBH_only(d, e, dbh) == estimated_bark_biomass

#Finally, check to see if additional equations worked for the three species

#Extract random tree belonging to one of three sp using additional eqs
sample_tree_ID <- sample(trees$tree_ID[trees$species %in% add_eq_sp] , size = 1)
trees$species[trees$tree_ID == sample_tree_ID]; trees$height[trees$tree_ID == sample_tree_ID]
#Extract estimated bark biomass for that tree
estimated_bark_biomass <- trees$bark_biomass[trees$tree_ID == sample_tree_ID]
#Get species for sammple tree
sample_sp <- trees$species[trees$tree_ID == sample_tree_ID]
#Perform manual biomass estimation using correct eq parameters
a = add_eqs$a[add_eqs$Species == sample_sp & add_eqs$Component == "Bark"] 
b = add_eqs$b[add_eqs$Species == sample_sp & add_eqs$Component == "Bark"] 
dbh = trees$dbh[trees$tree_ID == sample_tree_ID]
#Check whether estimated DBH in function is correct
get_biomass_add(a, b, dbh) == estimated_bark_biomass

#Visualize relationship between biomass/height/DBH and remove outliers ----

#Height vs. DBH
ggplot(trees, aes(x = dbh, y = height, color = species)) + 
  geom_point()

#Remove trees that are very short but have fat DBH (stumps/snags)
stumps <- trees %>% filter(dbh > 75 & height < 15); stumps
trees <- trees %>% filter(!tree_ID %in% stumps$tree_ID)

#Total biomass vs DBH
ggplot(trees, aes(x = dbh, y = tree_AGB, color = species)) + 
  geom_point()

#Total biomass vs height
ggplot(trees, aes(x = height, y = tree_AGB, color = species)) + 
  geom_point()

#Remove trees that are short but have high biomass (more stumps)
stumps <- trees %>% filter(tree_AGB > 500 & height < 10) %>% 
  select(tree_ID, tree_AGB, dbh, height, species, PlotID) %>%
  arrange(height); stumps
trees <- trees %>% filter(!tree_ID %in% stumps$tree_ID)

#Check height vs each biomass component
ggplot(trees, aes(x = height, y = wood_biomass, color = species)) + geom_point()
ggplot(trees, aes(x = height, y = branches_biomass, color = species)) + geom_point()
ggplot(trees, aes(x = height, y = foliage_biomass, color = species)) + geom_point()
ggplot(trees, aes(x = height, y = bark_biomass, color = species)) + geom_point()



#Aggregate biomass per plot ----

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

#Get std metrics for each plot ----

#Set wd
setwd("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_clipped_to_plots")

#Get las filenames
las_fns <- list.files()

#Subset filenames to target plots
plotid_target <- paste(biomass_by_plot$PlotID, ".las", sep = "")
table(las_fns %in% plotid_target)
las_fns <- las_fns[las_fns %in% plotid_target]

#Loop through each plot LAS and compute the set of standard lidar metrics 

#Create empty vectors
std_metrics <- list(length = length(las_fns))
PlotIDs = vector("numeric", length = length(las_fns))

#Read lidar data into high biomass las list
for (i in 1:length(las_fns)){
  #Load the las if its in the list
  las_i <- readLAS(file = las_fns[i], select = "xyzicrn")
  #Get plot ID from las filename
  PlotID_i = gsub('.las', '', las_fns[i])
  PlotIDs[i] = gsub('./', '', PlotID_i)
  #Compute the std metrics for plot i
  std_metrics_i <- cloud_metrics(las_i, func = .stdmetrics_z)
  #Add std metrics to the list
  std_metrics[[i]] <- std_metrics_i
  #Print progress
  print(paste(i, "out of", length(las_fns)))
}

#Create a dataframe with each metric for each plot ID
metrics_df <- data.frame(as.list(unlist(std_metrics[1])))
colnames(metrics_df) <- gsub("length.", "", colnames(metrics_df))

for(i in 2:length(std_metrics)){
  row_i <- data.frame(as.list(unlist(std_metrics[i])))
  metrics_df <- rbind(metrics_df, row_i)
}

#Add plot IDs to df
metrics_df <- cbind(PlotID = PlotIDs, metrics_df)



#Model total biomass using lidar metrics ----

#Join total biomass with metrics df
metrics_df <- left_join(biomass_by_plot, metrics_df, by = "PlotID")

#Define correlation function
corrs <- function(df, variable){
  
  #Correlate snag density with all predictor variables
  corrs <- cor(df, variable) 
  
  #Fix dataframe
  corrs <- data.frame(corrs)
  corrs <- cbind(metric = rownames(corrs), corrs)
  rownames(corrs) <- 1:nrow(corrs)
  
  #Get the absolute value of r and then re-order the dataframe by it
  corrs$abs_cor <- abs(corrs$corrs)
  corrs <- corrs[order(corrs$abs_cor, decreasing = T),]
  corrs <- subset(corrs, select = -abs_cor)
  
  return(corrs)
}

#Excluded rows
names(metrics_df[,c(1:6)])

#Find corrs
total_AGB_cors <- corrs(metrics_df[,-c(1:9)], metrics_df$total_AGB)
total_AGB_cors

#Plot correlation
ggplot(metrics_df, aes(x = zmean, y = total_AGB)) + 
  geom_point()

#Create a very simple model using mean height to predict total biomass
model1 <- lm(total_AGB~zmean, data = metrics_df)

summary(model1)
#Check plots that have outlier biomass values ----

#Let's define outlier for now as any plot with total AGB > 90 tons
outliers_df <- metrics_df %>% filter(total_AGB > 90)

#Extract the trees which belong to the outliers plots
trees_outliers <- trees %>% filter(PlotID %in% outliers_df$PlotID)

#Sort the tree outliers by tree AGB
trees_outliers <- trees_outliers[order(trees_outliers$tree_AGB, decreasing = TRUE),]
trees_outliers$tree_AGB

View(trees_outliers)
View(outliers_df)

#Visualize the outlier plots
outlier_fns <- paste(outliers_df$PlotID, ".las", sep = "")

for(i in 1:length(outlier_fns)){
  las_i <- readLAS(paste("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_clipped_to_plots/",
                         outlier_fns[i], sep = ""))
  plot(las_i)
}

#Flag outlier tree(s)
tree_outlier_ID <- vector()
tree_outlier_ID[1] <- trees_outliers$tree_ID[trees_outliers$tree_AGB > 10000 & trees_outliers$height < 10]

#Re-Implement Biomass calculations with outlier trees removed ----

#Remove outlier trees
trees <- trees %>% filter(!tree_ID %in% tree_outlier_ID)

#Create component biomass variables for trees df
trees$bark_biomass <- NA
trees$wood_biomass <- NA
trees$foliage_biomass <- NA
trees$branches_biomass <- NA

#Loop through each tree to calculate component biomass


for(i in 1:nrow(trees)){
  
  #Bark
  trees$bark_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$height[i],
                                                 component = "Bark")
  
  #Wood
  trees$wood_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                 dbh = trees$dbh[i],
                                                 height = trees$height[i],
                                                 component = "Wood")
  
  #Foliage
  trees$foliage_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                    dbh = trees$dbh[i],
                                                    height = trees$height[i],
                                                    component = "Foliage")
  
  #Branches
  trees$branches_biomass[i] <- get_component_biomass(species = trees$species[i],
                                                     dbh = trees$dbh[i],
                                                     height = trees$height[i],
                                                     component = "Branches")
  
  print(paste("Complete tree", i, "out of", nrow(trees)))
  
}

#Get total aboveground  biomass (Tree AGB)
trees$tree_AGB <- trees$wood_biomass +
  trees$bark_biomass +
  trees$branches_biomass + 
  trees$foliage_biomass  

#Check for NA values
table(is.na(trees$bark_biomass))
table(is.na(trees$wood_biomass))
table(is.na(trees$foliage_biomass))
table(is.na(trees$branches_biomass))
table(is.na(trees$tree_AGB))


#Re-Aggregate biomass per plot ----

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


#Export biomass by plot data and trees df ----

write.csv(biomass_by_plot, 
          "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_biomass_by_plot.csv")
write.csv(trees, 
          "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs//BC_trees_w_biomass_clean.csv")

