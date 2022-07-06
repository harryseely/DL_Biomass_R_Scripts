#The purpose of this script is to test whether simple linear regression
  #...using lidar metrics can predict with some accuracy plot level
  #...biomass for the BC lidar/plot data

#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)

#Get data -----

#Plot data
plots <- read.csv("D:/Sync/Data/Model_Input/model_input_plot_biomass_data.csv")

#Lidar data
las_fns <- list.files("D:/Sync/Data/Model_Input/lidar_data")

#Subset las_fns to plot data
plot_id_match <- paste(plots$PlotID, ".las", sep = "")

#Match las_fns to plot IDs
table(las_fns %in% plot_id_match)
las_fns <- las_fns[las_fns %in% plot_id_match]
rm(plot_id_match)

#Read in the detailed plot data for each dataset
bc_gov <- st_read("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs/BC_PLOTS_FINAL.shp")
petawawa <- st_read("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Petawawa/PRF_CNL_SPL_2018_plotPolys_CSRS_NAD83Zn18.shp")
romeo <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/ON/Romeo_Malette/Plots/tblPlot.csv")

#Compute standard lidar metrics for each plot ----

#Set wd
setwd("D:/Sync/Data/Model_Input/lidar_data")

#Loop through each plot LAS and compute the set of standard lidar metrics for each plot

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

#Model total biomass using lidar metrics across all datasets ----

#Join total biomass with metrics df
metrics_df <- left_join(plots, metrics_df, by = "PlotID")

#Add source dataset variable
metrics_df$dataset <- substr(metrics_df$PlotID, 1, 2)
metrics_df <- metrics_df %>% relocate(dataset, .before = "PlotID")

#Add a different datasource for infestation BC plots

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
names(metrics_df[,c(1:9)])

#Find corrs
total_AGB_cors <- corrs(metrics_df[,-c(1:9)], metrics_df$total_AGB)
total_AGB_cors

#Plot correlation
ggplot(metrics_df, aes(x = zmean, y = total_AGB, color = dataset)) + 
  geom_point()

#Create a very simple model using mean height to predict total biomass
model1 <- lm(total_AGB~zmean, data = metrics_df)

summary(model1)

#Create df with BC data excluded
metrics_df_BC_rm <- metrics_df %>% filter(dataset != "BC")

#Find corrs for model w/out BC data
total_AGB_cors_BC_rm <- corrs(metrics_df_BC_rm[,-c(1:9)], metrics_df_BC_rm$total_AGB)
total_AGB_cors_BC_rm

#Plot correlation
ggplot(metrics_df_BC_rm, aes(x = zmean, y = total_AGB, color = dataset)) + 
  geom_point()

#Create a model that excludes BC data 
model2 <- lm(total_AGB~zmean, data = metrics_df_BC_rm)
summary(model2)


#Examine BC data -----

#Subset dataset to BC data only
metrics_bc <- metrics_df[metrics_df$dataset == "BC",]

#Join with BC detailed data
metrics_bc <- left_join(metrics_bc, bc_gov, by = "PlotID")

#Check for correlations between lidar metrics and total_AGB
#Excluded rows
names(metrics_bc[,c(1:9, 46:ncol(metrics_bc))])

#Find corrs
total_AGB_cors <- corrs(metrics_bc[,-c(1:9, 46:ncol(metrics_bc))], 
                        metrics_bc$total_AGB)
total_AGB_cors

#Plot correlation with colour based on difference 
  #...between lidar aquisition and ground sampling
ggplot(metrics_bc, aes(x = zmean, y = total_AGB, color = dff_yrs)) + 
  geom_point()

#Plot correlation with colour based on infestation status 
ggplot(metrics_bc, aes(x = zmean, y = total_AGB, color = infsttn)) + 
  geom_point()

#Plot correlation with colour based on lidar project 
ggplot(metrics_bc, aes(x = zmean, y = total_AGB, color = ldr_prj)) + 
  geom_point()

#Mean height vs total AGB (basic plot)
ggplot(metrics_bc, aes(x = zmean, y = total_AGB)) + 
  geom_point()

#Make a linear regression model for BC data only
model_bc <- lm(total_AGB~zmean, data = metrics_bc)
summary(model_bc)

#Try plotting BC data with 

#Examine Petawawa data -----

#Subset dataset to petawawa data only
metrics_petawawa <- metrics_df[metrics_df$dataset == "PF",]

#Check for correlations between lidar metrics and total_AGB
#Excluded rows
names(metrics_petawawa[,c(1:9)])

#Find corrs
total_AGB_cors <- corrs(metrics_petawawa[,-c(1:9)], 
                        metrics_petawawa$total_AGB)
total_AGB_cors

#Plot correlation with colour based on difference 
#...between lidar aquisition and ground sampling
ggplot(metrics_petawawa, aes(x = zmean, y = total_AGB)) + 
  geom_point()

#Make a linear regression model for petawawa data only
model_petawawa <- lm(total_AGB~zmean, data = metrics_petawawa)
summary(model_petawawa)

#Examine romeo data -----

#Subset dataset to romeo data only
metrics_romeo <- metrics_df[metrics_df$dataset == "RM",]

#Check for correlations between lidar metrics and total_AGB
#Excluded rows
names(metrics_romeo[,c(1:9)])

#Find corrs
total_AGB_cors <- corrs(metrics_romeo[,-c(1:9)], 
                        metrics_romeo$total_AGB)
total_AGB_cors

#Plot correlation with colour based on difference 
#...between lidar aquisition and ground sampling
ggplot(metrics_romeo, aes(x = zmean, y = total_AGB)) + 
  geom_point()

#Make a linear regression model for romeo data only
model_romeo <- lm(total_AGB~zmean, data = metrics_romeo)
summary(model_romeo)

#Compare models for isolated datasets ----
summary(model_romeo)
summary(model_bc)
summary(model_petawawa)


