#Purpose: visualize BC LAS

#Get packages ----

library(sf)
library(lidR)
library(tidyverse)
library(mapview)
library(units)
library(stringr)
library(rgl)


#BC LAS visualization -----
las <- readLAS("D:/Sync/Data/Model_Input/lidar_data/BCGOV27.las")

plot(las)


exportPath = "D:/Sync/Figures/BCGOV27_rotating_point_cloud.gif"

plot(las, bg = "white")
movie3d(spin3d(), duration = 20, movie = exportPath)

    