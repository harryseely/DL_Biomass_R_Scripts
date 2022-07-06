#Download BC lidar data

#Get packages
library(tidyverse)
library(sf)
library(lidR)

#Read data
tiles <- read.csv("D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/Outputs/BC_Gov_target_lidar_info.csv")

#Get URL's
dwl_url <- tiles$s3Url

#Get file names
all_files <- tiles$filename

#SWet out directiory
outdir <- "D:/Sync/Data/Provincial_Plot_and_Lidar_Data/BC/BC_Lidar_tiles_raw"

# Iterate through each url
for (n in dwl_url) {
  # Print progress in the console
  message(sprintf("Downloading file %s (%d/%d)", basename(n), match(n, dwl_url), length(dwl_url)))
  
  # Send http request to download file form the URL and write it on disk
  # The httr package is part of the tidyverse
  httr::GET(n, httr::write_disk(file.path(outdir, basename(n)), overwrite = TRUE))
}

ctg <- readLAScatalog(outdir)

plot(ctg)