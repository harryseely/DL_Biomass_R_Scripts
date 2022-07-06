packages <- c("lidR", "sf", "rgdal", "raster", "terra", "dplyr", "tidyverse", 
              "magick", "OpenImageR", "whitebox", "BiocManager")

# Install Packages if not Installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
  whitebox::install_whitebox()
  BiocManager::install("EBImage")
}

# Load Packages
invisible(suppressMessages(suppressWarnings(lapply(packages, library, character.only = TRUE))))

# Remove Variables
rm(installed_packages)
rm(packages)