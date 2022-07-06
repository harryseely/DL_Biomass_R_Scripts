#Visualize hyperparameter tuning results

library(tidyverse)
library(readxl)

#Read data
df <- read_xlsx("D:/Sync/DL_Development/Hyperparameter_Tuning/Overnight_w_more_params__Hyperparameter_tuning_results_2022_06_24_07_04_08_.xlsx")


#Plot MSE over neuron multiplier (network architecture complexity)
plot(df$params_neuron_multiplier, df$value)

plot(df$number, df$value)

