#Visualize hyperparameter tuning results

library(tidyverse)
library(readxl)

#Read data
df <- read.csv("D:/Sync/DL_Development/Hyperparameter_Tuning/BC_n_Petawawa_data_added_trial_1_Hyperparameter_tuning_results_2022_07_18_11_07_49_.csv")


#Plot MSE over point density 
ggplot(df, aes(x = num_points, y = value)) + 
  geom_point() + 
  geom_smooth()

#Get mean MSE for each point density
pd_summary <- df %>% 
  group_by(num_points) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

plot(pd_summary$num_points, pd_summary$mean_value)
plot(pd_summary$num_points, pd_summary$median_value)

#Get mean MSE for number of augmentations
augs_summary <- df %>% 
  group_by(num_augs) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

plot(augs_summary$num_augs, augs_summary$mean_value)
plot(augs_summary$num_augs, augs_summary$median_value)

#Get mean MSE for batch size
batch_summary <- df %>% 
  group_by(batch_size) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

plot(batch_summary$batch_size, batch_summary$mean_value)
plot(batch_summary$batch_size, batch_summary$median_value)

#Learning rate
lr_summary <- df %>% group_by(lr) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

plot(lr_summary$lr, lr_summary$mean_value)
plot(lr_summary$lr, lr_summary$median_value)

#Ground filter height
ground_filter_height_summary <- df %>% 
  group_by(ground_filter_height) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

plot(ground_filter_height_summary$ground_filter_height, ground_filter_height_summary$mean_value)
plot(ground_filter_height_summary$ground_filter_height, ground_filter_height_summary$median_value)

#Patience
Patience_summary <- df %>% 
  group_by(patience) %>%
  summarise(mean_value = mean(value),
            median_value = median(value))

plot(Patience_summary$patience, Patience_summary$mean_value)
plot(Patience_summary$patience, Patience_summary$median_value)


#Activation function
ggplot(df, aes(x = activation_function, y = value)) +
  geom_bar(stat = "identity")

#Optimizer function
ggplot(df, aes(x = optimizer, y = value)) +
  geom_bar(stat = "identity")

#Groun

