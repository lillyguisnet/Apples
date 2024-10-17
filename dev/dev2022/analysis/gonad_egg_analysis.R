setwd("~/Apples/dev/dev2022/analysis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

xl_file = "gonad_egg_count.csv"
df <- read.csv(xl_file)

df <- group_by(df, image_id) %>%
  mutate(condition = strsplit(image_id, "_")[[1]][2]) %>%
  mutate(plate = str_replace(strsplit(image_id, "_")[[1]][3],"p", "")) %>%
  mutate(replicate = strtoi(str_replace(strsplit(image_id, "_")[[1]][4], "b", ""))) %>%
  mutate(day = str_replace(str_replace(strsplit(image_id, "_")[[1]][5],"d", ""), ".png", "")) %>%
  mutate(day = as.numeric(day)) %>%
  mutate(common_id = str_c(condition, plate, replicate, day, sep = "_"))


ggplot(df, aes(as.factor(day), egg_count, colour = as.factor(condition))) +
  geom_boxplot()



df_join <- full_join(df_norm, df, by =join_by(common_id))

df_bodyeggs <- group_by(df_join, common_id) %>%
  filter(img_id != "NA")

ggplot(df_bodyeggs, aes(norm_pruned_medialaxis_length, egg_count, colour = as.factor(condition.x))) +
  geom_point(size = 5)

ggplot(df_bodyeggs, aes(norm_mean_wormwidth, egg_count, colour = as.factor(condition.x))) +
  geom_point(size = 5)

ggplot(df_bodyeggs, aes(norm_area, egg_count, colour = as.factor(condition.x))) +
  geom_point(size = 5) +
  facet_grid(~as.factor(day.x))
  #facet_wrap(~as.factor(day.x), ncol = 1)

ggplot(df_bodyeggs, aes(norm_area, egg_count, colour = as.factor(condition.x))) +
  geom_point(size = 5)



# Assuming your data is in a dataframe called df
# df should have at least three columns: egg_count, norm_area, and day

# A list to store the residuals
df_bodyeggs$residuals <- NA

# Loop through each level of the day factor and fit a model
for(day_level in unique(df_bodyeggs$day.x)) {
  # Subset the dataframe for the current level of day
  subset_df <- subset(df_bodyeggs, day.x == day_level)
  
  # Fit the linear model for egg_count as a function of norm_area
  model <- lm(egg_count ~ norm_area, data = subset_df)
  
  # Calculate the residuals for the subset
  subset_df$residuals <- residuals(model)
  
  # Assign the residuals back to the main dataframe
  df_bodyeggs$residuals[df_bodyeggs$day.x == day_level] <- subset_df$residuals
}


library(ggplot2)

ggplot(df_bodyeggs, aes(x = norm_area, y = residuals, color = condition.x)) +
  geom_boxplot() +
  facet_grid(~as.factor(day.x))
  #theme_minimal() +
  #labs(color = 'Day', x = 'Normalized Area', y = 'Residuals') +
  #ggtitle('Residuals by Day and Normalized Area')






#Relative to relative worm's own condition area
# Step 1: Calculate the average area for day 1 for each condition
average_area_day1 <- df_bodyeggs %>%
  filter(day.x == 1) %>%
  group_by(condition.x) %>%
  summarize(average_area_day1 = mean(area, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Join this average area information back to the original dataset
data_with_avg <- df_bodyeggs %>%
  left_join(average_area_day1, by = "condition.x")

# Step 3: Calculate the normalized area
data_normalized <- data_with_avg %>%
  mutate(normalized_area_bycond = (area-average_area_day1) / average_area_day1)


ggplot(data_normalized, aes(normalized_area_bycond, egg_count, colour = as.factor(condition.x))) +
  geom_point(size = 5)



