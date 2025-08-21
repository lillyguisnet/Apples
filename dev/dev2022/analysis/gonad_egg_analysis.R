setwd("~/Apples/dev/dev2022/analysis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggh4x)

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







#######Plots for paper
df_bodyeggs <- df_bodyeggs %>%
  mutate(
    ancestry = case_when(
      condition.x %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition.x %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  ) %>%
  mutate(
    ancestry = case_when(
      condition.x %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition.x %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  )



###Eggs in gonads
png("eggs_in_gonads_by_area_day.png", width = 1900, height = 900)


df_bodyeggs$condition.x <- factor(df_bodyeggs$condition.x, levels = c("a", "b", "d", "c"))
y_max_for_grid <- max(df_bodyeggs$egg_count, na.rm = TRUE)
p <- ggplot(df_bodyeggs, aes(x = norm_area, y = egg_count, color = condition.x, shape = condition.x)) +
  geom_segment(x = 0, xend = 0, y = 0, yend = y_max_for_grid, color = "gray92") +
  geom_segment(x = 10, xend = 10, y = 0, yend = y_max_for_grid, color = "gray92") +
  geom_segment(x = 20, xend = 20, y = 0, yend = y_max_for_grid, color = "gray92") +
  geom_point(size = 1, alpha = 0.9) +
  facet_grid(~ day.y, scales = "fixed", labeller = labeller(day.y = c("1" = "Day 1", "2" = "Day 2", "3" = "Day 3", "4" = "Day 4", "5" = "Day 5", "6" = "Day 6"))) +
  labs(
    x = "Normalized body area",
    y = "Number of eggs in gonads"
  ) +
  scale_color_brewer(palette = "Set2", 
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "c" = "Scaffold ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth"),
                     name = element_blank()) +  # Changed legend title
  scale_shape_manual(values = c(16, 17, 18, 15),
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "c" = "Scaffold ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth"),
                     name = element_blank()) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    #axis.title = element_text(size = 8, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 2)),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.3, "lines"),  # Increased spacing between facets
    strip.background = element_rect(fill =  "#F0F0F0", color = NA),  # Light gray background for facet labels
    strip.text = element_text(size = 8, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 1, b = 1)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.y = element_line(color = "gray90"),
    #panel.grid.minor.y = element_line(color = "gray95"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    legend.position = c(0, 1),  # Move legend to top left
    legend.justification = c(0, 1),   # Align legend to top left
    legend.background = element_rect(fill = scales::alpha("white", 0.7), color = NA),  # Add semi-transparent white background to legend
    legend.key.height = unit(0.5, "lines"),  # Adjust spacing between legend items
    legend.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1)  # Add margin around legend
  ) +
  scale_x_continuous(breaks = c(0, 10, 20), labels = c("0", "10", "20")) +
  coord_cartesian(xlim = range(df_bodyeggs$norm_area, na.rm = TRUE)) +
  guides(color = guide_legend(override.aes = list(size = 2)))


ggsave("C:/Users/aurel/Documents/Apples/dev/dev2022/analysis/eggs_in_gonads_by_area_day_paper.png", p, dpi = 300, bg = "white", width = 95, height = 60, units = "mm")




dev.off()
