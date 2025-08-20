setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(openxlsx)
library(ggh4x)

xl_file = "brood2021.xlsx"
df_exp <- as.data.frame(read.xlsx(xl_file))
xl_file = "brood2022_data.xlsx"
df_gro <- as.data.frame(read.xlsx(xl_file))

df_exp_clean <- group_by(df_exp) %>%
  rename(growing = born) %>%
  rename(experimental = raised) %>%
  mutate(ancestry = growing) %>%
  mutate(set = "experimental")

contaminated_plates = c("b_17", "b_21", "b_23", "b_30", "c_15")

df_gro_clean <- group_by(df_gro) %>%
  mutate(ancestry = if_else(condition == "a" | condition == "b", "agar", "scaffold")) %>%
  mutate(growing = if_else(condition == "a" | condition == "d", "agar", "scaffold")) %>%
  mutate(experimental = growing) %>%
  mutate(worm_id = paste(condition, replicate, sep = "_")) %>%
  subset(!(worm_id %in% contaminated_plates)) %>%
  select(!condition) %>%
  select(!worm_id) %>%
  mutate(set = "growing")

df_joined <- full_join(df_exp_clean, df_gro_clean) %>%
  mutate(group = paste(ancestry, growing, experimental, sep = ":")) %>%
  group_by(group, replicate) %>%
  mutate(dead = any(eggs == -1) ) %>%
  filter(!dead) %>%
  select(!dead) %>%
  mutate(worm_id = paste(group, set, sep = "_")) %>%
  group_by(worm_id, replicate) %>%
  mutate(total_eggs = sum(eggs))

df_growing <- filter(df_joined, set != "experimental")
df_experimental <- filter(df_joined, set != "growing")


####Plots####

png("brood_joined.png", width = 1600, height = 900)

ggplot(df_joined, aes(group, eggs)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(group)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = group), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Total number of eggs laid") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
    
  )

dev.off()


df_total$day <- factor(df_total$day, levels = c("1", "2", "3", "4"), labels = c("Day 1", "Day 2", "Day 3", "Day 4") )


png("brood_results.png", width = 1600, height = 900)

ggplot(df_brood, aes(condition_words, eggs)) +                              ##df, x, y
  #geom_boxplot(aes(colour = as.factor(condition_words)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition_words), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day))

dev.off()


png("brood_results_nodead.png", width = 1600, height = 900)

ggplot(df_total, aes(condition_words, eggs)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition_words)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition_words), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Number of eggs laid") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()


png("brood_results_total.png", width = 1600, height = 900)

ggplot(df_total, aes(condition_words, total_eggs)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition_words)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition_words), show.legend = FALSE, cex = 2.8) +
  ylab("Number of eggs laid") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    #strip.text.x = element_text(size = 18)
    
  )

dev.off()


png("brood_results_replicate.png", width = 1600, height = 900)

ggplot(df_brood, aes(day, eggs, colour = as.factor(replicate), fill = worm_id)) +                              ##df, x, y
  geom_line(show.legend = FALSE, lwd = 1) +
  facet_grid(cols = vars(condition_words))

dev.off()




###Growing condition plot

# Create a box plot of eggs by growing condition
png("brood_results_growing_paper.png", width = 650, height = 900)

# Define colors for agar and scaffold
agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

# Create the plot
p <- ggplot(df_growing, aes(x = growing, y = total_eggs, fill = as.factor(growing))) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_wrap2(
    ~ ancestry,
    scales = "free_x",
    ncol = 2,
    strip = strip_themed(
      background_x = list(
        element_rect(fill = agar_color, color = NA),
        element_rect(fill = scaffold_color, color = NA)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 2, l = 1, r = 1)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 1, b = 0, l = 1, r = 1))
      )
    ),
    labeller = as_labeller(c("agar" = "Agar\nancestry", "scaffold" = "Scaffold\nancestry"))
  ) +
  labs(
    y = "Total brood size",
    x = "Growing habitat",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -4, b = 0)),
    #axis.title = element_text(size = 8, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 2)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    #panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    #strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    #aspect.ratio = 3,
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank()
  ) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
  scale_y_continuous(
    limits = c(100, NA),  # Start y-axis at 100
    breaks = seq(100, 400, by = 50),  # Set breaks every 50 units
    labels = scales::number_format(accuracy = 1)  # Format labels without decimal places
  )

ggsave("C:/Users/aurel/Documents/Apples/brood2022/brood_results_growing_paper.pdf", p, dpi = 300, bg = "white", width = 35, height = 60, units = "mm")



dev.off()

# Calculate descriptive statistics
stats <- df_growing %>%
  group_by(ancestry, growing) %>%
  summarise(
    mean = mean(total_eggs, na.rm = TRUE),
    median = median(total_eggs, na.rm = TRUE),
    sd = sd(total_eggs, na.rm = TRUE),
    min = min(total_eggs, na.rm = TRUE),
    max = max(total_eggs, na.rm = TRUE),
    n = n_distinct(replicate)
  )

print(stats)
# Save descriptive statistics to a CSV file
write.csv(stats, "brood_results_growing_stats.csv", row.names = FALSE)





###Experimental condition plot

# Create a box plot of eggs by growing condition
png("brood_results_experimental_paper.png", width = 650, height = 900)

agar_color <- "#66C2A5"  # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

p <- ggplot(df_experimental, aes(x = experimental, y = total_eggs, fill = as.factor(experimental))) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    nest_line = element_line(color = "black"),
    labeller = labeller(
      ancestry = c("agar" = "Agar\nancestry", "scaffold" = "Scaffold\nancestry"),
      growing = c("agar" = "Agar\ngrowth", "scaffold" = "Scaffold\ngrowth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 0.8),
        element_rect(fill = scaffold_color, color = "white", linewidth = 0.8)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2))
      )
    )
  ) +
  labs(
    y = "Total brood size",
    x = "Egg-laying habitat"
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -4, b = 0)),
    #axis.title = element_text(size = 8, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 2)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    #panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    #strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    #aspect.ratio = 3,
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_line(color = "gray90")
  ) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
  scale_y_continuous(
    limits = c(100, NA),  # Start y-axis at 100
    breaks = seq(100, 400, by = 50),  # Set breaks every 50 units
    labels = scales::number_format(accuracy = 1)  # Format labels without decimal places
  )


ggsave("C:/Users/aurel/Documents/Apples/brood2022/brood_results_experimental_paper.pdf", p, dpi = 300, bg = "white", width = 35, height = 67, units = "mm")




dev.off()


# Calculate descriptive statistics
stats <- df_experimental %>%
  group_by(ancestry, experimental) %>%
  summarise(
    mean = mean(total_eggs, na.rm = TRUE),
    median = median(total_eggs, na.rm = TRUE),
    sd = sd(total_eggs, na.rm = TRUE),
    min = min(total_eggs, na.rm = TRUE),
    max = max(total_eggs, na.rm = TRUE),
    n = n_distinct(replicate)
  )

print(stats)
# Save descriptive statistics to a CSV file
write.csv(stats, "brood_results_experimental_stats.csv", row.names = FALSE)






#By day plot
png("brood_results_day_paper.png", width = 1200, height = 900)

agar_color <- "#66C2A5"  # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette
facet_color <- "#F0F0F0"  # Pale gray for facet rectangle

p <- ggplot(df_joined %>% filter(group %in% c("agar:agar:agar", "scaffold:scaffold:scaffold")), 
       aes(x = group, y = eggs, fill = group)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.01) +
  facet_wrap2(~ day, ncol = 4, scales = "fixed", 
            labeller = labeller(day = function(x) paste("Day", x)),
            strip = strip_themed(
      background_x = list(
        element_rect(fill = facet_color, color = NA),
        element_rect(fill = facet_color, color = NA),
        element_rect(fill = facet_color, color = NA),
        element_rect(fill = facet_color, color = NA)
      ),
      text_x = list(
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2)),
        element_text(color = "#000000", size = 8, face = "plain", margin = margin(t = 2, b = 2))
      )
    )
             ) +
  labs(
    x = "Growing and ancestry habitat",
    y = "Number of offspring per day"
  ) +
  scale_fill_manual(values = c("agar:agar:agar" = agar_color, "scaffold:scaffold:scaffold" = scaffold_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8, color = "black", angle = 45, hjust = 1, margin = margin(t = -4, b = 0)),
    #axis.title = element_text(size = 8, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 2)),  # Increased y-axis title size
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = facet_color, color = NA),  # Add pale gray facet rectangle
    #strip.text = element_text(size = 18, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 0, b = 0)),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),  # Lighter color for y-axis grid lines
    panel.grid.minor.y = element_blank()  # Remove minor y-axis grid lines
  ) +
  scale_x_discrete(labels = c("agar:agar:agar" = "Agar", "scaffold:scaffold:scaffold" = "Scaffold"))

ggsave("C:/Users/aurel/Documents/Apples/brood2022/brood_results_day_paper.pdf", p, dpi = 300, bg = "white", width = 65, height = 60, units = "mm")



dev.off()


# Calculate descriptive statistics for the by-day plot
stats_by_day <- df_joined %>%
  filter(group %in% c("agar:agar:agar", "scaffold:scaffold:scaffold")) %>%
  group_by(day, group, set) %>%
  summarise(
    mean = mean(eggs, na.rm = TRUE),
    median = median(eggs, na.rm = TRUE),
    sd = sd(eggs, na.rm = TRUE),
    min = min(eggs, na.rm = TRUE),
    max = max(eggs, na.rm = TRUE),
    n = n_distinct(replicate)  # Count distinct replicates for each set
  ) %>%
  group_by(day, group) %>%
  summarise(
    mean = mean(mean),
    median = mean(median),
    sd = mean(sd),
    min = min(min),
    max = max(max),
    n = sum(n)  # Sum the number of distinct replicates from both sets
  ) %>%
  ungroup()

print(stats_by_day)

# Save descriptive statistics to a CSV file
write.csv(stats_by_day, "brood_results_by_day_stats.csv", row.names = FALSE)
