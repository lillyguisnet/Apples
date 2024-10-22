setwd("~/Apples/dev/dev2022/analysis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggh4x)

xl_file <- "allworms_metrics_body.csv"
df <- read.csv(xl_file)

df_wcond <- group_by(df, img_id) %>%
  mutate(condition = strsplit(img_id, "_")[[1]][1]) %>%
  mutate(plate = str_replace(strsplit(img_id, "_")[[1]][2],"p", "")) %>%
  mutate(replicate = strtoi(str_replace(strsplit(img_id, "_")[[1]][3], "a", ""))) %>%
  mutate(day = str_replace(strsplit(img_id, "_")[[1]][4],"d", "")) 


#Worm length normalized to worm center
df_dist <- df_wcond %>%
  #filter(day == 4) %>%
  group_by(img_id) %>%
  mutate(centered_length = medialaxis_length_list - medialaxis_length_list[length(medialaxis_length_list)/2])




#########Non-normalized to AD1 plots###########


###Plot medial axis distances, along worm, aligned to worm center, by day and cond
ggplot(df_dist, aes(centered_length, medial_axis_distances_sorted, colour = as.factor(replicate))) +
  geom_point() +
  facet_wrap(day~condition, ncol=4)


###Plot worm length by day
ggplot(df_dist, aes(day, pruned_medialaxis_length, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_wcond, aes(day, pruned_medialaxis_length, colour = as.factor(condition))) +
  geom_violin()


#Av length per day per cond
df_length <- df_wcond %>%
  select(condition, day, pruned_medialaxis_length) %>%
  group_by(condition, day) %>%
  summarise(worm_length = mean(pruned_medialaxis_length))
  




#Area per ind
df_area <- df_wcond %>%
  select(img_id, condition, day, area)%>%
  group_by(img_id, condition, day) %>%
  summarise(area_byid = mean(area))

ggplot(df_area, aes(day, area_byid, colour = as.factor(condition))) +
  geom_violin()
ggplot(df_area, aes(day, area_byid, colour = as.factor(condition))) +
  geom_boxplot()


#Mean width by ind
df_meanwidth <- df_wcond %>%
  select(img_id, condition, day, mean_wormwidth) %>%
  group_by(img_id, condition, day) %>%
  summarise(mean_wormwidth_byid = mean(mean_wormwidth))


ggplot(df_meanwidth, aes(day, mean_wormwidth_byid, colour = as.factor(condition))) +
  #geom_jitter() +
  geom_violin() 


#Mid length width by ind
df_midw <- df_wcond %>%
  select(img_id, condition, day, mid_length_width) %>%
  group_by(img_id, condition, day) %>%
  summarise(mid_length_width_byid = mean(mid_length_width))

ggplot(df_midw, aes(day, mid_length_width_byid, colour = as.factor(condition))) +
  geom_violin()


#Perimeter by ind
df_perimeter <- df_wcond %>%
  select(img_id, condition, day, perimeter) %>%
  group_by(img_id, condition, day) %>%
  summarise(perimeter_byid = mean(perimeter)) 

ggplot(df_perimeter, aes(day, perimeter_byid, colour = as.factor(condition))) +
  geom_violin()




###########	##Normalizes by ad1################

norm_ad1length <- function(x) {
  norm_length <- (2*(x-min(df_dist$centered_length[which(df_dist$condition=="a"&df_dist$day=="1")]))/(-min(df_dist$centered_length[which(df_dist$condition=="a"&df_dist$day=="1")]) - min(df_dist$centered_length[which(df_dist$condition=="a"&df_dist$day=="1")]))) - 1
  return(norm_length)
}


df_norm <- df_dist %>%
  mutate(day = as.numeric(day)) %>%
  mutate(common_id = str_c(condition, plate, replicate, day, sep = "_")) %>%
  mutate(av_ad1_area = mean(df_dist$area[which(df_dist$condition=="a" & df_dist$day=="1")])) %>%
  mutate(av_ad1_perimeter = mean(df_dist$perimeter[which(df_dist$condition=="a" & df_dist$day=="1")])) %>% 
  mutate(av_ad1_pruned_medialaxis_length = mean(df_dist$pruned_medialaxis_length[which(df_dist$condition=="a" & df_dist$day=="1")])) %>%  
  mutate(av_ad1_mean_wormwidth = mean(df_dist$mean_wormwidth[which(df_dist$condition=="a" & df_dist$day=="1")])) %>%
  mutate(av_ad1_mid_length_width = mean(df_dist$mid_length_width[which(df_dist$condition=="a" & df_dist$day=="1")])) %>%
  
  group_by(img_id) %>%
  mutate(norm_area = (area-av_ad1_area)/av_ad1_area) %>%
  mutate(norm_perimeter = (perimeter-av_ad1_perimeter)/av_ad1_perimeter) %>%
  mutate(norm_medial_axis_distances_sorted = (medial_axis_distances_sorted - av_ad1_mean_wormwidth)/av_ad1_mean_wormwidth) %>%
  mutate(norm_centered_length = norm_ad1length(centered_length)) %>%
  mutate(norm_pruned_medialaxis_length = (pruned_medialaxis_length - av_ad1_pruned_medialaxis_length)/av_ad1_pruned_medialaxis_length) %>%
  mutate(norm_mean_wormwidth = (mean_wormwidth-av_ad1_mean_wormwidth)/av_ad1_mean_wormwidth) %>%
  mutate(norm_mid_length_width = (mid_length_width - av_ad1_mid_length_width)/av_ad1_mid_length_width)


#Orient all widths from head to tail
correct_medial_axis_distances <- function(distances) {
  n <- length(distances)
  if (n <= 10) {
    return(distances)  # Not enough data points to compare, return as is
  }
  
  avg_first_10 <- mean(distances[1:10])
  avg_last_10 <- mean(distances[(n-9):n])
  
  if (avg_last_10 < avg_first_10) {
    return(distances)
  } else {
    return(rev(distances))
  }
}

df_headtotail <- df_norm %>%
  group_by(img_id) %>%
  mutate(corrected_medial_axis_distances = correct_medial_axis_distances(norm_medial_axis_distances_sorted)) %>%
  ungroup()

  
###Measures by day plots
ggplot(df_norm, aes(as.factor(day), norm_area, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_norm, aes(as.factor(day), norm_perimeter, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_norm, aes(as.factor(day), norm_pruned_medialaxis_length, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_norm, aes(as.factor(day), norm_mean_wormwidth, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_norm, aes(as.factor(day), norm_mid_length_width, colour = as.factor(condition))) +
  geom_boxplot()
  geom_dotplot()



###Across body plots
ggplot(df_headtotail, aes(norm_centered_length, corrected_medial_axis_distances, colour = as.factor(replicate))) +
  geom_point() +
  facet_wrap(day~condition, ncol=4)

ggplot(df_norm, aes(norm_centered_length, norm_medial_axis_distances_sorted, colour = as.factor(replicate))) +
  geom_point() +
  facet_wrap(day~condition, ncol=4)

ggplot(df_norm, aes(norm_centered_length, norm_medial_axis_distances_sorted, colour = as.factor(replicate))) +
  geom_point() +
  facet_wrap(condition~day, ncol=6)


ggplot(df_norm, aes(norm_centered_length, norm_medial_axis_distances_sorted, colour = as.factor(condition))) +
  geom_point() +
  facet_wrap(~day, ncol=6)

ggplot(df_norm, aes(norm_centered_length, norm_medial_axis_distances_sorted, colour = as.factor(day))) +
  geom_point() +
  facet_wrap(~condition, ncol=4)



#########Plot body length by body width################
df_blbw <- df_norm %>%
  mutate(blbwratio = pruned_medialaxis_length/mean_wormwidth) %>%
  filter()

df_blbw_norm <- df_blbw %>%
  mutate(av_ad1_blbwratio = mean(df_blbw$blbwratio[which(df_blbw$condition=="a" & df_blbw$day=="1")])) %>%
  mutate(norm_blbw = (blbwratio-av_ad1_blbwratio)/av_ad1_blbwratio)

unique(df_blbw_norm$norm_blbw)  
max(df_blbw_norm$norm_blbw) 
# 0.6034134
min(df_blbw_norm$norm_blbw) 
# -0.3715445

ggplot(df_blbw_norm, aes(condition, norm_blbw, colour = as.factor(condition))) +
  geom_boxplot() +
  geom_point() 


ggplot(df_blbw_norm, aes(condition, norm_blbw, colour = as.factor(condition))) +
  geom_boxplot() +
  #geom_point() +
  facet_grid(~as.factor(day))





#########Plots for paper#########
# Add columns for ancestry and growing conditions
df_norm <- df_norm %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  )




###Length by day
png("norm_body_length_day_paper.png", width = 1100, height = 900)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = as.factor(day), y = norm_pruned_medialaxis_length, fill = condition)) +
  geom_boxplot() +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    #nest_line = element_line(color = "black"),
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized body length",
    x = "Day"
  ) +
  scale_fill_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(
    limits = c(NA, 4.5),  # Adjust as needed
    breaks = seq(-1, 5, by = 1),  # Major grid lines at integers from -1 to 2
    minor_breaks = seq(-1, 5, by = 0.5),  # Minor grid lines every 0.5
    labels = scales::number_format(accuracy = 1)
  )

dev.off()




###Mean width by day
png("norm_mean_body_width_day_paper.png", width = 1100, height = 900)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = as.factor(day), y = norm_mean_wormwidth, fill = condition)) +
  geom_boxplot() +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    #nest_line = element_line(color = "black"),
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized mean body width",
    x = "Day"
  ) +
  scale_fill_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(
    limits = c(NA, 4.5),  # Adjust as needed
    breaks = seq(-1, 5, by = 1),  # Major grid lines at integers from -1 to 2
    minor_breaks = seq(-1, 5, by = 0.5),  # Minor grid lines every 0.5
    labels = scales::number_format(accuracy = 1)
  )

dev.off()



###Area by day
png("norm_body_area_day_paper.png", width = 1100, height = 900)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = as.factor(day), y = norm_area, fill = condition)) +
  geom_boxplot() +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    #nest_line = element_line(color = "black"),
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized body area",
    x = "Day"
  ) +
  scale_fill_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(
    limits = c(NA, NA),  # Adjust as needed
    breaks = seq(-15, 25, by = 5),  # Major grid lines every 5 around 0
    minor_breaks = seq(-15, 25, by = 1),  # Minor grid lines every 1
    labels = scales::number_format(accuracy = 1)
  )

dev.off()




#Perimeter by day 
png("norm_body_perimeter_day_paper.png", width = 1100, height = 900)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = as.factor(day), y = norm_perimeter, fill = condition)) +
  geom_boxplot() +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    #nest_line = element_line(color = "black"),
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized body perimeter",
    x = "Day"
  ) +
  scale_fill_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(
    limits = c(NA, NA),  # Adjust as needed
    breaks = seq(-14, 24, by = 1),  # Major grid lines every 5 around 0
    minor_breaks = seq(-14, 24, by = 0.5),  # Minor grid lines every 1
    labels = scales::number_format(accuracy = 1)
  )

dev.off()





#Mid-body width by day
png("norm_midbody_width_day_paper.png", width = 1100, height = 900)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = as.factor(day), y = norm_mid_length_width, fill = condition)) +
  geom_boxplot() +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    #nest_line = element_line(color = "black"),
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized mid-body width",
    x = "Day"
  ) +
  scale_fill_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(
    limits = c(NA, NA),  # Adjust as needed
    breaks = seq(-14, 24, by = 1),  # Major grid lines every 5 around 0
    minor_breaks = seq(-14, 24, by = 0.5),  # Minor grid lines every 1
    labels = scales::number_format(accuracy = 1)
  )

dev.off()



###Body length/width ratio - overall
df_blbw_norm <- df_blbw_norm %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  )



png("norm_body_length_width_ratio_overall_paper.png", width = 750, height = 1100)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

# Create the plot
ggplot(df_blbw_norm, aes(x = growing, y = norm_blbw, fill = as.factor(growing))) +
  geom_boxplot() +
  facet_wrap2(
    ~ ancestry,
    scales = "free_x",
    ncol = 2,
    labeller = labeller(ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry")),
    strip = strip_themed(
      background_x = list(
        element_rect(fill = agar_color, color = NA),
        element_rect(fill = scaffold_color, color = NA)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 10)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized body length/body width ratio",
    x = "Growing condition",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),  # Increased from 12 to 16
    axis.text.x = element_text(size = 20),  # Increased from 12 to 16
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),  # Added margin
    axis.title.x = element_text(margin = margin(t = 20)),  # Added extra margin for x-axis title
    axis.title.y = element_text(margin = margin(r = 20)),  # Added extra margin for y-axis title
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),  # Reduced top and bottom margins
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),  # Remove margins around the plot
    panel.grid.major.x = element_blank(),  # Remove x axis grid lines
    panel.grid.minor.x = element_blank()   # Remove minor x axis grid lines if present
  ) +
  scale_y_continuous(
    limits = c(NA, NA),  # Adjust as needed
    breaks = seq(-1, 1, by = 0.2),  # Major grid lines every 0.2
    minor_breaks = seq(-1, 1, by = 0.1),  # Minor grid lines every 0.1
    labels = scales::number_format(accuracy = 0.1)
  )


dev.off()



###Body length/width ratio - by day
png("norm_body_length_width_ratio_paper.png", width = 1100, height = 900)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_blbw_norm, aes(x = as.factor(day), y = norm_blbw, fill = condition)) +
  geom_boxplot() +
  facet_nested(
    ~ ancestry + growing,
    scales = "free_x",
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    y = "Normalized body length/body width ratio",
    x = "Day"
  ) +
  scale_fill_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  scale_y_continuous(
    limits = c(NA, NA),  # Adjust as needed
    breaks = seq(-1, 1, by = 0.2),  # Major grid lines every 0.5
    minor_breaks = seq(-1, 1, by = 0.1),  # Minor grid lines every 0.1
    labels = scales::number_format(accuracy = 0.1)
  )

dev.off()





###Length/width - by day
png("norm_body_length_width_by_day.png", width = 1200, height = 800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_headtotail, aes(x = norm_centered_length, y = corrected_medial_axis_distances, color = as.factor(day))) +
  geom_point(alpha = 0.6) +
  facet_nested(
    ~ ancestry + growing,
    scales = "fixed",  # Changed from "free_x" to "fixed" for same x-axis across all subplots
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    x = "Normalized body length",
    y = "Normalized body width",
    color = "Day"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "lines"),  # Increase the size of legend keys (dots)
    legend.key.height = unit(1.5, "lines"),  # Ensure the height matches the width
    legend.key.width = unit(1.5, "lines"),  # Ensure the width matches the height
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(2, "lines"),  # Increased spacing between facets
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  ) +
  coord_cartesian(xlim = c(-4, 4)) +  # Set x-axis limits from -4 to 4
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4))  # Set x-axis labels to -4, -2, 0, 2, 4

dev.off()
