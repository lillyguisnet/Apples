setwd("~/Apples/oro")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggh4x)

xl_file = "allmasks_removeoverlap_normredness_nosaturation_metrics.csv"
df <- read.csv(xl_file)

df_wcond <- group_by(df, maskid) %>%
  mutate(mask_index = str_remove(strsplit(maskid, "_")[[1]][1], "c")) %>%
  mutate(condition = strsplit(maskid, "_")[[1]][2]) %>%
  mutate(replicate = str_remove(strsplit(maskid, "_")[[1]][4], ".png")) 

#Worm length normalized to worm center
df_dist <- df_wcond %>%
  #filter(day == 4) %>%
  group_by(maskid) %>%
  mutate(centered_length = medialaxis_length_list - medialaxis_length_list[length(medialaxis_length_list)/2])




###Normalize worm metrics
norm_ad1length <- function(x) {
  norm_length <- (2*(x-min(df_dist$centered_length[which(df_dist$condition=="a")]))/(-min(df_dist$centered_length[which(df_dist$condition=="a")]) - min(df_dist$centered_length[which(df_dist$condition=="a")]))) - 1
  return(norm_length)
}

###Normalize by ad1
df_norm <- df_dist %>%
  mutate(common_id = str_c(condition, replicate, sep = "_")) %>%
  mutate(av_ad1_area = min(df_dist$area[which(df_dist$condition=="a")])) %>%
  mutate(av_ad1_perimeter = min(df_dist$perimeter[which(df_dist$condition=="a")])) %>% 
  mutate(av_ad1_pruned_medialaxis_length = min(df_dist$pruned_medialaxis_length[which(df_dist$condition=="a")])) %>%  
  mutate(av_ad1_mean_wormwidth = min(df_dist$mean_wormwidth[which(df_dist$condition=="a")])) %>%
  mutate(av_ad1_mid_length_width = min(df_dist$mid_length_width[which(df_dist$condition=="a")])) %>%
  
  group_by(maskid) %>%
  mutate(norm_area = (area-av_ad1_area)/av_ad1_area) %>%
  mutate(norm_perimeter = (perimeter-av_ad1_perimeter)/av_ad1_perimeter) %>%
  mutate(norm_medial_axis_distances_sorted = (medial_axis_distances_sorted - av_ad1_mean_wormwidth)/av_ad1_mean_wormwidth) %>%
  mutate(norm_centered_length = norm_ad1length(centered_length)) %>%
  mutate(norm_pruned_medialaxis_length = (pruned_medialaxis_length - av_ad1_pruned_medialaxis_length)/av_ad1_pruned_medialaxis_length) %>%
  mutate(norm_mean_wormwidth = (mean_wormwidth-av_ad1_mean_wormwidth)/av_ad1_mean_wormwidth) %>%
  mutate(norm_mid_length_width = (mid_length_width - av_ad1_mid_length_width)/av_ad1_mid_length_width) %>%
  
  mutate(minstd = mean_redness - std_dev_redness) %>%
  mutate(maxstd = mean_redness + std_dev_redness)



ggplot(df_norm, aes(condition, std_dev_redness, colour = as.factor(condition))) +
  geom_boxplot()

df_summary <- df_norm %>%
  select(maskid, condition, minstd, maxstd, mean_redness, min_redness, max_redness, median_redness, std_dev_redness, norm_pruned_medialaxis_length) %>%
  group_by(maskid, condition) %>%
  summarise(mean_redness = mean(mean_redness),
            minstd = mean(minstd),
            maxstd = mean(maxstd),
            min_redness = mean(min_redness), 
            max_redness = mean(max_redness),
            median_redness = mean(median_redness),
            std_dev_redness = mean(std_dev_redness),
            length_bymask = mean(norm_pruned_medialaxis_length),
  ) 

ggplot(df_summary, aes(length_bymask, mean_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin = minstd, ymax = maxstd), show.legend = FALSE)
  

df_redness <- df_norm %>%
  mutate(amin_mean_redness = min(df_norm$mean_redness[which(df_norm$condition=="a")])) %>%
  mutate(amin_min_redness = min(df_norm$min_redness[which(df_norm$condition=="a")])) %>%
  mutate(amin_max_redness = min(df_norm$max_redness[which(df_norm$condition=="a")])) %>%
  mutate(amin_median_redness = min(df_norm$median_redness[which(df_norm$condition=="a")])) %>%
  mutate(amin_std_dev_redness = min(df_norm$std_dev_redness[which(df_norm$condition=="a")])) %>%
  
  group_by(maskid) %>%
  mutate(norm_mean_redness = (mean_redness - amin_mean_redness)/abs(amin_mean_redness)) %>%
  mutate(norm_min_redness = (min_redness - amin_min_redness)/abs(amin_min_redness)) %>%
  mutate(norm_max_redness = (max_redness - amin_max_redness)/abs(amin_max_redness)) %>%
  mutate(norm_median_redness = (median_redness - amin_median_redness)/abs(amin_median_redness)) %>%
  mutate(norm_std_dev_redness = (std_dev_redness - amin_std_dev_redness)/abs(amin_std_dev_redness))
  

df_summaryred <- df_redness %>%
  select(maskid, condition, norm_mean_redness, norm_min_redness, norm_max_redness, norm_median_redness, norm_std_dev_redness, norm_pruned_medialaxis_length) %>%
  group_by(maskid, condition) %>%
  summarise(norm_mean_redness = mean(norm_mean_redness),
            norm_min_redness = mean(norm_min_redness), 
            norm_max_redness = mean(norm_max_redness),
            norm_median_redness = mean(norm_median_redness),
            norm_std_dev_redness = mean(norm_std_dev_redness),
            norm_length_bymask = mean(norm_pruned_medialaxis_length),
  ) 

ggplot(df_summaryred, aes(condition, norm_mean_redness, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_summaryred, aes(norm_length_bymask, norm_mean_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE)

ggplot(df_summaryred, aes(norm_length_bymask, norm_min_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE)

ggplot(df_summaryred, aes(norm_length_bymask, norm_max_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE)

ggplot(df_summaryred, aes(norm_length_bymask, norm_median_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE)

ggplot(df_summaryred, aes(norm_length_bymask, norm_std_dev_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE)



###Residuals
# A list to store the residuals
df_summaryred$residuals <- NA

# Fit the linear model for egg_count as a function of norm_area
model <- lm(norm_mean_redness ~ norm_length_bymask, data = df_summaryred)
  
# Calculate the residuals for the subset
df_summaryred$residuals <- residuals(model)
  


ggplot(df_summaryred, aes(x = norm_length_bymask, y = residuals, color = condition)) +
  geom_boxplot()
  #facet_grid(~as.factor(day.x))








rgb_to_gray <- function(r, g, b) {
  gray <- ( (0.299 * r) +
              (0.587 * g) +
              (0.114 * b)
  )
  return(gray)
}


##Normalize red channel
df_red <- df_norm %>%
  rename(sum_r_real = sum_b) %>%
  rename(sum_b_real = sum_r) %>%
  rename(sumbg_r_real = sumbg_b) %>%
  rename(sumbg_b_real = sumbg_r) %>%  
  
  group_by(maskid) %>%
  
  mutate(avr_bg_permask = sumbg_r_real/50) %>%
  mutate(avg_bg_permask = sumbg_g/50) %>%
  mutate(avb_bg_permask = sumbg_b_real/50) %>%
  
  mutate(avgray_bg_permask = rgb_to_gray(avr_bg_permask, avg_bg_permask, avb_bg_permask)) %>%
  
  mutate(avr_worm_permask = sum_r_real/area) %>%
  mutate(avg_worm_permask = sum_g/area) %>%
  mutate(avb_worm_permask = sum_b_real/area) %>%
  
  mutate(avgray_worm_permask = rgb_to_gray(avr_worm_permask, avg_worm_permask, avb_worm_permask)) %>%
  
  mutate(norm_r_permask_tobg = (avr_worm_permask - avr_bg_permask)) %>%
  mutate(norm_g_permask_tobg = (avg_worm_permask - avg_bg_permask)) %>%
  mutate(norm_b_permask_tobg = (avb_worm_permask - avb_bg_permask)) %>%
  
  mutate(norm_gray_permask_tobg = (avgray_worm_permask - avgray_bg_permask)) %>%
  mutate(norm_darkness_permask_tobg = 1/norm_gray_permask_tobg) %>%
  
  ungroup() %>%
  mutate(minr_a = min(norm_r_permask_tobg[which(df_norm$condition=="a")])) %>%
  mutate(ming_a = min(norm_g_permask_tobg[which(df_norm$condition=="a")])) %>%
  mutate(minb_a = min(norm_b_permask_tobg[which(df_norm$condition=="a")])) %>%
  
  mutate(mingray_a = min(norm_gray_permask_tobg[which(df_norm$condition=="a")])) %>%
  mutate(mindarkness_a = 1/mingray_a) %>%
  
  group_by(maskid) %>%
  mutate(norm_r_permask_toa = (norm_r_permask_tobg - minr_a)/abs(minr_a)) %>%
  mutate(norm_g_permask_toa = (norm_g_permask_tobg - ming_a)/abs(ming_a)) %>%
  mutate(norm_b_permask_toa = (norm_b_permask_tobg - minb_a)/abs(minb_a)) %>%
  
  mutate(norm_gray_permask_toa = (norm_gray_permask_tobg - mingray_a)/abs(mingray_a)) %>%
  mutate(norm_darkness_permask_toa = (norm_darkness_permask_tobg - mindarkness_a)/abs(mindarkness_a))


#minr_a <- min(df_red$norm_r_permask_tobg[df_red$condition == "a"])


  #summarise(norm_r_permask_tobg  = mean(norm_r_permask_tobg), minr_a = mean(minr_a))

  mutate(norm_r_permask_toa = ((norm_r_permask_tobg - minr_a)/abs(minr_a)))





###Plot results
df_summary <- df_red %>%
  select(maskid, condition, norm_r_permask_toa, norm_g_permask_toa, norm_b_permask_toa, norm_gray_permask_toa, norm_darkness_permask_toa, norm_pruned_medialaxis_length) %>%
  group_by(maskid, condition) %>%
  summarise(r_bymask = mean(norm_r_permask_toa),
            g_bymask = mean(norm_g_permask_toa), 
            b_bymask = mean(norm_b_permask_toa),
            gray_bymask = mean(norm_gray_permask_toa),
            darkness_bymask = mean(norm_darkness_permask_toa),
            length_bymask = mean(norm_pruned_medialaxis_length),
            ) 


ggplot(df_summary, aes(length_bymask, r_bymask, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE,)

ggplot(df_summary, aes(length_bymask, g_bymask, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE,)

ggplot(df_summary, aes(length_bymask, b_bymask, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE,)

ggplot(df_summary, aes(length_bymask, gray_bymask, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE,)

ggplot(df_summary, aes(length_bymask, darkness_bymask, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE,)




ggplot(df_red, aes(condition, norm_r_permask_toa, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_red, aes(condition, norm_g_permask_toa, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_red, aes(condition, norm_b_permask_toa, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_red, aes(condition, norm_gray_permask_toa, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_red, aes(condition, norm_darkness_permask_toa, colour = as.factor(condition))) +
  geom_boxplot()




ggplot(df_norm, aes(norm_pruned_medialaxis_length, mean_redness, colour = as.factor(condition))) +
  facet_wrap(~condition, ncol = 4) +
  geom_point(show.legend = FALSE,)

ggplot(df_norm, aes(norm_pruned_medialaxis_length, norm_area, colour = as.factor(condition))) +
  geom_point()






ggplot(df_norm, aes(as.factor(day), norm_mean_wormwidth, colour = as.factor(condition))) +
  geom_boxplot()

ggplot(df_norm, aes(as.factor(day), norm_mid_length_width, colour = as.factor(condition))) +
  #geom_boxplot() +
  geom_dotplot()




ggplot(df_norm, aes(norm_centered_length, norm_medial_axis_distances_sorted, colour = as.factor(replicate))) +
  geom_point() +
  facet_wrap(day~condition, ncol=4)


ggplot(df_norm, aes(norm_centered_length, norm_medial_axis_distances_sorted, colour = as.factor(day))) +
  geom_point() +
  facet_wrap(~condition)








######Plot for paper######
df_summaryred <- df_summaryred %>%
  mutate(
    ancestry = case_when(
      condition %in% c("a", "b") ~ "agar",
      TRUE ~ "scaffold"
    ),
    growing = case_when(
      condition %in% c("a", "d") ~ "agar",
      TRUE ~ "scaffold"
    )
  ) %>%
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


sample_sizes <- df_summaryred %>%
  group_by(ancestry, growing) %>%
  summarise(normed_length = mean(norm_mean_redness),
            n = n_distinct(norm_mean_redness))

write.table(sample_sizes, "clipboard", sep="\t", row.names=FALSE)
print(sample_sizes)


png("norm_redness_length_paper.png", width = 1200, height = 1200)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_summaryred, aes(x = norm_length_bymask, y = norm_mean_redness, color = condition)) +
  geom_point(size = 4, show.legend = FALSE) +
  facet_nested(
    ~ ancestry + growing,
    scales = "fixed",
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
    y = "Normalized mean redness"
  ) +
  scale_color_manual(values = c("a" = agar_color, "b" = scaffold_color, "c" = scaffold_color, "d" = agar_color)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),  # Increased top margin for x-axis title
    axis.title.y = element_text(margin = margin(r = 20)),  # Increased right margin for y-axis title
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(2, "lines"),  # Increased spacing between facets
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  coord_cartesian(xlim = range(df_summaryred$norm_length_bymask, na.rm = TRUE))

dev.off()
