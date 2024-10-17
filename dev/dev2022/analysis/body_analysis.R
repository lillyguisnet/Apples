setwd("~/Apples/dev/dev2022/analysis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

xl_file = "allworms_metrics_body.csv"
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




###Non-normalized to AD1 plots###


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




###Normalize by ad1###

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



###Plot body length by body width
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
  geom_point() +
  facet_grid(~as.factor(day))

