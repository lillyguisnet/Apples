setwd("~/Apples/dev/dev2022/analysis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

xl_file = "egg_features.csv"
df <- read.csv(xl_file)

df <- group_by(df, file_name) %>%
  mutate(condition = strsplit(file_name, "_")[[1]][2]) %>%
  mutate(plate = str_replace(strsplit(file_name, "_")[[1]][3],"p", "")) %>%
  mutate(replicate = strtoi(str_replace(strsplit(file_name, "_")[[1]][4], "b", ""))) %>%
  mutate(common_id = str_c(condition, plate, replicate, sep = "_"))

df_norm <- df %>%
  mutate(a_area = mean(df$area[which(df$condition=="a")])) %>%
  mutate(a_perimeter = mean(df$perimeter[which(df$condition=="a")])) %>%
  mutate(a_elongation = mean(df$elongation[which(df$condition=="a")])) %>%
  group_by(file_name) %>%
  mutate(norm_area = (area-a_area)/a_area) %>%
  mutate(norm_perimeter = (perimeter-a_perimeter)/a_perimeter) %>%
  mutate(norm_elongation = (elongation-a_elongation)/a_elongation)
  
  

ggplot(df_norm, aes(condition, norm_area, colour = as.factor(condition))) +
  geom_point() +
  geom_boxplot()

ggplot(df_norm, aes(condition, norm_perimeter, colour = as.factor(condition))) +
  geom_point() +
  geom_boxplot()

ggplot(df_norm, aes(condition, norm_elongation, colour = as.factor(condition))) +
  geom_point() +
  geom_boxplot()
