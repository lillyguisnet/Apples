setwd("~/Apples/dev/dev2022/analysis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggh4x)

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





#####Plots for the paper
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


  
###Egg area by condition
sample_sizes <- df_norm %>%
  group_by(ancestry, growing) %>%
  summarise(normed_length = mean(norm_area),
            n = n_distinct(norm_area))

write.table(sample_sizes, "clipboard", sep="\t", row.names=FALSE)
print(sample_sizes)

png("norm_egg_area_condition_paper.png", width = 750, height = 1100)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = growing, y = norm_area, fill = as.factor(growing))) +
  geom_boxplot() +
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
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    ),
    labeller = labeller(ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"))
  ) +
  labs(
    y = "Normalized egg area",
    x = "Growing condition",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
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
    strip.text = element_text(size = 18, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  )

dev.off()






###Egg elongation by condition
png("norm_egg_elongation_condition_paper.png", width = 750, height = 1100)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_norm, aes(x = growing, y = norm_elongation, fill = as.factor(growing))) +
  geom_boxplot() +
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
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    ),
    labeller = labeller(ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"))
  ) +
  labs(
    y = "Normalized egg elongation",
    x = "Growing condition",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
  scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2), labels = c("-0.2", "-0.1", "0", "0.1", "0.2")) +
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
    strip.text = element_text(size = 18, face = "plain"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    aspect.ratio = 3,
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  )

dev.off()
