library(ggplot2)
library(dplyr)
library(openxlsx)

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




