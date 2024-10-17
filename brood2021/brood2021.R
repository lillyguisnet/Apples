library(ggplot2)
library(dplyr)
library(openxlsx)

xl_file = "brood2021.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

df_brood <- group_by(df) %>%
  mutate(condition_words = paste(born, raised, sep = ":")) %>%
  mutate(worm_id = paste(condition_words, replicate)) %>%
  mutate(condition = ifelse(condition_words == "agar:agar", "a", NA)) %>%
  mutate(condition = ifelse(condition_words == "agar:scaffold", "b", condition))%>%
  mutate(condition = ifelse(condition_words == "scaffold:scaffold", "c", condition)) %>%
  mutate(condition = ifelse(condition_words == "scaffold:agar", "d", condition))

df_nodead <- group_by(df_brood) %>%
  filter(eggs != -1)

df_total <- group_by(df_brood, condition_words, replicate) %>%
  mutate(dead = any(eggs == -1) ) %>%
  filter(!dead) %>%
  mutate(worm_id = paste(condition_words, replicate)) %>%
  mutate(total_eggs = sum(eggs))


ggplot(df_total, aes(condition, total_eggs)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  ylab("Number of eggs laid") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    #strip.text.x = element_text(size = 18)
    
  )


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




