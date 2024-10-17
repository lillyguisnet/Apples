library(stringr)
library(dplyr)
library(ggplot2)
library(openxlsx)

xl_file = "dev.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

##Size

df_conditions <- group_by(df) %>%
  mutate(parent = if_else(condition == "A" | condition == "B", "agar", "scaffold")) %>%
  mutate(raised = if_else(condition == "A" | condition == "D", "agar", "scaffold")) %>%
  mutate(condition_words = paste(parent, raised, sep = ":")) %>%
  mutate(size_toscale = size/10)

df_conditions$day <- factor(df_conditions$day, levels = c("0", "1", "2", "3"), labels = c("eggs", "Day 1 (+24h)", "Day 2 (+48h)", "Day 3 (+72h)") )

png("dev_results.png", width = 1600, height = 900)

ggplot(df_conditions, aes(condition_words, size_toscale)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition_words)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition_words), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Length (Âµm)") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()


##Eggs

df_eggs <- filter(df_conditions, !is.na(eggs))

png("dev_eggs.png", width = 1600, height = 900)

ggplot(df_eggs, aes(condition_words, eggs)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition_words)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition_words), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Number of eggs in gonad") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()



png("dev_eggs_tosize.png", width = 1600, height = 900)

ggplot(df_eggs, aes(size_toscale, eggs)) +    
  geom_point(aes(colour = as.factor(condition_words)), show.legend = FALSE, size = 3) +
  facet_grid(cols = vars(day)) +
  ylab("Number of eggs in gonad") +
  xlab("Length (µm)") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()




