library(ggplot2)
library(dplyr)
library(openxlsx)

xl_file = "dev2022.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

df_clean <- group_by(df) %>%
  mutate(ancestry = if_else(condition == "a" | condition == "b", "agar", "scaffold")) %>%
  mutate(growing = if_else(condition == "a" | condition == "d", "agar", "scaffold"))



df_norm <- group_by(df_clean, day) %>%
  mutate(a_av = mean(length_px[which(condition == "a")])) %>%
  mutate(norm_length = (length_px - a_av)/a_av*100) %>%
  mutate(a_avm = mean(midwidth_px[which(condition == "a")])) %>%
  mutate(norm_mid = (midwidth_px - a_avm)/a_avm*100)


model <- aov(norm_length~condition, df_norm)
plot(TukeyHSD(model, comf.level = 0.95))


png("deveggsbyday_2022.png", width = 1600, height = 900)

ggplot(df_norm, aes(condition, nb_eggs)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Number of eggs in gonad") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()




png("deveggsbylength_2022.png", width = 1600, height = 900)

ggplot(df_norm, aes(norm_length, nb_eggs)) +                              ##df, x, y
  geom_point(aes(colour = as.factor(condition)), cex = 2.8) +  ##width = of boxes, lwd = line width of boxes
  #geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Number of eggs in gonad") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()



png("devlengthbymid_2022.png", width = 1600, height = 900)

ggplot(df_norm, aes(norm_mid, norm_length)) +                              ##df, x, y
  geom_point(aes(colour = as.factor(condition))) +  ##width = of boxes, lwd = line width of boxes
  #geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  geom_smooth(method = "lm", se = FALSE, aes(colour = as.factor(condition)))+
  facet_grid(cols = vars(day)) +
  ylab("Relative length by midbody") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()



png("devmid_2022.png", width = 1600, height = 900)

ggplot(df_norm, aes(condition, norm_mid)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Relative midbody width") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()



png("devlength_2022.png", width = 1600, height = 900)

ggplot(df_norm, aes(condition, norm_length)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  facet_grid(cols = vars(day)) +
  ylab("Relative length") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()
