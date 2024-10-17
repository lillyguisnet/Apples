library(ggplot2)
library(dplyr)
library(openxlsx)

xl_file = "firstanalysis.xlsx"
df <- as.data.frame(read.xlsx(xl_file))



df_clean <- group_by(df) %>%
  mutate(ancestry = if_else(condition == "a" | condition == "b", "agar", "scaffold")) %>%
  mutate(growing = if_else(condition == "a" | condition == "d", "agar", "scaffold"))



df_norm <- group_by(df_clean) %>%
  mutate(a_av = mean(norm_diff[which(condition == "a")])) %>%
  mutate(norm_fat = (norm_diff - a_av)/a_av*100)


model <- aov(norm_fat~condition, df_norm)
plot(TukeyHSD(model, comf.level = 0.95))


png("adultfatbycondition.png", width = 1600, height = 900)

ggplot(df_norm, aes(condition, norm_fat)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8) +
  #facet_grid(cols = vars(day)) +
  ylab("Relative average ORO staining intensity") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )

dev.off()