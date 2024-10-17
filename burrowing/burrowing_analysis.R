setwd("~/Apples/burrowing")
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyr)

xl_file = "burrowing_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))


df_clean <- group_by(df) %>%
  filter(timetotop_minutes != -1)


df_proportions <- group_by(df, condition) %>%
  mutate(perc_reached_condition = length(timetotop_minutes[which(timetotop_minutes != "-1")])/length(timetotop_minutes)) %>%

model <- aov(timetotop_minutes~condition, df_clean)
plot(TukeyHSD(model, comf.level = 0.95))

df_no6 <- filter(df_clean, day != 6)
model <- aov(timetotop_minutes~condition, df_no6)
plot(TukeyHSD(model, comf.level = 0.95))


ggplot(df_no6, aes(timetotop_minutes, colour = as.factor(condition))) +
  stat_ecdf(geom="line")


#long table
df_long <- df %>%
  crossing(time = seq(0, 180, by = 10)) %>%
  mutate(isattop = ifelse(timetotop_minutes == -1, 0, ifelse(time >= timetotop_minutes, 1, 0))) %>%
  group_by(condition, day, time) %>%
  mutate(perc_top = length(isattop[which(isattop == 1)])/length(isattop)) %>%
  ungroup() %>%
  group_by(condition, time) %>%
  mutate(perc_top_cond = mean(perc_top))

ggplot(df_long, aes(time, perc_top_cond, colour = condition)) +
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), cex = 2.8) +
  geom_line()



df_longse <- df %>%
  crossing(time = seq(0, 180, by = 10)) %>%
  mutate(isattop = ifelse(timetotop_minutes == -1, 0, ifelse(time >= timetotop_minutes, 1, 0))) %>%
  group_by(condition, day, time) %>%
  mutate(perc_top = length(isattop[which(isattop == 1)])/length(isattop)) %>%
  ungroup() %>%
  group_by(condition, time) %>%
  summarise(
    perc_top_cond = mean(perc_top),
    se = sd(perc_top) / sqrt(n())  # Calculate standard error
  )

# Create the plot
ggplot(df_longse, aes(time, perc_top_cond, colour = condition)) +
  geom_line(aes(linetype = condition), size = 1.2) +
  geom_point(aes(shape = condition, fill = condition), size = 4, stroke = 1.5) +
  geom_errorbar(aes(ymin = perc_top_cond - se, ymax = perc_top_cond + se), width = 5, size = 0.7) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        legend.position = "bottom") +
  labs(x = "Time", y = "Percentage at Top", 
       color = "Condition", linetype = "Condition", shape = "Condition", fill = "Condition") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 16))



df_long_no6 <- df %>%
  filter(day != 6) %>%
  crossing(time = seq(0, 180, by = 10)) %>%
  mutate(isattop = ifelse(timetotop_minutes == -1, 0, ifelse(time >= timetotop_minutes, 1, 0))) %>%
  group_by(condition, day, time) %>%
  mutate(perc_top = length(isattop[which(isattop == 1)])/length(isattop)) %>%
  ungroup() %>%
  group_by(condition, time) %>%
  mutate(perc_top_cond = mean(perc_top))

df_av <- group_by(df_long_no6, condition) %>%
  filter(timetotop_minutes != -1) %>%
  summarise(av_timetotop = mean(timetotop_minutes))

ggplot(df_long_no6, aes(time, perc_top_cond, colour = condition)) +
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), cex = 2.8) +
  geom_line()

df_violin <- df_long_no6 %>%
  mutate(timetotop_largequitters = if_else(timetotop_minutes == -1, 300, timetotop_minutes))

ggplot(df_violin, aes(condition, timetotop_largequitters, colour = condition)) +
  geom_violin(aes(alpha = 0.01))+
  geom_jitter(shape= 16, height = 0, aes(color = condition), cex = 2.8,  alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 300, 10)) +
  ylab("quitters+no6")





# df_proportions <- group_by(df_swim, condition, day, replicate) %>%
#   mutate(perc_quiescence = length(state[which(state == "0")])/length(state)) %>%
#   mutate(perc_slow = length(state[which(state == "1")])/length(state)) %>%
#   mutate(perc_swim = length(state[which(state == "2")])/length(state)) %>%
#   summarise(pqui = mean(perc_quiescence), 
#             pslow = mean(perc_slow), 
#             pswim = mean(perc_swim))
  
  
    
png("av_timetotop_day.png", width = 1600, height = 900)

ggplot(df_clean, aes(condition, timetotop_minutes)) +                              ##df, x, y
  geom_boxplot(alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(day)), cex = 2.8) +
  ylab("Time to top (minutes)")

dev.off()  
  
  
  
png("av_timetotop_wasmoving.png", width = 1600, height = 900)

ggplot(df_clean, aes(condition, timetotop_minutes)) +                              ##df, x, y
  geom_boxplot(alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(wasmoving)), cex = 2.8) +
  ylab("Time to top (minutes)")

dev.off()


png("av_timetotop.png", width = 1600, height = 900)

ggplot(df_clean, aes(condition, timetotop_minutes)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Time to top (minutes)")

dev.off()