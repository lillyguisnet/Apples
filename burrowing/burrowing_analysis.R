setwd("~/Apples/burrowing")
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyr)
library(ggh4x)

xl_file = "burrowing_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))


df_clean <- group_by(df) %>%
  filter(timetotop_minutes != -1)


df_proportions <- group_by(df, condition) %>%
  mutate(perc_reached_condition = length(timetotop_minutes[which(timetotop_minutes != "-1")])/length(timetotop_minutes))

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
  geom_line(aes(linetype = condition), linewidth = 1.2) +
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







######Plot for paper######
df_longse <- df %>%
  crossing(time = seq(0, 180, by = 10)) %>%
  mutate(isattop = ifelse(timetotop_minutes == -1, 0, 
                         ifelse(time >= timetotop_minutes, 1, 0))) %>%
  group_by(condition, time) %>%
  summarise(
    n = n(),
    successes = sum(isattop),
    perc_top_cond = successes/n,
    se = sqrt((perc_top_cond * (1 - perc_top_cond)) / n)
  )




#Percentage at top
png("av_timetotop_paper.png", width = 980, height = 900)

ggplot(df_longse, aes(time, perc_top_cond, colour = condition)) +
  geom_line(aes(linetype = condition), linewidth = 1.2) +
  geom_point(aes(shape = condition, fill = condition), size = 4, stroke = 1.5) +
  geom_errorbar(aes(ymin = perc_top_cond - se, ymax = perc_top_cond + se), width = 5, size = 0.7) +
  theme_minimal() +
  theme(aspect.ratio = 0.8,
        legend.position = c(0.2, 0.9),  # Moved the legend to the top left
        legend.background = element_blank(),  # Removed the legend outline
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(1, "cm"),  # Increased legend key size
        legend.text = element_text(size = 16),  # Increased legend text size
        legend.title = element_text(size = 18, face = "plain"),  # Increased legend title size and made it bold
        axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
        axis.text = element_text(size = 20, margin = margin(t = 15, r = 15, b = 15, l = 15)),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))) +
  labs(x = "Time (minutes)", y = "Percentage at top", 
       color = "Ancestry and Growth Condition", 
       linetype = "Ancestry and Growth Condition", 
       shape = "Ancestry and Growth Condition", 
       fill = "Ancestry and Growth Condition") +
  scale_color_brewer(palette = "Set2",
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "c" = "Scaffold ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth")) +
  scale_fill_brewer(palette = "Set2",
                    labels = c("a" = "Agar ancestry, Agar growth",
                               "b" = "Agar ancestry, Scaffold growth",
                               "c" = "Scaffold ancestry, Scaffold growth",
                               "d" = "Scaffold ancestry, Agar growth")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                        labels = c("a" = "Agar ancestry, Agar growth",
                                   "b" = "Agar ancestry, Scaffold growth",
                                   "c" = "Scaffold ancestry, Scaffold growth",
                                   "d" = "Scaffold ancestry, Agar growth")) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 16),
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "c" = "Scaffold ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth")) +
  scale_x_continuous(breaks = c(0, 60, 120, 180), labels = c("0", "60", "120", "180")) +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", sprintf("%.1f", x)))

dev.off()



#Time to top
df_clean <- df_clean %>%
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





agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

png("av_timetotop_paper_ancestry.png", width = 650, height = 900)

ggplot(df_clean, aes(x = growing, y = timetotop_minutes, fill = as.factor(growing))) +
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
    y = "Time to top (minutes)",
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
