setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(openxlsx)
library(emmeans)
library(lme4)

xl_file = "oxidativestress_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))
df$condition <- factor(df$condition, levels = c("a", "b", "d", "c"))

df_clean <- group_by(df, day, condition, replicate) %>%
  mutate(deadatstart = alive == 0 & time_hours == 0) %>%
  mutate(deadatstart = max(deadatstart)) %>%
  filter(deadatstart == FALSE)

df_short <- group_by(df_clean, condition, day, replicate) %>%
  mutate(timedead = if_else(alive == 1, time_hours + 2, 0)) %>%
  mutate(timedead = max(timedead)) %>%
  ungroup() %>%
  group_by(condition) %>%
  mutate(av_deathtime = mean(timedead))

ggplot(df_short, aes(condition, timedead)) +
  geom_violin(aes(alpha = 0.01))+
  geom_jitter(shape= 16, height = 0, aes(color = as.factor(day)), cex = 2.8,  alpha = 0.5) +
  ylab("hour of death")

ggplot(df_short, aes(condition, timedead)) +
  geom_boxplot(aes(alpha = 0.01))+
  geom_jitter(shape= 16, height = 0, aes(color = as.factor(day)), cex = 2.8,  alpha = 0.5) +
  ylab("hourdeath")



df_proportions <- group_by(df_clean, day, condition, time_hours) %>%
  mutate(num_dead_bygroup = sum(alive == 0)) %>%
  mutate(num_alive_bygroup = sum(alive == 1)) %>%
  ungroup() %>%
  mutate(perc_dead = num_dead_bygroup/(num_dead_bygroup+num_alive_bygroup)) %>%
  group_by(condition, time_hours) %>%
  mutate(percdead_byhour_cond = mean(perc_dead))


df_with_ci <- df_clean %>%
  group_by(day, condition, time_hours) %>%
  summarise(
    num_dead = sum(alive == 0),
    num_alive = sum(alive == 1),
    .groups = "drop"
  ) %>%
  mutate(perc_dead = num_dead / (num_dead + num_alive)) %>%
  group_by(condition, time_hours) %>%
  summarise(
    mean_perc_dead = mean(perc_dead),
    n = n(),
    se = sd(perc_dead) / sqrt(n),
    ci_lower = mean_perc_dead - qt(0.975, n-1) * se,
    ci_upper = mean_perc_dead + qt(0.975, n-1) * se,
    .groups = "drop"
  ) %>%
  # Ensure CI bounds are within [0, 1]
  mutate(
    ci_lower = pmax(0, ci_lower),
    ci_upper = pmin(1, ci_upper)
  )

# Create the plot with confidence intervals
ggplot(df_with_ci, aes(x = time_hours, y = mean_perc_dead, color = condition)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.4,
    alpha = 0.7,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 3, 
    alpha = 0.7,
    position = position_dodge(width = 0.5)
  ) +
  geom_line(
    aes(group = condition), 
    alpha = 0.5, 
    position = position_dodge(width = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Average Percentage of Dead Worms Over Time",
    subtitle = "Error bars represent 95% confidence intervals",
    x = "Time (Hours)",
    y = "Average Percentage Dead",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



df_with_se <- df_clean %>%
  group_by(day, condition, time_hours) %>%
  summarise(
    num_dead = sum(alive == 0),
    num_alive = sum(alive == 1),
    .groups = "drop"
  ) %>%
  mutate(perc_dead = num_dead / (num_dead + num_alive)) %>%
  group_by(condition, time_hours) %>%
  summarise(
    mean_perc_dead = mean(perc_dead),
    se_perc_dead = sd(perc_dead) / sqrt(n()),
    .groups = "drop"
  )


df_with_se <- df_with_se %>%
  group_by(time_hours) %>%
  mutate(
    condition_num = as.numeric(factor(condition)),
    x_pos = time_hours + (condition_num - mean(condition_num)) * 0.2
  ) %>%
  ungroup()

sample_sizes <- df %>%
  group_by(condition, time_hours) %>%
  summarise(n = n())

write.table(sample_sizes, "clipboard", sep="\t", row.names=FALSE)
print(sample_sizes)

# Set up aesthetic mappings
line_types <- c("solid", "dashed", "dotted", "dotdash")
point_shapes <- c(16, 17, 15, 18)
names(line_types) <- names(point_shapes) <- unique(df_with_se$condition)

# Create the plot
ggplot(df_with_se, aes(x = x_pos, y = mean_perc_dead, color = condition, group = condition)) +
  geom_errorbar(
    aes(ymin = mean_perc_dead - se_perc_dead, 
        ymax = mean_perc_dead + se_perc_dead),
    width = 0.1,
    size = 0.75
  ) +
  geom_line(aes(linetype = condition), size = 0.75) +
  geom_point(aes(shape = condition), size = 5) +
  scale_x_continuous(
    breaks = unique(df_with_se$time_hours_num),
    labels = levels(df_with_se$time_hours)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set2") +
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = point_shapes) +
  labs(
    title = "Average Percentage of Dead Worms Over Time",
    subtitle = "Error bars represent ± 1 standard error",
    x = "Time (Hours)",
    y = "Average Percentage Dead",
    color = "Condition",
    linetype = "Condition",
    shape = "Condition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.box = "vertical",
    legend.margin = margin(),
    aspect.ratio = 0.6,
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank(),
    #axis.text.x = element_text(hjust = 1)
  ) +
  guides(
    color = guide_legend(nrow = 2, override.aes = list(linetype = line_types, shape = point_shapes)),
    linetype = "none",
    shape = "none"
  )

ggsave("elongated_worm_plot.png", p, width = 12, height = 7, dpi = 300)

#SE ribbon
ggplot(df_with_se, aes(x = time_hours, y = mean_perc_dead, color = condition, fill = condition)) +
  geom_ribbon(
    aes(ymin = mean_perc_dead - se_perc_dead, 
        ymax = mean_perc_dead + se_perc_dead),
    alpha = 0.2,
    color = NA  # Remove ribbon outline
  ) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +  # Match fill colors to line colors
  labs(
    title = "Average Percentage of Dead Worms Over Time",
    subtitle = "Shaded area represents ± 1 standard error",
    x = "Time (Hours)",
    y = "Average Percentage Dead",
    color = "Condition",
    fill = "Condition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(fill = "none") 





ggplot(df_proportions, aes(condition, perc_dead, color = condition)) +                              ##df, x, y
  geom_boxplot(alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  #geom_jitter(shape= 16, position=position_jitter(0.3), aes(color = as.factor(day)), cex = 2.8) +
  facet_grid(~time_hours)
  ylab("Number alive")
  
ggplot(df_proportions, aes(time_hours, perc_dead, colour = as.factor(condition))) +
  stat_ecdf(geom = "point")


df_propsum <- group_by(df_proportions, condition, time_hours) %>%
  summarise(percdead_sum = mean(percdead_byhour_cond))

ggplot(df_propsum, aes(condition, percdead_sum, colour = as.factor(condition))) + 
  geom_col() +
  facet_grid(~time_hours)



model <- aov(perc_dead~time_hours*condition + Error(day/time_hours), data = df_proportions)
summary(model)
TukeyHSD(model, "condition")
pairwise.t.test(df_proportions$perc_dead, df_proportions$time_hours, p.adjust.method = "bonferroni")
pairwise.t.test(df_proportions$perc_dead, df_proportions$condition, p.adjust.method = "bonferroni")

lm_model <- lm(perc_dead ~ time_hours * condition + Error(replicate/time_hours), data = df_proportions)
emm <- emmeans(model, specs = pairwise ~ time_hours | condition)
comp <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(comp)

library(pbkrtest)
library(lmerTest)
fit <- lmer(perc_dead ~ time_hours * condition + (1|replicate), data = df_proportions)
emm <- emmeans(fit, specs = pairwise ~ time_hours | condition)
comp <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(comp)

library(multcomp)
model <- lmer(perc_dead ~ condition * time_hours + (1|day), data=df_proportions)
summary(model)

# Post Hoc Analysis
posthoc <- glht(model, linfct=mcp(condition="Tukey"))
summary(posthoc)

# Model Diagnostics
plot(residuals(model))

ggplot(df_proportions, aes(x=time_hours, y=perc_dead, color=condition, group=interaction(condition,time_hours))) +
  geom_line() +
  facet_wrap(~condition)



df_24 <- filter(df_proportions, time_hours == 2 | time_hours == 4)
model <- lmer(perc_dead ~ condition * time_hours + (1|day), data=df_24)
summary(model)
# Post Hoc Analysis
posthoc <- glht(model, linfct=mcp(condition="Tukey"))
summary(posthoc)





####Paper plots####
#Percentage dead over time
png("av_percentdead_paper.png", width = 950, height = 800)

p <- ggplot(df_with_se, aes(x = x_pos, y = mean_perc_dead, color = condition, group = condition)) +
  geom_errorbar(
    aes(ymin = mean_perc_dead - se_perc_dead, 
        ymax = mean_perc_dead + se_perc_dead),
    width = 0.1,
    size = 0.3
  ) +
  geom_line(aes(linetype = condition), linewidth = 0.3) +
  geom_point(aes(shape = condition, fill = condition), size = 2, stroke = 0.1) +
  scale_x_continuous(
    breaks = unique(df_with_se$time_hours),
    labels = unique(df_with_se$time_hours)
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ".", drop0trailing = TRUE), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set2", limits = c("a","b","d","c"),
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth",
                                "c" = "Scaffold ancestry, Scaffold growth")) +
  scale_fill_manual(values = c("a" = "#66C2A5",
                                 "b" = "#FC8D62",
                                 "d" = "#8DA0CB",
                                 "c" = "#E78AC3"),
                     limits = c("a","b","d","c"),
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth",
                                "c" = "Scaffold ancestry, Scaffold growth")) +
  scale_linetype_manual(values = c("a" = "solid", "b" = "dashed", "d" = "dotted", "c" = "dotdash"),
                        limits = c("a","b","d","c"),
                        labels = c("a" = "Agar ancestry, Agar growth",
                                   "b" = "Agar ancestry, Scaffold growth",
                                   "d" = "Scaffold ancestry, Agar growth",
                                   "c" = "Scaffold ancestry, Scaffold growth")) +
  scale_shape_manual(values = c("a" = 21, "b" = 24, "d" = 23, "c" = 22),
                     limits = c("a","b","d","c"),
                     labels = c("a" = "Agar ancestry, Agar growth",
                                "b" = "Agar ancestry, Scaffold growth",
                                "d" = "Scaffold ancestry, Agar growth",
                                "c" = "Scaffold ancestry, Scaffold growth")) +
  labs(
    x = "Time (Hours)",
    y = "Percentage of dead animals",
    color = "Ancestry and Growth Condition",
    linetype = "Ancestry and Growth Condition",
    shape = "Ancestry and Growth Condition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Move legend to top left corner
    legend.box = "vertical",
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.background = element_rect(fill = "white", color = NA),  # Add a white background to the legend
    legend.key = element_rect(color = NA),  # Remove the border around legend keys
    legend.text = element_text(size = 8),  # Increased legend text size
    legend.title = element_text(size = 8, face = "plain"),  # Increased legend title size and made it bold
    legend.key.size = unit(1, "cm"),  # Increased legend key size
    #aspect.ratio = 0.9,
    #panel.grid.major.x = element_line(color = "gray90"),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 8, margin = margin(t = 2), color = "black"),
    axis.title.y = element_text(size = 8, margin = margin(r = 2), color = "black")
  ) +
  guides(
    color = guide_legend(ncol = 1, override.aes = list(linetype = line_types, shape = point_shapes)),
    linetype = "none",
    shape = "none"
  )

ggsave("C:/Users/aurel/Documents/Apples/oxidativestress/av_percentdead_paper.png", p, dpi = 300, bg = "white", width = 65, height = 50, units = "mm")





dev.off()
