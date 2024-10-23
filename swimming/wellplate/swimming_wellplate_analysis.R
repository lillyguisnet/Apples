library(ggplot2)
library(dplyr)
library(openxlsx)
library(ggh4x)

xl_file = "swimming/wellplate/swimming_wellplate_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

df_swim <- group_by(df, condition, day, replicate) %>%
  filter(dead == 0) %>%
  mutate(av_bends = mean(bends)) %>%
  mutate(state = ifelse(bends <= 2, "0", NA)) %>%
  mutate(state = ifelse(bends >= 5, "2", state)) %>%
  mutate(state = ifelse(bends < 5 & bends > 2, "1", state)) %>%
  mutate(dayreplicate = paste(day, replicate, sep = "_")) %>%
  ungroup()

write.csv(df_swim, "df_swim.csv", row.names = FALSE)

df_proportions <- group_by(df_swim, mc_concentration, condition, day, replicate) %>%
  mutate(perc_quiescence = length(state[which(state == "0")])/length(state)) %>%
  mutate(perc_slow = length(state[which(state == "1")])/length(state)) %>%
  mutate(perc_swim = length(state[which(state == "2")])/length(state)) %>%
  summarise(pqui = mean(perc_quiescence), 
            pslow = mean(perc_slow), 
            pswim = mean(perc_swim))


df_swimonly <- group_by(df_swim) %>%
  filter(state == "2", visible == 1, side == 0, dead == 0) %>%
  group_by(condition, day, replicate) %>%
  mutate(av_bends = mean(bends))

df_noquiters <- group_by(df_swim, condition, day, replicate) %>%
  mutate(dayreplicatecondition = paste( condition, dayreplicate, sep = "_")) %>%
  mutate(quiter = any(bends <= 4) ) %>%
  filter(!quiter)

model <- aov(bends~condition, df_noquiters)
plot(TukeyHSD(model, comf.level = 0.95))


df_mc5 <- group_by(df_swim) %>%
  filter(mc_concentration == 0.5) %>%
  filter(visible == 1, side == 0, dead == 0) %>%
  group_by(condition, day, replicate) %>%
  mutate(av_bends = mean(bends))

df_noquiters_mc5 <- group_by(df_mc5, condition, day, replicate) %>%
  mutate(dayreplicatecondition = paste( condition, dayreplicate, sep = "_")) %>%
  mutate(quiter = any(bends <= 4) ) %>%
  filter(!quiter)

png("avbends_mc05.png", width = 1600, height = 900)
ggplot(df_mc5, aes(condition, av_bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec (swim only)") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )
dev.off()

png("avbends_noquit_mc05.png", width = 1600, height = 900)
ggplot(df_noquiters_mc5, aes(condition, av_bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec (swim only), 0.5% MC") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )
dev.off()


df_mc1 <- group_by(df_swim) %>%
  filter(mc_concentration == 1) %>%
  filter(visible == 1, side == 0, dead == 0) %>%
  group_by(condition, day, replicate) %>%
  mutate(av_bends = mean(bends))

df_noquiters_mc1 <- group_by(df_mc1, condition, day, replicate) %>%
  mutate(dayreplicatecondition = paste( condition, dayreplicate, sep = "_")) %>%
  mutate(quiter = any(bends <= 4) ) %>%
  filter(!quiter)

model <- aov(av_bends~condition, df_noquiters_mc1)
plot(TukeyHSD(model, comf.level = 0.95))

png("avbends_mc1.png", width = 1600, height = 900)
ggplot(df_mc1, aes(condition, av_bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec (swim only)") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )
dev.off()

png("avbends_noquit_mc1.png", width = 1600, height = 900)
ggplot(df_noquiters_mc1, aes(condition, av_bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec (swim only), 1% MC") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 18)
    
  )
dev.off()

ggplot(df_swim, aes(minutes, bends)) +
  geom_point() +
  facet_grid(dayreplicate~condition)

ggplot(df_swim, aes(minutes, bends)) +
  geom_bin2d(bins=20) +
  facet_grid(~condition)


png("wellplate_avbends_swimonlyav.png", width = 1600, height = 900)

ggplot(df_swimonly, aes(condition, av_bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec (swim only)")

dev.off()

png("wellplate_avbends_swimonlyallworms.png", width = 1600, height = 900)

ggplot(df_swimonly, aes(condition, bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec (swim only)")

dev.off()



png("wellplate_avswim.png", width = 1600, height = 900)

ggplot(df_proportions, aes(condition, pswim)) +
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE) +
  ylab("Percent pswim")

dev.off()


png("wellplate_avqui.png", width = 1600, height = 900)

ggplot(df_proportions, aes(condition, pqui)) +
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE) +
  ylab("Percent quienscent")

dev.off()



ggplot(df_proportions, aes(condition, tperc_swim)) +
  geom_col(aes(colour = as.factor(condition)))

ggplot(df_proportions, aes(condition, tperc_slow)) +
  geom_col(aes(colour = as.factor(condition)))

ggplot(df_proportions, aes(condition, tperc_quiescence)) +
  geom_col(aes(colour = as.factor(condition)))

mutate(tperc_quiescence = length(state[which(state == "0")])/length(state)/length(state)) %>%
  mutate(tperc_slow = length(state[which(state == "1")])/length(state)/length(state)) %>%  
  mutate(tperc_swim = length(state[which(state == "2")])/length(state)/length(state)) %>%

ggplot(df_swim, aes(length(condition))) +   
  geom_histogram(aes(fill = as.factor(state)), binwidth = 1) +
  facet_grid(~condition)



png("wellplate_bendshist.png", width = 1600, height = 900)

ggplot(df_swim, aes(bends)) +   
  geom_histogram(binwidth = 1) +
  # geom_histogram(aes(fill = as.factor(condition)), binwidth = 1) +
  #facet_grid(~condition)
  ylab("count")

dev.off()



png("wellplate_avbends.png", width = 1600, height = 900)

ggplot(df_swim, aes(condition, av_bends)) +                              ##df, x, y
  geom_boxplot(aes(colour = as.factor(condition)), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = as.factor(condition)), show.legend = FALSE, cex = 2.8) +
  ylab("Average bends per 5sec") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    #strip.text.x = element_text(size = 18)
    
  )

dev.off()





######Plots for paper######

# Create a histogram of bends by condition
df_swim <- df_swim %>%
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


png("swimming/wellplate/swimming_wellplate_bends_histogram_by_condition_paper.png", width = 1200, height = 800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_swim %>% filter(mc_concentration == 0), aes(x = bends, fill = as.factor(growing))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.6, color = "black") +
  facet_nested(
    ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    x = "Number of bends per 5 seconds in 0% MC",
    y = "Count"
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_continuous(breaks = seq(0, max(df_swim$bends[df_swim$mc_concentration == 0]), by = 5)) +
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
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_line(color = "gray95")
  )

dev.off()



#Percent of time in quiescence
df_proportions <- df_proportions %>%
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

# Filter for mc_concentration = 0
df_proportions_filtered <- df_proportions %>%
  filter(mc_concentration == 0)

png("swimming/wellplate/swimming_wellplate_percent_quiescence_by_condition.png", width = 750, height = 1100)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_proportions_filtered, aes(x = growing, y = pqui, fill = as.factor(growing))) +
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
    y = "Percent of time in quiescence",
    x = "Growing condition",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ".", drop0trailing = TRUE), limits = c(0, 1)) +
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



#Percent of time in slow
png("swimming/wellplate/swimming_wellplate_percent_slow_by_condition.png", width = 750, height = 1100)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_proportions_filtered, aes(x = growing, y = pslow, fill = as.factor(growing))) +
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
    y = "Percent of time in quiescence",
    x = "Growing condition",
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = c("agar" = "Agar", "scaffold" = "Scaffold")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ".", drop0trailing = TRUE), limits = c(0, 1)) +
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





#Swimming by condition and viscosity
df_noquiters <- df_noquiters %>%
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




# Prepare df_swim for all concentrations
df_swim <- df_swim %>%
  filter(visible == 1, side == 0, dead == 0) %>%
  group_by(condition, day, replicate, mc_concentration) %>%
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
  ungroup()

# Create df_noquiters for all concentrations and calculate average bends
df_noquiters <- df_swim %>%
  group_by(condition, day, replicate, mc_concentration) %>%
  mutate(dayreplicatecondition = paste(condition, day, replicate, sep = "_")) %>%
  mutate(quiter = any(bends <= 4)) %>%
  filter(!quiter) %>%
  summarise(av_bends = mean(bends)) %>%
  ungroup()


png("swimming/wellplate/swimming_wellplate_bends_by_condition_viscosity_paper.png", width = 1200, height = 800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette

ggplot(df_noquiters, aes(x = as.factor(mc_concentration), y = av_bends, fill = as.factor(growing))) +
  geom_boxplot(alpha = 0.6, color = "black") +
  facet_nested(
    ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    x = "Methylcellulose concentration (%)",
    y = "Average number of bends during swimming periods"
  ) +
  scale_fill_manual(values = c("agar" = agar_color, "scaffold" = scaffold_color)) +
  scale_x_discrete(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
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
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank()
  )

dev.off()




#Heatmap
png("swimming/wellplate/swimming_wellplate_heatmap.png", width = 1600, height = 800)

agar_color <- "#66C2A5"      # Light green from Set2 palette
scaffold_color <- "#FC8D62"  # Light orange from Set2 palette


ggplot(df_swim %>% filter(mc_concentration == 0), aes(minutes, bends)) +
  geom_bin2d(bins = 20) +
  facet_nested(
    ~ ancestry + growing,
    scales = "fixed",
    labeller = labeller(
      ancestry = c("agar" = "Agar ancestry", "scaffold" = "Scaffold ancestry"),
      growing = c("agar" = "Agar growth", "scaffold" = "Scaffold growth")
    ),
    strip = strip_nested(
      background_x = list(
        element_rect(fill = agar_color, color = "white", linewidth = 1.5),
        element_rect(fill = scaffold_color, color = "white", linewidth = 1.5)
      ),
      text_x = list(
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 8)),
        element_text(color = "#000000", size = 16, face = "plain", margin = margin(t = 5, b = 5))
      )
    )
  ) +
  labs(
    x = "Time (minutes)",
    y = "Number of bends per 5 seconds"
  ) +
  scale_fill_gradient() +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 22, face = "plain", margin = margin(t = 22, b = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold"),
    strip.text.x = element_text(margin = margin(t = 5, b = 5)),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank()
  )

dev.off()
