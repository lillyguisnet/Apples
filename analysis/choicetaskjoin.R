library(readxl)
library(dplyr)
library(ggplot2)
source("somestats.R")
library(rstatix)
# library(ggpubr)
# library(lemon)

### Prepare data ####

poor_df <- readxl::read_xlsx("ChoiceAssayPoor.xlsx")
equal_df <- readxl::read_xlsx("ChoiceAssay2017.xlsx")
all_df <- readxl::read_xlsx("choiceassayall.xlsx")

poor_df_tobind <- select(poor_df, Worm, loc, perc) %>%
  mutate(valence = "poor")

equal_df_tobind <- select(equal_df, Worm, loc, perc) %>%
  mutate(valence = "equal")

choice_df <- bind_rows(poor_df_tobind, equal_df_tobind) %>%
  mutate(wormvalence = paste(valence, Worm, sep = ""))

choice_df_errorbar <- group_by(choice_df, wormvalence, loc) %>%      ## df_stats
  mutate(sd_wv = sd(perc),
         se_wv = sderror(perc)
  ) %>%
  summarise(perc = mean(perc), sd_wv = mean(sd_wv), se_wv = mean(se_wv))

choice_df_diff <- group_by(choice_df, valence, Worm, loc) %>%
  summarise(mean_perc = mean(perc)) %>%
  group_by(valence, loc) %>%
  summarise(difference =  mean_perc[which(Worm == "Apple")] - mean_perc[which(Worm == "Plate")] ) %>%
  mutate(valenceloc = paste(valence, loc, sep = ""))

choice_index <- group_by(all_df, worm, valence, replicate) %>%
  mutate(allworms = sum(individuals)) %>%
  mutate(total = allworms - individuals[which(loc == "dead")]) %>%
  mutate(prefindex = (individuals[which(loc == "apple")] - individuals[which(loc == "lawn")]) / (total - individuals[which(loc == "agar")] ) ) %>%
  mutate(wormvalence = paste(valence, worm, sep = "")) %>%
  group_by(wormvalence) %>%
  mutate(av_index = mean(prefindex) )

index_errorbar <- group_by(choice_index, wormvalence) %>%
  mutate(sd_idx = sd(prefindex),
         se_idx = sderror(prefindex) ) %>%
  summarise( index = mean(prefindex), sd_idx = mean(sd_idx), se_idx = mean(se_idx)  )


### Stats ####

model  <- lm(perc ~ Worm*loc*valence, data = choice_df)       ## Assumptions
ggqqplot(residuals(model))        ## Normal
shapiro_test(residuals(model))    ## Normal
ggqqplot(choice_df, "perc", ggtheme = theme_bw()) +
  facet_grid(Worm + valence ~ loc, labeller = "label_both")   ## Normal
choice_df %>% levene_test(perc ~ Worm*loc*valence)    ## Equal variance



anova <- choice_df %>% anova_test(perc ~ Worm*loc*valence)  ## Three-way anova
lm(perc ~ Worm*loc*valence, data = choice_df) %>% tukey_hsd() %>% View()

anova <- choice_df %>% anova_test(perc ~ loc*valence)       ## Two-way anova loc*valence
lm(perc ~ loc*valence, data = choice_df) %>% tukey_hsd()

anova <- choice_df %>% anova_test(perc ~ loc*Worm)       ## Two-way anova loc*Worm
lm(perc ~ loc*Worm, data = choice_df) %>% tukey_hsd()
summary(lm(perc ~ loc*Worm, data = choice_df))

anova <- choice_index %>% anova_test(prefindex ~ worm*valence)       ## Two-way anova loc*valence
lm(prefindex ~ worm*valence, data = choice_index) %>% tukey_hsd()




### Make graphs ####



#### All comparisons ####

png("graphs/choice_colorblind.png", width = 1200, height = 800)

p <- ggplot(choice_df, aes(wormvalence, perc) ) +

  geom_bar(width = 0.8, stat = "summary", position = position_dodge(width = 0.9), aes(fill = loc, colour = loc), lwd = 1 ) +
  
  geom_errorbar(data = choice_df_errorbar, aes(x = wormvalence, ymin = perc-se_wv, ymax = perc+se_wv, colour = loc), position = position_dodge(width = 0.9), show.legend = FALSE, width = 0.2, size = 1, alpha = 0.6) +
  
  geom_jitter(shape= 16, position=position_dodge(width = 0.9), aes(color = loc), show.legend = FALSE, cex = 2.8) + ##cex = shape size
  
  
  scale_fill_manual(
    name = "Location",
    labels = c("Outside", "Scaffold patch", "Agar patch"), 
    values = alpha(c("#0072B2", "#D55E00", "#CC79A7"), 0.1)  #Color-blind palette vs ("#003399", "#990000", "#003300")
    ) +
  
  scale_color_manual( values = alpha(c("#0072B2", "#D55E00", "#CC79A7"), 0.6) ) +
  
  guides(colour = FALSE) + ## remove legend for colour aes
  
  ylab("% of individuals by location") +
  labs(color = "Location") +
  
  theme_bw() +
  theme(
    
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 28, colour = "#000000", vjust = 5),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),  
    panel.border = element_blank(),
    plot.margin = unit(c(5,0,8,8), "lines"),
    axis.text.x  = element_text(size=22, colour = "#000000", vjust = -4),
    axis.text.y  = element_text(size=23, colour = "#000000"),
    axis.line.y = element_line(color = "black", size = 1),
    
    legend.position = c(0.8, 0.85),
    legend.title = element_text(colour = "#000000", size = 22),
    legend.direction = "vertical",
    legend.text = element_text(colour = "#000000", size = 16),
    legend.key.size = unit(2.2,"line")
    
  ) +
  
  
  coord_cartesian(clip = 'off', ylim = c(0,100), xlim = c(1,4) ) +
  scale_y_continuous(breaks = seq(0, 100, 10),  expand = c(0,0)) +
  
  # 
  # annotate("segment", x = 0.7, xend = 1.3, y = -4, yend = -4, cex = 1) +       ## Scaffold Equal
  # annotate("segment", x = 0.7, xend = 0.7, y = -4, yend = -3, cex = 1) +
  # annotate("segment", x = 1.3, xend = 1.3, y = -4, yend = -3, cex = 1) +
  # 
  # annotate("segment", x = 1.7, xend = 2.3, y = -4, yend = -4, cex = 1) +       ## Agar Equal
  # annotate("segment", x = 1.7, xend = 1.7, y = -4, yend = -3, cex = 1) +
  # annotate("segment", x = 2.3, xend = 2.3, y = -4, yend = -3, cex = 1) +
  # 
  # annotate("segment", x = 2.7, xend = 3.3, y = -4, yend = -4, cex = 1) +       ## Scaffold Poor
  # annotate("segment", x = 2.7, xend = 2.7, y = -4, yend = -3, cex = 1) +
  # annotate("segment", x = 3.3, xend = 3.3, y = -4, yend = -3, cex = 1) +
  # 
  # annotate("segment", x = 3.7, xend = 4.3, y = -4, yend = -4, cex = 1) +       ## Scaffold Poor
  # annotate("segment", x = 3.7, xend = 3.7, y = -4, yend = -3, cex = 1) +
  # annotate("segment", x = 4.3, xend = 4.3, y = -4, yend = -3, cex = 1) +
  # 
  annotate("segment", x = 0.6, xend = 2.4, y = -14, yend = -14, cex = 1) +     ## Equal
  # annotate("segment", x = 0.6, xend = 0.6, y = -14, yend = -13, cex = 1) +
  # annotate("segment", x = 2.4, xend = 2.4, y = -14, yend = -13, cex = 1) +
  annotate("text", label = "1:1", x = 1.5, y = -18, size = 8) +
  
  annotate("segment", x = 2.6, xend = 4.4, y = -14, yend = -14, cex = 1) +     ## Poor Scaffold
  # annotate("segment", x = 2.6, xend = 2.6, y = -14, yend = -13, cex = 1) +
  # annotate("segment", x = 4.4, xend = 4.4, y = -14, yend = -13, cex = 1) +
  annotate("text", label = "3:1", x = 3.5, y = -18, size = 8) +
  
  annotate("text", label = "Growing condition", x = 0.1, y = -6, size = 7) +
  annotate("text", label = "Patch valence", x = 0.1, y = -15, size = 7) +
  
  # annotate("segment", x = -5, xend = -1, y = -10, yend = -10, cex = 1) +
  
  # annotate("text", label = "*", colour = "red", x = 1, y = 95, size = 10) +
  # annotate("text", label = "*", colour = "red", x = 2, y = 96, size = 10) +
  
  # annotate("segment", x = 0.7, xend = 1.3, y = 100, yend = 100, cex = 1) +
  # annotate("segment", x = 0.7, xend = 0.7, y = 100, yend = 98, cex = 1) +
  # annotate("segment", x = 1.3, xend = 1.3, y = 100, yend = 98, cex = 1) +
  # annotate("text", label = "*", x = 1, y = 102, size = 10) +
  # 
  # annotate("segment", x = 1, xend = 2, y = 110, yend = 110, cex = 1) +
  # annotate("segment", x = 1, xend = 1, y = 110, yend = 108, cex = 1) +
  # annotate("segment", x = 2, xend = 2, y = 110, yend = 108, cex = 1) +
  # annotate("text", label = "ns", x = 1.5, y = 113, size = 6) +
  
  
  scale_x_discrete(expand = c(0.2, 0),                ## Keep last
                   labels = c(
                     "equalApple" = "Scaffold",
                     "equalPlate" = "Agar",
                     "poorApple" = "Scaffold",
                     "poorPlate" = "Agar"
                   )
                   
  )
  

p

ggsave(p, filename = "graphs/eps_choice_colorblind.eps", device = "eps", width = 15, height = 10, dpi = 300)

dev.off()

#### Preference Index ####

# alpha_steps <- seq(from = -0.98, to = 0.99, by = 0.04191489)
# 
# alpha_df <- data.frame(x = alpha_steps, y = alpha_steps)


tiff("graphs/prefindex_review.tiff", width = 5000, height = 4000, res = 300)

p <- ggplot(index_errorbar, aes(x = wormvalence, y = index) ) +
  
  geom_bar(aes(fill = index, color = index), width = 0.6, stat = "identity", position = position_dodge(width = 0.9), lwd = 1, show.legend = FALSE ) +
  
 # geom_jitter(aes(x = choice_index$wormvalence, y = choice_index$prefindex, color = choice_index$prefindex), position = position_dodge(width = 0), shape= 16, show.legend = FALSE, cex = 2.8) + ##cex = shape size
  
  #geom_tile(aes(x = -0.08, width = 0.55, y = alpha_df$y, fill = alpha_df$y), show.legend = FALSE) +
  #geom_rect( aes(xmin = -0.2, xmax = 0.2, ymin = -1, ymax = 1, fill = alpha_df$y) ) +
  
  geom_errorbar(aes(x = wormvalence, ymin = index-se_idx, ymax = index+se_idx, color = index), width = 0.18, size = 1, show.legend = FALSE) +
  
  scale_color_gradientn(colors=alpha(c("#000000", "#000000"))) + #for jitter and border
  
  scale_fill_gradientn(colors=alpha(c("#a6a6a6", "#a6a6a6"))) + #for inside and tile
  
  #scale_fill_gradient2(low = '#003300', mid = "grey", high = '#990000', alpha = 0.1) +
  
  # scale_fill_manual(
  #   name = "Location",
  #   labels = c("Outside", "Scaffold", "Lawn"), 
  #   values = alpha(c("#003300", "#990000", "#003399"), 0.1)
  # ) +
  
  #scale_color_manual( alpha = 0.6 ) +
  
  # guides(colour = FALSE) + ## remove legend for colour aes
  # 
  ylab("Occupancy index") +
  # labs(color = "Location") +
  
  theme_bw() +
  theme(
    
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 45, colour = "#000000", vjust = 3),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),  
    panel.border = element_blank(),
    plot.margin = unit(c(5,4,10,17), "lines"),
    axis.text.x  = element_text(size=40, colour = "#000000", vjust = -4),
    axis.text.y  = element_text(size=40, colour = "#000000"),
    axis.line.y = element_line(color = "black", size = 1),
    
    legend.position = c(0.75, 0.85),
    legend.title = element_text(colour = "#000000", size = 20),
    legend.direction = "vertical",
    legend.text = element_text(colour = "#000000", size = 16)
    
  ) +
  

  coord_cartesian(clip = 'off', ylim = c(-0.2, 1), xlim = c(1,4) ) +
  scale_y_continuous(breaks = seq(-0.2, 1, 0.2),  expand = c(0,0)) +
  
  annotate("segment", x = 0.4, xend = 4.7, y = 0, yend = 0, cex = 1) +
  
 # annotate("text", label = "Scaffold lawn", x = -1.1, y = 1, size = 8) +
 # annotate("text", label = "Agar lawn", x = -1.1, y = -0.98, size = 8) +
  annotate("text", label = "Growing condition", x = -0.5, y = -0.3, size = 15) +
  annotate("text", label = "Patch valence", x = -0.3, y = -0.4, size = 15) +
  
 # annotate("rect", xmin = -0.28, xmax = 0.2, ymin = -1, ymax = -0.01, fill = "blue", alpha = alpha_steps) +
 # annotate("rect", xmin = -0.28, xmax = 0.2, ymin = 0.01, ymax = 1, fill = "red", alpha = alpha_steps) +
  
  # annotate("segment", x = 0.7, xend = 1.3, y = -0.28, yend = -0.28, cex = 1) +       ## Scaffold Equal
  # annotate("segment", x = 0.7, xend = 0.7, y = -0.28, yend = -0.26, cex = 1) +
  # annotate("segment", x = 1.3, xend = 1.3, y = -0.28, yend = -0.26, cex = 1) +
  # 
  # annotate("segment", x = 1.7, xend = 2.3, y = -0.28, yend = -0.28, cex = 1) +       ## Agar Equal
  # annotate("segment", x = 1.7, xend = 1.7, y = -0.28, yend = -0.26, cex = 1) +
  # annotate("segment", x = 2.3, xend = 2.3, y = -0.28, yend = -0.26, cex = 1) +
  # 
  # annotate("segment", x = 2.7, xend = 3.3, y = -0.28, yend = -0.28, cex = 1) +       ## Scaffold Poor
  # annotate("segment", x = 2.7, xend = 2.7, y = -0.28, yend = -0.26, cex = 1) +
  # annotate("segment", x = 3.3, xend = 3.3, y = -0.28, yend = -0.26, cex = 1) +
  # 
  # annotate("segment", x = 3.7, xend = 4.3, y = -0.28, yend = -0.28, cex = 1) +       ## Scaffold Poor
  # annotate("segment", x = 3.7, xend = 3.7, y = -0.28, yend = -0.26, cex = 1) +
  # annotate("segment", x = 4.3, xend = 4.3, y = -0.28, yend = -0.26, cex = 1) +
  
  annotate("segment", x = 0.6, xend = 2.4, y = -0.4, yend = -0.4, cex = 1) +     ## Equal
  # annotate("segment", x = 0.6, xend = 0.6, y = -0.38, yend = -0.37, cex = 1) +
  # annotate("segment", x = 2.4, xend = 2.4, y = -0.38, yend = -0.37, cex = 1) +
  annotate("text", label = "1:1", x = 1.5, y = -0.47, size = 15) +
  
  annotate("segment", x = 2.6, xend = 4.4, y = -0.4, yend = -0.4, cex = 1) +     ## Poor Scaffold
  # annotate("segment", x = 2.6, xend = 2.6, y = -0.38, yend = -0.37, cex = 1) +
  # annotate("segment", x = 4.4, xend = 4.4, y = -0.38, yend = -0.37, cex = 1) +
  annotate("text", label = "3:1", x = 3.5, y = -0.47, size = 15) +
  
  annotate("segment", x = 1, xend = 1, y = 0.98, yend = 1, cex = 1) +   #Stats
  annotate("segment", x = 2, xend = 2, y = 0.98, yend = 1, cex = 1) +
  annotate("segment", x = 3, xend = 3, y = 0.98, yend = 1, cex = 1) +
  annotate("segment", x = 4, xend = 4, y = 0.98, yend = 1, cex = 1) +
  
  annotate("segment", x = 1, xend = 2, y = 1, yend = 1, cex = 1) +
  annotate("segment", x = 3, xend = 4, y = 1, yend = 1, cex = 1) +
  
  annotate("segment", x = 1.5, xend = 1.5, y = 1, yend = 1.03, cex = 1) +
  annotate("segment", x = 3.5, xend = 3.5, y = 1, yend = 1.03, cex = 1) +
  
  annotate("segment", x = 1.5, xend = 3.5, y = 1.03, yend = 1.03, cex = 1) +
  
  annotate("text", label = "***", x = 2.5, y = 1.05, size = 15) +
  
  
  scale_x_discrete(expand = c(0.2, 0),                ## Keep last
                   labels = c(
                     "equalApple" = "Scaffold",
                     "equalPlate" = "Agar",
                     "poorApple" = "Scaffold",
                     "poorPlate" = "Agar"
                   )
                   
  )


p

ggsave(p, filename = "graphs/eps_prefindex.eps", device = "eps", width = 12.5, height = 10, dpi = 300) #semi-transparency not supported

dev.off()





### History difference ####

rline <- lm(difference ~ valenceloc, choice_df_diff)
model <- lm(perc ~ wormvalence, choice_df)

png("graphs/diffchoice.png", width = 800, height = 600)

p <- ggplot(choice_df_diff, aes(valenceloc, difference) ) +
  
  # geom_bar(stat = "summary", position = position_dodge(width = 0.9), aes(color = loc)) +
  geom_point(aes(color = loc)) +
  geom_smooth(method = "lm") +
  # stat_smooth(method = "lm") +
  # geom_line(aes(rline)) +
  
  ylab("Apple - Plate % difference") +
  scale_y_continuous(limits = c(-25,25))

p

dev.off()



### COMPOST ####

# facet_grid(. ~ valence) +
# panel.spacing = unit(2, "lines"), (theme)
# legend.spacing = unit(50, "lines")
# legend.key.size = unit(2, 'lines'),
# legend.key = element_rect(size =45)
# legend.text = element_text(margin = margin(r = 10, unit = "pt"))
# coord_flex_cart(bottom = brackets_horisontal(length = 0.2, tick.length = 0.25)) +

valence_annotation_df <- data.frame( # Different annotation for facets
  x1 = c(0.7),
  y1 = c(-14),
  x2 = c(2.3),
  y2 = c(-13),
  xlabel = c(1.5),
  ylabel = c(-16),
  text_label = c("Equal", "Poor"),
  facet_id = c("equal", "poor")
)
# geom_text(data = valence_annotation_df, aes(x = xlabel, y = ylabel, label = text_label)) +
# geom_segment(data = valence_annotation_df, aes( x = x1, xend = x2, y = y1, yend = y1  )) +

# choice_df_stats <- choice_df %>%
#   group_by(valence, Worm, loc) %>%
#   get_summary_stats(perc, type = "common") %>%
#   mutate(wormvalence = paste(valence, Worm, sep = ""))

