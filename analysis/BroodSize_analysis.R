library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsignif) #geom_signif
library(lemon) # https://cran.r-project.org/web/packages/lemon/vignettes/capped-axes.html
source("somestats.R")

##Make df with xl file##
brood_df <- readxl::read_xlsx("BroodSize.xlsx")


##Make png out of graph##
png("graphs/broodsize.png", width = 350, height = 560)

##Make box plot##
p <- ggplot(brood_df, aes(Plate, Eggs, fill = Plate)) +                              ##df, x, y
  geom_boxplot(aes(colour = Plate), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = Plate), show.legend = FALSE, cex = 2.8) + ##cex = shape size
  scale_color_manual(values= alpha(c("#003300", "#990000"), 0.6) ) +                 ##dark green & dark red personalized colors
  scale_fill_manual(values=c("#003300", "#990000")) +
  ylab("Number of eggs laid on day 1") +
  xlab("Raising condition") +
  theme_bw() +                                                         ##remove background 
  theme(axis.title.x = element_text(size=28, margin=margin(10,0,0,0), vjust = -0.2), ##adjust size and distance of x axis title from graph
        axis.title.y = element_text(size=28, margin=margin(0,18,0,0), hjust = -0.1), ##adjust size and distance of y axis title from graph
        axis.text.x  = element_text(size=25, colour = "#000000"),      ##adjust size and color of x axis text
        axis.text.y  = element_text(size=23, colour = "#000000"),      ##adjust size and color of y axis text
        
        plot.margin = unit(c(5,1,1,1), "lines"),                       ##Expand graph margins to leave space for outside plot elements
        
        axis.ticks.x = element_blank(),                                ##remove ticks on x axis
        # panel.grid.minor.x = element_blank(),                          ##remove major gridlines on x axis
        # panel.grid.major.x = element_blank(),                          ##remove minor gridlines on x axis
        # panel.grid.major.y = element_line(color = "#CCCCCC", size = 1),##Change color and size of major gridline on y axis
        # panel.grid.minor.y = element_line(size = 1),                   ##Change size of minor gridline on y axis
        panel.grid = element_blank(),
        panel.border = element_blank(),                                ##remove all boarder lines around graph
        axis.line.y = element_line(color = "black")                    ##add only boarder line on y axis
  ) +
  
  annotate("segment", x = 1, xend = 2, y = 43, yend = 43, cex = 1) +
  annotate("segment", x = 1, xend = 1, y = 43, yend = 42, cex = 1) +
  annotate("segment", x = 2, xend = 2, y = 43, yend = 42, cex = 1) +
  annotate("text", x = 1.5, y = 45, label = "*", size = 10) +
  
  coord_cartesian(clip = 'off') +                     ##Specify y-axis limits, allow elements to be unattached to plot elements
 
  coord_capped_cart(left = "both") +
  
  scale_y_continuous(expand = c(0,1.5)) +
  scale_x_discrete(expand = c(0.5,0))  ##minimize extra space around plot
  # stat_compare_means() 
  # geom_signif(comparisons = list(c("Agar", "Scaffold")), test = "t.test", tip_length = 0, map_signif_level = c("*" = 0.045), textsize = 6, margin_top = 0.02)

p 

dev.off()

##hand t-test##
with(brood_df, shapiro.test(Eggs[Plate == "Agar"]))         ##Normal
with(brood_df, shapiro.test(Eggs[Plate == "Scaffold"]))     #Normal
var.test(Eggs ~ Plate, data = brood_df)                     ##Equal vairance
t.test(Eggs ~ Plate, data = brood_df, var.equal = TRUE)     ##t-test assuming equal variance, unpaired (default)    ##0.03875

## Standard errors

seAgar <- sderror(brood_df$Eggs[which(brood_df$Plate == "Agar")])
seScf <- sderror(brood_df$Eggs[which(brood_df$Plate == "Scaffold")])

meanAgar <- mean(brood_df$Eggs[which(brood_df$Plate == "Agar")])
meanScf <- mean(brood_df$Eggs[which(brood_df$Plate == "Scaffold")])


brood_df_stats <- group_by(brood_df, Plate) %>%
  mutate(mean_eggs = mean(Eggs),
         sd_eggs = sd(Eggs),
         se_eggs = sderror(Eggs)
         ) %>%
  summarise(Eggs = mean(Eggs), mean_eggs = mean(mean_eggs), sd_eggs = mean(sd_eggs), se_eggs = mean(se_eggs))





### Bar plot

png("graphs/barbroodsize.png", width = 350, height = 560)

##Make box plot##
q <- ggplot(brood_df, aes(Plate, Eggs, fill = Plate)) +                              ##df, x, y
  
  geom_bar(stat = "summary", aes(colour = Plate, alpha = 0.2), alpha = 0.1, fill = c("#003300", "#990000"), show.legend = FALSE, width = 0.4, lwd = 1, fill = "None") +  ##width = of boxes, lwd = line width of boxes
  
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = Plate), show.legend = FALSE, cex = 2.8) +  ##cex = shape size
  
  geom_errorbar(data = brood_df_stats, aes(x = Plate, ymin = mean_eggs-se_eggs, ymax = mean_eggs+se_eggs, colour = Plate), show.legend = FALSE, width = 0.2, size = 1, alpha = 0.6) +
  
  scale_color_manual(values= alpha(c("#003300", "#990000"), 0.6) ) +                 ##dark green & dark red personalized colors
  
  ylab("Number of eggs laid on day 1") +
  xlab("Raising condition") +
  theme_bw() +                                                         ##remove background
  theme(axis.title.x = element_text(size=28, margin=margin(10,0,0,0), vjust = -0.2), ##adjust size and distance of x axis title from graph
        axis.title.y = element_text(size=28, margin=margin(0,18,0,0), hjust = -0.1), ##adjust size and distance of y axis title from graph
        axis.text.x  = element_text(size=25, colour = "#000000"),      ##adjust size and color of x axis text
        axis.text.y  = element_text(size=23, colour = "#000000"),      ##adjust size and color of y axis text

        plot.margin = unit(c(5,1,1,1), "lines"),                       ##Expand graph margins to leave space for outside plot elements TOP RIGHT BOTTOM LEFT

        axis.ticks.x = element_blank(),                                ##remove ticks on x axis
        # panel.grid.minor.x = element_blank(),                          ##remove major gridlines on x axis
        # panel.grid.major.x = element_blank(),                          ##remove minor gridlines on x axis
        # panel.grid.major.y = element_line(color = "#CCCCCC", size = 1),##Change color and size of major gridline on y axis
        # panel.grid.minor.y = element_line(size = 1),                   ##Change size of minor gridline on y axis
        panel.grid = element_blank(),
        panel.border = element_blank(),                                ##remove all boarder lines around graph
        axis.line.y = element_line(color = "black")                    ##add only boarder line on y axis
  ) +

  annotate("segment", x = 1, xend = 2, y = 43, yend = 43, cex = 1) +
  annotate("segment", x = 1, xend = 1, y = 43, yend = 42, cex = 1) +
  annotate("segment", x = 2, xend = 2, y = 43, yend = 42, cex = 1) +
  annotate("text", x = 1.5, y = 45, label = "*", size = 10) +
  

  coord_cartesian(ylim = c(0, 40), clip = 'off') +                     ##Specify y-axis limits, allow elements to be unattached to plot elements

  coord_capped_cart(left = "both") +

  scale_y_continuous(expand = c(0,1.5)) +
  scale_x_discrete(expand = c(0.5,0))  ##minimize extra space around plot

q 

dev.off()










##COMPOST##
# stat_compare_means(method = "t.test") 
# panel.background = element_blank(), ##remove entire background
# axis.line.y = element_line(colour = "black"),
# axis.title.y = element_text(family = "sans", size = 15, margin=margin(0,30,0,0)), ##margin movement TOP(x) RIGHT BOTTOM LEFT
# , margin = margin(6, 6, 6, 6)
# axis.title.y = element_text(color="cadetblue" , vjust= 0.35),
# geom_point(size = 1.4) 
# scale_y_continuous(breaks = c(0, 10, 20, 30, 40)) +
# ylim (0, 42) 
# geom_text(label = brood_df, family = "Times New Roman") +
# geom_text(brood_df, aes(Plate, Eggs, label = rownames(brood_df))) +
# coord_cartesian(ylim=c(0, 40))