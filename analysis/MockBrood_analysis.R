library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsignif) #geom_signif

##Make df with xl file##
eggs_df <- readxl::read_xlsx("C:/Users/User/Desktop/Winter19/Apples/MockBrood.xlsx")

##Make png out of graph##
png(filename = "mockbrood.png", width = 25, height = 18, units = "cm", res = 500)

##Make box plot##
p <- ggplot(eggs_df, aes(x = Day, y = Eggs, fill = Plate)) +           ##df, x, y
  geom_boxplot(position=position_dodge(0.65), aes(colour = Plate), show.legend = TRUE, width = 0.55, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitterdodge(jitter.width = 0.018, dodge.width = 0.65), aes(color = Plate), show.legend = TRUE, cex = 2.8) + ##cex = shape size
  scale_color_manual(values=c("#003300", "#990000")) +                 ##dark green & dark red personalized colors
  scale_fill_manual(values = c("white", "white")) +
  ylab("Number of eggs laid") +
  xlab("Day") +
  theme_bw() +                                                         ##remove background 
  theme(axis.title.x = element_text(size=28, margin=margin(10,0,0,0)), ##adjust size and distance of x axis title from graph
        axis.title.y = element_text(size=28, margin=margin(0,18,0,0)), ##adjust size and distance of y axis title from graph
        axis.text.x  = element_text(size=25, colour = "#000000"),      ##adjust size and color of x axis text
        axis.text.y  = element_text(size=23, colour = "#000000"),      ##adjust size and color of y axis text
        axis.ticks.x = element_blank(),                                ##remove ticks on x axis
        panel.grid.minor.x = element_blank(),                          ##remove major gridlines on x axis
        panel.grid.major.x = element_blank(),                          ##remove minor gridlines on x axis
        panel.grid.major.y = element_line(color = "#CCCCCC", size = 1),##Change color and size of major gridline on y axis
        panel.grid.minor.y = element_line(size = 1),                   ##Change size of minor gridline on y axis
        panel.border = element_blank(),                                ##remove all boarder lines around graph
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 19),
        legend.key.size = unit(1.1, "cm"),
        legend.position = c(.5, .6),
        axis.line.y = element_line(color = "black")                    ##add only board line on y axis
  ) 
  # scale_y_continuous(expand = c(0,1.5)) + scale_x_discrete(expand = c(0.5,0)) + ##minimize extra space around plot
  # geom_signif(comparisons = list(c("Agar", "Scaffold")), test = "t.test", tip_length = 0, map_signif_level = c("*" = 0.045), textsize = 6, margin_top = 0.02)

p 

dev.off()
