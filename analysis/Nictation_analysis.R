library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(lemon) # https://cran.r-project.org/web/packages/lemon/vignettes/capped-axes.html
source("somestats.R")

##Make df with xl file##
nic_df <- readxl::read_xlsx("Nictation.xlsx")

##hand t-test##
with(nic_df, shapiro.test(Nictation[Plate == "Agar"]))         ## Non-normal, all 0
with(nic_df, shapiro.test(Nictation[Plate == "Apple"]))        ## Non-normal, p = 0.01557
var.test(Nictation ~ Plate, data = nic_df)                     ## Non-equal variance (agar = 0)
t.test(Nictation ~ Plate, data = nic_df, var.equal = FALSE)    ## t-test assuming non-equal variance     p = 0.004519
wilcox.test(Nictation ~ Plate, data = nic_df)                  ## Independent 2-group Mann-Whitnet U Test, non-normal    p-value = 6.239e-05



##Stats df
nic_df_stats <- group_by(nic_df, Plate) %>%
  summarise(mean_nic = mean(Nictation), sd_nic = sd(Nictation), se_nic = sderror(Nictation))




##Make png out of graph##
tiff("graphs/nictation.tiff", width = 375, height = 530)

##Make box plot##
p <- ggplot(nic_df, aes(Plate, Nictation, fill = Plate)) +
  geom_boxplot(aes(colour = Plate), show.legend = FALSE, width = 0.4, lwd = 1) +
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = Plate), show.legend = FALSE, cex = 2.8) +
  scale_color_manual(values= alpha(c("#000000", "#000000")) ) +       ##Outline and jitter
  scale_fill_manual(values=c("#a6a6a6", "#a6a6a6")) +                      ##Fill
  ylab("Number of nictating animals") +
  xlab("Growing condition") +
  theme_bw() +                                                         ##remove background 
  theme(axis.title.x = element_text(size=28, margin=margin(10,0,0,0), vjust = -0.8), ##adjust size and distance of x axis title from graph
        axis.title.y = element_text(size=28, margin=margin(0,18,0,0), hjust = 0.5), ##adjust size and distance of y axis title from graph
        axis.text.x  = element_text(size=25, colour = "#000000", vjust = -1),      ##adjust size and color of x axis text
        axis.text.y  = element_text(size=23, colour = "#000000"),      ##adjust size and color of y axis text
        
        plot.margin = unit(c(5,1,1,1), "lines"),                       ##Expand graph margins to leave space for outside plot elements
        
        axis.ticks.x = element_blank(),                                ##remove ticks on x axis
        panel.grid = element_blank(),                                  ##remove all grid elements
        panel.border = element_blank(),                                ##remove all boarder lines around graph
        axis.line.y = element_line(color = "black")                    ##add only boarder line on y axis
  ) +

  
  # scale_y_continuous(labels = c(0, 20, 40, 55), expand = c(0,1.5)) +
  # 
  scale_x_discrete(labels=c("Agar" = "Agar", "Apple" = "Scaffold")) +
  
  coord_cartesian(clip = 'off', ylim = c(0, 50)) +

  scale_y_continuous(breaks = seq(0, 50, 10),  expand = c(0,0)) +
  # 
  # coord_capped_cart(left = "both") +            limits = c(0,50),
  # 
  # scale_x_discrete(expand = c(0.5,0)) +


  annotate("segment", x = 1, xend = 2, y = 54, yend = 54, cex = 1) +
  annotate("segment", x = 1, xend = 1, y = 54, yend = 53, cex = 1) +
  annotate("segment", x = 2, xend = 2, y = 54, yend = 53, cex = 1) +
  annotate("text", x = 1.5, y = 55.5, label = "**", size = 10) 

p 

ggsave(p, filename = "graphs/eps_nictation.eps", device = "eps", width = 7.075, height = 10, dpi = 300) #semi-transparency not supported

dev.off()





# ggplot(nic_df, aes(Plate, Nictation)) +                              ##df, x, y
#   geom_boxplot(aes(colour = Plate), show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
#   geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = Plate), show.legend = FALSE, cex = 2.8) + ##cex = shape size
#   scale_color_manual(values=c("#003300", "#990000")) +                 ##dark green & dark red personalized colors
#   ylab("Number of nictating animals") +
#   xlab("Raising condition") +
#   theme_bw() +                                                         ##remove background 
#   theme(axis.title.x = element_text(size=28, margin=margin(11,0,0,0)), ##adjust size and distance of x axis title from graph
#         axis.title.y = element_text(size=28, margin=margin(0,18,0,0)), ##adjust size and distance of y axis title from graph
#         axis.text.x  = element_text(size=25, colour = "#000000"),      ##adjust size and color of x axis text
#         axis.text.y  = element_text(size=23, colour = "#000000"),      ##adjust size and color of y axis text
#         axis.ticks.x = element_blank(),                                ##remove ticks on x axis
#         panel.grid.minor.x = element_blank(),                          ##remove major gridlines on x axis
#         panel.grid.major.x = element_blank(),                          ##remove minor gridlines on x axis
#         panel.grid.major.y = element_line(color = "#CCCCCC", size = 1),##change color and size of major gridline on y axis
#         panel.grid.minor.y = element_line(size = 1),                   ##change size of minor gridline on y axis
#         panel.border = element_blank(),                                ##remove all boarder lines around graph
#         axis.line.y = element_line(color = "black")                    ##add only board line on y axis
#   ) +
#   scale_y_continuous(expand = c(0,1)) + scale_x_discrete(expand = c(0.5,0)) + ##minimize extra space around plot
#   scale_x_discrete(labels=c("Agar" = "Agar", "Apple" = "Scaffold")) +
#   geom_segment(aes(x = "Agar", y = 52, xend = "Apple", yend = 52)) +
#   annotate(geom = "text", x = 1.5, y = 53, label = "*", color="black", size = 6)
# # geom_text(y=30, label="Scatter plot")
# # geom_text(data = brood_df, label = "***")
# # stat_compare_means(method = "t.test")
# # geom_signif(comparisons = list(c("Agar", "Scaffold")), test = "t.test", tip_length = 0, textsize = 6, margin_top = 0.02)