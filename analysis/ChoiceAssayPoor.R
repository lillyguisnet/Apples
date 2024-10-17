library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plyr)

##Index:
###atlocation/(Total-dead)*100

##Make df with xl file##
poor_df <- readxl::read_xlsx("ChoiceAssayPoor.xlsx")
##Group df by Plate
# dplyr::group_by(brood_df, Plate)

##Calculate preferances for each substrate##
# dplyr::group_by(choice_df, Worm)
# dplyr::mutate(perc_lawn = onlawn/Total*100) ##done in excel

# Transform to verctors
df$Worm <- as.factor(df$Worm)
df$loc <- as.factor(df$loc)

#Look at it in terminal
head(df)

# Put the data into shape
df1 <- poor_df %>%
  mutate(both = as.factor(paste0(Worm, " & ",loc))) %>%
  group_by(both) %>%
  dplyr::summarise(mean = mean(perc), 
                   sd = sd(perc), 
                   n = dplyr::n(), 
                   worm = Worm[1], 
                   loc =  loc[1]) %>%
  mutate(se = sd / sqrt(n),
         lb = mean - se,
         hb = mean + se,
         ic=se * qt((1-0.05)/2 + .5, n-1),
         lic = mean - ic,
         hic = mean + ic)

# Look at it in the terminal
head(df1)

##Make png out of graph##
png("graphs/choiceassaypoor.png", width = 15.5, height = 18, units = "cm", res = 500)

#Make the plot
p <- ggplot(df1, aes(x = both, y = mean, ymin = lb, ymax = hb, color = loc)) + 
  geom_bar(stat = "identity", fill = "white", size = 0.8, show.legend = FALSE) + 
  geom_errorbar(size = 0.8, width = 0.45, show.legend = FALSE) + 
  scale_color_manual(name = "Location",
                     values = c("#003300", "#990000", "#003399"),
                     limits = c("lawn", "apple", "agar"),
                     labels = c("agar" = "Outside", "apple" = "Scaffold (1:3 dilution)", "lawn" = "Lawn")
  ) +
  ylab("% of individuals at location") +
  xlab("Raising condition") +
  scale_x_discrete (limits = c("Plate & lawn", "Plate & apple", "Plate & agar", "Apple & lawn", "Apple & apple", "Apple & agar"),
                    labels=c("Plate & lawn" = "",
                             "Plate & apple" = "Agar",
                             "Plate & agar" = "",
                             "Apple & lawn" = "",
                             "Apple & apple" = "Scaffold",
                             "Apple & agar" = "")) +
  ggtitle("Food valence: Poor on scaffold") +
  theme_bw() +
  theme(axis.title.x = element_text(size=25, margin=margin(10,0,0,0)), ##adjust size and distance of x axis title from graph
        axis.title.y = element_text(size=28, margin=margin(0,18,0,0)), ##adjust size and distance of y axis title from graph
        axis.text.x  = element_text(size=26, colour = "#000000"),      ##adjust size and color of x axis text
        axis.text.y  = element_text(size=21, colour = "#000000"),      ##adjust size and color of y axis text
        axis.ticks.x = element_blank(),                                ##remove ticks on x axis
        panel.grid.minor.x = element_blank(),                          ##remove major gridlines on x axis
        panel.grid.major.x = element_blank(),                          ##remove minor gridlines on x axis
        panel.grid.major.y = element_line(color = "#CCCCCC", size = 1),##Change color and size of major gridline on y axis
        panel.grid.minor.y = element_line(size = 1),                   ##Change size of minor gridline on y axis
        panel.border = element_blank(),                                ##remove all boarder lines around graph
        plot.title = element_text(colour = "#000000", size = 26, hjust = 0.5),
        # legend.title = element_text(size = 22),
        # legend.text = element_text(size = 19),
        # legend.key.size = unit(1.1, "cm"),
        # legend.box.margin = margin(0, 0, 0, 0, "cm"),
        # legend.key = element_rect(legend.key.size = 2, legend.key.width = 2),
        # legend.position = c(.5, .8),
        axis.line.y = element_line(color = "black")                    ##add only board line on y axis
  ) +
  geom_vline(xintercept = 3.5, linetype = "longdash", color = "#666666", size = 0.8) +
  scale_y_continuous(breaks = c(0, 25, 50, 75), limits = c(0,95), expand = c(0,1))
  # scale_y_discrete(limits = c(0, 25, 50, 75))

p

dev.off()
 