library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)


file_names <- list.files("all2020")
is_xlsx <- str_detect(file_names, ".xlsx$")
df_filelist <- as.list(1:sum(is_xlsx))


counter <- 1
for(i in file_names[is_xlsx]) {
  cat (i, "\n")
  tmp_df <- as.data.frame(t(read_xlsx(paste0("all2020/",i), col_names = FALSE)))
  tmp_df <- tmp_df[-1, ]
  colnames(tmp_df) <- c("neck", "bg1", "bg2")
  split_name <- str_split(i, "\\d")
  tmp_df$worm <- split_name[[1]][1]
  df_filelist[[counter]] <- tmp_df
  counter <- counter + 1
}

df <- bind_rows(df_filelist)

gfp_df <- df %>%
  mutate(all_mean_bg = mean(c(bg1, bg2))) %>%
  mutate(all_mean_neck = mean(neck)) %>%
  mutate(all_norm_neck = all_mean_neck - all_mean_bg) %>%
  rowwise() %>%
  mutate(mean_bg = mean(c(bg1, bg2))) %>%
  mutate(norm_neck = neck - mean_bg)

write.xlsx(gfp_df, "gfp_data.xlsx")


with(gfp_df, shapiro.test(norm_neck[worm == "agar"]))         ## Non-normal, p = 0.008991
with(gfp_df, shapiro.test(norm_neck[worm == "apple"]))        ## Non-normal, p = 0.002964
var.test(norm_neck ~ worm, data = gfp_df)                     ## Non-equal variance, p = 0.01511
t.test(norm_neck ~ worm, data = gfp_df, var.equal = FALSE)    ## t-test assuming non-equal variance     p = 0.02322
wilcox.test(norm_neck ~ worm, data = gfp_df)                  ## Independent 2-group Mann-Whitnet U Test, non-normal    p-value = 0.07259


model  <- lm(norm_neck ~ worm, data = gfp_df)       ## Assumptions
ggqqplot(residuals(model))        ## Bi-modal?
shapiro_test(residuals(model))    ## Non-normal, p = 0.000478
gfp_df %>% levene_test(norm_neck ~ worm)    ## Non-equal variance, p = 0.0234



##Make png out of graph##
png("graphs/gfp.png", width = 350, height = 560)

##Make box plot##
p <- ggplot(gfp_df, aes(worm, norm_neck)) +                              ##df, x, y
  geom_boxplot(aes(colour = worm), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = worm), show.legend = FALSE, cex = 2.8)  ##cex = shape size
  # scale_color_manual(values= alpha(c("#003300", "#990000"), 0.6) ) +                 ##dark green & dark red personalized colors
  # scale_fill_manual(values=c("#003300", "#990000")) +
  # ylab("Number of eggs laid on day 1") +
  # xlab("Raising condition") +
  # theme_bw() +                                                         ##remove background 
  # theme(axis.title.x = element_text(size=28, margin=margin(10,0,0,0), vjust = -0.2), ##adjust size and distance of x axis title from graph
  #       axis.title.y = element_text(size=28, margin=margin(0,18,0,0), hjust = -0.1), ##adjust size and distance of y axis title from graph
  #       axis.text.x  = element_text(size=25, colour = "#000000"),      ##adjust size and color of x axis text
  #       axis.text.y  = element_text(size=23, colour = "#000000"),      ##adjust size and color of y axis text
  #       
  #       plot.margin = unit(c(5,1,1,1), "lines"),                       ##Expand graph margins to leave space for outside plot elements
  #       
  #       axis.ticks.x = element_blank(),                                ##remove ticks on x axis
  #       # panel.grid.minor.x = element_blank(),                          ##remove major gridlines on x axis
  #       # panel.grid.major.x = element_blank(),                          ##remove minor gridlines on x axis
  #       # panel.grid.major.y = element_line(color = "#CCCCCC", size = 1),##Change color and size of major gridline on y axis
  #       # panel.grid.minor.y = element_line(size = 1),                   ##Change size of minor gridline on y axis
  #       panel.grid = element_blank(),
  #       panel.border = element_blank(),                                ##remove all boarder lines around graph
  #       axis.line.y = element_line(color = "black")                    ##add only boarder line on y axis
  # ) +
  # 
  # annotate("segment", x = 1, xend = 2, y = 43, yend = 43, cex = 1) +
  # annotate("segment", x = 1, xend = 1, y = 43, yend = 42, cex = 1) +
  # annotate("segment", x = 2, xend = 2, y = 43, yend = 42, cex = 1) +
  # annotate("text", x = 1.5, y = 45, label = "*", size = 10) +
  # 
  # coord_cartesian(clip = 'off') +                     ##Specify y-axis limits, allow elements to be unattached to plot elements
  # 
  # coord_capped_cart(left = "both") +
  # 
  # scale_y_continuous(expand = c(0,1.5)) +
  # scale_x_discrete(expand = c(0.5,0))  ##minimize extra space around plot

p 

dev.off()




