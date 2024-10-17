library(stringr)
library(dplyr)
library(ggplot2)
library(sjmisc)
library(openxlsx)

file_names <- list.files("../11.26/gfprenamed/intensityprofileanalysis") # Find all files in folder
is_csv <- str_detect(file_names, ".csv$") # Check if all .csv
df_filelist <- as.list(1:sum(is_csv)) # Make list of size of number of files

counter <- 1
for(i in file_names[is_csv]) {
  cat (i, "\n")
  tmp_df <- as.data.frame(read.csv(paste0("../11.26/gfprenamed/intensityprofileanalysis/", i)))
  tmp_df <- tmp_df[-1, ]
  colnames(tmp_df) <- c("line", "intensity")
  split_name <- str_split(i, "_")
  tmp_df$worm <- split_name[[1]][1]
  tmp_position <- split_name[[1]][2]
  split_tmp_position <- str_split(tmp_position, "\\.")
  tmp_df$position <- split_tmp_position[[1]][1]
  df_filelist[[counter]] <- tmp_df
  counter <- counter + 1
}

df <- bind_rows(df_filelist)

df_gfp <- filter(df, line != 76) %>%
  mutate(center = if_else(line > 24 & line < 51, TRUE, FALSE))

df_randomkey <- as.data.frame(read.csv("C:/Users/aurel/prog/apples/renamekey.csv", header = FALSE, stringsAsFactors = FALSE))

df_randomkey <- group_by(df_randomkey) %>%
  rowwise() %>%
  mutate(worm = toString(V2) )

df_wkey_join <- full_join(df_gfp, df_randomkey, by = "worm") %>%
  filter(worm != 21)

df_wormcondition <- group_by(df_wkey_join) %>%
  rowwise() %>%
  mutate(condition = if_else(str_contains(V1, "Agar"), "Agar", "Scaffold" ) )

write.xlsx(df_wormcondition, "gfp_reanalyzed.xlsx")

df_normalised <- group_by(df_wormcondition, worm, position, center) %>%
  mutate(av_intensity_bycenter = mean(intensity)) %>%
  ungroup()

df_norm_intensity <- group_by(df_normalised, worm, position) %>%
  mutate(av_intensity_norm = mean(av_intensity_bycenter[which(center == TRUE)]) - mean(av_intensity_bycenter[which(center == FALSE)]) )

df_norm_worm <- group_by(df_norm_intensity, worm) %>%
  mutate(av_intensity_worm = mean(av_intensity_norm)) 

df_plot <- group_by(df_norm_worm, worm, condition) %>%
  summarise(av_intensity_worm = mean(av_intensity_worm))


png("graphs/gfp_reanalyzed.png", width = 350, height = 560)

ggplot(df_plot, aes(condition, av_intensity_worm)) +                              ##df, x, y
  geom_boxplot(aes(colour = condition), alpha = 0.1, show.legend = FALSE, width = 0.4, lwd = 1) +  ##width = of boxes, lwd = line width of boxes
  geom_jitter(shape= 16, position=position_jitter(0.018), aes(color = condition), show.legend = FALSE, cex = 2.8)

dev.off()




