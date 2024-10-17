setwd("~/Apples/smurf")
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyr)

xl_file = "smurf_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

df_perc <- mutate(df, total_worms = smurfed + not_smurfed) %>%
  mutate(perc_smurf = smurfed / total_worms)


ggplot(df_perc, aes(condition, perc_smurf, fill = as.factor(condition))) + 
  geom_col() +
  ylab("% smurfed @ day 11")
