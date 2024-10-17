library(readxl)
library(dplyr)
library(ggplot2)

df <- readxl::read_xlsx("../choice/choice2020.xlsx")
dfvert <- readxl::read_xlsx("../choice/choice2020vertical.xlsx")

df4 <- df %>% filter(round == "4") %>%
  mutate(unique_id = paste(dilution, "_", diameter, "_", plate)) %>%
  mutate(scaffold = total - agar - out - dead) %>%
  mutate(index = (scaffold - agar)/(total - dead - out)) %>%
  mutate(worms = total - dead) %>%
  mutate(perc_s = scaffold/worms*100) %>%
  mutate(perc_a = agar/worms*100) %>%
  mutate(perc_o = out/worms*100)

ggplot(df4, aes(unique_id, index)) +
  geom_boxplot(aes(colour = plate), alpha = 0.7, show.legend = TRUE, outlier.size = 2) +
  geom_jitter(width = 0.075)

dfall <- df %>%
  mutate(unique_id = paste(dilution, "_", diameter)) %>%
  mutate(round_id = paste(unique_id, "_", round)) %>%
  mutate(scaffold = total - agar - out - dead) %>%
  mutate(index = (scaffold - agar)/(total - dead - out))

ggplot(dfall, aes(plate, index)) +
  #geom_boxplot(aes(unique_id, index, colour = plate), alpha = 0.7, show.legend = TRUE, outlier.size = 2) +
  geom_jitter(aes(color = as.factor(round)), width = 0.075, cex = 3) +
  facet_grid(col = vars(unique_id))




choice_index <- dfvert %>% filter(round == "4") %>%
  group_by(plate, dilution, diameter, replicate) %>%
  mutate(allworms = sum(animals)) %>%
  mutate(total = allworms - animals[which(location == "dead")]) %>%
  mutate(prefindex = (animals[which(location == "scaffold")] - animals[which(location == "agar")]) / (total - animals[which(location == "out")] ) ) %>%
  mutate(wormvalence = paste(dilution, diameter, worm, sep = "_")) %>%
  group_by(wormvalence) %>%
  mutate(av_index = mean(prefindex) )
