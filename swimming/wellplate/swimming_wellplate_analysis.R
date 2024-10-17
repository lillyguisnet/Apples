library(ggplot2)
library(dplyr)
library(openxlsx)

xl_file = "swimming_wellplate_data.xlsx"
df <- as.data.frame(read.xlsx(xl_file))

df_swim <- group_by(df, condition, day, replicate) %>%
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
  filter(state == "2") %>%
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

ggplot(df_proportions, aes(condition, perc_quiescence)) +
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
  # facet_grid(~condition)
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
