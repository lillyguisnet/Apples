library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(moments)

##Make df with xl file##
cfu_df <- readxl::read_xlsx("cfu.xlsx")

cfu_df <- cfu_df %>%
  mutate(id = paste(round, plate, replicate, sample)) %>%
  mutate(idd = paste(round, plate, replicate, sample, dilution)) %>%
  group_by(idd) %>%
  mutate(cfubyidd = sum(cfu)) %>%
  group_by(id) %>%
  mutate(cfubyid = sum(cfu))


ggplot(cfu_df, aes(as.factor(quadrant), cfu, fill = plate)) +
  geom_boxplot(aes(colour = plate), alpha = 0.7, show.legend = TRUE, outlier.size = 2) +
  facet_grid(rows = (dilution ~ location))


ggplot(cfu_df, aes(quadrant, cfu, fill = idd)) +
  geom_point(aes(colour = idd), show.legend = FALSE) +
  facet_grid(rows = vars(plate)) +
  geom_line(aes(colour = idd), show.legend = FALSE) +
  scale_colour_hue() +
  scale_fill_hue()

ggplot(cfu_df, aes(quadrant, cfu, fill = idd)) +
  geom_point(aes(colour = idd), show.legend = FALSE) +
  facet_grid(rows = (plate ~ location)) +
  geom_line(aes(colour = idd), show.legend = FALSE) +
  scale_colour_hue() +
  scale_fill_hue()

ggplot(cfu_df, aes(as.factor(dilution), cfubyidd, fill = plate)) +
  geom_boxplot(aes(colour = plate), alpha = 0.7, show.legend = TRUE, outlier.size = 2) +
  facet_grid(rows = vars(location))

ggplot(cfu_df, aes(as.factor(plate), cfubyid, fill = plate)) +
  geom_boxplot(aes(colour = plate), alpha = 0.7, show.legend = TRUE, outlier.size = 2)




model <- lm(cfubyidd ~ plate, data = cfu_df)
summary(model)  #p-value: 6.473e-14

aov.model <- aov(cfubyidd ~ plate, data = cfu_df)
summary(aov.model) #6.47e-14

lm.model <- lm(cfubyidd ~ plate:dilution, data = cfu_df)
summary(lm.model) #6.47e-14

aov <- aov(cfubyidd ~ as.factor(plate):as.factor(dilution), data = cfu_df)
summary(aov) 
TukeyHSD(aov)

par <- aov(cfubyidd ~ as.factor(plate):as.factor(dilution) - 1, data = cfu_df)
summary(par) 
TukeyHSD(par)

aov <- aov(cfu ~ as.factor(plate)*as.factor(dilution), data = cfu_df)
summary(aov) 
TukeyHSD(aov)
plot(TukeyHSD(aov))

# Check assumptions not normal data?

skewness(cfu_df$cfu, na.rm = TRUE) #3.26

glmodel <- glm(cfu ~ as.factor(plate)*relevel(as.factor(dilution), "2"), data = cfu_df, family = quasi)
summary(glmodel)

drop1(glmodel, test = "F")
anova(glmodel, test = "F")




