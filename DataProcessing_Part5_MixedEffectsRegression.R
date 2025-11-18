library(stringr)
vegdata <- read.csv("./otherveg_cleaned.csv") # read in veg data
woodydata <- read.csv("./woodystems_cleaned.csv") # read in veg data
data1 <- cbind(vegdata, woodydata)
data1$birdID <- substr(data1$point_id, 1, 7)
data1 <- data1[,c(1, 20, 2:14, 16:19)]

#########
library(AICcmodavg); library(lme4)

# null model
null_mod <- glmer(response ~ studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))

# linear forms
bgmod <- glmer(response ~ perc_bare + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
llmod <- glmer(response ~ perc_ll + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
grassmod <- glmer(response ~ perc_grass + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
forbmod <- glmer(response ~ perc_forb + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
basalmod <- glmer(response ~ BasalArea + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
smstemsmod <- glmer(response ~ AvgNoSmStem + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
medstemsmod <- glmer(response ~ AvgNoMedStem + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
largestemsmod <- glmer(response ~ AvgNoLgStem + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))

# quadratic forms
bgmod2 <- glmer(response ~ perc_bare + I(perc_bare^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
llmod2 <- glmer(response ~ perc_ll + I(perc_ll^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
grassmod2 <- glmer(response ~ perc_grass + I(perc_grass^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
forbmod2 <- glmer(response ~ perc_forb + I(perc_forb^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
basalmod2 <- glmer(response ~ BasalArea + I(BasalArea^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
smstemsmod2 <- glmer(response ~ AvgNoSmStem + I(AvgNoSmStem^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
medstemsmod2 <- glmer(response ~ AvgNoMedStem + I(AvgNoMedStem^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))
largestemsmod2 <- glmer(response ~ AvgNoLgStem + I(AvgNoLgStem^2) + studyarea + (1|birdID), family = binomial, data = data1, control = glmerControl(optimizer="bobyqa"))

modlist1 <- list(null_mod = null_mod, 
                 # linear
                 bgmod = bgmod, 
                 llmod = llmod, 
                 grassmod = grassmod, 
                 forbmod = forbmod,
                 basalmod = basalmod,
                 smstemsmod = smstemsmod,
                 medstemsmod = medstemsmod, 
                 largestemsmod = largestemsmod,
                 # quadratic
                 bgmod2 = bgmod2, 
                 llmod2 = llmod2, 
                 grassmod2 = grassmod2, 
                 forbmod2 = forbmod2,
                 basalmod2 = basalmod2,
                 smstemsmod2 = smstemsmod2,
                 medstemsmod2 = medstemsmod2, 
                 largestemsmod2 = largestemsmod2)

aictab(modlist1)

##

library(ggeffects);library(ggplot2)

pred_stems <- ggpredict(largestemsmod2, terms = "AvgNoLgStem [all]")

ggplot(pred_stems, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "gray70", alpha = 0.4) +
  geom_line(color = "black", size = 1.1) +
  labs(x = "Woody Stem Density (stems/40m2)", y = "EWPW Use probability") +
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.text = element_text(size = 14)
  )



