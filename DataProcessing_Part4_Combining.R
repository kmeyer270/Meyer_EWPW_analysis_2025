vegdata <- read.csv("./otherveg_cleaned.csv") # read in veg data
woodydata <- read.csv("./woodystems_cleaned.csv") # read in veg data
data1 <- cbind(vegdata, woodydata)
#data1 <- subset(data1, studyarea == "michaux")

##########

useds <- subset(data1, response == 1)
randoms <- subset(data1, response == 0)

# nudds board
hist(useds$nudds, breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$nudds, breaks=30, col=rgb(0,0,1,0.5), add = T)

# bare
hist(useds$perc_bare, xlim = c(0,100), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$perc_bare, xlim = c(0,100),breaks=30, col=rgb(0,0,1,0.5), add = T)

# leaf litter
hist(useds$perc_ll, xlim = c(0,100), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$perc_ll, xlim = c(0,100),breaks=30, col=rgb(0,0,1,0.5), add = T)

# grass
hist(useds$perc_grass, xlim = c(0,100), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$perc_grass, xlim = c(0,100),breaks=30, col=rgb(0,0,1,0.5), add = T)

# forb
hist(useds$perc_forb, xlim = c(0,100), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$perc_forb, xlim = c(0,100),breaks=30, col=rgb(0,0,1,0.5), add = T)

# Rubus
hist(useds$perc_Rubus, xlim = c(0,100), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$perc_Rubus, xlim = c(0,100),breaks=30, col=rgb(0,0,1,0.5), add = T)

# BasalArea
hist(useds$BasalArea, xlim = c(0,60), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$BasalArea, xlim = c(0,60),breaks=30, col=rgb(0,0,1,0.5), add = T)

# small woody stems
hist(useds$AvgNoSmStem, xlim = c(0,300), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$AvgNoSmStem, xlim = c(0,300),breaks=30, col=rgb(0,0,1,0.5), add = T)

# Med woody stems
hist(useds$AvgNoMedStem, xlim = c(0,230), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$AvgNoMedStem, xlim = c(0,230),breaks=30, col=rgb(0,0,1,0.5), add = T)

# Large woody stems
hist(useds$AvgNoLgStem, xlim = c(0,150), breaks=30, col=rgb(1,0,0,0.5))
hist(randoms$AvgNoLgStem, xlim = c(0,150),breaks=30, col=rgb(0,0,1,0.5), add = T)

#########
library(AICcmodavg)

nullmod <- glm(response ~ 1, data = data1, family = "binomial")

#linear
bgmod <- glm(response ~ perc_bare + studyarea, data = data1, family = "binomial")
llmod <- glm(response ~ perc_ll, data = data1, family = "binomial")
grassmod <- glm(response ~ perc_grass, data = data1, family = "binomial")
forbmod <- glm(response ~ perc_forb, data = data1, family = "binomial")
rubusmod <- glm(response ~ perc_Rubus, data = data1, family = "binomial")
basalmod <- glm(response ~ BasalArea, data = data1, family = "binomial")
smstemsmod <- glm(response ~ AvgNoSmStem, data = data1, family = "binomial")
medstemsmod <- glm(response ~ AvgNoMedStem, data = data1, family = "binomial")
largestemsmod <- glm(response ~ AvgNoLgStem, data = data1, family = "binomial")

# quadratic
bgmod2 <- glm(response ~ perc_bare + I(perc_bare^2), data = data1, family = "binomial")
llmod2 <- glm(response ~ perc_ll + I(perc_ll^2), data = data1, family = "binomial")
grassmod2 <- glm(response ~ perc_grass + I(perc_grass^2), data = data1, family = "binomial")
forbmod2 <- glm(response ~ perc_forb + I(perc_forb^2), data = data1, family = "binomial")
rubusmod2 <- glm(response ~ perc_Rubus + I(perc_Rubus^2), data = data1, family = "binomial")
basalmod2 <- glm(response ~ BasalArea + I(BasalArea^2), data = data1, family = "binomial")
smstemsmod2 <- glm(response ~ AvgNoSmStem + I(AvgNoSmStem^2), data = data1, family = "binomial")
medstemsmod2 <- glm(response ~ AvgNoMedStem + I(AvgNoMedStem^2), data = data1, family = "binomial")
largestemsmod2 <- glm(response ~ AvgNoLgStem + I(AvgNoLgStem^2), data = data1, family = "binomial")


modlist1 <- list(nullmod = nullmod, 
                 # linear
                 bgmod = bgmod, 
                 llmod = llmod, 
                 grassmod = grassmod, 
                 forbmod = forbmod,
                 rubusmod = rubusmod, 
                 basalmod = basalmod,
                 smstemsmod = smstemsmod,
                 medstemsmod = medstemsmod, 
                 largestemsmod = largestemsmod,
                 # quadratic
                 bgmod2 = bgmod2, 
                 llmod2 = llmod2, 
                 grassmod2 = grassmod2, 
                 forbmod2 = forbmod2,
                 rubusmod2 = rubusmod2, 
                 basalmod2 = basalmod2,
                 smstemsmod2 = smstemsmod2,
                 medstemsmod2 = medstemsmod2, 
                 largestemsmod2 = largestemsmod2)

aictab(modlist1)

# predicting transect with new "plot" data
newdat <- data.frame(AvgNoLgStem = seq(0,max(data1$AvgNoLgStem),length.out=100)) 
pred1 <- predict(largestemsmod2, newdat, type="link", se.fit=TRUE)

critval <- 1.96 ## approx 95% CI
pred1$upr <- pred1$fit + (critval * pred1$se.fit)
pred1$lwr <- pred1$fit - (critval * pred1$se.fit)
fit <- pred1$fit

## transform
mod <- largestemsmod
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(pred1$upr)
lwr2 <- mod$family$linkinv(pred1$lwr)

pred1<-cbind(newdat, fit2, upr2, lwr2)

plot(-10,-10, xlim=c(0,max(data1$AvgNoLgStem)), ylim=c(0,1), xlab="Large Woody Stem Density", 
     ylab="EWPW Use Probability")
lines(pred1$AvgNoLgStem, pred1$fit2, lty=1, col="black")
lines(pred1$AvgNoLgStem, pred1$lwr2, lty=2, col="black")
lines(pred1$AvgNoLgStem, pred1$upr2, lty=2, col="black")


