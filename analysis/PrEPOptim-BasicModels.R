
## Explore PrEP Optim Datasets

library("EpiModelHIV")
library("tidyverse")

dy <- readRDS("analysis/data/prepOptim-Yearly-v2-500per.rds")

head(do, 30)
head(dy, 30)

# Descriptive stats

summary(dy$POAC)
summary(dy$OptimAdhrStarts)/52
# New range for POAC = 0:70

summary(dy$PORC)
summary(dy$OptimRetnPrev)
# New range for PORC = 0:3200

summary(dy$POIP)
summary(dy$prepCurr)

plot(dy$POIP, dy$prepCurr)

mod1 <- lm(PinfAvert ~ PSP, data = do)
summary(mod1)

dat <- data.frame(PSP = seq(min(do$PSP), max(do$PSP), length.out = 25))
pred <- predict(mod1, newdata = dat, se.fit = TRUE)
pred <- cbind(dat, pred)
pred

par(mar = c(3,3,2,1), mgp = c(2,1,0))
plot(pred$PSP, pred$fit, type = "b", col = "firebrick", lwd = 2, ylim = c(0, 0.5))


mod2 <- lm(PinfAvert ~ PHA, data = do)
summary(mod2)

dat <- data.frame(PHA = seq(min(do$PHA), max(do$PHA), length.out = 25))
pred <- predict(mod2, newdata = dat, se.fit = TRUE)
pred <- cbind(dat, pred)
pred

par(mar = c(3,3,2,1), mgp = c(2,1,0))
plot(pred$PHA, pred$fit, type = "b", col = "firebrick", lwd = 2, ylim = c(0, 0.5))


# convert from duration, x (in days), to rate per week: 1 - (2^(-1/(x/7)))
# convert from rate per week, r, to duration (in days): 7*(-1/(log2(1-r)))

do$PDur = 7*(-1/(log2(1-do$PDR)))
head(do, 25)

mod3 <- lm(PinfAvert ~ PDur, data = do)
summary(mod3)

dat <- data.frame(PDur = seq(min(do$PDur), max(do$PDur), length.out = 25))
pred <- predict(mod3, newdata = dat, se.fit = TRUE)
pred <- cbind(dat, pred)
pred

par(mar = c(3,3,2,1), mgp = c(2,1,0))
plot(pred$PDur, pred$fit, type = "b", col = "firebrick", lwd = 2, ylim = c(0, 0.5))


mod4 <- lm(PinfAvert ~ PSP + PHA + PDur, data = do)
summary(mod4)

dat <- expand.grid(PSP = 0.75, PHA = seq(0.80, 1, 0.05), PDur = seq(250, 850, 100))
dat

pred <- predict(mod4, newdata = dat, se.fit = TRUE)
pred <- cbind(dat, pred)
pred
