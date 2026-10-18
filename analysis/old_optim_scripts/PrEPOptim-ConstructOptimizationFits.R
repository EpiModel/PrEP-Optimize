
## Construct fits for optimization on PrEP Optim Datasets

library("tidyverse")

## Load data -------------------------------------------#

do <- readRDS("analysis/data/prepOptim-Overall-v1.rda")
dy <- readRDS("analysis/data/prepOptim-Yearly-v1.rda")

do$scenario = as.factor(do$scenario)
dy$scenario = as.factor(dy$scenario)

do$infAvert[which(is.na(do$infAvert))] = 0
do$PinfAvert[which(is.na(do$PinfAvert))] = 0
dy$infAvert[which(is.na(dy$infAvert))] = 0
dy$PinfAvert[which(is.na(dy$PinfAvert))] = 0


## Analysis inputs -------------------------------------#

# base case parameter values
PSP_base <- min(do$PSP)
PHA_base <- min(do$PHA)
PDR_base <- max(do$PDR)
n_years <- max(dy$years)

# maximum feasible change
PSP_inc_max <- max(do$PSP - PSP_base) ## THIS IS MADE UP (NO MAX) ##
PHA_inc_max <- max(do$PHA - PHA_base) ## THIS IS MADE UP (NO MAX) ##
PDR_dec_max <- max(PDR_base - do$PDR) ## THIS IS MADE UP (NO MAX) ##

# discount rate
discount_rate_annual <- 0.03


## Calculation of intermediate values -------------------#

# calculate change from base case
do$PSPinc <- do$PSP - PSP_base
do$PHAinc <- do$PHA - PHA_base
do$PDRdec <- PDR_base - do$PDR

dy$PSPinc <- dy$PSP - PSP_base
dy$PHAinc <- dy$PHA - PHA_base
dy$PDRdec <- PDR_base - dy$PDR


## Annual intervention cost calculations ---------------#

## PrEP initiation (PSP) ##
PSP_dollar_per_inc <- 10^6
PSP_inc_max <- 0.8 ## MADE UP -- NEEDS TO REFLECT MAX UPTAKE POSSIBLE

# proposed approach
# Assume an annual fixed cost per unit increase in initiation probability
# Total cost is a simple fixed cost
# Can we calculate this from trial data?

dy$costPSP <- dy$PSPinc * PSP_dollar_per_inc


## PrEP adherence (PHA) ##
adhere_prob_base <- 0.784
adhere_prob_counseling <- 0.87 ## MADE UP
cost_per_adhere_counseling <- 250 ## MADE UP

# max adherence increase
PHA_inc_max <- adhere_prob_counseling - adhere_prob_base

# proposeed approach
# pay per initiant
# calculate proportion of people initiating PrEP who received the adherence intervention
# p_counsel = (p_adhere_obs - p_adhere_base)/(p_adhere_int - p_adhere_base)

# i.e.,
dy$propAdhereCounsel <- dy$PHAinc / (adhere_prob_counseling - adhere_prob_base)
# need the number of initiations of PrEP per year (assume people re-initiating need to be recounseled to get benefit)
dy$nNewPrep = 100
dy$costPHA <- dy$nNewPrep * dy$propAdhereCounsel * cost_per_adhere_counseling



## PrEP discontinuation (PDR) ##
discontinue_rate_base <- 0.02138792
discontinue_rate_PrepAtHome <- 0.001 ## THIS IS MADE UP ##
cost_per_week_PrepAtHome <- 200 ## THIS IS MADE UP ##

# max decrease in attrition
PDR_dec_max <- discontinue_rate_base - discontinue_rate_PrepAtHome

# proposed approach
# calculate proportion who must be receiving PrEP at Home (PaH) to achieve observed overall retention
# e.g. r_obs = r_base * (1-p_pah) + r_pah * p_pah
# Re-arranging: p_pah = (r_obs-r_base)/(r_pah-r_base) = PDRdec / (r_base - r_pah)

# Proportion of PrEP weeks incurring PreP at Home costs
# (p_pah / r_pah) / (p_pah/r_pah + (1-p_pah)/r_base)
# re-arrange: (p_pah * r_base) / (p_pah*r_base + (1-p_pah)*r_pah)

# i.e.,
dy$propPAH <- dy$PDRdec / (discontinue_rate_base - discontinue_rate_PrepAtHome)
dy$propWeeksPAH <- (dy$propPAH * discontinue_rate_base) / (dy$propPAH * discontinue_rate_base + (1-dy$propPAH)*discontinue_rate_PrepAtHome)
dy$costPDR <- cost_per_week_PrepAtHome * dy$prepCurr * dy$propWeeksPAH

## Calculate total costs ##
dy$costTotal <- rowSums(dy[,c("costPSP","costPHA","costPDR")])

## Calculate discounted costs and outcomes -------------#
dy$discFact <- 1/(1+discount_rate_annual)^dy$years

dy$infAvertdisc <- dy$infAvert*dy$discFact
dy$costPSPdisc <- dy$costPSP*dy$discFact
dy$costPHAdisc <- dy$costPHA*dy$discFact
dy$costPDRdisc <- dy$costPDR*dy$discFact
dy$costTotaldisc <- dy$costTotal*dy$discFact

# sum and merge into overall results
do_disc <- aggregate(x=dy[,c("infAvertdisc","costPSPdisc","costPHAdisc","costPDRdisc","costTotaldisc")],
                     FUN=sum, by=list(dy$scenario))
do <- merge(x=do,y=do_disc,by.x="scenario",by.y="Group.1",all.x=TRUE)


## Fit response surface of averted infections -----------#

# discard samples outside of feasible range ## DO THIS BEFORE FITTING??? ##
v_scn_discards <- do$scenario[which((do$PSPinc > PSP_inc_max) |
                                      (do$PHAinc > PHA_inc_max) |
                                      (do$PDRdec > PDR_dec_max))]

v_scn_keep = do$scenario[which(!(do$scenario %in% v_scn_discards))]
do = do[which(do$scenario %in% v_scn_keep),]
dy = dy[which(dy$scenario %in% v_scn_keep),]

# Possible fits
f1 <- as.formula(infAvertdisc ~ PSPinc + PHAinc + PDRdec)
f2 <- as.formula(infAvertdisc ~ PSPinc + PHAinc + PDRdec + PSPinc*PHAinc + PHAinc*PDRdec + PSPinc*PDRdec)
f3 <- as.formula(infAvertdisc ~ poly(PSPinc,degree=2) + poly(PHAinc,degree=2) + poly(PDRdec,degree=2))
f4 <- as.formula(infAvertdisc ~ poly(PSPinc,degree=2) + poly(PHAinc,degree=2) + poly(PDRdec,degree=2) +
                   PSPinc*PHAinc + PHAinc*PDRdec + PSPinc*PDRdec + PSPinc*PHAinc*PDRdec)
f5 <- as.formula(infAvertdisc ~ poly(PSPinc,degree=2) + poly(PHAinc,degree=2) + poly(PDRdec,degree=2) +
                   PSPinc*PHAinc + PHAinc*PDRdec + PSPinc*PDRdec)
f6 <- as.formula(infAvertdisc ~ poly(PSPinc,degree=2) + poly(PDRdec,degree=2) +
                   PSPinc*PHAinc + PHAinc*PDRdec + PSPinc*PDRdec)

L1 <- lm(formula=f1, data=do)
L2 <- lm(formula=f2, data=do)
L3 <- lm(formula=f3, data=do)
L4 <- lm(formula=f4, data=do)
L5 <- lm(formula=f5, data=do)
L6 <- lm(formula=f6, data=do)

infAvert_modelList <- list(L1,L2,L3,L4,L5,L6)
names(infAvert_modelList) <- paste0("L",1:6)

v_aic <- sapply(infAvert_modelList,AIC)
v_rsquared <- sapply(infAvert_modelList,FUN=function(ell){summary(ell)$r.squared})



## Fit intermediate outcomes ----------------------------#

# Number of new PrEP initiations

#nNewPrep ~ PSPinc

# Number of person-weeks of PrEP

# prepCurr ~ PSPinc + PDRdec


