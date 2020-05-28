### Libraries
library(dplyr)
library(psych)
library(ggplot2)

### Load data
#df_prep <- readRDS("prepOptim-Yearly-v2-500per.rds")
df_prep <- readRDS("prepOptim-Yearly-v3-5000sets-100per.rds")

### Cost parameters

# INITIATION
# cost per percentage point of newly eligible for PrEP MSM reached by app (prep.optim.init.prob -> POIP in data frame)
cost.optim.init <- 100*100 # **placeholder value**

# ADHERENCE
# cost per PrEP initiant receiving adherence intervention (assume capacity constraint)
# Question: should cost be incured for full capacity (prep.optim.adhere.cap -> POAC in data frame)? 
#           Or only used capacity (OptimAdhrStarts)? **Thinking capacity**
cost.optim.adhr <- 250 # **placeholder value**

# RETENTION
# Cost per PrEP user enrolled in retention program (e.g. PrEP @ Home) each time step
# Question: should cost be incured for full capacity (prep.optim.retn.cap -> PORC in data frame)? 
#           Or only used capacity (OptimRetnPrev)? **Thinking capacity**
# Annual cost of XXX --> weekly cost of XXX/52 for each time step 
cost.optim.retn <- 2000 # **placeholder value**


### Data fields

# INITIATION
# parameters
#   prep.optim.init.prob: probability of being reached by app when newly PrEP-eligible
# outputs
#   OptimInitStarts: Number of app initiators per time step.
#   OptimInitPrev: Current prevalence of app use.
#	  PrEPStarts: Regular (non-intervention) PrEP starts per time step.
#	  PrEPStartsOptim: Intervention-based PrEP starts per time step.

# ADHERENCE
# parameters
#   prep.optim.adhere.cap: maximum capacity for adherence intervention
# outputs
#   OptimAdhrStarts: Incidence of new adherence intervention users per time step. 
#   OptimAdhrPrev: Number current on adherence intervention at time step.

# RETENTION
# parametesr
#  prep.optim.retn.cap: maximum capacity of the retention program (number of individuals)
# outputs
#  OptimRetnStarts: Incidence of new retention intervention users per time step
#  OptimRetnPrev: Number currently on retention intervention at time step


### Calculate budget allocations

df_prep$POAC_yr <- df_prep$POAC*52
df_prep$PAdhrCap <- df_prep$OptimAdhrStarts / df_prep$POAC_yr

# Initiation
df_prep$OptimInitBudget <- df_prep$POIP * cost.optim.init
df_prep$OptimInitExpenditure <- df_prep$POIP * cost.optim.init

# Adherence
df_prep$OptimAdhrBudget <- df_prep$POAC_yr * cost.optim.adhr
df_prep$OptimAdhrExpenditure <- df_prep$OptimAdhrStarts * cost.optim.adhr

# Retention
df_prep$OptimRetnBudget <- df_prep$PORC * cost.optim.retn
df_prep$OptimRetnExpenditure <- df_prep$OptimRetnPrev * cost.optim.retn

# Total
df_prep$TotalBudget <- df_prep$OptimInitBudget + df_prep$OptimAdhrBudget + df_prep$OptimRetnBudget
df_prep$TotalExpenditure <- df_prep$OptimInitExpenditure + df_prep$OptimAdhrExpenditure + df_prep$OptimRetnExpenditure

### Calculate 10-year outcomes
df_prep_10yr <- full_join(df_prep %>% group_by(scenario) %>% 
                            summarise_at(vars(incid,prepElig,prepCurr,infAvert),
                                         sum),
                          df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(PrEPHighAdr), #** eventually add avg. proportion of eligible on PrEP**#
                                         mean),
                          by="scenario")

df_prep_10yr <- full_join(df_prep_10yr,
                          df_prep %>% group_by(scenario) %>%
                            summarise_at(vars(POIP),
                                         first),
                          by="scenario")

df_prep_10yr <- full_join(df_prep_10yr,
                          df_prep %>% group_by(scenario) %>% 
                            summarise_at(vars(POAC_yr,PORC,
                                              OptimInitStarts, OptimAdhrStarts, OptimRetnPrev,
                                              OptimInitExpenditure,OptimAdhrExpenditure,OptimRetnExpenditure,TotalExpenditure,
                                              OptimInitBudget,OptimAdhrBudget,OptimRetnBudget,TotalBudget),
                                         sum),
                          by="scenario")

# Calculate to what extent programs operated at capacity (actual use / capacity)
df_prep_10yr$PAdhrCap <- df_prep_10yr$OptimAdhrStarts/df_prep_10yr$POAC_yr
df_prep_10yr$PRetnCap <- df_prep_10yr$OptimRetnPrev/df_prep_10yr$PORC

# Calculate effective capacity (e.g. minimum capacity needed to accommodate entire eligible population)
df_prep_10yr$EffAdhrCap <- ceiling(df_prep_10yr$POAC_yr * df_prep_10yr$PAdhrCap)
df_prep_10yr$EffRetnCap <- ceiling(df_prep_10yr$PORC * df_prep_10yr$PRetnCap)

# Calculate proportion of expenditure toward each intervention
df_prep_10yr$PInitExpense <- df_prep_10yr$OptimInitExpenditure / df_prep_10yr$TotalExpenditure
df_prep_10yr$PAdhrExpense <- df_prep_10yr$OptimAdhrExpenditure / df_prep_10yr$TotalExpenditure
df_prep_10yr$PRetnExpense <- df_prep_10yr$OptimRetnExpenditure / df_prep_10yr$TotalExpenditure

i_names <- c("scenario","prepElig","prepCurr","infAvert","PrEPHighAdr",
             "POIP","POAC_yr","PORC")

df_prep_10yr[order(-df_prep_10yr$PAdhrCap),c(1,4,5:8,22,23,15,24:26)]
df_prep_10yr[order(-df_prep_10yr$PRetnCap),c(1:11,21)]
df_prep_10yr[order(-df_prep_10yr$infAvert),c(1,4,5:8,22,23,15,24:26)]


# identify capacity maxima (for more optimal sampling)
L1 <- lm(data=df_prep_10yr[df_prep_10yr$PAdhrCap<0.85,],formula=EffAdhrCap~POIP)
L1$coefficients["POIP"]/100/10 # people per percentage per year
L1$coefficients["(Intercept)"]/10 # base max. capacity per year

L2 <- lm(data=df_prep_10yr[df_prep_10yr$PRetnCap<0.85,],formula=EffRetnCap~POIP)
L2$coefficients["POIP"]/100/10 # people per percentage per year
L2$coefficients["(Intercept)"]/10 # base max. capacity per year


L3 <- lm(data=df_prep_10yr,formula=infAvert~POIP+POAC_yr+PORC+POIP*POAC_yr+POIP*PORC)


i_plot <- which(df_prep_10yr$POIP>=0 & df_prep_10yr$POIP<=0.05)
plot(df_prep_10yr$PORC[i_plot],df_prep_10yr$infAvert[i_plot])

df_prep_10yr$POIP_cat <- cut(df_prep_10yr$POIP,seq(0,0.5,0.1))

ggplot(df_prep_10yr,aes(x=PORC,y=infAvert)) + geom_point(aes(x=PORC,y=infAvert,color=POIP_cat))

ggplot(df_prep_10yr,aes(x=POAC_yr,y=infAvert)) + geom_point(aes(x=POAC_yr,y=infAvert,color=POIP_cat))
