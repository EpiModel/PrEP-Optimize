
## PrEP-Optim Analysis

library("EpiModelHIV")
library("dplyr")
library("foreach")


# Reference Scenario ------------------------------------------------------

load("intervention/data/sim.n1000.rda")

df <- as.data.frame(sim, out = "mean")
dfr <- select(df, incid, s.num, ir100, prepCurr, prepCurr.hadr, prep.rand.stop)
dfr <- dfr[-1, ]
head(dfr)

years <- rep(1:10, each = 52)
dfr$years <- years

dfr <- group_by(dfr, years) %>%
  summarise(incid = sum(incid),
            s.num = sum(s.num)/52,
            prepCurr = sum(prepCurr)/52,
            prepCurrHA = sum(prepCurr.hadr)/52,
            prepRandSt = sum(prep.rand.stop)) %>%
  as.data.frame()
dfr$PSP <- sim$param$prep.start.prob
dfr$PHA <- sim$param$prep.adhr.dist[3]
dfr$PDR <- sim$param$prep.discont.rate
dfr$inf.avert <- NA
dfr$scenario <- "n1000"
dfr

dfr.tot <- data.frame(incid = sum(dfr$incid), s.num = sum(dfr$s.num),
                      prepCurr = sum(dfr$prepCurr), prepCurrHA = sum(dfr$prepCurrHA),
                      prepRandSt = sum(dfr$prepRandSt),
                      PSP = dfr$PSP[1], PHA = dfr$PHA[1], PDR = dfr$PDR[1],
                      inf.avert = NA, scenario = dfr$scenario[1])
dfr.tot



# Counterfactual Scenarios ------------------------------------------------

fn <- list.files("intervention/data/", pattern = "sim", full.names = TRUE)
fn <- fn[2]

counter_df_byyear <- function(fn, dfr) {
  load(fn)
  df <- as.data.frame(sim, out = "mean")
  dfc <- select(df, incid, s.num, ir100, prepCurr, prepCurr.hadr, prep.rand.stop)
  dfc <- dfc[-1, ]
  dfc$years <- rep(1:10, each = 52)
  df2 <- group_by(dfc, years) %>%
    summarise(incid = sum(incid),
              s.num = sum(s.num)/52,
              prepCurr = sum(prepCurr)/52,
              prepCurrHA = sum(prepCurr.hadr)/52,
              prepRandSt = sum(prep.rand.stop)) %>%
    as.data.frame()
  df2$PSP <- sim$param$prep.start.prob
  df2$PHA <- sim$param$prep.adhr.dist[3]
  df2$PDR <- sim$param$prep.discont.rate
  df2$infAvert <- dfr$incid - df2$incid
  df2$scenario <- strsplit(fn, "[.]")[[1]][2]
  return(df2)
}
counter_df_byyear(fn, dfr)

counter_df_overall <- function(fn, dfr.tot) {
  load(fn)
  df <- as.data.frame(sim, out = "mean")
  dfc <- select(df, incid, s.num, ir100, prepCurr, prepCurr.hadr, prep.rand.stop)
  dfc <- dfc[-1, ]
  df2 <- data.frame(incid = sum(dfc$incid), s.num = sum(dfc$s.num)/52,
                    prepCurr = sum(dfc$prepCurr)/52, prepCurrHA = sum(dfc$prepCurr.hadr)/52,
                    prepRandSt = sum(dfc$prep.rand.stop))
  df2$PSP <- sim$param$prep.start.prob
  df2$PHA <- sim$param$prep.adhr.dist[3]
  df2$PDR <- sim$param$prep.discont.rate
  df2$infAvert <- dfr.tot$incid - df2$incid
  df2$scenario <- strsplit(fn, "[.]")[[1]][2]
  return(df2)
}
counter_df_overall(fn, dfr.tot)



# Scale Up on Hyak --------------------------------------------------------

load("data/hold/sim.n1000.rda")

df <- as.data.frame(sim, out = "mean")
dfr <- select(df, incid, s.num, ir100, prepCurr, prepCurr.hadr, prep.rand.stop)
dfr <- dfr[-1, ]
head(dfr)

years <- rep(1:10, each = 52)
dfr$years <- years

dfr <- group_by(dfr, years) %>%
  summarise(incid = sum(incid),
            s.num = sum(s.num)/52,
            prepCurr = sum(prepCurr)/52,
            prepCurrHA = sum(prepCurr.hadr)/52,
            prepRandSt = sum(prep.rand.stop)) %>%
  as.data.frame()
dfr$PSP <- sim$param$prep.start.prob
dfr$PHA <- sim$param$prep.adhr.dist[3]
dfr$PDR <- sim$param$prep.discont.rate
dfr$inf.avert <- NA
dfr$scenario <- "n1000"
dfr

dfr.tot <- data.frame(incid = sum(dfr$incid), s.num = sum(dfr$s.num),
                      prepCurr = sum(dfr$prepCurr), prepCurrHA = sum(dfr$prepCurrHA),
                      prepRandSt = sum(dfr$prepRandSt),
                      PSP = dfr$PSP[1], PHA = dfr$PHA[1], PDR = dfr$PDR[1],
                      inf.avert = NA, scenario = dfr$scenario[1])
dfr.tot


fn <- list.files("data/", pattern = "sim", full.names = TRUE)
# fn <- fn[2]

doParallel::registerDoParallel(parallel::detectCores())
tdf <- foreach(i = 1:length(fn)) %dopar% {
  counter_df(fn[i], dfr)
}

tdf <- do.call("rbind", tdf)
tdf

all <- rbind(dfr, tdf)
all


