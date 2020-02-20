
## PrEP-Optim Analysis

library("EpiModelHIV")
library("dplyr")
library("foreach")


# Reference Scenario ------------------------------------------------------

load("data/sim.n500.rda")

sim <- truncate_sim(sim, at = 261)
df <- as.data.frame(sim, out = "mean")
df <- df[-nrow(df), ]
dim(df)
head(df)
tail(df)
dfr <- df
head(dfr)

years <- rep(1:10, each = 52)
dfr$years <- years
head(dfr)

dfr <- group_by(dfr, years) %>%
  summarise(incid = sum(incid),
            prepCurr = mean(prepCurr),
            OptimInitStarts = sum(OptimInitStarts),
            OptimInitPrev = mean(OptimInitPrev),
            PrEPStarts = sum(PrEPStarts),
            PrEPStartsOptim = sum(PrEPStartsOptim),
            OptimAdhrStarts = sum(OptimAdhrStarts),
            OptimAdhrPrev = mean(OptimAdhrPrev),
            PrEPHighAdr = mean(PrEPHighAdr),
            OptimRetnStarts = sum(OptimRetnStarts),
            OptimRetnPrev = mean(OptimRetnPrev),
            PrEPStopsInd = sum(PrEPStopsInd),
            PrEPStopsRand = sum(PrEPStopsRand),
            PrEPStopsRandOptim = sum(PrEPStopsRandOptim)) %>%
  as.data.frame()
dfr$POIP <- sim$param$prep.optim.init.prob
dfr$PSPO <- sim$param$prep.start.prob.optim
dfr$POAC <- sim$param$prep.optim.adhr.cap
dfr$PADO <- sim$param$prep.adhr.dist.optim[3]
dfr$PORC <- sim$param$prep.optim.retn.cap
dfr$PDRO <- sim$param$prep.discont.rate.optim
dfr$infAvert <- NA
dfr$scenario <- "n500"
dfr <- select(dfr, scenario, everything())


# Counterfactual Scenarios ------------------------------------------------

fn <- list.files("data/", pattern = "sim", full.names = TRUE)
fn <- fn[1]

load(fn)
plot(sim, y = c("OptimAdhrStarts", "OptimRetnStarts"))

counter_df_byyear <- function(fn, dfr) {
  load(fn)
  sim <- truncate_sim(sim, at = 261)
  dfc <- as.data.frame(sim, out = "mean")
  dfc <- dfc[-nrow(dfc), ]
  dfc$years <- rep(1:10, each = 52)
  df2 <- group_by(dfc, years) %>%
    summarise(incid = sum(incid),
              prepCurr = mean(prepCurr),
              OptimInitStarts = sum(OptimInitStarts),
              OptimInitPrev = mean(OptimInitPrev),
              PrEPStarts = sum(PrEPStarts),
              PrEPStartsOptim = sum(PrEPStartsOptim),
              OptimAdhrStarts = sum(OptimAdhrStarts),
              OptimAdhrPrev = mean(OptimAdhrPrev),
              PrEPHighAdr = mean(PrEPHighAdr),
              OptimRetnStarts = sum(OptimRetnStarts),
              OptimRetnPrev = mean(OptimRetnPrev),
              PrEPStopsInd = sum(PrEPStopsInd),
              PrEPStopsRand = sum(PrEPStopsRand),
              PrEPStopsRandOptim = sum(PrEPStopsRandOptim)) %>%
    as.data.frame()
  df2$POIP <- sim$param$prep.optim.init.prob
  df2$PSPO <- sim$param$prep.start.prob.optim
  df2$POAC <- sim$param$prep.optim.adhr.cap
  df2$PADO <- sim$param$prep.adhr.dist.optim[3]
  df2$PORC <- sim$param$prep.optim.retn.cap
  df2$PDRO <- sim$param$prep.discont.rate.optim
  df2$infAvert <- dfr$incid - df2$incid
  df2$scenario <- strsplit(fn, "[.]")[[1]][2]
  df2 <- select(df2, scenario, everything())
  return(df2)
}
counter_df_byyear(fn, dfr)

strsplit(fn[1], "[.]")[[1]][2]



# Scale Up on Hyak --------------------------------------------------------

library("EpiModelHIV")
library("dplyr")
library("doParallel")
library("foreach")

load("data/sim.n500.rda")

sim <- truncate_sim(sim, at = 261)
df <- as.data.frame(sim, out = "mean")
df <- df[-nrow(df), ]
dim(df)
head(df)
tail(df)
dfr <- df
head(dfr)

years <- rep(1:10, each = 52)
dfr$years <- years
head(dfr)

dfr <- group_by(dfr, years) %>%
  summarise(incid = sum(incid),
            prepCurr = mean(prepCurr),
            OptimInitStarts = sum(OptimInitStarts),
            OptimInitPrev = mean(OptimInitPrev),
            PrEPStarts = sum(PrEPStarts),
            PrEPStartsOptim = sum(PrEPStartsOptim),
            OptimAdhrStarts = sum(OptimAdhrStarts),
            OptimAdhrPrev = mean(OptimAdhrPrev),
            PrEPHighAdr = mean(PrEPHighAdr),
            OptimRetnStarts = sum(OptimRetnStarts),
            OptimRetnPrev = mean(OptimRetnPrev),
            PrEPStopsInd = sum(PrEPStopsInd),
            PrEPStopsRand = sum(PrEPStopsRand),
            PrEPStopsRandOptim = sum(PrEPStopsRandOptim)) %>%
  as.data.frame()
dfr$POIP <- sim$param$prep.optim.init.prob
dfr$PSPO <- sim$param$prep.start.prob.optim
dfr$POAC <- sim$param$prep.optim.adhr.cap
dfr$PADO <- sim$param$prep.adhr.dist.optim[3]
dfr$PORC <- sim$param$prep.optim.retn.cap
dfr$PDRO <- sim$param$prep.discont.rate.optim
dfr$infAvert <- NA
dfr$scenario <- "n500"
dfr <- select(dfr, scenario, everything())
dfr

fn <- list.files("data/", pattern = "sim.n1[0-4][0-9][0-9].rda", full.names = TRUE)
fn <- list.files("data/", pattern = "sim.n1[5-9][0-9][0-9].rda", full.names = TRUE)

# create yearly dataset
registerDoParallel(detectCores())
tdf <- foreach(i = 1:length(fn)) %dopar% {
  counter_df_byyear(fn[i], dfr)
}
stopImplicitCluster()

for (i in 1:length(fn)) {
  df <- counter_df_byyear(fn[i], dfr)
  if (i == 1) {
    tdf <- df
  } else {
    tdf <- rbind(tdf, df)
  }
  cat("\n File ", fn[i], " complete ...")
}

tdf <- do.call("rbind", tdf)
dim(tdf)
head(tdf, 30)

all <- rbind(dfr, tdf)
head(all, 30)

all$PinfAvert <- all$infAvert/all$incid
hist(all$PinfAvert)

saveRDS(all, file = "data/prepOptim-Yearly-v2-500per.rds", compress = "xz")
saveRDS(all, file = "data/prepOptim-Yearly-v2-100per.rds", compress = "xz")

