
## PrEP-Optim Analysis: Build Datasets

library("methods")
suppressMessages(library("EpiModelHIV"))
suppressMessages(library("dplyr"))
suppressMessages(library("doParallel"))
suppressMessages(library("foreach"))

load("data/sim.n0000.rda")

sim <- truncate_sim(sim, at = 261)
df <- as.data.frame(sim, out = "mean")
df <- df[-nrow(df), ]
# dim(df)
# head(df)
# tail(df)
dfr <- df
# head(dfr)

years <- rep(1:10, each = 52)
dfr$years <- years
# head(dfr)

dfr <- group_by(dfr, years) %>%
  summarise(incid = sum(incid),
            prepElig = mean(prepElig),
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
dfr$scenario <- "n0000"
dfr <- select(dfr, scenario, everything())
dfr


fn <- list.files("data/", pattern = "*.rda", full.names = TRUE)
fn

# create yearly dataset
counter_df_byyear <- function(fn, dfr) {
  load(fn)
  sim <- truncate_sim(sim, at = 261)
  dfc <- as.data.frame(sim, out = "mean")
  dfc <- dfc[-nrow(dfc), ]
  dfc$years <- rep(1:10, each = 52)
  df2 <- group_by(dfc, years) %>%
    summarise(incid = sum(incid),
              prepElig = mean(prepElig),
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
  # cat(" \t ", fn)
  return(df2)
}
counter_df_byyear(fn[1], dfr)

registerDoParallel(detectCores()-1)
tdf <- foreach(i = 1:length(fn)) %dopar% {
  counter_df_byyear(fn[i], dfr)
}
stopImplicitCluster()

tdf <- do.call("rbind", tdf)
dim(tdf)
head(tdf, 30)

all <- rbind(dfr, tdf)
head(all, 30)

all$PinfAvert <- all$infAvert/all$incid
all$pCov <- all$prepCurr/all$prepElig

saveRDS(all, file = "data/prepOptim-Yearly-v4-1000sets-250per.rds", compress = "xz")

system("scp mox:/gscratch/csde/sjenness/poptim/data/*.rds analysis/data")
