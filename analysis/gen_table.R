# Load packages
library(dplyr)
library(psych)
library(ggplot2)
library(tidyverse)
library(mgcv)
library(patchwork)
library(kableExtra)
library(nima)
library(viridis)
library(metR)
library(lubridate)
library(scales)
library(RColorBrewer)

# Table 1
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
# Linear costs for PrEP support program capacities
cost.i <- (12/6 * 10 * (80 * 395 * 100) + (10 * (46.5 + 0.1*(2*40 + 2*17.85 + 2*80.31)) * 80 * 100))
cost.a <- ((315.18+246+3.90) * 10)
cost.r <- 10 * (60.5 + 0.1*(40*4 + 17.85*2 + 80.31*2 + 9.46*2) + 12.44)
load("analysis/optim_data/optim_res_table.rda")
table_optim <- res
table_optim <- table_optim %>% mutate(poip_spending = commas(round(poip * cost.i, 0)),
                                      poac_spending = commas(round(poac * cost.a, 0)),
                                      porc_spending = commas(round(porc * cost.r, 0)),
                                      poip_spending_pct = format(round(100 * poip * cost.i / budget, 1), nsmall = 1),
                                      poac_spending_pct = format(round(100 * poac * cost.a / budget, 1), nsmall = 1),
                                      porc_spending_pct = format(round(100 * porc * cost.r / budget, 1), nsmall = 1),
                                      infections_averted_ct = format(round(1199.484*infAvert/100, 1), nsmall = 1),
                                      infections_averted_pct = format(round(infAvert, 1), nsmall = 1),
                                      dollars_per_inf = commas(round(budget / (1199.484*infAvert/100), 0)),
                                      poip = commas(round(100*poip, 1)),
                                      poac = commas(round(poac, 0)),
                                      porc = commas(round(porc, 0))) %>%
  select(c(poip, poac, porc,
           poip_spending, poac_spending, porc_spending,
           poip_spending_pct, poac_spending_pct, porc_spending_pct,
           infections_averted_ct, infections_averted_pct, dollars_per_inf))

t_table_optim <- table_optim %>%
  tibble::rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from=rowname, values_from=value)
colnames(t_table_optim) <- c("Budget constraint", "@700,000", "@1,500,000",
                             "@3,000,000", "@4,500,000", "@6,000,000")

t_table_optim$"Budget constraint" <- c("Initiation percentage (%)",
                                       "Adherence capacity",
                                       "Retention capacity",
                                       "Uptake",
                                       "Adherence",
                                       "Retention",
                                       "Uptake",
                                       "Adherence",
                                       "Retention",
                                       "10-year cumulative infections averted",
                                       "Percentage 10-year cumulative infections averted (%)",
                                       "Cost per infection averted (@)")

t_table_optim %>% kable(format = "html", align = "lrrrrrr", escape = FALSE) %>%
  pack_rows(index = c("Optimal parameter value" = 3,
                      "Total spending by intervention (@)" = 3,
                      "Percentage of spending by intervention (%)" = 3,
                      "Outcomes" = 3)) %>% kable_styling("striped", full_width = TRUE) %>%
  gsub("@", "$", .)

readr::write_csv(t_table_optim, "analysis/T1.csv")
