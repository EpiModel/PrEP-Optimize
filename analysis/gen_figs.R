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

## Figures numbered according to order in Optim_Summary.rmd

# Figure 1
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rda")

fig1 <- ggplot(df_prep_10yr, aes(x = POAC_yr, y = PrEPHighAdr, color = POIP_cat)) +
  geom_jitter() +
  scale_color_manual(values = c("#D55E00", "#999999", "#009E73")) +
  labs(title = "Figure 1",
       y = "High Adherence Proportion",
       x = "Yearly Adherence Intervention Capacity",
       color = "Initiation Intervention Capacity Level") +
  theme_classic()

# Figure 2
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rda")

fig2 <- ggplot(df_prep_10yr, aes(x = PORC, y = pCov, color = POIP_cat)) +
  geom_jitter() +
  scale_color_manual(values = c("#D55E00", "#999999", "#009E73")) +
  labs(title = "Figure 2",
       y = "PrEP Coverage Proportion",
       x = "Retention Intervention Capacity",
       color = "Initiation Intervention Capacity Level") +
  theme_classic()

# Figure 3
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rda")

fig3 <- ggplot(df_prep_10yr, aes(x = POIP, y = pCov, color = PORC_cat)) +
  geom_jitter() +
  scale_color_manual(values = c("#D55E00", "#999999", "#009E73")) +
  labs(title = "Figure 3",
       y = "PrEP Coverage Proportion",
       x = "PrEP Intervention Initiation Percentage (Uptake Intervention Capacity)",
       color = "Retention Intervention Capacity Level") +
  theme_classic()

# Figure 4
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")

vis.gam(gam,
        view = c("PORC", "POIP"),
        type = "response",
        theta = -45,
        xlab = "Retention Capacity",
        ylab = "Uptake Capacity",
        zlab = "% Infections Averted",
        main = "Figure 4: % Infections Averted 3D - Uptake/Retention")

# Figure 5
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")

vis.gam(gam,
        view = c("PORC", "POIP"),
        plot.type = "contour",
        type = "response",
        xlab = "Retention Capacity",
        ylab = "Uptake Capacity",
        main = "Figure 5: % Infections Averted Contour - Uptake/Retention")

# Figure 6
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")

vis.gam(gam, view = c("POAC_yr", "POIP"),
        type = "response",
        theta = -45,
        xlab = "Adherence Capacity",
        ylab = "Uptake Capacity",
        zlab = "% Infections Averted",
        main = "Figure 6: % Infections Averted 3D - Uptake/Adherence")

# Figure 7
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")

vis.gam(gam,
        view = c("POAC_yr", "POIP"),
        plot.type = "contour",
        type = "response",
        xlab = "Adherence Capacity",
        ylab = "Uptake Capacity",
        main = "Figure 7: % Infections Averted Contour - Uptake/Adherence")

# Figure 8
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")

vis.gam(gam,
        view = c("POAC_yr", "PORC"),
        type = "response",
        theta = -45,
        xlab = "Adherence Capacity",
        ylab = "Retention Capacity",
        zlab = "% Infections Averted",
        main = "Figure 8: % Infections Averted 3D - Retention/Adherence")

# Figure 9
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")

vis.gam(gam,
        view = c("POAC_yr", "PORC"),
        plot.type = "contour",
        type = "response",
        xlab = "Adherence Capacity",
        ylab = "Retention Capacity",
        main = "Figure 9: % Infections Averted Contour - Retention/Adherence")

# Figure 10
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
porc <- readRDS(file = "analysis/optim_data/fig10_df.rda")

fig10 <- ggplot(porc, aes(x = PORC, y = pred, color = POAC_cat)) +
  geom_point() +
  facet_wrap(.~as.factor(POIP_cat)) +
  labs(y = "Predicted Percent Infections Averted (%)",
       x = "Retention Intervention Capacity",
       color = "Adherence Intervention Capacity",
       title = "Figure 10") +
  theme_classic()

# Figure 11
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
poac <- readRDS(file = "analysis/optim_data/fig11_df.rda")

ggplot(poac, aes(x = POAC_yr, y = pred, color = PORC_cat)) +
  geom_point() +
  facet_wrap(.~as.factor(POIP_cat)) +
  labs(y = "Predicted Percent Infections Averted (%)",
       x = "Adherence Intervention Capacity",
       color = "Retention Intervention Capacity",
       title = "Figure 11") +
  theme_classic()

# Figure 12
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
poip <- readRDS(file = "analysis/optim_data/fig12_df.rda")

ggplot(poip, aes(x = POIP, y = pred, color = PORC_cat)) +
  geom_point() +
  facet_wrap(.~as.factor(POAC_cat)) +
  labs(y = "Predicted Percent Infections Averted (%)",
       x = "Uptake Intervention Capacity",
       color = "Retention Intervention Capacity",
       title = "Figure 12") +
  theme_classic()

# Figure 13
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rda")

fig13 <- ggplot(data = res_plot, aes(x = budget/1000, y = infAvert)) +
  geom_area() +
  labs(y = "Predicted Percent Infections Averted (%)",
       x = "Budget (per $1,000)",
       title = "Figure 13") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot$budget/1000), max(res_plot$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 10, by = 1), 2), expand = c(0, 0))

# Figure 14
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rda")

fig14 <- ggplot(data = res_plot, aes(x = budget/1000, y = cost_per_inf)) +
  geom_area() +
  labs(y = "Overall cost per infection averted",
       x = "Budget (per $1,000)",
       title = "Figure 14") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot$budget/1000), max(res_plot$budget/1000), by = 500), 1), expand = c(0,0))

# Figure 15
### Removed ###

# Figure 16
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rda")

fig16 <- ggplot(data = res_plot, aes(x = budget/1000, y = poac)) +
  geom_area() +
  labs(y = "Adherence Capacity",
       x = "Budget (per $1,000)",
       title = "Figure 16") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot$budget/1000), max(res_plot$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.03))

# Figure 17
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rda")

fig17 <- ggplot(data = res_plot, aes(x = budget/1000, y = poip)) +
  geom_area() +
  labs(y = "PrEP Intervention Initiation Percentage",
       x = "Budget (per $1,000)",
       title = "Figure 17") +
  theme_classic() +
  scale_x_continuous(breaks = round(seq(min(res_plot$budget/1000), max(res_plot$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot$poip), max(res_plot$poip), by = 0.01), 2), expand = c(0, 0))

# Figure 18
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rda")

fig18 <- porc_plot <- ggplot(data = res_plot, aes(x = budget/1000, y = porc)) +
  geom_area() +
  labs(y = "Retention Capacity",
       x = "Budget (per $1,000)",
       title = "Figure 18") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot$budget/1000), max(res_plot$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(100, 2000, by = 100), 2), expand = c(0, 0))

# Figure 19
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop.rda")

fig19 <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area()+
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention",
       title = "Figure 19") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000), max(res_plot_area_prop$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0))

# Figure 20
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area.rda")

fig20 <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention",
       title = "Figure 20") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0, 0))

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
colnames(t_table_optim) <- c("Budget constraint", "@700,000", "@1,500,000", "@3,000,000", "@4,500,000", "@6,000,000")
# newrow <- c(NA, NA, NA, NA, NA)
# newrow <- c("", "", "", "", "")
# newrow <- c("-", "-", "-", "-", "-", "-")
# t_table_optim = rbind(newrow, t_table_optim)
# t_table_optim = rbind(t_table_optim[1:4,], newrow, t_table_optim[-(1:4),])
# t_table_optim = rbind(t_table_optim[1:8,], newrow, t_table_optim[-(1:8),])
# t_table_optim = rbind(t_table_optim[1:12,], newrow, t_table_optim[-(1:12),])

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

t_table_optim %>% kable(format = "html", align = "lrrrrrr", escape = FALSE) %>% pack_rows(index = c("Optimal parameter value" = 3,
                                                "Total spending by intervention (@)" = 3,
                                                "Percentage of spending by intervention (%)" = 3,
                                                "Outcomes" = 3)) %>% kable_styling("striped", full_width = TRUE) %>%
  gsub("@", "$", .)


# Figure 21
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop_adh.rda")

fig21 <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area() +
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention",
       title = "Figure 21") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000), max(res_plot_area_prop$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0))

# Figure 22
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- saveRDS(file = "analysis/optim_data/res_plot_area_adh.rda")

fig22 <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention",
       title = "Figure 22") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0, 0))

# Figure 23
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
source("analysis/budget_path_heatmap.R")

fig23 <- ggplot(data = plot.dat, aes(x = POIP_c/1000, y = PORC_c/1000, z = avert_pct)) +
  geom_tile(aes(x = POIP_c/1000, y = PORC_c/1000, fill = avert_pct)) +
  stat_contour(color = "black") +
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"),
                       values = scales::rescale(c(0, 5, 7.5, 10))) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma, expand = c(0,0)) +
  scale_x_continuous(label = comma, expand = c(0,0)) +
  coord_cartesian() +
  labs(title = "",
       x = "Uptake Budget (per $1,000)",
       y = "Retention Budget (per $1,000)") +
  geom_text_contour(aes(z = avert_pct), nudge_x = -50, nudge_y = -50, color = "black", alpha = .5, check_overlap = FALSE) +
  coord_fixed(ratio = 1) +
  geom_line(data = res.plot.dat, aes(x = POIP_c/1000, y = PORC_c/1000), linetype = 1, size = 1, color = "black") +
  geom_point(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000), size = 2) +
  geom_text(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000,
                                      label = paste0("$", round(budget/1000000, 1), "m")), nudge_x = 100, nudge_y = -50)


