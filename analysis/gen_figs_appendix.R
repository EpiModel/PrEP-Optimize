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
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rds")

fig1 <- ggplot(df_prep_10yr, aes(x = POAC_yr, y = PrEPHighAdr, color = POIP_cat)) +
  geom_jitter() +
  scale_color_manual(values = c("#D55E00", "#999999", "#009E73")) +
  labs(title = "Figure 1",
       y = "High Adherence Proportion",
       x = "Yearly Adherence Intervention Capacity",
       color = "Initiation Intervention Capacity Level") +
  theme_classic()

# Figure 3
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rds")

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
gam <- readRDS(file = "analysis/optim_data/gam.rds")

vis.gam(gam,
        view = c("PORC", "POIP"),
        type = "response",
        theta = -45,
        xlab = "Retention Capacity",
        ylab = "Uptake Capacity",
        zlab = "% Infections Averted",
        main = "Figure 4: % Infections Averted 3D - Uptake/Retention")

# Figure 6
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rds")

vis.gam(gam, view = c("POAC_yr", "POIP"),
        type = "response",
        theta = -45,
        xlab = "Adherence Capacity",
        ylab = "Uptake Capacity",
        zlab = "% Infections Averted",
        main = "Figure 6: % Infections Averted 3D - Uptake/Adherence")

# Figure 7
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rds")

vis.gam(gam,
        view = c("POAC_yr", "POIP"),
        plot.type = "contour",
        type = "response",
        xlab = "Adherence Capacity",
        ylab = "Uptake Capacity",
        main = "Figure 7: % Infections Averted Contour - Uptake/Adherence")

# Figure 8
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rds")

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
gam <- readRDS(file = "analysis/optim_data/gam.rds")

vis.gam(gam,
        view = c("POAC_yr", "PORC"),
        plot.type = "contour",
        type = "response",
        xlab = "Adherence Capacity",
        ylab = "Retention Capacity",
        main = "Figure 9: % Infections Averted Contour - Retention/Adherence")

# Figure 11
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
poac <- readRDS(file = "analysis/optim_data/fig11_df.rds")

fig11 <- ggplot(poac, aes(x = POAC_yr, y = pred, color = PORC_cat)) +
  geom_point() +
  facet_wrap(.~as.factor(POIP_cat)) +
  labs(y = "Predicted Percent Infections Averted (%)",
       x = "Adherence Intervention Capacity",
       color = "Retention Intervention Capacity",
       title = "Figure 11") +
  theme_classic()

# Figure 12
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
poip <- readRDS(file = "analysis/optim_data/fig12_df.rds")

fig12 <- ggplot(poip, aes(x = POIP, y = pred, color = PORC_cat)) +
  geom_point() +
  facet_wrap(.~as.factor(POAC_cat)) +
  labs(y = "Predicted Percent Infections Averted (%)",
       x = "Uptake Intervention Capacity",
       color = "Retention Intervention Capacity",
       title = "Figure 12") +
  theme_classic()

# Figure 13
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rds")

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
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rds")

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
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rds")

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
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rds")

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
res_plot <- readRDS(file = "analysis/optim_data/res_plot.rds")

fig18 <- porc_plot <- ggplot(data = res_plot, aes(x = budget/1000, y = porc)) +
  geom_area() +
  labs(y = "Retention Capacity",
       x = "Budget (per $1,000)",
       title = "Figure 18") +
  theme_classic()+
  scale_x_continuous(breaks = round(seq(min(res_plot$budget/1000), max(res_plot$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(100, 2000, by = 100), 2), expand = c(0, 0))

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


