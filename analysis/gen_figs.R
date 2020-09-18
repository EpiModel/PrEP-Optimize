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

ggplot(df_prep_10yr, aes(x = POIP, y = pCov, color = PORC_cat)) +
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

# Figure 9

# Figure 10

# Figure 11

# Figure 12

# Figure 13

# Figure 14

# Figure 15
### Removed ###

# Figure 16

# Figure 17

# Figure 18

# Figure 19

# Figure 20

# Table 1

# Figure 21

# Figure 22

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


