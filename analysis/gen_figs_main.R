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
library(patchwork)

# Figure 2
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rds")

fig2 <- ggplot(df_prep_10yr, aes(x = PORC, y = pCov, color = POIP_cat)) +
  geom_jitter() +
  scale_color_manual(labels = c("Low", "Medium", "High"),
                     values = c("#D55E00", "#999999", "#009E73")) +
  labs(y = "PrEP Coverage",
       x = "Retention Capacity",
       color = "Initiation Capacity") +
  theme_bw() +
  theme(
    # aspect.ratio = 1/4,
    # strip.background = element_rect(fill = "black"),
    legend.margin = margin(-8, 0, -2, 0),
    legend.position = "bottom"
  )
fig2
# ggsave(filename = "analysis/output_files/Plots/fig2.png")
ggsave(filename = "analysis/Fig1a.pdf", height = 6, width = 6)

# Figure 5
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")
contour <- readRDS("analysis/optim_data/heatmap_dat.rds")

### Using your sample code more directly, if you prefer
# ggplot(contour, aes(x = POIP, y = PORC, z = avert_pct)) +
#   geom_raster(aes(fill = avert_pct), interpolate = TRUE) +
#   geom_contour(aes(z = avert_pct), col = "white", alpha = 0.5, lwd = 0.5) +
#   geom_text_contour(aes(z = avert_pct), stroke = 0.1, size = 3.5) +
#   theme_minimal() +
#   scale_y_continuous(expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(x = "Initiation Percentage", y = "Retention Capacity") +
#   scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1)

### Using an adaption of my previous heatmap ggplot.
fig5 <- ggplot(data = contour, aes(x = POIP*100, y = PORC, z = avert_pct)) +
  geom_tile(aes(x = POIP*100, y = PORC, fill = avert_pct)) +
  stat_contour(color = "black") +
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"),
                       values = scales::rescale(c(0, 5, 7.5, 10))) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma, expand = c(0,0)) +
  scale_x_continuous(label = comma, expand = c(0,0)) +
  coord_cartesian() +
  labs(title = "",
       x = "Initiation Percentage (%)",
       y = "Retention Capacity") +
  geom_text_contour(aes(z = avert_pct), nudge_x = .5, color = "black", alpha = 1, check_overlap = FALSE)

# pdf("analysis/Fig1b.pdf", height = 6, width = 6)
# par(mar=c(3,3,1,1), mgp = c(2,1,0))
# vis.gam(gam,
#         view = c("PORC", "POIP"),
#         plot.type = "contour",
#         type = "response",
#         xlab = "Retention Capacity",
#         ylab = "Uptake Capacity",
#         main = "")
# dev.off()


# Figure 10
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
porc <- readRDS(file = "analysis/optim_data/fig10_df.rds")

fig10 <- ggplot(porc, aes(x = PORC, y = pred, color = POAC_cat)) +
  geom_line(size = 1.25) +
  facet_wrap(.~as.factor(POIP_cat)) +
  labs(y = "Infections Averted (%)",
       x = "Retention Capacity",
       color = "Adherence Capacity") +
  scale_color_manual(labels = c("Low", "Medium", "High"),
                     values = c("#D55E00", "#999999", "#009E73")) +
  theme_bw() +
  theme(
    legend.margin = margin(-8, 0, -2, 0),
    legend.position = "bottom"
  ) +
  geom_ribbon(aes(ymin = pred.ll, ymax = pred.ul, fill = POAC_cat), alpha = 0.4, linetype = "dashed")
fig10
ggsave(filename = "analysis/Fig2.pdf", height = 6, width = 12)

ggsave(filename = "analysis/output_files/Plots/fig10.png")

# Figure 19
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop.rds")

fig19 <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area()+
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000), max(res_plot_area_prop$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0)) +
  theme(
    legend.position = "none"
  )
fig19
ggsave(filename = "analysis/output_files/Plots/fig19.png")

# Figure 20
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area.rds")

fig20 <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0, 0))
fig20

ggsave(filename = "analysis/output_files/Plots/fig20.png")

# Figure 21
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop_adh.rds")

fig21 <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area() +
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000), max(res_plot_area_prop$budget/1000), by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0)) +
  theme(
    legend.position = "none"
  )
fig21
ggsave(filename = "analysis/output_files/Plots/fig21.png")

# Figure 22
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area_adh.rds")

fig22 <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw()+
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000), max(res_plot_area$budget/1000), by = 500), 2), expand = c(0, 0))
fig22
ggsave(filename = "analysis/output_files/Plots/fig22.png")

# Figure 23 (previously unnumbered)?
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
ggsave(filename = "analysis/output_files/Plots/fig23.png")

# Figure 1: Two panel figure combining Figure 2 and Figure 5 from the summary doc
# fig 5 is not a ggplot object so my usual way of combining plots with the patchwork package won't work
# If you need me to, I can try to replicate the vis.gam plot in ggplot

# Figure 2: Two panel figure combining Figure 19 and Figure 21 from the summary doc
fig19 | fig21

# Alternatively, Figure 10 as a single panel plot
fig10

# Alternatively, Fig 19 and 20 as one two-panel plot, and 21 and 22 as another
fig19 | fig20
ggsave("analysis/Fig3.pdf", height = 6, width = 12)

fig21 | fig22
ggsave("analysis/Fig4.pdf", height = 6, width = 12)

# Or as a 4-panel
(fig19 | fig20) / (fig21 | fig22)
