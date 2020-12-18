
##
## PrEP Optimization Model
## Figures
##

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
library(ggpubr)

# Figure 1 ----------------------------------------------------------------

# Panel A
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
df_prep_10yr <- readRDS(file = "analysis/optim_data/df_prep_10yr.rds")

fig1a <- ggplot(df_prep_10yr, aes(x = PORC, y = pCov, color = POIP_cat)) +
  geom_jitter() +
  scale_color_manual(labels = c("Low", "Medium", "High"),
                     values = c("#D55E00", "#999999", "#009E73")) +
  labs(y = "PrEP Coverage",
       x = "Persistence Capacity",
       color = "Initiation Capacity") +
  theme_bw() +
  theme(
    # aspect.ratio = 1/4,
    # strip.background = element_rect(fill = "black"),
    legend.margin = margin(-8, 0, -2, 0),
    legend.position = "bottom"
  )
fig1a

# Panel B
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
gam <- readRDS(file = "analysis/optim_data/gam.rda")
contour <- readRDS("analysis/optim_data/heatmap_dat.rds")

fig1b <- ggplot(data = contour, aes(x = POIP*100, y = PORC, z = avert_pct)) +
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
       x = "Initiation Capacity (Probability of Starting Intervention)",
       y = "Persistence Capacity") +
  geom_text_contour(aes(z = avert_pct), nudge_x = .5, color = "black", alpha = 1,
                    check_overlap = FALSE)
fig1b
# ggsave(filename = "analysis/Fig1b.pdf", height = 6, width = 6)

fig1a | fig1b
ggsave("analysis/Fig1.pdf", height = 6, width = 12)


# Older approach with built-in gam plot
# pdf("analysis/Fig1b.pdf", height = 6, width = 6)
# par(mar=c(3,3,1,1), mgp = c(2,1,0))
# vis.gam(gam,
#         view = c("PORC", "POIP"),
#         plot.type = "contour",
#         type = "response",
#         xlab = "Persistence Capacity",
#         ylab = "Uptake Capacity",
#         main = "")
# dev.off()


# Figure 2 ----------------------------------------------------------------

rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
porc <- readRDS(file = "analysis/optim_data/fig10_df.rds")
porc$POIP_cat <- factor(porc$POIP_cat,
                        labels = c("Low Initiation", "Medium Initiation",
                                   "High Initiation"))
fig2 <- ggplot(porc, aes(x = PORC, y = pred, color = POAC_cat)) +
  geom_line(size = 1) +
  facet_wrap(.~as.factor(POIP_cat)) +
  labs(y = "Infections Averted (%)",
       x = "Persistence Capacity",
       color = "Adherence Capacity",
       fill = "Adherence Capacity") +
  geom_ribbon(aes(ymin = pred.ll, ymax = pred.ul, fill = POAC_cat),
              alpha = 0.4, color = NA) +
  scale_color_manual(labels = c("Low", "Medium", "High"),
                     values = c("#D55E00", "#999999", "#009E73")) +
  scale_fill_manual(labels = c("Low", "Medium", "High"),
                     values = c("#D55E00", "#999999", "#009E73")) +
  theme_bw() +
  theme(
    legend.margin = margin(-8, 0, -2, 0),
    legend.position = "bottom"
  )
fig2
ggsave(filename = "analysis/Fig2.pdf", height = 6, width = 12)



# Figure 3 ----------------------------------------------------------------

# Panel A
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop.rds")
res_plot_area_prop$program <- factor(res_plot_area_prop$program,
                                     labels = c("Adherence", "Initiation", "Persistence"))

fig3a <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area() +
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000),
                                        max(res_plot_area_prop$budget/1000),
                                        by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0)) +
  theme(
    legend.position = "none"
  )
fig3a

# Panel B
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area.rds")
res_plot_area$program <- factor(res_plot_area$program,
                                labels = c("Adherence", "Initiation", "Persistence"))
fig3b <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0, 0))
fig3b

fig3a | fig3b
ggsave("analysis/Fig3.pdf", height = 6, width = 12)



# Figure 4 ----------------------------------------------------------------

# Panel A
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop_adh.rds")
res_plot_area_prop$program <- factor(res_plot_area_prop$program,
                                     labels = c("Adherence", "Initiation", "Persistence"))
fig4a <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area() +
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000),
                                        max(res_plot_area_prop$budget/1000),
                                        by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0)) +
  theme(
    legend.position = "none"
  )
fig4a

# Panel B
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area_adh.rds")
res_plot_area$program <- factor(res_plot_area$program,
                                labels = c("Adherence", "Initiation", "Persistence"))
fig4b <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0, 0))
fig4b

fig4a | fig4b
ggsave("analysis/Fig4.pdf", height = 6, width = 12)



# Figure 3/4 Combined (Big Fig 3) -----------------------------------------

# Panel A
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop.rds")
res_plot_area_prop$program <- factor(res_plot_area_prop$program,
                                     labels = c("Adherence", "Initiation", "Persistence"))

fig3a <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area() +
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000),
                                        max(res_plot_area_prop$budget/1000),
                                        by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0)) +
  theme(
    legend.position = "none"
  )
fig3a

# Panel B
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area.rds")
res_plot_area$program <- factor(res_plot_area$program,
                                labels = c("Adherence", "Initiation", "Persistence"))
fig3b <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0, 0))
fig3b


# Panel C
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area_prop <- readRDS(file = "analysis/optim_data/res_plot_area_prop_adh.rds")
res_plot_area_prop$program <- factor(res_plot_area_prop$program,
                                     labels = c("Adherence", "Initiation", "Persistence"))
fig3c <- ggplot(res_plot_area_prop, aes(x = budget/1000, y = value, fill = program)) +
  geom_area() +
  labs(y = "Budget Fraction",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area_prop$budget/1000),
                                        max(res_plot_area_prop$budget/1000),
                                        by = 500), 1), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = .1), 2), expand = c(0, 0)) +
  theme(
    legend.position = "none"
  )
fig3c

# Panel D
rm(list = setdiff(ls(), grep("fig", ls(), value = TRUE)))
res_plot_area <- readRDS(file = "analysis/optim_data/res_plot_area_adh.rds")
res_plot_area$program <- factor(res_plot_area$program,
                                labels = c("Adherence", "Initiation", "Persistence"))
fig3d <- ggplot(res_plot_area, aes(x = budget/1000, y = value/1000, fill = program)) +
  geom_area() +
  labs(y = "Funds Allocated (per $,1000)",
       x = "Total Budget Size (per $1,000)",
       fill = "Intervention") +
  theme_bw() +
  scale_x_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0,0)) +
  scale_y_continuous(breaks = round(seq(min(res_plot_area$budget/1000),
                                        max(res_plot_area$budget/1000),
                                        by = 500), 2), expand = c(0, 0))
fig3d

# (fig3a | fig3b) / (fig3c | fig3d) + plot_annotation(tag_levels = 'A')
# ggsave("analysis/Fig3-4-Comb.pdf", height = 12, width = 12)

ggarrange(fig3a, fig3b, fig3c, fig3d, ncol =2, nrow = 2, common.legend = TRUE,
          legend="bottom", labels = "AUTO")
ggsave("analysis/Fig3-4-Comb.pdf", height = 10, width = 10)


# Other -------------------------------------------------------------------

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
       x = "Initiation Budget (per $1,000)",
       y = "Persistence Budget (per $1,000)") +
  geom_text_contour(aes(z = avert_pct), nudge_x = -50, nudge_y = -50,
                    color = "black", alpha = .5, check_overlap = FALSE) +
  coord_fixed(ratio = 1) +
  geom_line(data = res.plot.dat, aes(x = POIP_c/1000, y = PORC_c/1000),
            linetype = 1, size = 1, color = "black") +
  geom_point(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000), size = 2) +
  geom_text(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000,
                                      label = paste0("$", round(budget/1000000, 1), "m")),
            nudge_x = 100, nudge_y = -50)
# ggsave(filename = "analysis/output_files/Plots/fig23.png")

