library(directlabels)
library(viridis)
library(metR)
library(lubridate)
library(scales)
library(RColorBrewer)
#source("analysis/vis.gam.custom.R")
gam <- readRDS("gam.rda")
rm(list = setdiff(ls(), c("gam", "vis.gam.custom")))

# dat <- vis.gam.custom(gam,
#                       view = c("POIP", "PORC"),
#                       plot.type = "contour",
#                       type = "response",
#                       xlab = "Retention Capacity",
#                       ylab = "Uptake Capacity",
#                       main = "Figure 5: % Infections Averted Contour - Uptake/Retention",
#                       n.grid = 300)

# Linear costs for PrEP support program capacities
cost.i <- (12/6 * 10 * (80 * 395 * 100) + (10 * (46.5 + 0.1*(2*40 + 2*17.85 + 2*80.31)) * 80 * 100))
cost.a <- ((315.18+246+3.90) * 10)
cost.r <- 10 * (60.5 + 0.1*(40*4 + 17.85*2 + 80.31*2 + 9.46*2) + 12.44)

n <- 100

grid.df <- expand.grid(POIP = seq(0, 0.10, length.out = round(n * (.10 * cost.i) / (2000 * cost.r))),
                       POAC_yr = 0,
                       PORC = seq(0, 2000, length.out = n))
pred <- exp(predict(gam,
                    newdata = grid.df))
pred.df <- cbind(pred, grid.df)


plot.dat <- data.frame(POIP = pred.df$POIP,
                       PORC = pred.df$PORC,
                       POIP_c = pred.df$POIP * cost.i,
                       PORC_c = pred.df$PORC * cost.r,
                       budget = pred.df$POIP * cost.i + pred.df$PORC * cost.r,
                       avert_pct = pred.df$pred)

load("optim_res_costsharing.rda")
res <- res %>% filter(converge == 0)
res.plot.dat <- data.frame(POIP = res$poip,
                           PORC = res$porc,
                           POIP_c = res$poip * cost.i,
                           PORC_c = res$porc * cost.r,
                           budget = res$poip * cost.i + res$porc * cost.r,
                           avert_pct = res$infAvert)

res <- res %>% filter(converge == 0)
res.plot.labs <- data.frame(POIP = res$poip,
                           PORC = res$porc,
                           POIP_c = res$poip * cost.i,
                           PORC_c = res$porc * cost.r,
                           budget = res$poip * cost.i + res$porc * cost.r,
                           avert_pct = res$infAvert) %>% slice(1, 16, 36, 64, 92)


ggplot(data = plot.dat, aes(x = POIP_c/1000, y = PORC_c/1000, z = avert_pct)) +
  geom_tile(aes(x = POIP_c/1000, y = PORC_c/1000, fill = avert_pct)) +
  stat_contour(color = "black") +
  #scale_fill_viridis(discrete=FALSE, option = "B") + theme_classic() +
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"),
                       values = scales::rescale(c(0, 5, 7.5, 10))) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_cartesian() +
  labs(title = "",
       x = "Uptake Budget (per $1,000)",
       y = "Retention Budget (per $1,000)") +
geom_text_contour(aes(z = avert_pct), nudge_x = -50, nudge_y = -50, color = "black", alpha = .5, check_overlap = FALSE) +
  coord_fixed(ratio = 1) +
  geom_line(data = res.plot.dat, aes(x = POIP_c/1000, y = PORC_c/1000), linetype = 1, size = 1, color = "black") +
  geom_point(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000), size = 2) +
  geom_text(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000, label = paste0("$", round(budget/1000000, 1), "m")), nudge_x = 100, nudge_y = -50)


ggplot(data = plot.dat, aes(x = POIP_c/1000, y = PORC_c/1000, z = avert_pct)) +
  geom_tile(aes(x = POIP_c/1000, y = PORC_c/1000, fill = avert_pct)) +
  stat_contour(color = "black") +
  #scale_fill_viridis(discrete=FALSE, option = "B") + theme_classic() +
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
  geom_text(data = res.plot.labs, aes(x = POIP_c/1000, y = PORC_c/1000, label = paste0("$", round(budget/1000000, 1), "m")), nudge_x = 100, nudge_y = -50)


