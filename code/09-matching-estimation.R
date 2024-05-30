# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-29
#
# Script Name: matching-estimation.R
#
# Script Description: Estimates VB-HC association with matching and balancing, produces Figures [TKTK] as well as Tables [TKTK]
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS./. \n\n", sep = "")

# LOAD DATA ------------------------------------
load(".//data/main_df_28May24.RData")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Weighting (Entropy Balance) ----------------------------------------------------

w1 <- weightit(vb_count ~ pc_st  + pc_muslim + pc_christian , data =  df |> drop_na(count, vb_count, pc_st, pc_muslim, pc_christian), method = "ebal")

fit <- lm_weightit(count ~ vb_count * (pc_st  + pc_muslim + pc_christian ), data = df |> drop_na(count, vb_count, pc_st, pc_muslim, pc_christian), weightit = w1)

values <- with(df |> drop_na(count, vb_count, pc_st, pc_muslim, pc_christian),
               seq(quantile(vb_count, .1),
                   quantile(vb_count, .9),
                   length.out = 20))

avg_predictions(fit, variables = list(vb_count = values)) |> 
  ggplot(aes(x = vb_count)) +
  geom_line(aes(y = estimate), color = swift_pal()(4)[1]) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = .3, fill = swift_pal()(4)[4]) +
  labs(x = "VB School Count", y = "E[HC Count|VB Count]",
       title = "Expected HC Count at different levels of VB Presence",
       subtitle = "Estimates from entropy-balanced weighted regression",
       caption = "EBal on: % Muslim, % Christian, % ST population") +
  theme_bw() + scale_color_taylor()
ggsave(".//results/figs/ebalance_weighting.png", width = 11, height = 8, dpi = 300)


# Matching (Genetic; Nearest Neighbor) ------------------------------------


cpt_df <- df |> drop_na(tot_pop, pc_sc, pc_st, pc_muslim, pc_christian, pc_illt, dirt_road, tdist_500k, nightlight_08, prim_school, state_ut) |> mutate(Tr = vb_count > 0) |> dplyr::select(tot_pop, pc_sc, pc_st, pc_muslim, pc_christian, pc_illt, dirt_road, tdist_500k, nightlight_08, prim_school, state_ut, Tr, count)


# Match code shown below; Running the matches is time intensive (especially genetic matching), so R objects produced by `matchit()` are loadable from file as a default.

# nnm <- matchit(Tr ~ tot_pop + pc_sc + pc_st + pc_muslim + pc_christian + pc_illt + dirt_road + tdist_500k + nightlight_08 + prim_school,
               # data = cpt_df, method = "nearest")
# genm <- matchit(Tr ~ tot_pop + pc_sc + pc_st + pc_muslim + pc_christian + pc_illt + dirt_road + tdist_500k + nightlight_08 + prim_school,
                # data = cpt_df, method = "genetic")
# save(genm, file = ".//data/genmatch.Rdata")
# save(nnm, file = ".//data/nearestneighbormatch.RData")
load(".//data/genmatch.RData")
load(".//data/nnmatch.RData")

plot(summary(nnm))
png(filename = ".//results/figs/genmatch_diagn.png", width = 1000, height = 750)
plot(summary(genm))
dev.off()

plot(nnm, type = "density", which.xs = ~ pc_st + pc_muslim + prim_school, interactive = F)
png(filename = ".//results/figs/genmatch_density.png", width =1000, height = 750)
plot(genm, type = "density", which.xs = ~ pc_st + pc_muslim + prim_school, interactive = F)
dev.off()

# We use GenM because the quality is better 
gen.data <- match.data(genm)
gen_fit <- lm(count ~ Tr * (tot_pop + pc_sc + pc_st + pc_muslim + pc_christian + pc_illt + dirt_road + tdist_500k + nightlight_08 + prim_school), data = gen.data, weights = weights)

marginaleffects::avg_comparisons(gen_fit,
                                 variables = "Tr",
                                 newdata = subset(gen.data, Tr == TRUE),
                                 wts = "weights") |> knitr::kable(format = "latex") |> kableExtra::save_kable(file = ".//results/tabs/genmatch_results.tex")
