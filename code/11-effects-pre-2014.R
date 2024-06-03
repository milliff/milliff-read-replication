# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-30
#
# Script Name: effects-pre-2014.R
#
# Script Description: Separates estimation into pre- and post-2014 elections; creates Figures [TKTK] as well as Tables [TKTK]
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")

library(tidyverse);library(modelsummary);library(tayloRswift)

# LOAD DATA ------------------------------------
load("./data/twoperiod_df.RData")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Pre-Post Model ----------------------------------------------------------

# Estimation code shown below; Running  models is time intensive (due to unit-clustered errors and unit FEs), so R model objects  are loadable from file as a default.

# twoperiod_count_nofe <- lm_robust(n ~ vb_count*period, data = df, fixed_effects = district, clusters = id.x) 
# twoperiod_count  <- lm_robust(n ~ vb_count*period, data = df, fixed_effects = id.x)

# postperiod_count <- lm_robust(n ~ vb_count + tot_pop, data = df |> filter(period == "count_post14"), fixed_effects = district, clusters = id.x)
# preperiod_count  <- lm_robust(n ~ vb_count + tot_pop, data = df |> filter(period == "count_pre14"), fixed_effects = district, clusters = id.x)
load("./data/twoperiod_nofe_model.RData")
load("./data/twoperiod_fe_model.RData")
load("./data/preperiod_fe_model.RData")
load("./data/postperiod_fe_model.RData")

modelsummary(list("Interaction (Dist. FE)" = twoperiod_count_nofe,"Interaction (Unit FE)" = twoperiod_count, "Pre-Period" = preperiod_count, "Post-Period" = postperiod_count), stars = T,
                           coef_map = c("vb_count" = "VB School Count (post-2014)",
                                        "periodcount_pre14" = "Pre-2014 Indicator",
                                        "vb_count:periodcount_pre14" = "VB Schools x pre-2014"),
                           add_rows =  data.frame("FEs By:", "District", "Subdistrict", "District", "District"),
                           output = "./results/tabs/TabA20.tex")

modelplot(list("Interaction (Dist FE)" = twoperiod_count_nofe, "Interaction (Unit FE)" = twoperiod_count, "Pre-2014 Only" = preperiod_count, "Post-2014 Only" = postperiod_count),
                        coef_map = c("vb_count" = "VB School Count",
                                     "periodcount_pre14" = "Pre-2014 Indicator",
                                     "vb_count:periodcount_pre14" = "VB Schools x pre-2014")) +   
  scale_color_taylor(palette = "taylorRed" ) + labs(title = "Varying Association of VB School Presence on pre- vs post-2014 Hate crimes") + geom_vline(xintercept = 0, lty = 3, color = "darkgrey")
ggsave("./results/figs/FigA5.png", height = 8, width = 11, dpi = 300)
