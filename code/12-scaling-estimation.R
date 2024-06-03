# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-30
#
# Script Name: 12-scaling-estimation.R
#
# Script Description: Scaled predictors 
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")
library(tidyverse); library(estimatr); library(modelsummary)

# LOAD Data ------------------------------------
load("./data/main_df_28May24.RData")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Scale the Main predictor ------------------------------------------------

scaled_vb <- lm_robust(count ~ scale(vb_count) + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                         wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                         dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                         mean_time_decision + vio_crim,
                       data = df, fixed_effects = state_ut, clusters = district)
scaled_hc <- lm_robust(scale(count) ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                         wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                         dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                         mean_time_decision + vio_crim,
                       data = df, fixed_effects = state_ut, clusters = district)

scaled_both <- lm_robust(scale(count) ~ scale(vb_count) + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                         wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                         dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                         mean_time_decision + vio_crim,
                       data = df, fixed_effects = state_ut, clusters = district)

modelsummary::modelsummary(list("Scaled VB" = scaled_vb, "Scaled HC" = scaled_hc, "Both Scaled" = scaled_both), stars = T,
                           coef_map = c("scale(vb_count)" = "VB Count (scaled)",
                                        "vb_count" = "VB Count (Raw)"),
                           add_rows = data.frame(c("Covs.", "State FEs?"),
                                                 c("All", "Yes"),
                                                 c("All", "Yes"),
                                                 c("All", "Yes")),
                           output = "./results/tabs/TabA9.tex")
