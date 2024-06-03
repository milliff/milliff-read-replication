# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-29
#
# Script Name: vb-placement.R
#
# Script Description: Recreates models of VB school placement in Tables A.13 and A.14
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")
library(tidyverse); library(estimatr); library(modelsummary); 

# LOAD DATA ------------------------------------
load("./data/main_df_28May24.RData")



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Table A13, A14 ---------------------------------------------------------------

df <- df |> mutate(any_hc = ifelse(count > 0, 1, 0),
                             any_fat = ifelse(fatal > 0, 1, 0),
                             any_vb = ifelse(vb_count > 0, 1, 0),
                             mean_time_decision = as.numeric(mean_time_decision))

vb_presence <- lm_robust(any_vb ~ I(prim_school-vb_count) + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_number <- lm_robust(vb_count ~ I(prim_school-vb_count) + tot_pop, data = df, clusters = district, fixed_effects = state_ut)


vb_presence_rel <- lm_robust(any_vb ~ pc_muslim + pc_christian + pc_sc + pc_st + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_number_rel <- lm_robust(vb_count ~ pc_muslim + pc_christian + pc_sc + pc_st + tot_pop, data = df, clusters = district, fixed_effects = state_ut)

vb_presence_chr <- lm_robust(any_vb ~ pc_christian + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_number_chr <- lm_robust(vb_count ~ pc_christian + tot_pop, data = df, clusters = district, fixed_effects = state_ut)

vb_presence_scst <- lm_robust(any_vb ~ I(pc_sc + pc_st) + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_number_scst <- lm_robust(vb_count ~ I(pc_sc + pc_st) + tot_pop, data = df, clusters = district, fixed_effects = state_ut)

vb_presence_wfd <- lm_robust(any_vb ~ wf_male + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_number_wfd <- lm_robust(vb_count ~ wf_male + tot_pop, data = df, clusters = district, fixed_effects = state_ut)

vb_presence_dev <- lm_robust(any_vb ~  dirt_road +  tdist_100k + tdist_500k + nightlight_08 + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_number_dev <- lm_robust(vb_count ~  dirt_road +  tdist_100k + tdist_500k + nightlight_08 + tot_pop, data = df, clusters = district, fixed_effects = state_ut)

vb_presence_all <- lm_robust(any_vb ~ I(prim_school - vb_count) + pc_muslim + pc_christian + pc_sc + pc_st + wf_male + dirt_road +  tdist_100k + tdist_500k + nightlight_08 + tot_pop, data = df, clusters = district, fixed_effects = state_ut)
vb_count_all  <- lm_robust(vb_count ~ I(prim_school - vb_count) + pc_muslim + pc_christian + pc_sc + pc_st + wf_male + dirt_road +  tdist_100k + tdist_500k + nightlight_08 + tot_pop, data = df, clusters = district, fixed_effects = state_ut)

modelsummary::modelsummary(list(vb_presence, vb_presence_rel, vb_presence_chr, vb_presence_scst, vb_presence_wfd, vb_presence_dev, vb_presence_all), stars = T, title = "Correlations with VB School Presence", 
                           coef_map = c("I(prim_school - vb_count)" = "Non-VB Primary School Count",
                                        "vb_count" = "VB Schools",
                                        
                                        "pc_sc" = "% SC Population",
                                        "pc_st" = "% ST Population",
                                        "pc_illt" = "% Illiterate Population",
                                        "pc_male" = "% Male Population",
                                        
                                        "pc_muslim" = "% Muslim Population",
                                        "pc_christian" = "% Christian Population",
                                        "I(pc_sc + pc_st)" = "% SC + ST",
                                        "wf_male" = "Male Main Employment",
                                        "tar_road" = "% Settlements w/ Paved Road",
                                        "dirt_road" = "% Settlements w/ Improved Road",
                                        "power_all" = "% Settlements Electrified",
                                        "tdist_100k" = "Distance to Town > 100k",
                                        "tdist_500k" = "Distance to Town > 500k",
                                        "nightlight_08" = "Avg. Nightlight Emission, 2008",
                                        "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                        "vio_crim" = "IPC Violent Crime Count",
                                        "tot_crim" = "IPC Total Crime Count",
                                        "tot_pop" = "Population"
                           ), output = "./results/tabs/TabA14.tex")

modelsummary::modelsummary(list(vb_number, vb_number_rel, vb_number_chr, vb_number_scst, vb_number_wfd, vb_number_dev, vb_count_all), stars = T, title = "Correlations with VB School Count", 
                           coef_map = c("I(prim_school - vb_count)" = "Non-VB Primary School Count",
                                        "vb_count" = "VB Schools",
                                        
                                        "pc_sc" = "% SC Population",
                                        "pc_st" = "% ST Population",
                                        "pc_illt" = "% Illiterate Population",
                                        "pc_male" = "% Male Population",
                                        
                                        "pc_muslim" = "% Muslim Population",
                                        "pc_christian" = "% Christian Population",
                                        "I(pc_sc + pc_st)" = "% SC + ST",
                                        "wf_male" = "Male Main Employment",
                                        "tar_road" = "% Settlements w/ Paved Road",
                                        "dirt_road" = "% Settlements w/ Improved Road",
                                        "power_all" = "% Settlements Electrified",
                                        "tdist_100k" = "Distance to Town > 100k",
                                        "tdist_500k" = "Distance to Town > 500k",
                                        "nightlight_08" = "Avg. Nightlight Emission, 2008",
                                        "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                        "vio_crim" = "IPC Violent Crime Count",
                                        "tot_crim" = "IPC Total Crime Count",
                                        "tot_pop" = "Population"
                           ), output = "./results/tabs/TabA13.tex")
