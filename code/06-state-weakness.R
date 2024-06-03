# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-29
#
# Script Name: state-weakness.R 
#
# Script Description: Reproduces analyses focused on state capacity/weakness in Figure 9 and in Tables A.10, A.11 and A.12
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")

library(tidyverse); library(estimatr); library(modelsummary)
library(tmap); library(scales); library(tayloRswift)

# LOAD DATA ------------------------------------
load("./data/main_df_28May24.RData")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Figure 9 ---------------------------------------------------------------

df$swindex_ad    <- scales::rescale(scales::rescale(as.numeric(df$mean_time_decision)) + scales::rescale(df$dist_to_groads) + scales::rescale(df$access50k) + (1-scales::rescale(df$nightlights10)) + scales::rescale(df$ucdp))
df$swindex_shrug <- scales::rescale(scales::rescale(df$tdist_500k) + (1-scales::rescale(df$tar_road)) + (1-scales::rescale(df$nightlight_08)) + scales::rescale(as.numeric(df$mean_time_decision)))

sw_mod        <- lm_robust(count ~ swindex_shrug , data = df, clusters = district, fixed_effects = state_ut)
court_mod     <- lm_robust(count ~ mean_time_decision , data = df, clusters = district, fixed_effects = state_ut)
remote_mod    <- lm_robust(count ~ tdist_500k , data = df, clusters = district, fixed_effects = state_ut)
unimp_mod     <- lm_robust(count ~ I(1-dirt_road) , data = df, clusters = district, fixed_effects = state_ut)
light_mod     <- lm_robust(count ~ I(1-nightlight_08) , data = df, clusters = district, fixed_effects = state_ut)
component_mod <- lm_robust(count ~ mean_time_decision + tdist_500k + I(1-dirt_road)+ I(1-nightlight_08) , data = df, clusters = district, fixed_effects = state_ut)


modelsummary::modelplot(list("Index" =sw_mod , "Court Delays" = court_mod , "Remoteness" = remote_mod, "Road Improvement" = unimp_mod , "Night Lights" = light_mod), stars = T, title = "State Weakness Models", 
                        coef_map = c("swindex_shrug" = "SW Index",
                                     "mean_time_decision" = "Court Delays", 
                                     "tdist_500k" = "Distance to Town > 500k",
                                     "I(1 - dirt_road)" = "Lacking Imp. Roads",
                                     "I(1 - nightlight_08)" = "Night Lights (rev. score)"
                        )) + labs(title = "State Weakness Predictors of Hate Crimes",
                                  caption = "HC2 Robust errors, clustered by district; state FEs; Separate Models") + geom_vline(xintercept = 0, color = "grey", lty = 3) + scale_color_taylor() + coord_flip() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("./results/figs/Fig9.png", width = 11, height = 8, dpi = 300)
# Table A10 ----------------------------------------------------------------

modelsummary::modelsummary(list(sw_mod, court_mod, remote_mod, unimp_mod, light_mod, component_mod), stars = T, title = "State Weakness Models", 
                           coef_map = c("swindex_shrug" = "State Weakness Index",
                                        "mean_time_decision" = "Avg. Days to Court Decision, 2010", 
                                        "tdist_500k" = "Distance to Town > 500k",
                                        "I(1 - dirt_road)" = "% Settlements without Improved Road",
                                        "I(1 - nightlight_08)" = "Avg. Nightlight Emission, 2008 (rev. score)"
                           ), fmt=5, output = "./results/tabs/TabA10.tex")


# Table A11 ---------------------------------------------------------------

sw_mod <- lm_robust(count ~ swindex_shrug, data = df, clusters = district, fixed_effects = state_ut)
sw_vb_mod  <- lm_robust(count ~ vb_count + swindex_shrug, data = df, clusters = district, fixed_effects = state_ut)
sw2_mod <- lm_robust(count~ swindex_ad, data = df, clusters = district, fixed_effects = state_ut)
sw2_vb_mod  <- lm_robust(count ~ vb_count + swindex_ad, data = df, clusters = district, fixed_effects = state_ut)

modelsummary::modelsummary(list(sw_mod, sw2_mod, sw_vb_mod, sw2_vb_mod), stars = T, title = "State Weakness Models", 
                           coef_map = c("vb_count" = "VB Schools",
                                        "swindex_shrug" = "State Weakness Index (SHRUG)",
                                        "swindex_ad" = "State Weakness Index (AidData)",
                                        "mean_time_decision" = "Avg. Days to Court Decision, 2010", 
                                        "tdist_500k" = "Distance to Town > 500k",
                                        "I(1 - dirt_road)" = "% Settlements without Improved Road",
                                        "I(1 - nightlight_08)" = "Avg. Nightlight Emission, 2008 (rev. score)"
                           ), fmt=3, output = "./results/tabs/TabA11.tex")


# Table A12 ---------------------------------------------------------------


shrug_mod <- lm_robust(count ~ vb_count  + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + tar_road + dirt_road +  tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + tot_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)
aiddata_mod <- lm_robust(count ~ vb_count  + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + dist_to_groads +  access50k + nightlights10 + mean_time_decision + vio_crim + tot_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)

modelsummary::modelsummary(list("SHRUG Covariates" = shrug_mod, "AidData Covariates" = aiddata_mod),
                           coef_omit = "state_ut",
                           coef_map = c("vb_count" = "VB Schools"
                           ),
                           stars = T, 
                           output = "./results/tabs/TabA12.tex")
