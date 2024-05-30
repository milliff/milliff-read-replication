# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-29
#
# Script Name: additional-estimation.R
#
# Script Description: Replicates main-estimation.R with different functional forms and different GLMs in tables A6 and A7.
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")

# LOAD DATA ------------------------------------
load("../data/main_df_28May24.RData")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Table A6 ----------------------------------------------------------------

df <- df |> mutate(any_hc = ifelse(count > 0, 1, 0),
                             any_fat = ifelse(fatal > 0, 1, 0),
                             any_vb = ifelse(vb_count > 0, 1, 0),
                             mean_time_decision = as.numeric(mean_time_decision))

count_mod_ext <- lm_robust(count ~ any_vb, data = df, clusters = district, fixed_effects = state_ut)
count_mod_ext_cov <- lm_robust(count ~ any_vb + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                                 wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                                 dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                                 mean_time_decision + vio_crim, data = df, clusters = district, fixed_effects = state_ut)
count_mod_ext_ext <- lm_robust(any_hc ~ any_vb + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                                 wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                                 dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                                 mean_time_decision + vio_crim, data = df, clusters = district, fixed_effects = state_ut)
count_mod_fat_ext <- lm_robust(any_fat~ any_vb + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                                 wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                                 dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                                 mean_time_decision + vio_crim, data = df, clusters = district, fixed_effects = state_ut)


modelsummary::modelsummary(list(count_mod_ext, count_mod_ext_cov, count_mod_ext_ext, count_mod_fat_ext), stars = T,
                           coef_map = c("any_vb" = "VB Schools (Ext. Margin)",
                                        "tot_pop" = "Population",
                                        "pc_sc" = "% SC Population",
                                        "pc_st" = "% ST Population",
                                        "pc_illt" = "% Illiterate Population",
                                        "pc_male" = "% Male Population",
                                        "wf_male" = "Male Main Employment",
                                        "pc_muslim" = "% Muslim Population",
                                        "pc_christian" = "% Christian Population",
                                        "prim_school" = "Total Primary Schools",
                                        "tar_road" = "% Settlements w/ Paved Road",
                                        "dirt_road" = "% Settlements w/ Improved Road",
                                        "power_all" = "% Settlements Electrified",
                                        "tdist_100k" = "Distance to Town > 100k",
                                        "tdist_500k" = "Distance to Town > 500k",
                                        "nightlight_08" = "Avg. Nightlight Emission, 2008",
                                        "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                        "vio_crim" = "IPC Violent Crime Count",
                                        "tot_crim" = "IPC Total Crime Count"), output = "../results/tabs/TabA6.tex")


# Table A7 ----------------------------------------------------------------

nb_count <- MASS::glm.nb(count ~vb_count + state_ut + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                           wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                           dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                           mean_time_decision + vio_crim, data = df)

pois_count <- glm(count ~vb_count + state_ut + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                    wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                    dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                    mean_time_decision + vio_crim, data = df, family = "poisson")


modelsummary::modelsummary(list("Negative Binomial" = nb_count, "Poisson" =pois_count), stars = T, 
                           title = "VB Schools and Hate Crime Counts, Zero-Inflated Models with State FEs", coef_map = c("vb_count" = "VB Schools",
                                                                                                                         "tot_pop" = "Population",
                                                                                                                         "pc_sc" = "% SC Population",
                                                                                                                         "pc_st" = "% ST Population",
                                                                                                                         "pc_illt" = "% Illiterate Population",
                                                                                                                         "pc_male" = "% Male Population",
                                                                                                                         "wf_male" = "Male Main Employment",
                                                                                                                         "pc_muslim" = "% Muslim Population",
                                                                                                                         "pc_christian" = "% Christian Population",
                                                                                                                         "prim_school" = "Total Primary Schools",
                                                                                                                         "tar_road" = "% Settlements w/ Paved Road",
                                                                                                                         "dirt_road" = "% Settlements w/ Improved Road",
                                                                                                                         "power_all" = "% Settlements Electrified",
                                                                                                                         "tdist_100k" = "Distance to Town > 100k",
                                                                                                                         "tdist_500k" = "Distance to Town > 500k",
                                                                                                                         "nightlight_08" = "Avg. Nightlight Emission, 2008",
                                                                                                                         "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                                                                                                         "vio_crim" = "IPC Violent Crime Count",
                                                                                                                         "tot_crim" = "IPC Total Crime Count"), output = "../results/tabs/TabA7.tex")
