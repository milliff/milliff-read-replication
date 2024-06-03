# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-28
#
# Script Name: main-estimation.R
#
# Script Description: Estimates main models in Figures 3 and 4, as well as Tables A3, A4, and A5.
#
#
# Notes:
#
#
# --------------------------------------------



# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")

library(tidyverse); library(estimatr); library(modelsummary); library(tayloRswift)


# LOAD DATA ------------------------------------
load("./data/main_df_28May24.RData")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# Figure 3 ----------------------------------------------------------------

count_mod <- lm_robust(count ~ vb_count, data = df, fixed_effects = state_ut, clusters = district)
fatal_mod <- lm_robust(fatal ~ vb_count, data = df, fixed_effects = state_ut, clusters = district)
ctfir_mod <- lm_robust(counterFIR ~ vb_count, data = df, fixed_effects = state_ut, clusters = district)

rbind.data.frame(broom::tidy(count_mod), broom::tidy(fatal_mod), broom::tidy(ctfir_mod)) |> 
  ggplot() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, y = estimate, x = outcome, color = outcome)) + 
  geom_hline(yintercept = 0, color = "grey", lty = 3) + 
  ylim(-0.005, 0.023) +
  labs(x = "Outcome", y = "Point Estimate\n(Outcome ~ VB Schools)",
       caption = "With State/UT FEs, district-clustered SEs") + 
  scale_x_discrete(labels = c("count" = "HC Count", 
                              "counterFIR" = "Counter FIR Count",
                              "fatal" = "Fatal HC Count")) + 
  theme_bw() + 
  scale_color_taylor() + 
  theme(legend.position = "none")
ggsave("./results/figs/Fig3.png", width = 10, height = 8, dpi = 300)


# Figure 4 ----------------------------------------------------------------

count_mod_cov <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                             wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                             dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                             mean_time_decision + vio_crim,
                           data = df, fixed_effects = state_ut, clusters = district)
fatal_mod_cov <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                             wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                             dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                             mean_time_decision + vio_crim,
                           data = df, fixed_effects = state_ut, clusters = district)
ctfir_mod_cov <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                             wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                             dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                             mean_time_decision + vio_crim,
                           data = df, fixed_effects = state_ut, clusters = district)

rbind.data.frame(broom::tidy(count_mod_cov), broom::tidy(fatal_mod_cov), broom::tidy(ctfir_mod_cov)) |> 
  filter(term == "vb_count") |> 
  ggplot() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, y = estimate, x = outcome, color = outcome)) + 
  geom_hline(yintercept = 0, color = "grey", lty = 3) + 
  ylim(-0.005, 0.023) +
  labs(x = "Outcome", y = "Point Estimate\n(Outcome ~ VB Schools)",
       caption = "With State/UT FEs, district-clustered SEs,\nCovs. for demography, local public goods, court delays, crime stats") + 
  scale_x_discrete(labels = c("count" = "HC Count", 
                              "counterFIR" = "Counter FIR Count",
                              "fatal" = "Fatal HC Count")) + 
  theme_bw() + 
  scale_color_taylor() + 
  theme(legend.position = "none")
ggsave("./results/figs/Fig4.png", width = 10, height = 8, dpi = 300)




# Table A3 ----------------------------------------------------------------

a3_c1 <- lm_robust(count ~ vb_count, data = df)
a3_c2 <- lm_robust(count ~ vb_count, data = df, clusters = district)
a3_c3 <- lm_robust(count ~ vb_count, data = df, clusters = district, fixed_effects = state_ut)
a3_c4 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male, data = df, clusters = district, fixed_effects = state_ut)
a3_c5 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian, data = df, clusters = district, fixed_effects = state_ut)
a3_c6 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08, data = df, clusters = district, fixed_effects = state_ut)
a3_c7 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision, data = df, clusters = district, fixed_effects = state_ut)
a3_c8 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)
a3_c9 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp + change_pc_muslim + change_pc_christian, data = df, clusters = district, fixed_effects = state_ut)
a3_c10 <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp + change_pc_muslim + change_pc_christian + BJP_share_14 + BJP_gov_17, data = df, clusters = district)

modelsummary::modelsummary(list(a3_c1, a3_c2, a3_c3, a3_c4, a3_c5, a3_c6, a3_c7, a3_c8, a3_c9, a3_c10),
                           coef_map = c("vb_count" = "VB Schools",
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
                                        "tot_crim" = "IPC Total Crime Count",
                                        "ucdp" = "Pre-2008 UCDP Events",
                                        "change_pc_muslim" = "Change in Muslim Pop. 2001-2011",
                                        "change_pc_christian" = "Change in Christian Pop. 2001-2011",
                                        "BJP_share_14" = "2014 BJP Voteshare (State)",
                                        "BJP_gov_17" = "BJP Gov. in Power (State)?"),
                           stars = T, 
                           add_rows = data.frame("State FEs?", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
                           title = "Models of Hate Crime Count",
                           output = "./results/tabs/TabA3.tex")

# Table A4 ----------------------------------------------------------------

a4_c1 <- lm_robust(fatal ~ vb_count, data = df)
a4_c2 <- lm_robust(fatal ~ vb_count, data = df, clusters = district)
a4_c3 <- lm_robust(fatal ~ vb_count, data = df, clusters = district, fixed_effects = state_ut)
a4_c4 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male, data = df, clusters = district, fixed_effects = state_ut)
a4_c5 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian, data = df, clusters = district, fixed_effects = state_ut)
a4_c6 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school +  tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08, data = df, clusters = district, fixed_effects = state_ut)
a4_c7 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision, data = df, clusters = district, fixed_effects = state_ut)
a4_c8 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)
a4_c9 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp + change_pc_muslim + change_pc_christian, data = df, clusters = district, fixed_effects = state_ut)
a4_c10 <- lm_robust(fatal ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp + change_pc_muslim + change_pc_christian + BJP_share_14 + BJP_gov_17, data = df, clusters = district)

modelsummary::modelsummary(list(a4_c1, a4_c2, a4_c3, a4_c4, a4_c5, a4_c6, a4_c7, a4_c8, a4_c9, a4_c10),
                           coef_map = c("vb_count" = "VB Schools",
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
                                        "tot_crim" = "IPC Total Crime Count",
                                        "ucdp" = "Pre-2008 UCDP Events",
                                        "change_pc_muslim" = "Change in Muslim Pop. 2001-2011",
                                        "change_pc_christian" = "Change in Christian Pop. 2001-2011",
                                        "BJP_share_14" = "2014 BJP Voteshare (State)",
                                        "BJP_gov_17" = "BJP Gov. in Power (State)?"),
                           stars = T, 
                           add_rows = data.frame("State FEs?", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
                           title = "Models of Fatal Hate Crime Count",
                           output = "./results/tabs/TabA4.tex")

# Table A5 ----------------------------------------------------------------

a5_c1 <- lm_robust(counterFIR ~ vb_count, data = df)
a5_c2 <- lm_robust(counterFIR ~ vb_count, data = df, clusters = district)
a5_c3 <- lm_robust(counterFIR ~ vb_count, data = df, clusters = district, fixed_effects = state_ut)
a5_c4 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male, data = df, clusters = district, fixed_effects = state_ut)
a5_c5 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian, data = df, clusters = district, fixed_effects = state_ut)
a5_c6 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08, data = df, clusters = district, fixed_effects = state_ut)
a5_c7 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision, data = df, clusters = district, fixed_effects = state_ut)
a5_c8 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)
a5_c9 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp + change_pc_muslim + change_pc_christian, data = df, clusters = district, fixed_effects = state_ut)
a5_c10 <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road + tdist_100k + tdist_500k + nightlight_08 + mean_time_decision + vio_crim + ucdp + change_pc_muslim + change_pc_christian + BJP_share_14 + BJP_gov_17, data = df, clusters = district)

modelsummary::modelsummary(list(a5_c1, a5_c2, a5_c3, a5_c4, a5_c5, a5_c6, a5_c7, a5_c8, a5_c9, a5_c10),
                           coef_map = c("vb_count" = "VB Schools",
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
                                        "tot_crim" = "IPC Total Crime Count",
                                        "ucdp" = "Pre-2008 UCDP Events",
                                        "change_pc_muslim" = "Change in Muslim Pop. 2001-2011",
                                        "change_pc_christian" = "Change in Christian Pop. 2001-2011",
                                        "BJP_share_14" = "2014 BJP Voteshare (State)",
                                        "BJP_gov_17" = "BJP Gov. in Power (State)?"),
                           stars = T, 
                           add_rows = data.frame("State FEs?", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
                           title = "Models of Counter-FIR Count",
                           output = "./results/tabs/TabA5.tex")
