# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-28
#
# Script Name: treatment_duration_estimation.R
#
# Script Description: Estimates models in Figures 6, 7, and 8; as well as Figure A.3 and Table A.8
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



# Figure 6 ----------------------------------------------------------------

count_mod_ff <- lm_robust(count ~ vb_count + final_founding + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                            wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                            dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                            mean_time_decision + vio_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)
fatal_mod_ff <- lm_robust(fatal ~ vb_count + final_founding + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                            wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                            dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                            mean_time_decision + vio_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)
ctfir_mod_ff <- lm_robust(counterFIR ~ vb_count + final_founding + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                            wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                            dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                            mean_time_decision + vio_crim + ucdp, data = df, clusters = district, fixed_effects = state_ut)


rbind.data.frame(broom::tidy(count_mod_ff), broom::tidy(fatal_mod_ff), broom::tidy(ctfir_mod_ff)) |> 
  filter(term == "vb_count") |> 
  ggplot() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, y = estimate, x = outcome, color = outcome)) + 
  geom_hline(yintercept = 0, color = "grey", lty = 3) + 
  ylim(-0.005, 0.023) +
  labs(x = "Outcome", y = "Point Estimate\n(Outcome ~ VB Schools)",
       caption = "With State/UT FEs, district-clustered SEs,\nCovs. for demography, local public goods, court delays, crime stats + VB Founding Year") + 
  scale_x_discrete(labels = c("count" = "HC Count", 
                              "counterFIR" = "Counter FIR Count",
                              "fatal" = "Fatal HC Count")) + 
  theme_bw() + 
  scale_color_taylor() + 
  theme(legend.position = "none")
ggsave("./results/figs/Fig6.png", width = 10, height = 8, dpi = 300)


# Figure 7 ----------------------------------------------------------------

df$vb_pres <- ifelse(df$vb_count > 0, 1, 0)

flex <- df |> select(count, vb_count, final_founding, district, state_ut,
                           tot_pop, pc_sc, pc_st, pc_illt, pc_male, wf_male, pc_muslim,
                           pc_christian, tar_road, dirt_road, tdist_100k, tdist_500k, nightlight_08, mean_time_decision, vio_crim) |>
  mutate(district = as.factor(district),
         state_ut = as.factor(state_ut)) |> 
  as.data.frame() |> interflex::interflex(estimator = "binning", Y = "count", D = "vb_count", X = "final_founding",
                                          Z = c("state_ut", "tot_pop", "pc_sc", "pc_st", "pc_illt",
                                                "pc_male", "wf_male", "pc_muslim", "pc_christian", 
                                                "tar_road", "dirt_road", "tdist_100k", "tdist_500k", "nightlight_08", "mean_time_decision", "vio_crim"),
                                          na.rm = T, Xlabel = "VB School Founding Year", Ylabel = "Hate Crime Count",
                                          Dlabel = "VB Schools",  cl = "district", nbins = 3)

png("./results/figs/Fig7.png")
plot(flex, bin.labs = F, theme.bw = T) + labs(title = "Marginal effect of VB School count on Hate Crimes", subtitle = "Moderated by Founding Date of School",
                                              caption = "Includes all controls from main specification\nErrors clustered by district, State FEs."
)
dev.off()


# Figure 8 ----------------------------------------------------------------

count_mod_duration   <- lm_robust(count ~ school_years, data = df, clusters = district, fixed_effects = state_ut)
fatal_mod_duration   <- lm_robust(fatal ~ school_years, data = df, clusters = district, fixed_effects = state_ut)
ctfir_mod_duration   <- lm_robust(counterFIR ~ school_years, data = df, clusters = district, fixed_effects = state_ut)
modelsummary::modelsummary(list("HC Count" = count_mod_duration, "Fatal HC Count" = fatal_mod_duration, "Counter FIR Count" = ctfir_mod_duration), stars = T, title = "Correlation between VB Schools and Attempted Lynching Count With State FEs",  coef_omit = "state")



rbind.data.frame(broom::tidy(count_mod_duration), broom::tidy(fatal_mod_duration), broom::tidy(ctfir_mod_duration)) |> 
  filter(term == "school_years") |> 
  ggplot() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, y = estimate, x = outcome, color = outcome)) + 
  geom_hline(yintercept = 0, color = "grey", lty = 3) + 
  ylim(-0.001, 0.01) +
  labs(x = "Outcome", y = "Point Estimate\n(Outcome ~ VB Schools)",
       caption = "Schools X Years of Presence\nHC2 Robust Errors, Clustered by District, State FEs") + 
  scale_x_discrete(labels = c("count" = "HC Count", 
                              "counterFIR" = "Counter FIR Count",
                              "fatal" = "Fatal HC Count")) + 
  theme_bw() + 
  scale_color_taylor() + 
  theme(legend.position = "none")
ggsave("./results/figs/Fig8.png", width = 10, height = 8, dpi = 300)


# Figure A3 ---------------------------------------------------------------

count_mod_total   <- lm_robust(count ~ total_years, data = df, clusters = district, fixed_effects = state_ut)
fatal_mod_total   <- lm_robust(fatal ~ total_years, data = df, clusters = district, fixed_effects = state_ut)
ctfir_mod_total   <- lm_robust(counterFIR ~ total_years, data = df, clusters = district, fixed_effects = state_ut)
modelsummary::modelsummary(list("HC Count" = count_mod_total, "Fatal HC Count" = fatal_mod_total, "Counter FIR Count" = ctfir_mod_total), stars = T, title = "Correlation between VB Schools and Attempted Lynching Count With State FEs",  coef_omit = "state")



rbind.data.frame(broom::tidy(count_mod_total), broom::tidy(fatal_mod_total), broom::tidy(ctfir_mod_total)) |> 
  filter(term == "total_years") |> 
  ggplot() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, y = estimate, x = outcome, color = outcome)) + 
  geom_hline(yintercept = 0, color = "grey", lty = 3) + 
  ylim(-0.001, 0.01) +
  labs(x = "Outcome", y = "Point Estimate\n(Outcome ~ VB Schools)",
       caption = "Total Years of Presence\nHC2 Robust Errors, Clustered by District, State FEs") + 
  scale_x_discrete(labels = c("count" = "HC Count", 
                              "counterFIR" = "Counter FIR Count",
                              "fatal" = "Fatal HC Count")) + 
  theme_bw() + 
  scale_color_taylor() + 
  theme(legend.position = "none")
ggsave("./results/figs/FigA3.png", width = 10, height = 8, dpi = 300)



# Table A8 ----------------------------------------------------------------

count_mod_ff   <- lm_robust(count ~ vb_count  + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road +  tdist_100k + tdist_500k + nightlight_08  +final_founding, data = df, clusters = district, fixed_effects = state_ut)
fatal_mod_ff   <- lm_robust(fatal ~  vb_count  + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road +  tdist_100k + tdist_500k + nightlight_08 +final_founding, data = df, clusters = district, fixed_effects = state_ut) 
counter_mod_ff <- lm_robust(counterFIR ~ vb_count  + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male + pc_muslim + pc_christian + prim_school + tar_road + dirt_road +  tdist_100k + tdist_500k + nightlight_08 +final_founding, data = df, clusters = district, fixed_effects = state_ut) 



modelsummary::modelsummary(list("HC Count" = count_mod_ff, "Fatal HC Count" = fatal_mod_ff, "Counter FIR Count" = counter_mod_ff), stars = T,
                           title = "Correlation between VB Schools and Attempted Lynching Count With School Founding Year and Other Controls",
                           add_rows = data.frame("State FEs?", "Y", "Y", "Y"),
                           coef_omit = "state", output = "./results/tabs/TabA8.tex")


