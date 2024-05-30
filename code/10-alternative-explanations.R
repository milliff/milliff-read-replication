# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-29
#
# Script Name: alternative-explanations.R
#
# Script Description: Analyses for Figures [TKTK] as well as Tables [TKTK]
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


# School Expansion --------------------------------------------------------

df$pc_school      <- df$prim_school / (df$tot_pop+1) # Some population stats are zero
df$pc_school_novb <- I(df$prim_school - df$vb_count)/ (df$tot_pop+1)
primary_school_model <- lm_robust(count ~ pc_school, data = df, fixed_effects = state_ut, clusters = district)
primary_school_model2 <- lm_robust(count ~ pc_school_novb, data = df, fixed_effects = state_ut, clusters = district)
modelsummary::modelsummary(list(primary_school_model, primary_school_model2), coef_map = c("pc_school" = "Schools per Capita",
                                                                                           "pc_school_novb" = "Non-VB Schools per Capita"),
                           stars = T, output = "../results/tabs/schools_expansion.tex")

# Impunity ----------------------------------------------------------------

cfir_map <- df |> group_by(district) |> 
  summarise(cfir_count = sum(counterFIR),
            hc_count = sum(count),
            cfir_pc = cfir_count/hc_count)

cfir_map_st <- df |> group_by(state_ut) |> 
  summarise(cfir_count = sum(counterFIR),
            hc_count = sum(count),
            cfir_pc = cfir_count/hc_count)

tmap_mode("plot")
tm_shape(cfir_map_st) +
  tm_fill(
    col = "cfir_pc",
    palette = "Reds",
    style = "cont",
    contrast = c(0.15, 1),
    title = "% Counter FIR",
    id = "shapeID",
    showNA = T,
    textNA = "No Hate Crimes",
    alpha = 0.8,
    colorNA = "grey90"
  ) + tm_layout(title = "Prevalence of Counter-FIR Filing in States/UTs with Hate Crimes",
                legend.position = c("right", "bottom")) |> 
  tmap_save(filename = "../results/figs/counter_fir_state_map.png")

tmap_mode("plot")
tm_shape(cfir_map) +
  tm_fill(
    col = "cfir_pc",
    palette = "Reds",
    style = "cont",
    contrast = c(0.15, 1),
    title = "% Counter-FIR",
    id = "shapeID",
    showNA = T,
    textNA = "No Hate Crimes",
    alpha = 0.8,
    colorNA = "grey90"
  ) + tm_layout(title = "Prevalence of Counter-FIR Filing in Districts with Hate Crimes",
                legend.position = c("right", "bottom")) |> 
  tmap_save(filename = "../results/figs/counter_fir_district_map.png")

counter_mod_shr <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                               wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                               dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                               mean_time_decision + vio_crim, data = df, clusters = district, fixed_effects = state_ut) 
counter_mod_ff <- lm_robust(counterFIR ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                              wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                              dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                              mean_time_decision + vio_crim + final_founding, data = df, clusters = district, fixed_effects = state_ut) 
counter_bjp <- lm_robust(counterFIR ~ BJP_gov_17 + BJP_share_14 + vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                           wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                           dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                           mean_time_decision + vio_crim, clusters = district, data = df)

modelsummary::modelsummary(list("Counter FIR (All)" = counter_mod_shr, "Counter FIR (IM)" = counter_mod_ff, "Counter FIR (BJP)" = counter_bjp),
                           coef_rename = c("vb_count" = "VB Schools",
                                           "count" = "HC Count",
                                           "final_founding" = "School Founding Year",
                                           "tot_pop" = "Population",
                                           "pc_sc" = "% SC Population",
                                           "pc_st" = "% ST Population",
                                           "pc_illt" = "% Illiterate Population",
                                           "pc_male" = "% Male Population",
                                           "wf_male" = "Male Main Employment",
                                           "pc_muslim" = "% Muslim Population",
                                           "pc_christian" = "% Christian Population",
                                           "prim_school" = "Total Primary Schools",
                                           "tar_road" = "% Settlements w/ paved road",
                                           "dirt_road" = "% Settlements w/ improved road",
                                           "power_all" = "% Settlements electrified",
                                           "tdist_100k" = "Distance to town > 100k",
                                           "tdist_500k" = "Distance to town > 500k",
                                           "nightlight_08" = "Average Nightlight Emission, 2008",
                                           "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                           "vio_crim" = "IPC Violent Crime Count",
                                           "BJP_share_14" = "2014 BJP Voteshare (State)",
                                           "BJP_gov_17" = "BJP Gov. in Power (State)?"),
                           add_rows = data.frame("State FEs?", "Y", "Y", "N"),
                           stars = T, output = "../results/tabs/counter_fir_models.tex")

# Simple BJP models

bjp_hc      <- lm_robust(count ~ BJP_share_14, data = df, clusters = district)
bjp_hc_cov  <- lm_robust(count ~ BJP_share_14 + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                           wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                           dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                           mean_time_decision + vio_crim, data = df, clusters = district)
bjp_hc_cov_vb  <- lm_robust(count ~ BJP_share_14 + vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                           wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                           dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                           mean_time_decision + vio_crim, data = df, clusters = district)

bjp_gov <-  lm_robust(count ~ BJP_gov_17, data = df, clusters = district)
bjp_gov_cov  <- lm_robust(count ~ BJP_gov_17 + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                           wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                           dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                           mean_time_decision + vio_crim, data = df, clusters = district)
bjp_gov_cov_vb  <- lm_robust(count ~ BJP_gov_17 + vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                              wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                              dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                              mean_time_decision + vio_crim, data = df, clusters = district)

modelsummary::modelsummary(list("BJP Vote Share 2014 (Bivariate)" = bjp_hc, "BJP in State Gov. (Bivariate)" = bjp_gov,
                                "BJP Vote Share 2014" = bjp_hc_cov, "BJP in State Gov." = bjp_gov_cov,
                                "BJP Vote Share 2014 (w/ VB Schools)" = bjp_hc_cov_vb, "BJP in State Gov. (w/ VB Schools)" = bjp_gov_cov_vb),
                           coef_map = c("BJP_share_14" = "2014 BJP Voteshare (State)",
                                           "BJP_gov_17" = "BJP Gov. in Power (State)?",
                                           "vb_count" = "VB Schools",
                                           "count" = "HC Count",
                                           "final_founding" = "School Founding Year",
                                           "tot_pop" = "Population",
                                           "pc_sc" = "% SC Population",
                                           "pc_st" = "% ST Population",
                                           "pc_illt" = "% Illiterate Population",
                                           "pc_male" = "% Male Population",
                                           "wf_male" = "Male Main Employment",
                                           "pc_muslim" = "% Muslim Population",
                                           "pc_christian" = "% Christian Population",
                                           "prim_school" = "Total Primary Schools",
                                           "tar_road" = "% Settlements w/ paved road",
                                           "dirt_road" = "% Settlements w/ improved road",
                                           "power_all" = "% Settlements electrified",
                                           "tdist_100k" = "Distance to town > 100k",
                                           "tdist_500k" = "Distance to town > 500k",
                                           "nightlight_08" = "Average Nightlight Emission, 2008",
                                           "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                           "vio_crim" = "IPC Violent Crime Count",
                                        "(Intercept)"= "(Intercept)"), 
                           stars = T, output = "../results/tabs/bjp_impunity_models.tex")

# Schools --> HCs ---------------------------------------------------------

df$posthoc <- ifelse(df$final_founding > 2014, 1, 0)
df$posthoc[is.na(df$posthoc)] <- 0
df$posthoc09 <- ifelse(df$final_founding > 2009, 1, 0)
df$posthoc09[is.na(df$posthoc09)] <- 0

placebo09 <- lm_robust(count ~ posthoc09 + vb_count, data = df, fixed_effects = state_ut, clusters = district)
placebo14 <- lm_robust(count ~ posthoc + vb_count, data = df, fixed_effects = state_ut, clusters = district)

# Adjusting for total number of schools, subdistricts that are "treated" after violence are systematically *less* likely to have hate crimes

modelsummary::modelsummary(list("Post 2009 VB Construction" = placebo09, "Post 2014 VB Construction" = placebo14),
                           stars = T,
                           coef_map = c("posthoc09" = "VB Founded post-2009",
                                        "posthoc09:vb_count" = "Founded post-2009 X Count",
                                        "posthoc" = "VB Founded post-2014",
                                        "posthoc:vb_count" = "Founded post-2014 X Count",
                                        "vb_count" = "VB School Count (Total)",
                                        "(Intercept)" = "(Intercept)"
                           ),
                           add_rows = data.frame("FEs by:", "State/UT", "State/UT"),
                           output = "../results/tabs/schools_follow_hcs.tex")

# Religious Demography Change ---------------------------------------------

muslim_biv <- lm_robust(count ~ change_pc_muslim, data = df, clusters= district, fixed_effects = state_ut)
christian_biv <- lm_robust(count ~ change_pc_christian, data = df, clusters = district, fixed_effects = state_ut)
count_rel_change <- lm_robust(count ~ vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                                wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                                dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                                mean_time_decision + vio_crim + change_pc_muslim + change_pc_christian, clusters = district, data = df, fixed_effects = state_ut)
fatal_rel_change <- lm_robust(fatal ~  + vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                                wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                                dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                                mean_time_decision + vio_crim + change_pc_muslim + change_pc_christian, clusters = district, data = df, fixed_effects = state_ut)
counter_rel_change <- lm_robust(counterFIR ~  + vb_count + tot_pop + pc_sc + pc_st + pc_illt + pc_male + 
                                  wf_male + pc_muslim + pc_christian + prim_school + tar_road + 
                                  dirt_road +  tdist_100k + tdist_500k + nightlight_08 + 
                                  mean_time_decision + vio_crim + change_pc_muslim + change_pc_christian, clusters = district, data = df, fixed_effects = state_ut)

modelsummary::modelsummary(list("HC Count" = muslim_biv, "HC Count" = christian_biv, "HC Count" = count_rel_change, "Fatal HC Count" = fatal_rel_change, "Counter-FIR Count" = counter_rel_change), stars = T, title = "Correlation between VB Schools and Attempted Lynching Count, Adjusting for Recent Change in Religious Demography",
                           coef_map = c("change_pc_muslim" = "2001-2011 Change in Muslim Pop.",
                                        "change_pc_christian" = "2001-2011 Change in Christian Pop.",
                                        "vb_count" = "VB Schools",
                                        "count" = "HC Count",
                                        "final_founding" = "School Founding Year",
                                        "tot_pop" = "Population",
                                        "pc_sc" = "% SC Population",
                                        "pc_st" = "% ST Population",
                                        "pc_illt" = "% Illiterate Population",
                                        "pc_male" = "% Male Population",
                                        "wf_male" = "Male Main Employment",
                                        "pc_muslim" = "% Muslim Population",
                                        "pc_christian" = "% Christian Population",
                                        "prim_school" = "Total Primary Schools",
                                        "tar_road" = "% Settlements w/ paved road",
                                        "dirt_road" = "% Settlements w/ improved road",
                                        "power_all" = "% Settlements electrified",
                                        "tdist_100k" = "Distance to town > 100k",
                                        "tdist_500k" = "Distance to town > 500k",
                                        "nightlight_08" = "Average Nightlight Emission, 2008",
                                        "mean_time_decision" = "Avg. Days to Court Decision, 2010",
                                        "vio_crim" = "IPC Violent Crime Count",
                                        "(Intercept)"= "(Intercept)"),
                           add_rows = data.frame("State FEs?", "Y", "Y", "Y", "Y", 'Y'),
                           output = "../results/tabs/change_relig_demog.tex")

