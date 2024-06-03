# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-29
#
# Script Name: diff-in-diff-estimation.R
#
# Script Description: Estimates models for Figures A.6 and A.7 as well as Table A.21
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")
library(did); library(didimputation); library(tidyverse); library(DRDID); library(did2s); library(DIDmultiplegt); library(di)

# LOAD DATA ------------------------------------
load("./data/did_df.RData")

# Prep data
panel <- panel |> mutate(numeric_id = as.numeric(c_code01))


panel <- panel |> mutate(pc_sc = tot_sc/tot_pop,
                         pc_st = tot_st/tot_pop,
                         pc_illt = tot_illt/tot_pop,
                         pc_male = m_pop/tot_pop,
                         wf_male = m_mnw/m_pop)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------




##### Figure A6; Table A21 --------------------------------------------------------------


# Borusyak, Jaravel, and Speiss (2023) ------------------------------------
# Add'l assumption of restricted causal effects; BUT! Keeps always-treated units.

borusyak <- did_imputation(data = panel, yname = "count", gname = "final_founding", tname = "year",
                           idname = "numeric_id", cluster_var = "district")

data.frame(Estimate = borusyak$estimate, SE = borusyak$std.error, CI_low = borusyak$conf.low, CI_hi = borusyak$conf.high) |> knitr::kable(caption = "", format = "latex") |> kableExtra::save_kable(file = "./results/tabs/TabA14.tex")

borusyak_es <- did_imputation(data = panel, yname ="count", gname = "final_founding", tname = "year",
                              idname = "numeric_id", cluster_var = "district",
                              horizon = T, pretrends = -5:-1)
borusyak_es |> 
  mutate(term = as.numeric(term),
         t_sig = factor(ifelse(estimate/std.error > 2.576, 3, #99%
                               ifelse(estimate/std.error > 1.96, 2, #95%
                                      ifelse(estimate/std.error > 1.64, 1, 0))))) |> #90% 
  ggplot() + 
  annotate("rect", xmin = -5, xmax = 72, ymin = borusyak$conf.low, ymax = borusyak$conf.high,
           alpha = .5,fill = "grey") +
  geom_segment(aes(x = -5, y = borusyak$estimate, xend = 72, yend = borusyak$estimate), color = "grey") +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0, lty = 2, color = "darkgrey") +
  geom_pointrange(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = factor(t_sig, ))) +
  theme_bw() +
  labs(title = "Staggered Treatment DiD Estimates, VB Schools and Hate Crimes",
       subtitle = "Average Effect by Time from Treatment with Imputation Estimation from Borusyak, Jaravel, and Speiss (2023)",
       x = "Time Period pre/post Treatment",
       y = "Estimate",
       color = "Sig. Level") +
  scale_color_manual(labels = c("<90%", "90%", "95%", "99%"), values=c("#D7D3CB",  "#BDADAB", "#43475B", "#B1532A"))
ggsave("./results/figs/FigA6.png", width = 11, height = 8, dpi = 300)





###### Figure A7 Aggregation Plot --------------------------------------------------------

# Callaway and Sant'Anna (2021) -------------------------------------------
# Drops units already treated in period 1 (2008)

out <- att_gt(yname = "count",
              gname = "final_founding",
              idname = "numeric_id",
              tname = "year",
              xformla = ~ 1 ,
              data = panel,
              est_method = "reg")
summary(out)
ggdid(out)

# Event study interpretation of group-time effects

es <- aggte(out, type = "dynamic")

# de Chaisemartin and D'Haultfoeuille (2020)-------------------------------------
# Drops units already treated in period 1 (2008)

chaisemartin <- did_multiplegt(df = panel, Y = "count", G = "numeric_id",
                               T = "year", D = "vb_treat",
                               dynamic = 5, placebo = 5, brep = 5, # Too few, but it's very slow, so this is just to get the thing to run; I've saved a plot
                               parallel = T)

ests <-  chaisemartin[grepl("^placebo_|^effect|^dynamic_", names(chaisemartin))]
ret  <-  data.frame(
  term      = names(ests),
  estimate  = as.numeric(ests),
  std.error = as.numeric(chaisemartin[grepl("^se_placebo|^se_effect|^se_dynamic", names(chaisemartin))]),
  N         = as.numeric(chaisemartin[grepl("^N_placebo|^N_effect|^N_dynamic", names(chaisemartin))])
) |>
  # For CIs we'll assume standard normal distribution
  within({
    conf.low  = estimate - std.error*(qnorm(1-(1-.95)/2))
    conf.high = estimate + std.error*(qnorm(1-(1-.95)/2))
  })

# Sun and Abraham 2020 ----------------------------------------------------
# Drops units already treated in period 1 (2008)
# Cohort specific ATTs

out_sunab <- feols(count ~ sunab(final_founding, year) +  state_ut + tot_pop + pc_sc + pc_st + pc_illt + pc_male + wf_male | numeric_id + year, data = panel)
out_sunab
coefplot(out_sunab, main = "Staggered Treatment DiD Estimates, VB Schools and Hate Crimes",
         sub = "Average Effect by Time from Treatment via Sun and Abraham (2020) Estimator",
         xlab = "Vidya Bharati School Count",
         ylab = "Estimate and 95% CI",
         ci_width = "5%")


# Create a plot with all the ES estimates together. 

chaisemartin <- ret |>
  within({
    term = gsub("^placebo_", "-", term)
    term = gsub("^effect", "0", term)
    term = gsub("^dynamic_", "", term)
    term = as.integer(term)
  }) |> mutate(method = "dCDH") |> select(-N)

sunab <- out_sunab |> etable() |> as.data.frame()
colnames(sunab) <- c("term", "estimate")
sunab <- sunab |> 
  slice(3:17) |> 
  within({
    term = gsub("^year = ","",term)
    term = as.integer(term)
  }) |> 
  mutate(method = "SA") |> 
  separate_wider_delim(cols = "estimate", delim = " ", names = c("estimate", "std.error")) |> 
  within({
    estimate = gsub("\\*", "", estimate)
    estimate = gsub("\\.$", "", estimate)
    std.error = gsub("^\\(", "", std.error)
    std.error = gsub("\\)$", "", std.error)
    estimate = as.numeric(estimate)
    std.error = as.numeric(std.error)
  }) |> 
  mutate(conf.high = estimate + 1.96*std.error,
         conf.low = estimate - 1.96*std.error)

santanna <- data.frame(term = es$egt,
                       estimate = es$att.egt,
                       std.error = es$se.egt) |> 
  mutate(method = "CA",
         conf.high = estimate + 1.96*std.error,
         conf.low = estimate - 1.96*std.error)

borusyak_es <- borusyak_es |> select(-lhs) |> mutate(method = "BJS",
                                                     term = as.numeric(term))

plot_frame <- bind_rows(chaisemartin, sunab, santanna, borusyak_es)

ggplot(plot_frame) +
  geom_vline(xintercept = 0, lty = 2, color = "darkgrey") +
  geom_hline(yintercept = 0, lty = 2, color = "darkgrey") +
  geom_pointrange(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, group = method, color = method, shape = method), position = position_dodge(width = .5)) +
  theme_bw() + scale_color_taylor() +
  labs(title = "Comparing DiD Estimation Methods",
       subtitle= "Effect of VB School Opening on Hate Crime Count",
       x = "Years Since Treatment",
       y = "Estimate",
       color = "Estimator",
       shape = "Estimator",
       caption = "BJS = Borusyak, Jaravel, and Speiss (2021)\nCA = Callaway and Sant'Anna (2021)\ndCDH = de Chaisemartin and D'Haultfœuille (2020)\nSA = Sun and Abraham (2020)")

ggplot(plot_frame) +
  geom_vline(xintercept = 0, lty = 2, color = "darkgrey") +
  geom_hline(yintercept = 0, lty = 2, color = "darkgrey") +
  geom_pointrange(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, group = method, color = method, shape = method), position = position_dodge(width = .5)) +
  theme_bw() + scale_color_taylor() + xlim(c(-7,7)) + ylim(-0.0013,0.0013) +
  labs(title = "Comparing DiD Estimation Methods",
       subtitle= "Effect of VB School Opening on Hate Crime Count",
       x = "Years Since Treatment",
       y = "Estimate",
       color = "Estimator",
       shape = "Estimator",
       caption = "BJS = Borusyak, Jaravel, and Speiss (2021)\nCA = Callaway and Sant'Anna (2021)\ndCDH = de Chaisemartin and D'Haultfœuille (2020)\nSA = Sun and Abraham (2020)")
ggsave("./results/figs/FigA7.png", width = 11, height = 8, dpi = 300)
