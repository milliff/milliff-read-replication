# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-24
#
# Script Name: hcw-descriptive-statistics.R
#
# Script Description: Produces data description and summary statistics for Hate Crime Watch data in Figure 1, Figure 3, Figure A.1, Table A.1
#
#
# Notes:
#
#
# --------------------------------------------

# LOAD DATA ------------------------------------
hc    <- read_csv("../data/hc_locs.csv")
subdi <- st_read("../data/subdistricts.json") |> st_make_valid()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Figure 1 ----------------------------------------------------------------

hc <- hc %>% mutate(yearmon = zoo::as.yearmon(Date)) 

hc %>% 
  mutate(year_event = lubridate::year(yearmon)) %>% 
  group_by(year_event) %>%
  filter(! `Context/Bias Indicator` %in% c("Other", "Communal clashes")) %>% 
  ggplot() + 
  geom_bar(aes(x = as.factor(year_event), group = `Context/Bias Indicator`,
               fill = `Context/Bias Indicator`)) +
  geom_vline(xintercept = "2014", lty = 2, color = "grey") + 
  facet_wrap(~`Context/Bias Indicator`, ncol =  2) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 14, angle = 90),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        strip.text.x = element_text(size = 20),
        legend.position = "none") + 
  labs(title = "", group = "", fill = "",
       x = "", y= "Number of Events") + 
  scale_fill_brewer(palette = "Set1")

ggsave("../results/figs/Fig1.png", width = 10, height = 8, dpi = 300)


# Figure 3 ---------------------------------------------------------------

hc_in_subdi <- hc %>% 
  filter(complete.cases(lon, lat)) %>% 
  mutate_at(vars(lon, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326, # WGS84
    stringsAsFactors = F,
    remove = T
  ) 

hc_in_subdi <- hc_in_subdi|> 
  st_join(
    subdi, join = st_within
    )

hc_count <- count(as_tibble(hc_in_subdi), id)
hc_subdi_df <- left_join(subdi, hc_count, by = "id") |> 
  mutate(n_noNAs = ifelse(is.na(n), 0, n))

tmap_mode("plot")
tm_shape(hc_subdi_df) +
  tm_fill(
    col = "n_noNAs",
    palette = "Reds",
    style = "cont",
    contrast = c(0.1, 1),
    title = "Hate Crime Incidents",
    id = "shapeID",
    showNA = FALSE,
    alpha = 0.8
  )

tmap_save(filename = "../results/figs/Fig3b.png")


# Figure A1 ---------------------------------------------------------------

hc %>% group_by(yearmon) %>% 
  mutate(fatality_rate = sum(Fatal)/n()) %>% 
  ggplot() + 
  geom_smooth(aes(x = yearmon, y = fatality_rate*100), span = 1,
              color = ptol_pal()(2)[2]) +
  geom_bar(aes(x = yearmon, group = factor(Fatal),
               fill = factor(Fatal)), color = "white") +theme_minimal() +
  scale_y_continuous("Count", 
                     sec.axis = sec_axis(~ ., name = "% Fatal")) +
  scale_fill_ptol() +
  labs(title = "Hate Crime Events by Fatality Outcome",
       fill = "Fatalities?",
       x = "Year, Month",
       caption = "Percent Fatal Events, Smoothed with 1-year span.")
ggsave("../results/figs/FigA1.png", width = 10, height = 8, dpi = 300)


# Table A1 ----------------------------------------------------------------

tmp <- hc %>% mutate(`Context/Bias Indicator` = factor(`Context/Bias Indicator`),
                     State = factor(State)) %>%  select(-c(Date, yearmon, Municipality, loc, lat, lon)) %>%  report_sample()
knitr::kable(tmp, caption = "Descriptive Statistics for lynching attack data from Citizens' Religious Hate Crime Watch (HCW).", format = "latex") |> save_kable(file = "../results/tabs/TabA1.tex")

# Additional Figures ---------------------------------------------------

hc %>% group_by(yearmon) %>% mutate(count = n()) %>% 
  ggplot() +
  geom_bar(aes(x = yearmon)) +
  theme_minimal() + 
  geom_smooth(aes(x = yearmon, y = count)) +
  labs(title = "Hate Crime Events by Month",
       x = "Year, Month", y = "Count")

ggsave("../results/figs/hc_by_month.png", width = 10, height = 8, dpi = 300)
