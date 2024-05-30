# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-24
#
# Script Name: vb-descriptive-statistics.R
#
# Script Description: Produces data description and summary statistics for Vidhya Bharati school data in Figure 3A and Figure 6
#
#
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")

# LOAD DATA ------------------------------------
vb    <- read_csv("../data/vb_locs.csv")
subdi <- st_read("../data/subdistricts.json") |> st_make_valid()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Figure 3 ----------------------------------------------------------------

vb_in_subdi <- vb %>% 
  filter(complete.cases(lon, lat)) %>% 
  mutate_at(vars(lon, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326, # same as WGS84
    stringsAsFactors = F,
    remove = T
  ) 

vb_in_subdi <- vb_in_subdi |> 
  st_join(subdi, join = st_within)


vb_count <- count(as_tibble(vb_in_subdi), id)
vb_subdi_df <- left_join(subdi, vb_count, by = "id") |> 
                            mutate(n_noNAs = ifelse(is.na(n), 0, n)) |> left_join()

# Little maps
tmap_mode("plot")
tm_shape(vb_subdi_df) +
  tm_fill(
    col = "n_noNAs",
    palette = "Oranges",
    style = "cont",
    contrast = c(0.1, 1),
    title = "Vidya Bharati Schools",
    id = "shapeID",
    showNA = FALSE,
    alpha = 0.8
  ) 
tmap_save(filename = "../results/figs/Fig3a.png")


# Figure 6 ----------------------------------------------------------------
subdi_vb <- vb_in_subdi |> 
  group_by(id) |> 
  summarise(vb_count = n(),
            final_founding = min(final_founding))
subdi_vb <- st_join(vb_subdi_df, subdi_vb)

years  <- c(1946:2018)
units  <- data.frame(id = subdi_vb$id.x,
                     c_code01 = subdi_vb$c_code01,
                     sub_dist_i = subdi_vb$sub_dist_i,
                     treat = subdi_vb$final_founding) |> distinct()

husk   <- data.frame(year = rep(years, each = nrow(units)),
                     id = rep(units[,1], times = length(years)),
                     c_code01 = rep(units[,2], times = length(years)),
                     sub_dist_i = rep(units[,3], times = length(years)),
                     treat = rep(units[,4], times = length(years))) 
husk <- husk |> mutate(treat = ifelse(is.na(treat), 0, treat)) |>   mutate(vb = ifelse(treat <= year & treat != 0, 1, 0),)

panelview(data = husk, D = "vb", index = c("id", "year"), by.timing = T, display.all = T,
          pre.post = T, axis.lab.angle = 90,
          axis.lab.gap = c(10,10),
          main = "Vidhya Bharati School Expansion",
          xlab = "Year",
          ylab = "Subdistrict",
          legend.labs = c("No VB", "Pre VB", "Has VB"))
ggsave(filename = "../results/figs/Fig6.png", width = 10, height = 8, dpi = 300)



# Additional Figures ---------------------------------------------------------------------

tmp <- vb %>% mutate(State = factor(STATE)) %>%  select(State, final_founding) %>%  report_sample(group_by = "State")
knitr::kable(t(tmp), caption = "Statewise Breakdown of VB School Presence", format = "latex") |> save_kable(file = "../results/tabs/vb_by_state.tex")

ggplot(vb) +
  geom_bar(aes(x = final_founding), fill = "red") +
  facet_wrap(~STATE) +
  theme_bw() +
  labs(x = "Year",
       y = "Count",
       title = "State-wise count of Vidhya Bharati School Openings by Year",
       caption = "States/UTs with no Vidhya Bharati schools are omitted.")
ggsave(filename = "../results/figs/vb_founding_by_state.png", width = 10, height = 8, dpi = 300)

