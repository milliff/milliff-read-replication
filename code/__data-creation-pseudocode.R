# --------------------------------------------
#
# Author: Aidan Milliff
# Copyright (c) Aidan Milliff, 2024
# Email:  milliff.a@gmail.com
#
# Date: 2024-05-30
#
# Script Name: __data-creation-pseudocode.R
#
# Script Description: Shows code for creation of main_df.RData, and did_df.RData the primary analysis datasets.
#                     We incorporate information from a number of data-sources which are available to researchers,
#                     but which we do not have license to share in their un-modified forms. These datasets include, among others,
#                     the Socioeconomic High-resolution Rural-Urban Geographic platform (SHRUG, https://www.devdatalab.org/shrug),
#                     Court pendancy data from Ash et al. 2021 (https://www.devdatalab.org/judicial-data), and development data from 
#                     the AidData/GeoQuery project at William and Mary (https://www.aiddata.org/geoquery).
#
#                     Below, we reproduce the code that we use to incorporate those data (as well as public data from the Indian government) 
#                     into our analysis. 
#
#                     NOTE: THIS CODE WILL NOT PRODUCE OUR MAIN ANALYSIS DATA UNLESS THE FILEPATHS ARE CHANGED TO POINT TO ACTUAL DATA
#                           FROM THE ABOVE-NAMED SOURCES. THE MAIN ANALYSIS DATA IS AVAILABLE IN THIS REPOSITORY AS A FINAL PRODUCT ONLY.
# Notes:
#
#
# --------------------------------------------


# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")

# Packages
# library(fuzzyjoin); library(tidyverse); library(readxl); library(zoo); library(ggmap); library(sf)


# LOAD DATA ------------------------------------
# None here

# CUSTOM FUNCTIONS
print_count_of_unique_values <- function(df, column_name, remove_items_with_freq_equal_or_lower_than = 0, return_df = F, 
                                         sort_desc = T, return_most_frequent_value = F)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

####################################################################################
####################################################################################
# Geocoding ---------------------------------------------------------------

## Hate Crimes
# Read raw data from India Religious Hatecrime Watch
hc <- readxl::read_xlsx("HateCrimeWatch.xlsx") |>
        mutate(yearmon = as.yearmon(Date)) 

# Paste locality and state together to avoid ambiguity
cols   <- c("Municipality", "State")
hc$loc <- apply(hc[, cols], 1, paste, collapse = ", ")

# Geocode using Google Maps API call; requires a Google API key; see ggmap documentation
hc <- hc |>  mutate_geocode(loc)

# Write Geo-coded hatecrime dataset to file
write_csv(hc, "hc_locs.csv")

## VB Schools
# Read raw data scraped from Vidhya Bharati online alumni directory
vb <- read_csv("vb_schools.csv")

# Paste locality name, district name, and "India" --- avoiding archaic place names (Prantas) used by Vidhya Bharati but not by Google or the present day GoI
cols <- c("locality", "district")
vb$loc2 <- apply(vb[, cols], 1, paste, collapse = ", ")
vb$loc <- paste0(vb$loc2, ", India", sep = "")

# Geocode using Google Maps API call
vb <- vb |>  mutate_geocode(loc)

# Write Geo-coded VB dataset to file
write_csv(vb, "vb_locs.csv")

# Check geocoding for VB data, since we were missing state names

# plot
ggmap(india_map) + geom_point(aes(x=lon, y=lat), data = vb2, pch = ".", color = "orange")+
  labs(title= "Geocode Redo")

# check modes
vb$latlon <- paste(vb$lon, vb$lat, sep = ", ")
modes <- print_count_of_unique_values(vb, "latlon", remove_items_with_freq_equal_or_lower_than = 1, return_df = T)

# Separate and reverse code 
modes <- modes |> separate(Item, c("lon", "lat"), sep = ", ") %>% filter(Frequency > 19)
modes$lon <- as.numeric(modes$lon)
modes$lat <- as.numeric(modes$lat)

# Reverse geo-code to see if the place names match up (they do)
locs <- do.call(rbind,
                lapply(1:nrow(modes),
                       function(x)revgeocode(as.numeric(modes[x,1:2])))
)
modes <- cbind(modes, locs)
modes


####################################################################################
####################################################################################
# Map Data Sources to Subdistrict Polygons --------------------------------

## Pull in additional data sources

# 1. DISE (District Information System for Education, i.e. school census records that match VB school names/locations)
# Because of minor differences in transliteration across sources, this match is done using string-distance matching.
# We select the closest match, discarding all matches if [DISTANCE METRIC] > 50.5. 

dise <- read_csv("dise.csv")

# 2. Subdistrict shapefiles (from https://earthworks.stanford.edu/)

sf::sf_use_s2(FALSE)
subdi <- st_read('subdistricts.json') |>  st_make_valid()

## Merge VB and DISE; Assign to Polygons

vb_dise <- dise |> left_join(vb)


# Turn datasets into SF objects

dise_sf <- vb_dise %>% 
  filter(complete.cases(lon, lat)) %>% 
  mutate_at(vars(lon, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326, # same as WGS84
    stringsAsFactors = F,
    remove = T
  )

hc_sf <- hc %>% 
  filter(complete.cases(lon, lat)) %>% 
  mutate_at(vars(lon, lat), as.numeric) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    agr = "constant",
    crs = 4326, # WGS84
    stringsAsFactors = F,
    remove = T
  )

# Assign points to polygons

dise_in_subdi <- st_join(dise_sf, subdi, join = st_within)
hc_in_subdi   <- st_join(hc_sf, subdi, join = st_within)

# Add additional variables to points

subdi_hc <- hc_in_subdi |>  filter(`Context/Bias Indicator` %in% c("Cow protection", "Religious conversion", "Interfaith Relationship", "Insult to religion or blasphemy", "Minor dispute")) |> # Removing "communal clashes" which are not lynching-type events
    group_by(id) |> 
    summarise(count = n(),
              name = first(name),
              fatal = sum(Fatal),
              pc_fatal = sum(Fatal)/count,
              FIR = sum(FIR),
              pc_FIR = sum(FIR)/count,
              counterFIR = sum(CounterFIR),
              pc_counterFIR = sum(CounterFIR)/count,
              district = first(district),
              state_ut = first(state_ut))

subdi_hc <-  st_join(subdi, subdi_hc, by = "id")

cols <-  c("count", "fatal", "pc_fatal","FIR", "pc_FIR", "counterFIR", "pc_counterFIR")
subdi_hc[cols][is.na(subdi_hc[cols])] <- 0 # If a subdistrict has no hate crime events; put hate crime stats at ZERO, not NA


subdi_dise <- dise_in_subdi % |> 
  mutate(years_open = 2009 - final_founding) |> # How many years of treatment before start of outcome var?
  group_by(id) |> 
  summarise(vb_count = n(),
            final_founding = min(final_founding), # IMPORTANT CHOICE: Final founding means our best guess as to the first time a VB school existed in the subdi
            avg_year = mean(final_founding),  # IMPORTANT CHOICE: avg_year means the average of the founding years for each school
            school_years = sum(years_open), # Years * units of treatment
            total_years = max(years_open)) # Years of treatment, regardless of units

subdi_dise <-  st_join(subdi, subdi_dise)
subdi_dise["vb_count"][is.na(subdi_dise["vb_count"])] <- 0 # If a subdistrict has no known VB schools, set the count to zero

dise_hc <- st_join(subdi_dise, subdi_hc, join = st_within)
names(dise_hc)[names(dise_hc) == 'id.x.x'] <- 'id'
dise_hc <- dise_hc  %>%  select(id, state_ut, district, vb_count, count, fatal, pc_fatal, FIR, pc_FIR, counterFIR, pc_counterFIR, geometry, final_founding, avg_year)

vb_hc <- st_join(dise_hc, subdi, join = st_within)


####################################################################################
####################################################################################
# Add Other Data Sources as Covariates ------------------------------------

##### 2001 Census Religion Figs. 
rel <- data.frame()

files <- list.files(path = "~/census/2001_by_religion/")

# Read in religion table data from xls files (state-wise)
for (i in 1:length(files)){
  tmp <- readxl::read_xls(path = paste0("~/census/2001_by_religion/", files[i]),
                          col_names = c("tab.name", "state.code", "dist.code", "subdist.code", "town.code", "name", "tru",
                                        "total.persons", "total.males", "total.females",
                                        "hindu.persons", "hindu.males", "hindu.females",
                                        "muslim.persons", "muslim.males", "muslim.females",
                                        "christian.persons", "christian.males", "christian.females",
                                        "sikh.persons", "sikh.males", "sikh.females",
                                        "buddhist.persons", "buddhist.males", "buddhist.females",
                                        "jain.persons", "jain.males", "jain.females",
                                        "other.persons", "other.males", "other.females",
                                        "norel.persons", "norel.males", "norel.females"))
  rel <- rbind(rel, tmp)
}

# concatenate state, district, subdistrict, town codes into census code
rel$c_code01 <- paste0(rel$state.code, rel$dist.code, rel$subdist.code, rel$town.code)

# Get rid of the "rural" and "urban" distinctions for now; one observation per subdistrict

rel <- rel |> filter(tru == "Total")

# There are some things that end up with "missing" c_codes, but can be filled in by hand.

# The codes in vb_allevents / rel
# Hyderabad NONE / 28 05 0000 00000000
# Chennai NONE / 33 02 0000 00000000
# Bidar 29 05 0004 00000000 / 29 05 0000 00000000
# Mumbai Suburban NONE / 27 22 0000 00000000
# Mumbai NONE / 27 23 0000 00000000
# Ozhukarai (M) NONE/ 34 02 0000 40201000
# Asansol (WB) 1909003200000000 / 19 09 0000 00000000
# Kolkata 1917000100000000 / 19 17 0000 00000000
# Haora 1916001500000000 / 19 16 0000 00000000
# Senpati, Manipur MANY / 14 01 0000 00000000
# Daman 26445 / 25 02 0000 00000000
# Karaikal M NONE/ 34 04 0000 40401000
# Pondichery M NONE / 34 02 0000 40202000

vb_hc$c_code01[which(vb_hc$name == "HYDERABAD")] <- "2805000000000000"
vb_hc$c_code01[which(vb_hc$name == "Chennai")] <- "3302000000000000"
vb_hc$c_code01[which(vb_hc$name == "Bidar")] <- "2905000000000000"
vb_hc$c_code01[which(vb_hc$name == "MUMBAI SUBURBAN")] <- "2722000000000000"
vb_hc$c_code01[which(vb_hc$name == "MUMBAI")] <- "2723000000000000"
vb_hc$c_code01[which(vb_hc$name == "OZHUKARAI MUNICIPALI")] <- "3402000040201000"
vb_hc$c_code01[which(vb_hc$name == "Asansol Urban Agglomeration")] <- "1909000000000000"
vb_hc$c_code01[which(vb_hc$name == "KolKata")] <- "1917000000000000"
vb_hc$c_code01[which(vb_hc$name == "Haora")] <- "1916000000000000"
vb_hc$c_code01[which(vb_hc$district == "Senapati" & vb_hc$tot_nm_hh==0)] <- "1401000000000000"
vb_hc$c_code01[which(vb_hc$name == "Daman")] <- "2502000000000000"
vb_hc$c_code01[which(vb_hc$name == "KARAIKAL MUNICIPALIT")] <- "3404000040401000"
vb_hc$c_code01[which(vb_hc$name == "PONDICHERRY MUNICIPA")] <- "3402000040202000"


# Join
vb_hc_rel <- vb_hc |> left_join(rel, by = "c_code01")

# Proportion Muslim and Proportion Christian variables

vb_hc_rel <- vb_hc_rel |> mutate(pc_muslim = muslim.persons/total.persons,
                           pc_christian = christian.persons/total.persons)

##### Add Shrug Data (v1.5-Samosa)
sh_anc  <- read.csv("~/shrug-v1.5.samosa-ancillary-csv/shrug_ancillary.csv")
sh_nl   <- read.csv("~/shrug-v1.5.samosa-nl-csv/shrug_nl_wide.csv")
sh_ec   <-  read.csv("~/shrug-v1.5.samosa-pop-econ-census-csv/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc01.csv")
sh_keys <- read.csv("~/shrug-v1.5.samosa-ancillary-csv/shrug-v1.5.samosa-keys-csv/shrug_pc01_subdistrict_key.csv")

shrug <- sh_anc |> left_join(sh_nl, by = "shrid") |> left_join(sh_ec, by = "shrid") |>
  left_join(sh_keys, by = "shrid") |>
  select(pc01_district_id, pc01_subdistrict_id, pc01_state_id,
         pc01_district_name, pc01_subdistrict_name, pc01_state_name,
         pc01_vd_p_sch, pc01_vd_m_sch, pc01_vd_s_sch, pc01_vd_s_s_sch,
         pc01_vd_tar_road, pc01_vd_dirt_road, pc01_vd_power_all,
         tdist_100, tdist_500, total_light_cal2008, num_cells) |>
  group_by(pc01_state_id, pc01_district_id, pc01_subdistrict_id) |>
  summarise(prim_school = sum(pc01_vd_p_sch, na.rm = T),
            mid_school = sum(pc01_vd_m_sch, na.rm = T),
            sec_school = sum(pc01_vd_s_sch, na.rm = T),
            sen_school = sum(pc01_vd_s_s_sch, na.rm = T),
            tar_road = mean(pc01_vd_tar_road, na.rm = T),
            dirt_road = mean(pc01_vd_dirt_road, na.rm = T),
            power_all = mean(pc01_vd_power_all, na.rm = T),
            tdist_100k = mean(tdist_100, na.rm = T),
            tdist_500k = mean(tdist_500, na.rm = T),
            nightlight_08 = mean(total_light_cal2008/num_cells, na.rm = T)
            )

# Merge in

vb_hc_rel_shrug <- vb_hc_rel |> mutate(pc01_district_id = as.numeric(dist.code),
                             pc01_subdistrict_id = as.numeric(subdist.code),
                             pc01_state_id = as.numeric(state.code)) |>
  left_join(shrug, by = c("pc01_district_id","pc01_subdistrict_id", "pc01_state_id"))

vb_hc_rel_shrug <- vb_hc_rel_shrug  |> mutate(pc_sc = tot_sc/tot_pop,
                           pc_st = tot_st/tot_pop,
                           pc_illt = tot_illt/tot_pop,
                           pc_male = m_pop/tot_pop,
                           wf_male = m_mnw/m_pop,
                           state_ut = factor(state_ut))


##### Add Court Pendancy Data (from Ash et al.)


cases <- readr::read_csv("~/cases_2010.csv")
cases$date_of_decision[which(is.na(cases$date_of_decision))] <- as.Date("2019-01-01")
cases$date_of_decision[which(cases$date_of_decision > as.Date("2019-01-01"))] <- NA
cases$date_of_decision[which(cases$date_of_decision < as.Date("2010-01-01"))] <- NA
cases$dist_code <- as.numeric(cases$dist_code)
cases$state_code <- as.numeric(cases$state_code)
keys  <- readr::read_csv("~/cases_district_key.csv")

case_dist <- cases |> left_join(keys) |>  
  group_by(state_name, district_name) |> 
  summarise(case_count = n(),
            mean_time_decision = mean(date_of_decision - date_of_filing, na.rm = T)
  )

case_dist$district <- iconv(case_dist$district_name, to="UTF-8")
case_dist$state <- iconv(case_dist$state_name, to="UTF-8")
vb_hc_rel_shrug_court <- vb_hc_rel_shrug |> stringdist_left_join(case_dist,
                                              by = c("district", "state_ut" =  "state"),
                                              method = "jw",
                                              max_dist = 3,
                                              distance_col = "dist") |> 
  group_by(district.x, state_ut) |> 
  slice_min(district.dist) |> 
  distinct(id, .keep_all = T)

# rename to save hassle

vb_hc_rel_shrug_court <- vb_hc_rel_shrug_court |> rename("district" = "district.x")

##### Add "Crime in India" data; IPC Crimes from 2008

crim08 <- read.csv("~/tabula-ipc_crimes.csv") # 2008 IPC Crimes (Crime in India 2008)

# Get rid of the stupid leading numbers
crim08$District <- gsub("^\\d+ ", "", crim08$District)
crim08$district <- tolower(crim08$District)
tmp_courts$district <- tolower(tmp_courts$district)
crim08$state <- tolower(crim08$State)
tmp_courts$state_ut <- tolower(tmp_courts$state_ut)


# This is the fuzzy join that works pretty well; last bits are critical to avoid it "exploding" with matches
vb_hc_rel_shrug_court_crim <- vb_hc_rel_shrug_court |> filter(!is.na(district)) |> filter(!is.na(state_ut)) |> stringdist_left_join(crim08,
                                                                                                        by = c("district", "state_ut" = "state"),
                                                                                                        method = "jw",
                                                                                                        max_dist = 3,
                                                                                                        distance_col = "dist") |> 
  group_by(district.x, state_ut) |> slice_min(district.dist...287) |> distinct(geometry, id, .keep_all = T)


# Group by district
vb_hc_rel_shrug_court_crim <- vb_hc_rel_shrug_court_crim |>  
  rename("murder" = "Murder..IPC.302..303.",
         "att_murder" = "Attempt.to.Commit.Murder..IPC.307.",
         "kidnap" = "Total.Kidnapping.and.Abduction..IPC.363.369..371.373.",
         "riots" = "Riots..IPC.143.145..147.151..153..153A..153B..157..158..160.",
         "rape" = "Total.Rape..IPC.376.",
         "arson" = "Arson..IPC.435..436..438.",
         "dowry" = "Dowry.Death..IPC.304B.",
         "tot_crim" = "Total.Cognizable.IPC.Crimes",
         "district" = "district.x") |> 
  mutate(vio_crim = murder + att_murder + kidnap + riots + rape + arson + dowry)


##### Add State Capacity data from AidData

aiddata <- readr::read_csv("~/aiddata_results.csv")
vb_hc_rel_shrug_court_crim_aid <- vb_hc_rel_shrug_court_crim |> left_join(aiddata |> mutate(id = as.character(id)))

vb_hc_rel_shrug_court_crim_aid$swindex <- scales::rescale(scales::rescale(as.numeric(vb_hc_rel_shrug_court_crim_aid$mean_time_decision)) + scales::rescale(vb_hc_rel_shrug_court_crim_aid$dist_to_groads.none.mean) +
                                                             scales::rescale(vb_hc_rel_shrug_court_crim_aid$access_50k.none.mean) + (1-scales::rescale(vb_hc_rel_shrug_court_crim_aid$v4composites_calibrated_201709.2011.mean)) +
                                                             scales::rescale(vb_hc_rel_shrug_court_crim_aid$ucdp)) # There's missingness here, so I create "nocourts" as well

vb_hc_rel_shrug_court_crim_aid$swindex_nocourts <- scales::rescale(scales::rescale(vb_hc_rel_shrug_court_crim_aid$dist_to_groads.none.mean) +
                                                                      scales::rescale(vb_hc_rel_shrug_court_crim_aid$access_50k.none.mean) +
                                                                      (1-scales::rescale(vb_hc_rel_shrug_court_crim_aid$v4composites_calibrated_201709.2011.mean)) +
                                                                      scales::rescale(vb_hc_rel_shrug_court_crim_aid$ucdp))

save(vb_hc_rel_shrug_court_crim_aid, "main_df.RData")


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Panel Data Creation -----------------------------------------------------


years  <- c(2009:2018)
units  <- data.frame(id = vb_hc_rel_shrug_court_crim_aid$id,
                     c_code01 = vb_hc_rel_shrug_court_crim_aid$c_code01,
                     sub_dist_i = vb_hc_rel_shrug_court_crim_aid$sub_dist_i) |> distinct()

husk   <- data.frame(year = rep(years, each = nrow(units)),
                     id = rep(units[,1], times = length(years)),
                     c_code01 = rep(units[,2], times = length(years)),
                     sub_dist_i = rep(units[,3], times = length(years))) 


has_hc   <- husk |> left_join(vb_hc_rel_shrug_court_crim_aid,  by = c("id", "year" = "hc_year", "c_code01", "sub_dist_i"), relationship = "one-to-one") # For id-years with HC events, pull those into the panel format

no_hc  <- has_hc |> filter(is.na(count)) |> select(year, c_code01, id) |>
  left_join(vb_hc_rel_shrug_court_crim_aid, relationship = "many-to-one", multiple = "any") # for id-years without HC events, pull those into panel format

has_hc <- has_hc |> filter(count >= 0) # Get rid of the ones that were matched in no_hc

panel <- bind_rows(has_hc, no_hc) |> filter(!is.na(count)) # recombine

panel_todo <- bind_rows(has_hc, no_hc) |> filter(is.na(count)) |> select(year, id, c_code01) # Check that nothing was dropped. Nice! this should be zero


# Now, create a "treatment" variable --------------------------------------

# Goal here is to get an indicator for "arrival" of a VB school

panel <- panel |> mutate(vb_treat = ifelse(final_founding <= year & final_founding != 0, 1, 0))

# vb_treat is an indicator for whether a VB school is open in that year in that district

save(panel, file = "did_df.RData")
