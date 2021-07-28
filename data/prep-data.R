library(tidyverse)
library(lubridate)
library(stringr)

# Watershed labels
watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek", 
                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek", 
                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek", 
                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek", 
                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River", 
                      "Feather River", "Yuba River", "Lower-mid Sacramento River", 
                      "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River", 
                      "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River", 
                      "Tuolumne River", "San Joaquin River")

# repeat for all of the habitat arrays

# HABITAT ======================================================================

# delta habitats 
north_delta_habitat <- DSMhabitat::delta_habitat[, , 1] %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>%
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979, 
         region = "North Delta", 
         value = sqm/4046.86)
  
south_delta_habitat <- DSMhabitat::delta_habitat[, , 2] %>%
  as.data.frame() %>% 
  mutate(month = 1:12) %>%
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979, 
         region = "South Delta", 
         value = sqm/4046.86)


delta_habitat <- bind_rows(
  mutate(north_delta_habitat, species = "Fall Run"),
  mutate(north_delta_habitat, species = "Spring Run"),
  mutate(north_delta_habitat, species = "Winter Run"),
  mutate(north_delta_habitat, species = "Steelhead"), 
  mutate(south_delta_habitat, species = "Fall Run"),
  mutate(south_delta_habitat, species = "Spring Run"),
  mutate(south_delta_habitat, species = "Winter Run"),
  mutate(south_delta_habitat, species = "Steelhead")
) %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-')),
         data_type = 'Monthly Rearing Area') %>% 
  select(date, value, data_type, region, species)


# Bypass habitats -----------------
# In channel
sutter_habitat <- DSMhabitat::sutter_habitat %>% 
  reshape::melt() %>%
  as.data.frame() %>%
  setNames(c('month', 'year', 'sqm')) %>% 
  group_by(month, year) %>% 
  summarise(value = sum(sqm)/4046.86) %>% ungroup() %>% 
  mutate(region = "Sutter Bypass")

yolo_habitat <- DSMhabitat::yolo_habitat %>% 
  reshape::melt() %>%
  as.data.frame() %>%
  setNames(c('month', 'year', 'sqm')) %>% 
  group_by(month, year) %>% 
  summarise(value = sum(sqm)/4046.86) %>% ungroup() %>% 
  mutate(region = "Yolo Bypass")


bypass_habitats <- bind_rows(
  mutate(sutter_habitat, species = "Fall Run"),
  mutate(sutter_habitat, species = "Spring Run"),
  mutate(sutter_habitat, species = "Winter Run"),
  mutate(sutter_habitat, species = "Steelhead"),
  mutate(yolo_habitat, species = "Fall Run"),
  mutate(yolo_habitat, species = "Spring Run"),
  mutate(yolo_habitat, species = "Winter Run"),
  mutate(yolo_habitat, species = "Steelhead")
) %>% 
mutate(date = ymd(paste(year, month, 1, sep = '-')),
       data_type = 'Monthly Rearing Area') %>% 
  select(date, value, data_type, region, species)




# Monthly floodplain rearing habitat -------------------------------------------
fr_fp_habitat <- map_df(1:20, function(i) {
  DSMhabitat::fr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Fall Run")

wr_fp_habitat <- map_df(1:20, function(i) {
  DSMhabitat::wr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Winter Run")

sr_fp_habitat <- map_df(1:20, function(i) {
  DSMhabitat::sr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Spring Run")

st_fp_habitat <- map_df(1:20, function(i) {
  DSMhabitat::st_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb),
         species = "Steelhead")

floodplain_habitat <- bind_rows(
  fr_fp_habitat, 
  wr_fp_habitat, 
  sr_fp_habitat, 
  st_fp_habitat
) %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-')),
         value = ifelse(str_detect(watershed, 'Sacramento'), sqm/4046.86, sqm/4046.86*.27), # suitable acres
         data_type = 'Monthly Floodplain Rearing Area') %>% 
  select(date, value, data_type, region = watershed, species)

# Monthly spawning rearing habitat ---------------------------------------------

fr_spawn_habitat <- map_df(1:20, function(i) {
  DSMhabitat::fr_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Fall Run")

sr_spawn_habitat <- map_df(1:20, function(i) {
  DSMhabitat::sr_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Spring Run")

wr_spawn_habitat <- map_df(1:20, function(i) {
  DSMhabitat::wr_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Winter Run")

st_spawn_habitat <- map_df(1:20, function(i) {
  DSMhabitat::st_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = watershed_labels)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = match(month, month.abb), 
         species = "Steelhead")

spawning_habitat <- bind_rows(
  fr_spawn_habitat, 
  wr_spawn_habitat,
  sr_spawn_habitat, 
  st_spawn_habitat
) %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-')),
         value = sqm/4046.86, # suitable acres
         data_type = 'Monthly Spawning Area') %>% 
  select(date, value, data_type, region = watershed, species)

glimpse(spawning_habitat)

# Monthly fry inchannel --------------------------------------------------------

fr_fry_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::fr_fry[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Fall Run")


sr_fry_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::sr_fry[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Spring Run")


wr_fry_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::wr_fry[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Winter Run")

st_fry_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::st_fry[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Steelhead")

fry_inchannel_habitat <- bind_rows(
  fr_fry_inchannel_habitat, 
  wr_fry_inchannel_habitat,
  sr_fry_inchannel_habitat,
  st_fry_inchannel_habitat
) %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-')),
         value = sqm/4046.86, # suitable acres
         data_type = 'Monthly In-channel Rearing Area') %>% 
  select(date, value, data_type, region = watershed, species)

glimpse(fry_inchannel_habitat)

# Monthly Juvenile Rearing habitat ---------------------------------------------

fr_juv_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::fr_juv[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Fall Run")

sr_juv_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::sr_juv[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Spring Run")


wr_juv_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::wr_juv[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Winter Run")


st_juv_inchannel_habitat <- map_df(1:20, function(i) {
  cvpiaData::st_juv[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Steelhead")


juv_inchannel_habitat <- bind_rows(
  fr_juv_inchannel_habitat, 
  wr_juv_inchannel_habitat,
  sr_juv_inchannel_habitat,
  st_juv_inchannel_habitat
) %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-')),
         value = sqm/4046.86, # suitable acres
         data_type = 'Monthly In-channel Rearing Area') %>% 
  select(date, value_juv = value, data_type, region = watershed, species)


# TODO we need to combine fry_inchannel_habitat and juv_inchannel habitat We are
# waiting for adam to respond to our query on steelhead based timing of switch
# from fry to juv habitat area used by model
inchannel_habitat <- fry_inchannel_habitat %>% 
  left_join(juv_inchannel_habitat) %>% 
  mutate(value = case_when(
    species == 'Fall Run' & month(date) %in% 1:4 ~ value,
    species == 'Fall Run' & month(date) %in% 5:8 ~ value_juv,
    species == 'Spring Run' & month(date) %in% c(11, 12, 1) ~ value,
    species == 'Spring Run' & month(date) %in% 2:10 ~ value_juv,
    species == 'Winter Run' & month(date) %in% 9:12 ~ value,
    species == 'Winter Run' & month(date) %in% 1:5 ~ value_juv,
    species == 'Steelhead' ~ value_juv, # TODO fix when we know more from Adam
    TRUE ~ as.numeric(NA)
  )) %>% 
  filter(!is.na(value)) %>% 
  select(date, value, data_type, region, species)

glimpse(inchannel_habitat)

habitat <- bind_rows(
  floodplain_habitat,
  spawning_habitat,
  inchannel_habitat, # TODO update when adam tells us about steelhead, 
  bypass_habitats, 
  delta_habitat
  )

glimpse(habitat)

write_rds(habitat, 'data/habitat.rds')

# FLOW =========================================================================

# monthly mean flow ---------------------------------

watershed_flows <- 
  cvpiaFlow::flows_cfs %>% 
  gather(watershed, flow_cfs, -date) %>% 
  mutate(data_type = "Monthly Mean Flow") %>% 
  select(date, region = watershed, value = flow_cfs, data_type)

bypass_flows <- 
  cvpiaFlow::bypass_flows %>% 
  select(date, `Sutter Bypass` = sutter4, `Yolo Bypass` = yolo2) %>% 
  gather(region, value, -date) %>% 
  mutate(data_type = "Monthly Mean Flow")

watershed_and_bypass_flows <- bind_rows(watershed_flows, bypass_flows)

delta_flows <- 
  cvpiaFlow::delta_flows %>% 
  select(date, 
         `North Delta` = n_dlt_inflow_cfs, 
         `South Delta` = s_dlt_inflow_cfs) %>% 
  gather(delta_flow_type, flow_cfs, -date) %>% 
  mutate(data_type = "Monthly Mean Flow") %>% 
  select(date, region = delta_flow_type, value = flow_cfs, data_type)

# monthly mean diverted --------------------------------

watershed_monthly_mean_diverted <- 
  cvpiaFlow::total_diverted %>% 
  gather(watershed, total_diverted, -date) %>% 
  mutate(data_type = "Monthly Mean Diverted") %>% 
  select(date, region = watershed, value = total_diverted, data_type)

delta_monthly_mean_diverted <- 
  cvpiaFlow::delta_flows %>% 
  select(date, 
         `South Delta` = s_dlt_div_cfs, 
         `North Delta` = n_dlt_div_cfs) %>% 
  gather(watershed, total_diverted_cfs, -date) %>% 
  mutate(data_type = "Monthly Mean Diverted") %>%
  select(date, region = watershed, value = total_diverted_cfs, data_type)

# monthly mean proportion diverted ----------------------

# watershed 
watershed_monthly_mean_prop_diverted <- 
  cvpiaFlow::proportion_diverted %>% 
  gather(watershed, proportion_diverted, -date) %>% 
  mutate(data_type = "Monthly Mean Proportion Diverted") %>% 
  select(date, region = watershed, value = proportion_diverted, 
         data_type)

# delta
delta_monthly_mean_prop_diverted <- 
  cvpiaFlow::delta_flows %>% 
  select(date, 
         `South Delta` = s_dlt_prop_div, 
         `North Delta` = n_dlt_prop_div) %>% 
  gather(watershed, proportion_diverted, -date) %>% 
  mutate(data_type = "Monthly Mean Proportion Diverted") %>%
  select(date, region = watershed, value = proportion_diverted, data_type)


flows <- bind_rows(
  watershed_and_bypass_flows, 
  delta_flows, 
  watershed_monthly_mean_diverted, 
  delta_monthly_mean_diverted, 
  watershed_monthly_mean_prop_diverted, 
  delta_monthly_mean_prop_diverted
)

write_rds(flows, "data/flows.rds")


# TEMPERATURE ==================================================================

# monthly mean temp ----------------

# watershed 
watershed_and_bypass_temperatures <- 
  cvpiaTemperature::juv_temp %>% 
  mutate(data_type = "Monthly Mean Temperature") %>% 
  select(date, region = watershed, value = monthly_mean_temp_c, data_type)
  

# delta 
delta_temperatures <- 
  cvpiaTemperature::delta_temps %>% 
  mutate(data_type = "Monthly Mean Temperature") %>% 
  select(date, region = watershed, value = monthly_mean_temp_c, data_type)

# degree days ---------------------
degree_days <- cvpiaTemperature::deg_days %>%
  select(date, region = watershed, value = degdays) %>% 
  mutate(data_type = "Degree Days")


temperatures <- bind_rows(
  watershed_and_bypass_temperatures, 
  delta_temperatures, 
  degree_days
)

write_rds(temperatures, "data/temperatures.rds")


# Habitat Scaling Factors ------------------------------------------------------

# Fall Run Scales
# SPAWN
# 1 Upper Sac
# 2 Butte
# 3 Clear
# 4 Deer
# 5 Mill
# 6 Feather
# 7 Yuba
# 8 American
# 9 Cosumness
# 10 Mokelumne
# 11 Merced
# 12 Stanislaus
# 13 Tuolumne

# REAR 
# 1 Upper Sac
# 2 Butte
# 3 Clear
# 4 Deer
# 5 Mill
# 6 Upper-mid Sac (corridor for above)
# 7 Sutter (corridor for above) is changed below
# 8 Feather 
# 9 Yuba
# 10 Lower-mid Sac (corridor for above)
# 11 Yolo (corridor for above) is changed below
# 12 American
# 13 Lower Sac (corridor for above)
# 14 Cosumness 
# 15 Mokelumne 
# 16 Merced
# 17 Stanislaus 
# 18 Tuolumne
# 19 SJ (corridor for Merced, Stan, and Tuolumne)
# here 1-13 are spawning scalars
# 14-n are the rearing scalars
fall_run_scales <- c(1.8615848, 0.5000000, 
            0.5000000, 1.4230370, 
            0.5887938, 2.0000000, 
            0.5034449, 0.5502821,
            1.6139332, 0.9551340, 
            1.6993421, 0.9627230, 
            0.9959632, 0.5000000, 
            1.8237525, 2.0000000,
            1.9999999, 2.0000000, 
            2.0000000, 0.9783833, 
            1.5406860, 0.6596480, 
            1.9999994, 1.9999994,
            0.5000423, 0.6147676, 
            0.6598354, 0.8103934, 
            1.2434156, 1.4492968, 
            0.9347787, 1.6509423,
            0.5000000, 1.9800862)




fr_spawning_scaled_watersheds <- 
  cvpiaData::watershed_ordering$watershed[c(1, 6, 7, 10, 12, 19, 20, 
                                            23, 26, 27, 28, 29, 30)]

fr_rearing_scaled_watersheds <- 
  cvpiaData::watershed_ordering$watershed[c(1, 6, 7, 10, 12, 16, 
                                            19, 20, 21, 23, 24,
                                            26, 27, 28, 29, 30, 31, 17, 22)]

fr_habitat_scales <- tibble(
  region = c(fr_spawning_scaled_watersheds, 
                fr_rearing_scaled_watersheds, "North Delta", "South Delta"),
  type = c(rep("Spawning", length(fr_spawning_scaled_watersheds)), 
           rep("Rearing", length(fr_rearing_scaled_watersheds) + 2)), 
  scale = fall_run_scales, 
  species = "Fall Run"
) 

# Spring Run 
# spawn
# 1-5
# 1 Butte
# 2 Deer
# 3 Mill
# 4 Feather
# 5 Yuba

# rear 
# 6 Butte
# 7 Deer
# 8 Mill
# 9 Upper-mid Sac (corridor for above)
# 10 Feather 
# 11 Yuba
# 12 Lower-mid Sac (corridor for above)
# 13 Lower Sac (corridor for above)
# 14 Sutter (corridor for above) is changed below
# 15 Yolo (corridor for above) is changed below
# 16 North Delta
# 17 South Delta

spring_run_scales <- c(2.0000000, 1.5463308, 1.7198077, 0.5084355, 0.5324278, 0.6570386, 0.5000000, 0.5000000, 0.6368001, 0.5000000, 1.4077922,
                       1.9999997, 1.7208838, 0.9353273, 1.8738324, 1.3860064, 1.8283264)


sr_spawning_scaled_watersheds <- 
  cvpiaData::watershed_ordering$watershed[c(6, 10, 12, 19, 20)]

sr_rearing_scaled_watersheds <- 
  c(cvpiaData::watershed_ordering$watershed[c(6, 10, 12, 16, 19, 20, 21, 
                                            24, 17, 22)], "North Delta", "South Delta")


sr_habitat_scales <- tibble(
  region = c(sr_spawning_scaled_watersheds, sr_rearing_scaled_watersheds), 
  type = c(rep("Spawning", length(sr_spawning_scaled_watersheds)), 
           rep("Rearing", length(sr_rearing_scaled_watersheds))), 
  scale = spring_run_scales, 
  species = "Spring Run"
)


# Winter Run


winter_run_scales <- c(1.8199040, 1.0000000, 1.1455608, 0.7771235, 1.8103872, 1.0804059, 1.2378641, 0.9892381,
                       1.2751834)


# spawn 
# 1 Upper Sac

# rear
# 2 Upper Sac
# 3 Upper-mid Sac (corridor for above)
# 4 Lower-mid Sac (corridor for above)
# 5 Lower Sac (corridor for above)
# 6 Sutter (corridor for above) is changed below
# 7 Yolo (corridor for above) is changed below
# 8 North Delta
# 9 South Delta


wr_spawning_scaled_watersheds <- 
  cvpiaData::watershed_ordering$watershed[c(1)]

wr_rearing_scaled_watershds <- 
  c(cvpiaData::watershed_ordering$watershed[c(1, 16, 21, 24, 17, 22)], 
    "North Delta", "South Delta")


wr_habitat_scales <- tibble(
  region = c(wr_spawning_scaled_watersheds, wr_rearing_scaled_watershds), 
  type = c(rep("Spawning", length(wr_spawning_scaled_watersheds)), 
           rep("Rearing", length(wr_rearing_scaled_watershds))), 
  scale = winter_run_scales, 
  species = "Winter Run"
)


habitat_scales <- bind_rows(
  wr_habitat_scales, 
  sr_habitat_scales, 
  fr_habitat_scales
) 

write_rds(habitat_scales, "data/habitat_scales.rds")

# attach the scales as part of the habitat dataframe here

# habitat modified is just the habitat dataset with an additional 
# column type that will allow us to left join to the scales dataset
habitat_modified <- read_rds("data/habitat.rds") %>%
  mutate(
    type = case_when(
      data_type == "Monthly In-channel Rearing Area" ~ "Rearing", 
      data_type == "Monthly Rearing Area" ~ "Rearing", 
      data_type == "Monthly Spawning Area" ~ "Spawning", 
      TRUE ~ as.character(NA)
    )
  )

habitat_with_scales <- habitat_modified %>% as_tibble() %>% 
  left_join(habitat_scales) %>% 
  mutate(scale = ifelse(is.na(scale), 1, scale), 
         scaled_habitat = value * scale)


write_rds(habitat_with_scales, "data/habitat_with_scales.rds")
