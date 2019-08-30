library(tidyverse)
library(lubridate)
library(stringr)

# repeat for all of the habitat arrays

# HABITAT ======================================================================

# Monthly floodplain rearing habitat -------------------------------------------
fr_fp_habitat <- map_df(1:20, function(i) {
  cvpiaData::fr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Fall Run")

wr_fp_habitat <- map_df(1:20, function(i) {
  cvpiaData::wr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Winter Run")

sr_fp_habitat <- map_df(1:20, function(i) {
  cvpiaData::sr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Spring Run")

st_fp_habitat <- map_df(1:20, function(i) {
  cvpiaData::st_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
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
  cvpiaData::fr_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Fall Run")

sr_spawn_habitat <- map_df(1:20, function(i) {
  cvpiaData::sr_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Spring Run")

wr_spawn_habitat <- map_df(1:20, function(i) {
  cvpiaData::wr_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Winter Run")

st_spawn_habitat <- map_df(1:20, function(i) {
  cvpiaData::st_spawn[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"), 
         species = "Steelhead")

spawning_habitat <- bind_rows(
  fr_spawn_habitat, 
  wr_spawn_habitat,
  sr_spawn_habitat, 
  st_spawn_habitat
) %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-')),
         value = sqm/4046.86, # suitable acres
         data_type = 'Monthly Spawning Rearing Area') %>% 
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
  inchannel_habitat # TODO update when adam tells us about steelhead
  )

glimpse(habitat)

write_rds(habitat, 'data/habitat.rds')

# FLOW =========================================================================

# monthly mean flow ---------------------------------

watershed_and_bypass_flows <- 
  cvpiaFlow::flows_cfs %>% 
  gather(watershed, flow_cfs, -date) %>% 
  mutate(data_type = "Monthly Mean Flow") %>% 
  select(date, region = watershed, value = flow_cfs, data_type)

delta_flows <- 
  cvpiaFlow::delta_flows %>% 
  select(date, n_dlt_inflow_cfs, s_dlt_inflow_cfs) %>% 
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
  select(date, s_dlt_div_cfs, n_dlt_div_cfs) %>% 
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
  select(date, s_dlt_prop_div, n_dlt_prop_div) %>% 
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


# Habitat Scaling Factors 

# here 1-13 are spawning scalars
# 14-n are the rearing scalars
scales <- c(1.8615848, 0.5000000, 
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

spawning_scaled_watersheds <- 
  cvpiaData::watershed_ordering$watershed[c(1, 6, 7, 10, 12, 19, 20, 
                                            23, 26, 27, 28, 29, 30)]

rearing_scaled_watersheds <- 
  cvpiaData::watershed_ordering$watershed[c(1, 6, 7, 10, 12, 16, 
                                            19, 20, 21, 23, 24,
                                            26, 27, 28, 29, 30, 31, 17, 22)]

habitat_scales <- tibble(
  watershed = c(spawning_scaled_watersheds, rearing_scaled_watersheds, "North Delta", "South Delta"),
  type = c(rep("Spawning", length(spawning_scaled_watersheds)), 
           rep("Rearing", length(rearing_scaled_watersheds) + 2)), 
  scale = scales
  ) 

write_csv(habitat_scales, "data/habitat-scales.csv")





















