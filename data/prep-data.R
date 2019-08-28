library(tidyverse)
library(lubridate)
library(stringr)

# repeat for all of the habitat arrays

# HABITAT ======================================================================

# delta habitats 
north_delta_habitat <- cvpiaData::dlt_hab[, , 1] %>%
  as.data.frame() %>% 
  mutate(month = 1:12) %>%
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979, 
         region = "North Delta", 
         value = sqm/4046.86)
  
south_delta_habitat <- cvpiaData::dlt_hab[, , 2] %>%
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
sutter_ic_habitat <- cvpiaData::inchannel_bypass[1:4, ,] %>% 
  colSums() %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>% 
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979) 

sutter_fp_habitat <- cvpiaData::floodplain_bypass[1:4, ,] %>% 
  colSums() %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>% 
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979) 

sutter_habitat <- bind_rows(sutter_ic_habitat, sutter_fp_habitat) %>% 
  group_by(month, year) %>% 
  summarise(value = sum(sqm)/4046.86) %>% ungroup() %>% 
  mutate(region = "Sutter Bypass")

yolo_ic_habitat <- cvpiaData::inchannel_bypass[5:6, ,] %>% 
  colSums() %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>% 
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979) 

yolo_fp_habitat <- cvpiaData::floodplain_bypass[5:6, ,] %>% 
  colSums() %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>% 
  gather(year, sqm, -month) %>% 
  mutate(year = as.numeric(str_extract(year, "[0-9]+")) + 1979) 

yolo_habitat <- bind_rows(yolo_ic_habitat, yolo_fp_habitat) %>% 
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




























