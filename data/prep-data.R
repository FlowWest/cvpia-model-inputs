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
  mutate(data_type = "Monthly Mean Temperature") %>% 
  select(date, region = watershed, value = flow_cfs, data_type)

delta_flows <- 
  cvpiaFlow::delta_flows %>% 
  select(date, n_dlt_inflow_cfs, s_dlt_inflow_cfs) %>% 
  gather(delta_flow_type, flow_cfs, -date) %>% 
  mutate(data_type = "Monthly Mean Temperature") %>% 
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
  select(date, watershed, value = monthly_mean_temp_c, data_type)
  

# delta 
delta_temperatures <- 
  cvpiaTemperature::delta_temps %>% 
  mutate(data_type = "Monthly Mean Temperature") %>% 
  select(date, watershed, value = monthly_mean_temp_c, data_type)

# degree days ---------------------
degree_days <- cvpiaTemperature::deg_days %>%
  select(date, watershed, value = degdays) %>% 
  mutate(data_type = "Degree Days")


temperatures <- bind_rows(
  watershed_and_bypass_temperatures, 
  delta_temperatures, 
  degree_days
)

write_rds(temperatures, "data/temperatures.rds")




























