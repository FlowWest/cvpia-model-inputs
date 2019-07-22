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
         spcies = "Spring Run")

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
)

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

monthly_spawning_habitat <- bind_rows(
  fr_spawn_habitat, 
  wr_spawn_habitat,
  sr_spawn_habitat, 
  st_spawn_habitat
)

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
)

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
)































# FLOW =========================================================================

# monthly mean flow 
watershed_and_watershed_flows <- 
  cvpiaFlow::flows_cfs %>% 
  gather(watershed, flow_cfs, -date)

delta_flows <- cvpiaFlow::delta_flows %>% 
  gather(delta_flow_type, flow_cfs, -date)

# monthly mean diverted 

watershed_monthly_mean_diverted <- 
  cvpiaFlow::total_diverted %>% 
  gather(watershed, total_diverted, -date)

delta_monthly_mean_diverted <- 
  cvpiaFlow::delta_flows %>% 
  select(date, s_dlt_div_cfs, n_dlt_div_cfs) %>% 
  gather(delta_region, total_diverted_cfs, -date)

# monthly mean proportion diverted



# TEMPERATURE ==================================================================




