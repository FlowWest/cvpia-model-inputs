# repeat for all of the habitat arrays

# Monthly floodplain rearing habitat
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



# Monthly spawning rearing habitat
?cvpiaData::sr_fp

# Monthly rearing area


























