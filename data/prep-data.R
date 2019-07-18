# repeat for all of the habitat arrays
map_df(1:20, function(i) {
  cvpiaData::fr_fp[,,i] %>% 
    as.data.frame() %>% 
    mutate(year = i + 1979) %>% 
    bind_cols(watershed = cvpiaData::watershed_ordering$watershed)
}) %>% 
  gather(month, sqm, -watershed, -year) %>% 
  mutate(month = str_extract(month, "[0-9]+"))
