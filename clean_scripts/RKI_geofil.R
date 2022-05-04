RKI_geofil <- function(x, y){
  x %>% filter(geo_ID == y)
}