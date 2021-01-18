#trim rasterstack to user provided geographic shapefile

trim_to_shapefile <- function(environmental_raster, cookie_cutter){
  
  reduced_stack <- crop(environmental_raster, cookie_cutter)
  trim <- mask(reduced_stack, cookie_cutter)
  return(trim)
}