#trim rasterstack to user provided geographic shapefile

#' trim to shapefile
#'
#' A function to trim a stack of environmental data to a given shapefile
#' @param environmental_raster A raster or raster stack
#' @param cookie_cutter A geographic shapefile provided by the user to trim the raster(s) to
trim_to_shapefile <- function(environmental_raster, cookie_cutter){

  reduced_stack <- raster::crop(environmental_raster, cookie_cutter)
  trim <- raster::mask(reduced_stack, cookie_cutter)
  return(trim)
}
