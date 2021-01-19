#function to download the 19 bioclimatic raster layers from Worldclim database
#' get world clim
#'
#' A function to download the 19 bioclimatic variables from the world clim database
#' @return A raster stack of the 19 bioclimatic world clim variables
get_world_clim <- function(){

  # Download Worldclim raster data using {dismo}
  # Only need to do this once
  wc5 <- raster::getData('worldclim', var = 'bio', res = 5)

  # Create a list of .bil files that exist in the working directory
  files = list.files(path="wc5", pattern='\\.bil', full.names=TRUE)

  # Combine all list elements into a stack; this way we can treat all the rasters as a unit in modeling
  cs <- raster::stack(files)

  return(cs)

}
