#function to download the 19 bioclimatic raster layers from Worldclim database
get_world_clim <- function(){
  
  # Download Worldclim raster data using {dismo} 
  # Only need to do this once 
  wc5 <- getData('worldclim', var = 'bio', res = 5)
  
  # Create a list of .bil files that exist in the working directory
  files = list.files(path="wc5", pattern='\\.bil', full.names=TRUE)
  
  # Combine all list elements into a stack; this way we can treat all the rasters as a unit in modeling
  cs <- stack(files)
  
  return(cs)
  
}