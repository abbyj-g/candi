#remove correlated variables
#user manually selects which layers to remove from rasterstack 
remove_corr_variables <- function(raster_stack, variables_to_be_removed){
  
  names <- c(names(raster_stack))
  n <- match(variables_to_be_removed, names)
  
  new <- dropLayer(raster_stack, n)
  
  return(new)
}