#make correlation matrix between the 19 bioclim variables
#User can use results to make decisions about removing highly correlated variables

make_corr_matrix <- function(occurrences, environment_data){
  #trim environmental layers to the extent of the occurrence data
  #this trimmed version ONLY for the purpose of correlation analysis
  
  #getting the max and min lon and lat for the extent of the clipping mask
  collapsed_occ_list <- do.call(rbind, occurrences)
  max_lat <- max(collapsed_occ_list$latitude)
  min_lat <- min(collapsed_occ_list$latitude)
  max_lon <- max(collapsed_occ_list$longitude)
  min_lon <- min(collapsed_occ_list$longitude)
  rect_lats <- c(max_lat, max_lat, min_lat, min_lat)
  rect_lons <- c(max_lon, min_lon, min_lon, max_lon)
  rect_coords <- cbind(rect_lons, rect_lats)
  
  ##make spatial polygon##
  p <- Polygon(rect_coords)
  ps <- Polygons(list(p),1)
  polygon_mask <- SpatialPolygons(list(ps))
  
  ##crop raster stack##
  cropped_clim_corr <- crop(environment_data, polygon_mask)
  
  #correlation analysis between layers
  print("Calculating correlation coefficients of climatic variables")
  corr <- layerStats(cropped_clim_corr, 'pearson', na.rm=TRUE)
  c_matrix <- corr$`pearson correlation coefficient`
  name <- paste0("correlationBioClim", Sys.Date(), ".csv")
  print(paste0("Saving correlation coefficients to file in your current working directory: ",
               getwd(), "/", name))
  write.csv(c, file = name)
  
  #make a correlation plot, highlighting  absolute values greater than 0.8
  df <- as.data.frame(c_matrix)
  ggcorr(df, geom = "blank", label = TRUE, hjust = 0.75) +
    geom_point(size = 10, aes(alpha = abs(coefficient) > 0.8)) +
    scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
    guides(color = FALSE, alpha = FALSE)
  
  #export plot as a pdf, saved as correlation_plot.pdf in your working directory
  name <- paste0("correlationsBioClim", Sys.Date(), ".pdf")
  print(paste0("Saving PDF of correlation coefficients plot to your current working directory: ",
               getwd(), "/", name))
  print("Correlation coefficients greater than .8 or less than -.8 are highlighted in grey.")
  ggsave(name, plot = last_plot(), device = "pdf", path = NULL,
         scale = 1, width = NA, height = NA)
  
  return(c)
}