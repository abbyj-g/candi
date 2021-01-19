#' make corr matrix
#'
#' A functiont that creates a correlation matrix between the environmental data variables of correlation within the area of the maximum extents of occurrence points
#' @param ocurrences A list of data frames with the occurrence data for each species
#' @param environment_data A raster stack of environmental raster data variables
#' @return a pdf of the correlation matrix is saved to the local disk
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
  p <- sp::Polygon(rect_coords)
  ps <- sp::Polygons(list(p),1)
  polygon_mask <- sp::SpatialPolygons(list(ps))

  ##crop raster stack##
  cropped_clim_corr <- raster::crop(environment_data, polygon_mask)

  #correlation analysis between layers
  print("Calculating correlation coefficients of climatic variables")
  corr <- raster::layerStats(cropped_clim_corr, 'pearson', na.rm=TRUE)
  c_matrix <- corr$`pearson correlation coefficient`
  name <- paste0("correlationBioClim", Sys.Date(), ".csv")
  print(paste0("Saving correlation coefficients to file in your current working directory: ",
               getwd(), "/", name))
  write.csv(c_matrix, file = name)

  #make a correlation plot, highlighting  absolute values greater than 0.8
  df <- raster::as.data.frame(c_matrix)
  ggplot2::ggcorr(df, geom = "blank", label = TRUE, hjust = 0.75) +
    ggplot2::geom_point(size = 10, aes(alpha = abs(coefficient) > 0.8)) +
    ggplot2::scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
    ggplot2::guides(color = FALSE, alpha = FALSE)

  #export plot as a pdf, saved as correlation_plot.pdf in your working directory
  name <- paste0("correlationsBioClim", Sys.Date(), ".pdf")
  print(paste0("Saving PDF of correlation coefficients plot to your current working directory: ",
               getwd(), "/", name))
  print("Correlation coefficients greater than .8 or less than -.8 are highlighted in grey.")
  ggplot2::ggsave(name, plot = last_plot(), device = "pdf", path = NULL,
         scale = 1, width = NA, height = NA)

  return(c)
}
