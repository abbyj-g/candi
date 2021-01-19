#' remove points outside native range
#'
#' A function that removes occurrence points from each data frame in the list if it lies outside a given native range
#' @param df_list A list of data frames with latitude and longitude data for each species.
#' @param botan_map A map used to calibrate the projection. Default set to kew_map_level_2
#' @param nat_range_df A list of shapefiles of the native ranges of the species in the df_list. Can be obtained from Kew Garden website
#' @return The same list of data frames with occurrences removed that fell outside the defined native range of the species
remove_points_outside_nat_range <- function(df_list, botan_map = kew_map_level_2, nat_range_df) {
  #recover()
  results_list <- list()
  nat_range_df <- raster::as.data.frame(nat_range_df)
  for (i in 1:length(df_list)){
    df <- df_list[[i]] # select dataframe of occurrences

    # process natural range values
    nat_range <- nat_range_df[which(nat_range_df$species == names(df_list)[i]), "range"]
    nat_range <- unlist(strsplit(nat_range, " "))

    sp::coordinates(df) <- ~longitude+latitude #create a spatial points df from gbif results
    sp::proj4string(df) <- botan_map@proj4string #give it the same proj as our world map
    ovr <- sp::over(df, botan_map)

    for (j in 1:nrow(ovr)) {

      if (!ovr[j,"LEVEL2_COD"] %in% nat_range){
        df[j,] <- NA
      }
    }

    df <- raster::as.data.frame(df)
    removed <- sum(is.na(df$species))
    print(paste0("Removed ", removed, " occurrences outside the native range of ",
                 names(df_list)[i],", ", raster::nrow(df)-removed, " occurrences remain."))
    results_list[[i]] <- df[complete.cases(df),]
    names(results_list)[i] <- names(df_list)[i]

  }

  return(results_list)

}
