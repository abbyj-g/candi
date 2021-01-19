#~~~remove ocean points~~~
#funtion to remove points in the ocean given a world map shapefile
#library(sp)
#library(maptools)
#' remove ocean points
#'
#' A function to remove points in the ocean given a world map shapefile
#' @param df_list A list of data frames with latitude and longitude data for each species.
#' @param world_map A shapefile of the world's coastlines, can be obtained from the wrld_simpl data from the maptools package
#' @return The same list of dataframes with localities lying over the ocean removed
remove_ocean_points <- function(df_list, world_map){
  results_list <- list()
  for (i in 1:length(df_list)){

    df <- df_list[[i]]

    sp::coordinates(df) <- ~longitude+latitude #create a spatial points df from gbif results
    sp::proj4string(df) <- world_map@proj4string #give it the same proj as our world map
    ovr <- sp::over(df, world_map) #check what country each point is in, given our world map

    df@data <- cbind(df@data, ovr) #bind together the original dataframe with the over results

    df <- raster::as.data.frame(df)

    if (sum(is.na(df$NAME_ENGLI)) > 0) {
      df_new <- df[-which(is.na(df$NAME_ENGLI)),]
      rows_original <- raster::nrow(df)
      rows_new <- raster::nrow(df_new)
      print(paste0("Removed ", rows_original - rows_new,
                   " localities in the ocean ", names(df_list)[i], ", ",
                   rows_new, " localities left."))
      df_return <- df_new[,c("latitude", "longitude", "source", "species")] #remove points with no associated country
    } else {
      print(paste0("No ocean points detected in ", names(df_list)[i]))
      df_return <- df[,c("latitude", "longitude", "source", "species")]
    }

    results_list[[i]] <- tibble::as.tibble(df_return)
  }
  names(results_list) <- names(df_list)
  return(results_list)
}
