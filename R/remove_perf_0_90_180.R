#~~~remove perfect 0, 90, 180 coords~~~
#function to remove points with perfect 0, 90, or 180 coordinates
#' remove perfect 0, 90, 180 coordinates
#'
#' A function to remove points with perfect 0, 90, or 180 coordinates
#' @param df_list A list of data frames with latitude and longitude data for each species.
#' @return The same list of data frames with any points with perfect 0, 90, or 180 coordinates removed
remove_perf_0_90_180 <- function(df_list) {
  results_list <- list()
  for (i in 1:length(df_list)){
    df <- df_list[[i]]
    dont_keep <- which(df$latitude == 0 | df$latitude == 90 |
                         df$longitude == 0 | df$longitude == 180)
    if (length(dont_keep > 0)) {
      print(paste0("Removing ", dont_keep,
                   "localities that fall exactly at 0, 90, or 180 degrees from ",
                   names(results_list)[i], ", ", raster::nrow(df[-dont_keep, ]), " localities left." ))
      df <- df[-dont_keep, ]
    } else {print(paste0("No localities fall exactly at 0, 90, or 180 degrees in ", names(df_list)[i]))}
    results_list[[i]] <- df
  }
  names(results_list) <- names(df_list)
  return(results_list)
}
