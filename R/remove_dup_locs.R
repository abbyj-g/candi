#~~~remove duplicates~~~
# function to remove duplicated latitude and longitudes
#' remove duplicate localities
#'
#' A function to remove duplicates latitude and longitudes
#' @param df_list A list of data frames with latitude and longitude data for each species. Object generated from CANDI occurrence data aquisition functions
#' @return The same list of dataframes with the duplicate localities removed
remove_dup_locs <- function(df_list) {
  results_list <- list()
  for (i in 1:length(df_list)){
    df <- df_list[[i]]
    if ("latitude" %in% colnames(df)){
      df_new <-  df[!duplicated(df[,c("latitude", "longitude")]),]
      rows_originial <- raster::nrow(df)
      rows_new <- raster::nrow(df_new)
      print(paste0("Removed ", rows_originial - rows_new,
                   " duplicated localities from ", names(df_list)[i], ", ",
                   rows_new, " unique localities left."))
      results_list[[i]] <- df_new
    } else {results_list[[i]] <- df}
  }
  names(results_list) <- names(df_list)
  return(results_list)
}
