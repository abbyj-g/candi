#remove null items
#'remove null items
#'
#'a function to remove null items
#' @param df_list A list of data frames with latitude and longitude data for each species.
#' @return The same list of data frames with any elements in the list removed that were null

remove_null_items <- function(df_list){
  new_list <- df_list[!unlist(lapply(df_list, is.null))]
  return(new_list)
}
