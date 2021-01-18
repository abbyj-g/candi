#~~~remove few records~~~
#remove entries with fewer than n records

#' Less than
#'
#' helper function for remove_lessthan
#helper function for remove_lessthan()
lessthan <- function(x, n) {
  if(!is.null(x)) {
    if(nrow(x) < n) {
      x <- NULL
    }
  }
  return(x)
}

#' remove less than
#'
#' remove data frames in the list with fewer than n occurrence records
#' @param df_list A list of data frames with latitude and longitude data for each species.
#' @param n the threshold of minimum number of occurrence records a dataframe must have to not be removed from the list
#' @return The same list of data frames with any elements in the list removed that had less than n records

remove_lessthan <- function(df_list, n = 5){ results_list <- lapply(df_list, lessthan, n = n) }
