#~~~remove few records~~~
#remove entries with fewer than n records
#helper function for remove_lessthan()
lessthan <- function(x, n) {
  if(!is.null(x)) {
    if(nrow(x) < n) {
      x <- NULL
    }
  }
  return(x)
}

#remove entries w/ fewer than n records
remove_lessthan <- function(df_list, n = 5){ results_list <- lapply(df_list, lessthan, n = n) }
