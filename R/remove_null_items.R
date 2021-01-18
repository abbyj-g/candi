#remove null items 
remove_null_items <- function(df_list){
  new_list <- df_list[!unlist(lapply(df_list, is.null))]
  return(new_list)
}
