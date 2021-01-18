#remove points outside of native range
#shapefile of native range required
remove_points_outside_nat_range <- function(df_list, botan_map = kew_map_level_2, nat_range_df) {
  #recover()
  results_list <- list() 
  nat_range_df <- as.data.frame(nat_range_df)
  for (i in 1:length(df_list)){
    df <- df_list[[i]] # select dataframe of occurrences
    
    # process natural range values
    nat_range <- nat_range_df[which(nat_range_df$species == names(df_list)[i]), "range"]
    nat_range <- unlist(strsplit(nat_range, " "))
    
    coordinates(df) <- ~longitude+latitude #create a spatial points df from gbif results
    proj4string(df) <- botan_map@proj4string #give it the same proj as our world map
    ovr <- over(df, botan_map)
    
    for (j in 1:nrow(ovr)) {
      
      if (!ovr[j,"LEVEL2_COD"] %in% nat_range){
        df[j,] <- NA
      }
    }
    
    df <- as.data.frame(df)
    removed <- sum(is.na(df$species))
    print(paste0("Removed ", removed, " occurrences outside the native range of ", 
                 names(df_list)[i],", ", nrow(df)-removed, " occurrences remain."))
    results_list[[i]] <- df[complete.cases(df),]
    names(results_list)[i] <- names(df_list)[i]
    
  }
  
  return(results_list)
  
}