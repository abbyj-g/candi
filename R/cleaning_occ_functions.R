### CLEANING OCCURRENCE DATA FUNCTIONS ###
##########################################
#The functions in this script are:
# remove_dup_locs
# remove_ocean_points
# remove_perf_0_90_180
# remove_lessthan
# remove_points_outside_nat_range

#list is a list of dataframes of occ data. Each element in the list is a different species 

#~~~remove duplicates~~~
# function to remove duplicated latitude and longitudes  
remove_dup_locs <- function(df_list) {
  results_list <- list()
  for (i in 1:length(df_list)){
    df <- df_list[[i]]
    if ("latitude" %in% colnames(df)){
      df_new <-  df[!duplicated(df[,c("latitude", "longitude")]),]
      rows_originial <- nrow(df)
      rows_new <- nrow(df_new)
      print(paste0("Removed ", rows_originial - rows_new, 
                   " duplicated localities from ", names(df_list)[i], ", ", 
                   rows_new, " unique localities left."))
      results_list[[i]] <- df_new
    } else {results_list[[i]] <- df}
  }
  names(results_list) <- names(df_list)
  return(results_list)
} 

#~~~remove ocean points~~~
#funtion to remove points in the ocean given a world map shapefile
#library(sp)
#library(maptools)
remove_ocean_points <- function(df_list, world_map){
  results_list <- list()
  for (i in 1:length(df_list)){
    
    df <- df_list[[i]]
    
    coordinates(df) <- ~longitude+latitude #create a spatial points df from gbif results
    proj4string(df) <- world_map@proj4string #give it the same proj as our world map
    ovr <- over(df, world_map) #check what country each point is in, given our world map
    
    df@data <- cbind(df@data, ovr) #bind together the original dataframe with the over results 
    
    df <- as.data.frame(df)
    
    if (sum(is.na(df$NAME_ENGLI)) > 0) {
      df_new <- df[-which(is.na(df$NAME_ENGLI)),]
      rows_original <- nrow(df)
      rows_new <- nrow(df_new)
      print(paste0("Removed ", rows_original - rows_new, 
                   " localities in the ocean ", names(df_list)[i], ", ", 
                   rows_new, " localities left."))
      df_return <- df_new[,c("latitude", "longitude", "source", "species")] #remove points with no associated country 
    } else {
      print(paste0("No ocean points detected in ", names(df_list)[i]))
      df_return <- df[,c("latitude", "longitude", "source", "species")]
    }
    
    results_list[[i]] <- as.tibble(df_return)
  }  
  names(results_list) <- names(df_list)
  return(results_list)
}

#~~~remove perfect 0, 90, 180 coords~~~
#function to remove points with perfect 0, 90, or 180 coordinates
remove_perf_0_90_180 <- function(df_list) {
  results_list <- list()
  for (i in 1:length(df_list)){
    df <- df_list[[i]]
    dont_keep <- which(df$latitude == 0 | df$latitude == 90 | 
                         df$longitude == 0 | df$longitude == 180)
    if (length(dont_keep > 0)) {
      print(paste0("Removing ", dont_keep, 
                   "localities that fall exactly at 0, 90, or 180 degrees from ",
                   names(results_list)[i], ", ", nrow(df[-dont_keep, ]), " localities left." ))
      df <- df[-dont_keep, ] 
    } else {print(paste0("No localities fall exactly at 0, 90, or 180 degrees in ", names(df_list)[i]))}
    results_list[[i]] <- df
  }
  names(results_list) <- names(df_list)
  return(results_list)
}

#~~~remove few records~~~

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

#remove null items 
remove_null_items <- function(df_list){
  new_list <- df_list[!unlist(lapply(df_list, is.null))]
  return(new_list)
}


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