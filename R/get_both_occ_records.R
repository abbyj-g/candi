#get both gbif and bien records and combine
#downloads data from gbif and bien databases and combines records given list of species names, sp
#get both gbif and bien records and combine
get_both_occ_records <- function(sp, limit = limit) {
  bien_results <- get_bien_occ_records(sp = sp) #get BIEN records
  gbif_results <- get_gbif_occ_records(sp = sp, limit = limit) #get GBIF records
  
  if (is.null(bien_results) & is.null(gbif_results)) {
    return(NULL)
  } else {
    #if there are only results from BIEN: 
    if (!is.null(bien_results) & is.null(gbif_results)) {
      as.tibble(bien_results) %>%
        dplyr::select(latitude, longitude) %>%
        mutate(source = rep("BIEN", times = nrow(bien_results))) %>%
        mutate(species = rep(sp, times = nrow(bien_results))) -> results
    }
    
    #if there are only results from GBIF:
    if (is.null(bien_results) & !is.null(gbif_results)) {
      as.tibble(gbif_results) %>%
        dplyr::select(latitude = decimalLatitude, 
                      longitude = decimalLongitude) %>%
        mutate(source = rep("GBIF", times = nrow(gbif_results))) %>%
        mutate(species = rep(sp, times = nrow(gbif_results))) -> results
    }  
    
    #if there are results from both, combine them
    if (!is.null(bien_results) & !is.null(gbif_results)) {
      as.tibble(bien_results) %>%
        dplyr::select(latitude, longitude) %>%
        mutate(source = rep("BIEN", times = nrow(bien_results))) %>%
        mutate(species = rep(sp, times = nrow(bien_results))) %>%
        bind_rows(data.frame(latitude = gbif_results$decimalLatitude,
                             longitude = gbif_results$decimalLongitude,
                             species = rep(sp, times = nrow(gbif_results)),
                             source = rep("GBIF", times = nrow(gbif_results)))) -> results
    } 
    
    return(results)
  }
}

# changing this general function to loop over vector of species and return a list 
get_occ_records <- function(species, database, limit = 500){
  
  if (database == "bien"){
    results <- list()
    for (i in 1:length(species)){
      print(paste0("obtaining records from BIEN of ", species[i]))
      bien_results <- get_bien_occ_records(sp = species[i])
      if (is.null(bien_results)) { print(paste0("No results found of ", species[i])) }
      results[[i]] <- bien_results
    }
    names(results) <- species
    return(results)
  }
  
  if (database == "gbif"){
    results <- list()
    for (i in 1:length(species)){
      print(paste0("obtaining records from GBIF of species: ", species[i]))
      gbif_results <- get_gbif_occ_records(sp = species[i], limit = limit)
      if (is.null(gbif_results)) {print(paste0("No results found of ", species[i]))}
      results[[i]] <- gbif_results
    }
    names(results) <- species
    return(results)
  }
  
  if (database == "both"){
    results <- list()
    for (i in 1:length(species)){
      print(paste0("obtaining records from BIEN and GBIF of species: ", species[i]))
      both_results <- get_both_occ_records(sp = species[i], limit = limit)
      if (is.null(both_results)) {print(paste0("No results found of ", species[i]))}
      results[[i]] <- both_results
      if (!is.null(both_results)) {names(results)[i] <- species[i]}
    }
    return(results)
  }
  
  else { stop("Please set database argument equal to either bien, gbif, or both to select data source") }
}