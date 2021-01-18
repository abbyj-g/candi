#' Get occurrence records
#'
#' A function that downloads occurrence records for each species in a list of species from either BIEN, GBIF, or both databases
#' @param species A list of species separated by spaces. First letter of genus names are capitalized and specific epithet is lowercase with a space separating the two.
#' @param limit A threshold of maximum number of occurrences to download. Default set to 500
#' @param database User specifies if they want occurrence data downloaded from BIEN, GBIF, or both. Value must be "bien", "gbif", or "both"
#' @return A list of dataframes of occurrence data for each species in species
#' @examples
#' get_occ_records(c("Notholaena standleyi","Pellaea truncata", "Astrolepis integerrima"), "both", limit = 500)
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
