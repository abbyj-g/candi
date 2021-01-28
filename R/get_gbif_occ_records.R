#' get gbif records
#'
#' A function to download occurrence data from the GBIF database for a given list of species
#' @param sp A species name. First letter of genus names are capitalized and specific epithet is lowercase with a space separating the two.
#' @param limit A threshold of maximum number of occurrences to download. Default set to 500
#' @return A dataframe of occurrence data for species sp
#' @examples
#' get_gbif_occ_records(c("Notholaena standleyi","Pellaea truncata"), limit = 100)
get_gbif_occ_records <- function(sp, limit = 500) {
  res <- rgbif::occ_search(scientificName = sp, limit = limit, hasCoordinate	= TRUE)
  if ("data.frame" %in% class(res$data)) {
    #res %>%  #remove entries with some error codes (could make this a function argument...)
      results <- rgbif::occ_issues(res, -bri, -cdiv, -cdout, -cucdmis,
                 -preneglat, -preneglon, -preswcd,
                 -txmatfuz, -txmathi, -txmatnon, -zerocd)
    results <- results$data
    return(results)
  } else { return( NULL ) }
}
