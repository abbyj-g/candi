#' get bien records
#'
#' A function to download occurrence data from the BIEN database for a given list of species
#' @param sp A species name. First letter of genus names are capitalized and specific epithet is lowercase with a space separating the two.
#' @return A dataframe of occurrence data for species sp
#' @examples
#' get_bien_occ_records(c("Notholaena standleyi","Pellaea truncata"))
get_bien_occ_records <- function(sp) {
  records <- BIEN::BIEN_occurrence_species(sp)
  records <- records[!is.na(records$latitude),]
  if (nrow(records) < 1 ) {
    return(NULL) } else {
      return(records)
    }
}
