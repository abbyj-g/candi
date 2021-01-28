#' get both gbif and bien records and combine
#'
#' A function to download occurrence data for a species from both the BIEN and GBIF database and combine the records
#' @param sp A species name. First letter of genus names are capitalized and specific epithet is lowercase with a space separating the two.
#' @param limit A threshold of maximum number of occurrences to download. Default set to 500
#' @return A dataframe of occurrence data for species sp
#' @examples
#' get_both_occ_records("Notholaena standleyi", limit = 100)
get_both_occ_records <- function(sp, limit = 500) {
  bien_results <- get_bien_occ_records(sp = sp) #get BIEN records
  gbif_results <- get_gbif_occ_records(sp = sp, limit = limit) #get GBIF records

  if (is.null(bien_results) & is.null(gbif_results)) {
    return(NULL)
  } else {
    #if there are only results from BIEN:
    if (!is.null(bien_results) & is.null(gbif_results)) {
      a <- tibble::as_tibble(bien_results)
        b <- dplyr::select(a, latitude, longitude)
        c <- dplyr::mutate(b, source = rep("BIEN", times = nrow(bien_results)))
        results <- dplyr::mutate(c, species = rep(sp, times = nrow(bien_results)))
    }

    #if there are only results from GBIF:
    if (is.null(bien_results) & !is.null(gbif_results)) {
      a <- tibble::as_tibble(gbif_results)
        b <- dplyr::select(a, latitude = decimalLatitude,
                      longitude = decimalLongitude)
        c <- dplyr::mutate(b, source = rep("GBIF", times = nrow(gbif_results)))
        results <- dplyr::mutate(c, species = rep(sp, times = nrow(gbif_results)))
    }

    #if there are results from both, combine them
    if (!is.null(bien_results) & !is.null(gbif_results)) {
      a <- tibble::as_tibble(bien_results)
        b <- dplyr::select(a, latitude, longitude)
        c <- dplyr::mutate(b, source = rep("BIEN", times = nrow(bien_results)))
        d <- dplyr::mutate(c, species = rep(sp, times = nrow(bien_results)))
        results <- dplyr::bind_rows(d, data.frame(latitude = gbif_results$decimalLatitude,
                             longitude = gbif_results$decimalLongitude,
                             species = rep(sp, times = nrow(gbif_results)),
                             source = rep("GBIF", times = nrow(gbif_results))))
    }

    return(results)
  }
}
