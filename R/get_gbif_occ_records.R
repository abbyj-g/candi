#get gbif records
#downloads data from gbif database given list of species names, sp
get_gbif_occ_records <- function(sp, limit = limit) {
  res <- occ_search(scientificName = sp, limit = limit, hasCoordinate	= TRUE) 
  if ("data.frame" %in% class(res$data)) {
    res %>%  #remove entries with some error codes (could make this a function argument...)
      occ_issues(-bri, -cdiv, -cdout, -cucdmis, 
                 -preneglat, -preneglon, -preswcd,
                 -txmatfuz, -txmathi, -txmatnon, -zerocd) -> results
    results <- results$data 
    return(results)
  } else { return( NULL ) }
} 
