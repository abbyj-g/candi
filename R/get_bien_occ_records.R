#get bien records
#downloads data from bien database given list of species names, sp
get_bien_occ_records <- function(sp) {
  records <- BIEN_occurrence_species(sp)
  records <- records[!is.na(records$latitude),]
  if (nrow(records) < 1 ) { 
    return(NULL) } else {
      return(records) 
    }
}