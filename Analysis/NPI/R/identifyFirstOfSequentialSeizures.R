identifyFirstOfSequentialSeizures <- function() {
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/getSeizureIDs.R")
  
  subject_ids <-  list(
    '23_003',
    '24_002',
    '24_005',
    '25_002',
    '25_003',
    '25_005'
  )

  for ( id in subject_ids ) {
#    txt <- paste0( "id_", id, " <- getSeizureIDs('", id, "')")
    txt <- paste0( "nbrs <- getSeizureIDs('", id, "')")
    eval(parse(text=txt))
    print( diff(nbrs) )
    
    
  }
  
  
    
}