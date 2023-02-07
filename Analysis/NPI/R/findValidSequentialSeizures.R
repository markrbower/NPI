findValidSequentialSeizures <- function( conn, subjectID, minDurationSeconds, validEpochs, includeRecovery = F, includeREM = F ) {
  # This code is out-of-date and not used anymore.
  # Most of the functionality was replacee by "findValidEpochs".
  
  clearAllDBcons()

  D <- NPI:::durationRequirement( conn, 'epochs', subjectID, minDurationSeconds )
  data <- D$data
  validIdx <- D$validIdx
  query <- paste0( 'select distinct event_start from seizures where subject=', subjectID, ';' )
  rs <- dbGetQuery( conn, query )
  zTimes <- sort( rs$event_start )
  
  validSzIdx <- NPI:::validSequentialSeizures( data, zTimes, validIdx, minDurationSeconds )
  return( validSzIdx )
}
