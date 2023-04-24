getSeizureIDs <- function( subjectID ) {
  conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password='', host='localhost', dbName='NV')# for software test
  query <- paste0( "use NV;" )
  DBI::dbGetQuery( conn, query )

  query <- paste0( paste0( "select centerTime from tasks where subject='", subjectID, "';" ) )
  rs_tasks <- DBI::dbGetQuery( conn, query )
  validSeizureStartTime <- rs_tasks$centerTime
  
  query <- paste0( paste0( "select start from epochs where (label='old' or label='new') and subject='", subjectID, "';" ) )
  rs_epochs <- DBI::dbGetQuery( conn, query )
  allSeizureStartTime <- rs_epochs$start
  
  topconnect::clearAllDBcons()
  
  seizureNbr <- vector(mode='integer','length'=length(validSeizureStartTime))
  seizureTime <- vector(mode='integer','length'=length(validSeizureStartTime))
  for ( idx in seq(1,length(validSeizureStartTime))) {
    subjectSzIdx <- which( allSeizureStartTime == validSeizureStartTime[idx] )
    seizureNbr[idx] <- subjectSzIdx
    seizureTime[idx] <- allSeizureStartTime[subjectSzIdx]
  }
  nbr_time <- data.frame( nbr=seizureNbr, time=seizureTime )
}