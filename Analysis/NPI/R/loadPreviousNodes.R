loadPreviousNodes <- function( t0, CW, compArgs ) {
  library(stringr)
  library(DBI)
  
  dbp <- compArgs$findClass( 'databaseProvider' )
  conn <- dbp$connect()
  P <- compArgs$get('P')
  query <- paste0( 'select time,incident,clusterid,waveform from ', P, ' where subject=\'', compArgs$get('subject'), '\' and channel=\'', compArgs$get('channel'), '\' and seizureUsed=', as.numeric(compArgs$get('case')$centerTime), " and time > ", t0-CW, " and time < ", t0, ";" )
#  print( query )
  rs <-  DBI::dbGetQuery( conn, query )
  if ( nrow(rs) == 0 ) {
    output <- data.frame( time=integer(length=0), incident=character(length=0), clusterid=integer(length=0), waveform=character(length=0))
  } else {
    output <- data.frame( time=rs$time, incident=rs$incident, clusterid=rs$clusterid, waveform=rs$waveform )
  }
  dbDisconnect( conn )
  return( output )
}
