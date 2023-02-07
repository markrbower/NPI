loadPreviousPeaks <- function( t0, CW, compArgs ) {
  library(stringr)
  library(DBI)
  
  dbp <- compArgs$findClass( 'databaseProvider' )
  conn <- dbp$connect()
  P <- compArgs$get('P')
  query <- paste0( 'select time,incident,clusterid,waveform from ', P, ' where subject=\'', compArgs$get('subject'), '\' and channel=\'', compArgs$get('channel'), '\' and seizureUsed=', as.numeric(compArgs$get('case')$centerTime), " and time > ", t0-CW, " and time < ", t0, ";" )
#  print( query )
  rs <-  DBI::dbGetQuery( conn, query )
  if ( length(rs) == 0 ) {
    peak_matrix <- matrix( nrow=0, ncol=0 )
  } else {
    peak_matrix <- as.matrix( sapply( 1:nrow(rs), function(x) {as.numeric( unlist( stringr::str_split(rs$waveform[x],',') ) ) } ) )
    if ( !is.null(peak_matrix) ) {
      if ( length(peak_matrix)==0 ) {
        peak_matrix <- matrix( nrow=0, ncol=0 )
      } else if ( nrow(peak_matrix)>0 & ncol(peak_matrix)>0 ) {
        if ( is.na(peak_matrix[1,1] ) ) {
          peak_matrix <- matrix( nrow=0, ncol=0 )
        }
      }
    } else {
      peak_matrix <- matrix( nrow=0, ncol=0 )
    }
  }
  if ( !is.null(peak_matrix) ) {
    attr( peak_matrix, 'T' ) <- rs$time
    attr( peak_matrix, 'clusterid' ) <- integer( length=length(rs$time) )
    attr( peak_matrix, 'counter' ) <- 0
  }
  DBI::dbDisconnect(conn)
  return( peak_matrix )
}
