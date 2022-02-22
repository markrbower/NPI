maxIDfromVectorOrDatabase <- function( vec, compArgs ) {
  library( RMySQL )
  
  # Check database for largest ID for this case
  dbp <- compArgs$findClass( 'databaseProvider' )
  conn <- dbp$connect()
  P_table <- compArgs$get( 'P' )
  query <- paste0( "select max(clusterid) as maxID from ", P_table, whereClauseFromCase( compArgs ), ";" )
  rs <- dbGetQuery( conn, query )
  maxID_db <- max(0,rs$maxID,na.rm=TRUE)
  DBI::dbDisconnect( conn )

  # Check the vec for the largest ID
  maxID_vec <- max( max( vec, na.rm=TRUE ), 0 )
  
  # Take the max of both.
  maxID <- max( maxID_db, maxID_vec )
}
