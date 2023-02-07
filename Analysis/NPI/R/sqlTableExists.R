sqlTableExists <- function( conn, dbtable ) {
  query <- paste0( 'show tables like \'', dbtable, '\';' )
#  query <- paste0( 'SELECT count(*) as count FROM information_schema.tables WHERE table_name = \'', dbtable, '\';' )
  rs <- DBI::dbGetQuery( conn, query )
  return( nrow(rs)>0 )
}
