getSubjectCode <- function( compArgs, filename ) {
  
  db_provider <- compArgs$findClass( 'databaseProvider' )
  conn <- db_provider$connect()
  
  # Does the subject already have a code?
  query <- paste0( 'select count(*) as count from offsets where actual=\'', compArgs$get('subject'), ';' )
  rs <- DBI::dbGetQuery( conn, query )
  if ( rs$count > 0 ) { # This subject is in the table
    query <- paste0( 'select encoded,offset from offsets where actual=\'', compArgs$get('subject'), ';' )
    rs <- DBI::dbGetQuery( conn, query )
    return( list( encoding=rs$encoding, offset=rs$offset ) )
  } else { # Need to generate new info
    # time offset
    file_password <- getPassword( compArgs )
    info <- meftools::mef_info( c(filename, getPassword( compArgs )) )
    offset <- info$header$recordingStartTime
    # encoding
    value <- rs$count + 1
    encoding <- sprintf(paste0("%0", 2, "d"), value)
    return( list( encoding=rs$encoding, offset=rs$offset ) )
  }
}

