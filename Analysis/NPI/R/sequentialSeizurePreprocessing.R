sequentialSeizurePreprocessing <- function( dbhost, dbname, dbuser, dbpassword, basedir_mef=NULL, durationMinutes=5 ) {
  library(uuid)
  # 'duration' is the number of minutes an epoch must last to be considered valid
  print( getwd() )
  minDurationSeconds <- durationMinutes * 60
  
  # This one does all patients
#  conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=dbpassword, host=dbhost, dbName=dbName)   # for software test
#  query <- paste0( "use NV;" )
#  DBI::dbGetQuery( conn, query )
#  persistBehavioralEvents( conn )
#  DBI::dbDisconnect(conn)
  
  subject_ids <-  list(
    '23_003',
    '24_002',
    '24_005',
    '25_002',
    '25_003',
    '25_005'
  )

  for ( subject_id in subject_ids ) {
    print( subject_id )
    conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=dbpassword, host=dbhost, dbName=dbName)   # for software test
    query <- paste0( "use NV;" )
    DBI::dbGetQuery( conn, query )

    persistDiscontinuitiesIntoEpochs( conn, subject=subject_id, basedir_mef=basedir_mef )

    validEpochs <- NPI:::findValidEpochs( conn=conn, project=dbname, subjectID=subject_id, minDuration=durationMinutes )
    
    filename <- paste0( "validEpochs_", subject_id, ".RData" )
    save( file=filename, validEpochs )
    DBI::dbDisconnect(conn)
  }

#  tabulateBehavioralResult( validEpochs )
  
}

