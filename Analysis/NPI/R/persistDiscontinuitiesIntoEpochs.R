persistDiscontinuitiesIntoEpochs <- function( conn, subject, basedir_mef=NULL ) {
  # Inputs:
  # - conn    Database connection returned by db()
  # - projet  String with database name
  # - subject String describing the subject
  # - basedir String of path to data root directory for MEF files
  # Side Effect:
  # - modifies database-name.epochs table with start/stop of discontinuities
  
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/filenameFromCase.R')

  # Find an example file
  if ( is.null( basedir_mef ) ) {
    #  basedir_mef <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
    # basedir_mef <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_', subject, '_2' )
    basedir_mef <- paste0( '/Volumes/Data/NV/NVC1001_', subject, '_2' )
  }
  subjectdir_mef <- paste0( basedir_mef, "/NVC1001_", subject, "_2/")
  files <- list.files( subjectdir_mef, "*.mef", full.names=TRUE )
  if ( length(files) == 0 ) {
    print( paste0( "ERROR: persistDiscontinuitiesIntoEpochs: No matching files in ", basedir_mef ) )
    return()
  }
  mef_filename <- files[[1]]
  print( mef_filename )
  vault <- topsecret::get_secret_vault()
  password_key <- paste0( 'MEF_password' )
  info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
  suid <- info$header$session_unique_ID

  contiguousStarts <- which( info$discontinuities == 1 )
  contiguousStops <- c(contiguousStarts-1, ncol(info$ToC) )
  contiguousStops <- contiguousStops[-1]
  contiguousStartTimes <- info$ToC[1,contiguousStarts]
  # The time of the first "missing" sample is ...
  contiguousStopTimes <- info$ToC[1,contiguousStops] + info$header$block_interval
  
  # Persist
  fields <- list( 'subject', 'session', 'label', 'start', 'stop' )
  
  dib <- topconnect::databaseInsertBuffer( dbName='NV', dbTable='epochs', fields=fields, limit=100, updates=NULL, dbuser='root', host='localhost', password='' )

  N <- length( contiguousStopTimes )
  n <- 0
  while ( n < N ) {
    n <- n + 1
    dib$insert( list(subject=subject, session=suid, label='continuity', start=contiguousStartTimes[n], stop=contiguousStopTimes[n]) )    
#    dib$insert( list(subject=subject, label='\'continuity\'', start=contiguousStartTimes[n], stop=contiguousStopTimes[n]) )    
  }
  dib$flush()
}
