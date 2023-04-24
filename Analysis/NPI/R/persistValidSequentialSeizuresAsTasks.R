persistValidSequentialSeizuresAsTasks <- function( dbhost, dbname, dbuser, dbpassword, basedir_mef=NULL, durationMinutes=5, institution='Yale', lab='RNCP', experiment='SequentialSRC', signalType='NV', iterationType='directory' ) {
  library(uuid)
  
  durationUS <- durationMinutes * 1E6

  # This one does all patients
  conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=dbpassword, host=dbhost, dbName=dbName)# for software test
  query <- paste0( "use NV;" )
  DBI::dbGetQuery( conn, query )
#  persistBehavioralEvents( conn )
  DBI::dbDisconnect(conn)
  
  subject_ids <-  list(
    '23_003',
    '24_002',
    '24_005',
    '25_002',
    '25_003',
    '25_005'
  )
  
#  load(file='~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/useMatrix.RData') # contains the numbers of seizures to be analyzed as tasks for each subject
  sysInfo <- Sys.info()
  
  for ( subject_idx in seq(1,length(subject_ids)) ) {
    subject_id <- subject_ids[[subject_idx]]
    conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=dbpassword, host=dbhost, dbName=dbName,timeout=Inf)   # for software test
    query <- paste0( "use NV;" )
    DBI::dbGetQuery( conn, query )
    query <- paste0( 'select session from epochs where subject=\'', subject_id, '\' limit 1;' )
    rs <- DBI::dbGetQuery( conn, query )
    session <- rs$session[1]
#    DBI::dbDisconnect(conn)
    
#    persistDiscontinuitiesIntoEpochs( conn, subject=subject_id, basedir_mef=basedir_mef )

    validEpochs <- NPI:::findValidEpochs( conn=conn, project=dbname, subjectID=subject_id, minDuration=durationMinutes, basedir=basedir_mef )
    out_filename <- paste0( 'validEpochs_', subject_id )
    save(file=out_filename, validEpochs )
    
#    useSeizures <- use_matrix[subject_idx,]
#    conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=dbpassword, host=dbhost, dbName=dbName,timeout=Inf)   # for software test
    for ( row in seq_along(validEpochs$behavior$Ei_start) ) {
      
      centerTime <- validEpochs$behavior$seizure_start[row]
      analysisStart <- validEpochs$behavior$Ei_start[row] - durationUS
      analysisStop <- validEpochs$recovery$Ri_stop[row]
      query <- paste0( 'insert into tasks (username,institution,lab,nodename,experiment,subject,path,' )
      query <- paste0( query, 'service,taskName,signalType,iterationType,centerTime,analysisStart,analysisStop,UUID) values (' )
      query <- paste0( query, '\'', sysInfo['user'], '\',' )
      query <- paste0( query, '\'', institution, '\',' )
      query <- paste0( query, '\'', lab, '\',' )
      query <- paste0( query, '\'', sysInfo['nodename'], '\',' )
      query <- paste0( query, '\'', experiment, '\',' )
      query <- paste0( query, '\'', subject_id, '\',' )
      query <- paste0( query, '\'', basedir_mef, '\',' )
      query <- paste0( query, '\'', dbname, '\',' )
      query <- paste0( query, '\'', 'validSeizure', '\',' )
      query <- paste0( query, '\'', signalType, '\',' )
      query <- paste0( query, '\'', iterationType, '\',' )
      query <- paste0( query,  centerTime,', ' )
      query <- paste0( query, '\'', analysisStart, '\',' )
      query <- paste0( query, '\'', analysisStop, '\',' )
      query <- paste0( query, '\'', session, '\'' )
      query <- paste0( query, ');')
      DBI::dbGetQuery( conn, query )
    }
    DBI::dbDisconnect(conn)
  }
}

