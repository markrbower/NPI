persistValidSeizuresAsTasks <- function( subjectID, duration, dbhost, dbname, dbuser, dbpassword ) {
  library(uuid)
  
  validEpochs <- NPO:::findValidEpochs( project=dbname, subjectID=subjectID, minDuration=duration )
  valid_idx <- findValidSeizures( validEpochs, includeRecovery = T, includeREM = T)
  validSeizureInfo <- data.frame(seizureStart=behavior$seizure_start[valid_idx],
                                 analysisStart=behavior$Ei_start[valid_idx],
                                 analysisStop=recovery$Ri_stop[valid_idx])  
  
  conn <- topconnect::db( user=dbuser, dbname=dbname, host=dbhost, password=dbpassword )
  query <- paste0( "select * from tasks where subject=\'", subjectID, "\';")
  context <- DBI::dbGetQuery( conn, query )

  for ( row in seq(1,nrow(validSeizureInfo) ) ) {
    info <- Sys.info()
    centerTime <- validSeizureInfo$seizureStart[row]
    parameters <- paste( "analysisStart::", validSeizureInfo$analysisStart[row],
                         ":::analysisStop::", validSeizureInfo$analysisStop[row] )
    query <- paste0( 'insert into tasks (username,institution,lab,nodename,experiment,subject,path,' )
    query <- paste0( query, 'service,taskName,signalType,iterationType,centerTime,parameters,UUID) values (' )
    query <- paste0( query, '\'', info['user'], '\',' )
    query <- paste0( query, '\'', context$institution, '\',' )
    query <- paste0( query, '\'', context$lab, '\',' )
    query <- paste0( query, '\'', info['nodename'], '\',' )
    query <- paste0( query, '\'', context$experiment, '\',' )
    query <- paste0( query, '\'', context$subject, '\',' )
    query <- paste0( query, '\'', context$path, '\',' )
    query <- paste0( query, '\'', context$service, '\',' )
    query <- paste0( query, '\'', 'validSeizure', '\',' )
    query <- paste0( query, '\'', context$signalType, '\',' )
    query <- paste0( query, '\'', context$iterationType, '\',' )
    query <- paste0( query,  centerTime,', ' )
    query <- paste0( query, '\'', parameters, '\',' )
    query <- paste0( query, '\'', uuid::UUIDgenerate(), '\'' )
    query <- paste0( query, ');')
    DBI::dbGetQuery( conn, query )
  }
}

