persistFirstOfSequentialValidSeizuresToTasks <- function( dbhost, dbname, dbuser, dbpassword, basedir_mef=NULL, durationMinutes=5, institution='Yale', lab='RNCP', experiment='SequentialSRC', signalType='NV', iterationType='directory' ) {
    
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/getSeizureIDs.R")
  
  subject_ids <-  list(
    '23_003',
    '24_002',
    '24_005',
    '25_002',
    '25_003',
    '25_005'
  )
  
  useDictionary <- list()
  allValidFirstSeizures <- list()
  validFirstSeizures <- list()

  for ( subject_id in subject_ids ) {
    txt <- paste0( "nbr_time <- getSeizureIDs('", subject_id, "')")
    eval(parse(text=txt))
    dnbrs <- diff( nbr_time$nbr )
    
    # run a state-machine to find "1 1", keeping the index of only the first.
    candidates <- vector()
    buffer <- -1
    for ( cnt in seq(1:(length(dnbrs)-1) ) ) {
      if ( dnbrs[cnt] != 1 ) { # reset
        buffer <- -1
      } else {
        if ( buffer == -1 ) {
          buffer <- 1
          candidates <- cbind( candidates, cnt )
        } else { # reset
          buffer <- -1
        }
      }
    }
    keepers <- linspace( 1, length(candidates), 6 )
    validFirstSeizures <- candidates[round(keepers)]
    useDictionary[[subject_id]] <- validFirstSeizures
 
    sysInfo <- Sys.info()
    
    conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=dbpassword, host=dbhost, dbName=dbName,timeout=Inf)   # for software test
    query <- paste0( 'use NV;' )
    DBI::dbGetQuery( conn, query )
    query <- paste0( 'select session from epochs where subject=\'', subject_id, '\' limit 1;' )
    rs <- DBI::dbGetQuery( conn, query )
    session <- rs$session[1]
    
    for ( szNbr in seq_along(validFirstSeizures) ) {
      centerTime <-nbr_time$time[szNbr]
      analysisStart <- validFirstSeizures[szNbr]
      analysisStop <- analysisStart + 1
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
      query <- paste0( query, '\'', 'firstSeizureOfValidPair', '\',' )
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
  } # subject
  return( useDictionary  )
}

