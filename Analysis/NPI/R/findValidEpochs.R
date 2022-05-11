findValidEpochs <- function( conn, project, subjectID, minDuration ) {
  # Following the Methods in the manuscript (Bower et al., 2022),
  # From each seizure onset,
  #   backtrack to find the lowest latency Sleep epoch lasting minDuration,
  #   without encountering a different seizure,
  #   then find the lowest-latency, surrounding Wake epochs,
  #   forwardtrack and do the same.
  #   Ensure that "base" and "pre" do not overlap.
  #   Find "late" as the last Wake epoch with no intervening sleep to "early".
  #
  # Returns two dataframes:
  # - 10-element dataframe of sleep/wake/seizure
  #   the starts/stops for 10 epochs
  # - 4-element dataframe of REM
  #   the starts/stops for 4 REM epochs within the sleep (NREM) epochs
  
  MIN_SLEEP_DURATION <- 240 # The number of contiguous 30 sec bins spent in sleep (240 is 2 hours)
  
  case <- data.frame( subject=subjectID, channel_name=paste0("NVC1001_",subjectID,"_01.mef") )
  #  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
  #  basedir <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_', case$subject, '_2' )
  #  basedir <- paste0( '/Volumes/Data/NV/NVC1001_', case$subject, '_2' )
  basedir <- paste0( '/Volumes/eplab/Raw_Data/NV_Human/NVC1001_', case$subject, '_2' )
  mef_filename <- NPI:::filenameFromCase( basedir, case )
  vault <- topsecret::get_secret_vault()
  password_key <- paste0( 'MEF_password' )
#  load( file="deleteThisTmpInfo" )
  info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
  T0 <- info$header$recording_start_time

  runLength <- minDuration * 2; # Assumes 'minDuration' is in minutes and behaviors are recorded in 30 sec windows.
  
  # Find all seizures.
#  query <- paste0("select start,stop from epochs where subject=\'",subjectID,"\' and label='seizure';")
  query <- paste0("select start,stop from epochs where subject=\'",subjectID,"\' and (label='new' or label='old' or label='seizure');")
  seizureTimes <- DBI::dbGetQuery( conn, query )

  # Find all sleep-stage behaviors
  query_base <- paste0("select label,start,stop from epochs where subject=\'",subjectID,"\' and ")
  query <- paste0( query_base, "(label='NREM1' or label='NREM2' or label='NREM3' ")
  query <- paste0( query, "or label='WAKE' or label='REM');")
  behaviorTimes <- DBI::dbGetQuery( conn, query )
  IDX_LIGHT  <- which( behaviorTimes[,'label'] == "NREM1" | behaviorTimes[,'label'] == "NREM2" )
  IDX_DEEP   <- which( behaviorTimes[,'label'] == "NREM3" | behaviorTimes[,'label'] == "NREM4" )
  IDX_NREM <- union( IDX_LIGHT, IDX_DEEP )
  IDX_REM <- which( behaviorTimes[,'label'] == "REM" )
  
  # Special case! Don't let intermittent 'WAKE' bins disrupt 'SLEEP' runs.
  IDX_SLEEP <- union( IDX_NREM, IDX_REM )
  IDX_AWAKE <- which( behaviorTimes[,'label'] == "WAKE" )
  
  # Do you want to give LIGHT/DEEP different numbers?
  behaviorTimes[,'number'] <- behaviorTimes[,'label']
  AWAKE <- 1
  SLEEP <- 0
  REM   <- 3
  LIGHT <- 4
  DEEP  <- 5
  behaviorTimes[ IDX_AWAKE, 'number']   <- AWAKE
  behaviorTimes[ IDX_REM, 'number']     <- REM
  behaviorTimes[ IDX_LIGHT, 'number']   <- LIGHT
  behaviorTimes[ IDX_DEEP, 'number']    <- DEEP

  # SLEEP / WAKE
  behaviorTimes[ IDX_AWAKE,'sleep_wake'] <- AWAKE
  behaviorTimes[ IDX_SLEEP,'sleep_wake'] <- SLEEP
  
  # Allow "transint waking" of 1 minute (2 bins)
  # erosion
  X <- behaviorTimes[,'sleep_wake']
  L <- length(X)
  for ( count in seq(1,2) ) {
    X[2:(L-1)] <- X[1:(L-2)] & X[2:(L-1)] & X[3:L]
  }
  # dilation
  for ( count in seq(1,2) ) {
    X[2:(L-1)] <- X[1:(L-2)] | X[2:(L-1)] | X[3:L]
  }
  behaviorTimes[,'sleep_wake'] <- X

  # Loop.
  behavior <- data.frame()
  recovery <- data.frame()
  rem <- data.frame()
  
  for ( index in 1:nrow(seizureTimes) ) {
    print( paste0( index, " of ", nrow(seizureTimes) ) )
    seizure <- seizureTimes[index,]
    
    # BEFORE THE SEIZURE
    # Find the lowest latency to a preceding sleep, leaving room for 'base'line and 'pre'.
    idx <- which( behaviorTimes[,'stop'] < (seizure$start-15*60*1E6) )
    valid <- NPI:::lastRunOfLength( idx[1], MIN_SLEEP_DURATION, behaviorTimes[idx,'sleep_wake'], SLEEP ) # SLEEP at least 15 min
    Esleep <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    # Is there an intervening pre-seizure?
    if ( nrow(Esleep)==0 ) {
      Esleep <- data.frame( start=NA, stop=NA )
      interveningPRE <- NA
    } else {
      idx <- which( (seizureTimes > Esleep$stop) & (seizureTimes < seizure$start) )
      if ( length(idx) == 0 ) {
        interveningPRE <- FALSE
      } else {
        interveningPRE <- TRUE
      }
    }
    
    # Find the preceding Wake.
    idx <- which( behaviorTimes[,'stop'] <= Esleep$start )
    valid <- NPI:::lastRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], AWAKE ) # AWAKE
    valid['start'] <- valid['stop']-runLength+1
    Ei <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Ei)==0 ) Ei <- data.frame( start=NA, stop=NA )
    
    # Find the following Wake.
    idx <- which( behaviorTimes[,'start'] >= Esleep$stop & behaviorTimes[,'start'] < seizure$start)
    valid <- NPI:::firstRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], AWAKE ) # AWAKE
    valid['stop'] <- valid['start'] + runLength - 1
    base <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(base)==0 ) base <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained REM episode between these AWAKE epochs
    idx <- which( behaviorTimes[,'start'] >= Esleep$start & behaviorTimes[,'stop'] <= Esleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], 1, behaviorTimes[idx,'number'], REM ) # REM
    valid['start'] <- valid['stop']-runLength+1
    Er <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Er)==0 ) Er <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained LIGHT episode between these AWAKE epochs
    idx <- which( behaviorTimes[,'start'] >= Esleep$start & behaviorTimes[,'stop'] <= Esleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], LIGHT ) # LIGHT
    valid['start'] <- valid['stop']-runLength+1
    Elt <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Elt)==0 ) Elt <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained DEEP episode between these AWAKE epochs
    idx <- which( behaviorTimes[,'start'] >= Esleep$start & behaviorTimes[,'stop'] <= Esleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], DEEP ) # DEEP
    valid['start'] <- valid['stop']-runLength+1
    Edp <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Edp)==0 ) Edp <- data.frame( start=NA, stop=NA )

    # AFTER THE SEIZURE
    # Find the lowest latency to a following sleep.
    idx <- which( behaviorTimes[,'start'] >= (seizure$stop + 15*60*1E6) )
    valid <- NPI:::firstRunOfLength( idx[1], MIN_SLEEP_DURATION, behaviorTimes[idx,'sleep_wake'], SLEEP ) # SLEEP
    Osleep <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    
    # Find the preceding Wake.
    idx <- which( behaviorTimes[,'stop'] < Osleep$start )
    valid <- NPI:::lastRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], AWAKE ) # AWAKE
    valid['start'] <- valid['stop']-runLength+1
    post <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(post)==0 ) post <- data.frame( start=NA, stop=NA )
    
    # Find the following Wake.
    idx <- which( behaviorTimes[,'start'] > Osleep$stop )
    valid <- NPI:::firstRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], AWAKE ) # AWAKE
    valid['stop'] <- valid['start'] + runLength - 1
    Oearly <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Oearly)==0 ) Oearly <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained REM episode
    idx <- which( behaviorTimes[,'start'] >= Osleep$start & behaviorTimes[,'stop'] <= Osleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], 1, behaviorTimes[idx,'number'], REM ) # REM
    valid['stop'] <- valid['start'] + runLength - 1
    Or <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Or)==0 ) Or <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained LIGHT episode
    idx <- which( behaviorTimes[,'start'] >= Osleep$start & behaviorTimes[,'stop'] <= Osleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], LIGHT ) # LIGHT
    valid['stop'] <- valid['start'] + runLength - 1
    Olt <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Olt)==0 ) Olt <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained DEEP episode
    idx <- which( behaviorTimes[,'start'] >= Osleep$start & behaviorTimes[,'stop'] <= Osleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], DEEP ) # DEEP
    valid['stop'] <- valid['start'] + runLength - 1
    Odp <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Odp)==0 ) Odp <- data.frame( start=NA, stop=NA )
    
    # Compute "pre"
    pre <- data.frame(start=seizure$start-minDuration*60*1E6,stop=seizure$start)
    if ( nrow(pre)==0 ) pre <- data.frame( start=NA, stop=NA )

    # Find the pOst "late" epoch.
    # After "early" and before subsequent sleep
    # first, find the next sleep epoch after "early"
    # call this time-period the "R" period for "Recovery"
    idx <- which( behaviorTimes[,'start'] >= Oearly$stop )
    valid <- NPI:::firstRunOfLength( idx[1], MIN_SLEEP_DURATION, behaviorTimes[idx,'sleep_wake'], SLEEP ) # SLEEP
    Rsleep <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Rsleep)==0 ) Rsleep <- data.frame( start=NA, stop=NA )
    

    # Is there an intervening post-seizure?
    if ( nrow(Rsleep)==0 ) {
      Rsleep <- data.frame( start=NA, stop=NA )
      interveningPOST <- NA
    } else {
      idx <- which( (seizureTimes > seizure$stop) & (seizureTimes < Rsleep$start) )
      if ( length(idx) == 0 ) {
        interveningPOST <- FALSE
      } else {
        interveningPOST <- TRUE
      }
    }

    # Find the shortest-latency awake period preceding Recovery sleep as "Rlate"
    idx <- which( behaviorTimes[,'stop'] <= Rsleep$start )
    valid <- NPI:::lastRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], AWAKE ) # AWAKE
    valid['start'] <- valid['stop']-runLength+1
    Rlate <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Rlate)==0 ) Rlate <- data.frame( start=NA, stop=NA )
    
    # Find the shortest-latency awake period following recovery sleep as "Ri"
    idx <- which( behaviorTimes[,'start'] >= Rsleep$stop )
    valid <- NPI:::firstRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], AWAKE ) # AWAKE
    valid['stop'] <- valid['start'] + runLength - 1
    Ri <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Ri)==0 ) Ri <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained REM episode
    idx <- which( behaviorTimes[,'start'] >= Rsleep$start & behaviorTimes[,'stop'] <= Rsleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], 1, behaviorTimes[idx,'number'], REM ) # REM
    valid['stop'] <- valid['start'] + runLength - 1
    Rr <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Rr)==0 ) Rr <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained LIGHT episode
    idx <- which( behaviorTimes[,'start'] >= Rsleep$start & behaviorTimes[,'stop'] <= Rsleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], LIGHT ) # LIGHT
    valid['stop'] <- valid['start'] + runLength - 1
    Rlt <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Rlt)==0 ) Rlt <- data.frame( start=NA, stop=NA )
    
    # Find the longest contained DEEP episode
    idx <- which( behaviorTimes[,'start'] >= Rsleep$start & behaviorTimes[,'stop'] <= Rsleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], runLength, behaviorTimes[idx,'number'], DEEP ) # DEEP
    valid['stop'] <- valid['start'] + runLength - 1
    Rdp <- data.frame(start=behaviorTimes$start[valid['start']], stop=behaviorTimes$stop[valid['stop']] )
    if ( nrow(Rdp)==0 ) Rdp <- data.frame( start=NA, stop=NA )

    # Ensure that "base" and "pre" do not overlap
    if ( is.na(base$stop) | is.na(pre$start) | base$stop > pre$start ) {
      base$stop <- NA
      pre$start <- NA
    }    

    # Build the row and add to the result dataframe
    if ( !is.na(Ei$start) & !is.na(Ri$stop) ) {
      tryCatch({
        row_b <- data.frame( Ei_start=Ei$start, Ei_stop=Ei$stop, Esleep_start=Esleep$start, Esleep_stop=Esleep$stop,
                           base_start=base$start, base_stop=base$stop, pre_start=pre$start, pre_stop=pre$stop,
                           post_start=post$start, post_stop=post$stop, Osleep_start=Osleep$start, Osleep_stop=Osleep$stop,
                           Oearly_start=Oearly$start, Oearly_stop=Oearly$stop,
                           seizure_start=seizure$start, seizure_stop=seizure$stop,
                           interveningPRE=interveningPRE, interveningPOST=interveningPOST )
        
        row_r <- data.frame( Rlate_start=Rlate$start, Rlate_stop=Rlate$stop, Rsleep_start=Rsleep$start,      
                           Rsleep_stop=Rsleep$stop, Ri_start=Ri$start, Ri_stop=Ri$stop )
        
        row_e <- data.frame( Er_start=Er$start, Er_stop=Er$stop, Or_start=Or$start, Or_stop=Or$stop,
                           Rr_start=Rr$start, Rr_stop=Rr$stop,
                           Elt_start=Er$start, Elt_stop=Er$stop, Olt_start=Or$start, Olt_stop=Or$stop,
                           Rlt_start=Rlt$start, Rlt_stop=Rlt$stop,
                           Edp_start=Er$start, Edp_stop=Er$stop, Odp_start=Or$start, Odp_stop=Or$stop,
                           Rdp_start=Rdp$start, Rdp_stop=Rdp$stop )
        if ( complete.cases(row_b) & complete.cases(row_r) & complete.cases(row_e) ) {
          if ( nrow(behavior)==0 ) {
            behavior <- row_b            
          } else {
            behavior <- rbind( behavior, row_b )
          }
          if ( nrow(recovery)==0 ) {
            recovery <- row_r            
          } else {
            recovery <- rbind( recovery, row_r )
          }
          if ( nrow(rem)==0 ) {
            rem <- row_e            
          } else {
            rem <- rbind( rem, row_e )
          }
        }
      }, error=function(cond) {
        print( "Missed one")
      })
    }
      
  }
  
  result <- list( behavior=behavior, recovery=recovery, rem=rem, seizureTimes=seizureTimes )
  return( result )
}
