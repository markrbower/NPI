findValidEpochs <- function( conn, project, subjectID, minDurationMinutes, basedir ) {
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
  
  MIN_SLEEP_DURATION <- 420 * 60E6 # The number of seconds spent in sleep (15 min)
  minDuration <- minDurationMinutes * 60E6
  
  case <- data.frame( subject=subjectID, channel_name=paste0("NVC1001_",subjectID,"_01.mef") )
  #  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
  #  basedir <- paste0( '/Users/markrbower/Documents/Data/NV/NVC1001_', case$subject, '_2' )
  #  basedir <- paste0( '/Volumes/Data/NV/NVC1001_', case$subject, '_2' )
  #  basedir <- paste0( '/Volumes/eplab/Raw_Data/NV_Human/NVC1001_', case$subject, '_2' )
  subjectdir <- paste0( 'NVC1001_', subjectID, '_2/')
  mef_filename <- paste0( basedir, subjectdir, NPI:::filenameFromCase( case ) )
  print( paste0( "mef_filename: ", mef_filename ) )
  vault <- topsecret::get_secret_vault()
  password_key <- paste0( 'MEF_password' )
  print( paste0( "mef password: ", secret::get_secret(password_key,key=secret::local_key(),vault=vault) ) )
#  load( file="deleteThisTmpInfo" )
#  load("tmp_info.RData" )
  info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
  T0 <- info$header$recording_start_time
  print( paste0( "T0: ", T0 ) )
  sf <- info$header$sampling_frequency
  print( paste0( "sf: ", sf ) )
  
#  minDuration <- minDuration * 2; # Assumes 'minDuration' is in minutes and behaviors are recorded in 30 sec windows.
  
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
  IDX_NREM <- sort( union( IDX_LIGHT, IDX_DEEP ) )
  IDX_REM <- which( behaviorTimes[,'label'] == "REM" )
  
  # Special case! Don't let intermittent 'WAKE' bins disrupt 'SLEEP' runs.
  IDX_SLEEP <- sort( union( IDX_NREM, IDX_REM ) )
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
  
  behaviorTimes[,'duration'] <- behaviorTimes[,'stop'] - behaviorTimes[,'start']
 
# This approach was used when "epochs" entries described each bin   
#  # Allow "transint waking" of 1 minute (2 bins)
#  # erosion
#  X <- behaviorTimes[,'sleep_wake']
#  L <- length(X)
#  for ( count in seq(1,2) ) {
#    X[2:(L-1)] <- X[1:(L-2)] & X[2:(L-1)] & X[3:L]
#  }
#  # dilation
#  for ( count in seq(1,2) ) {
#    X[2:(L-1)] <- X[1:(L-2)] | X[2:(L-1)] | X[3:L]
#  }
#  behaviorTimes[,'sleep_wake'] <- X
  # From 'behaviorTimes' to 'sleep_wake'
  sleep_wake <- data.frame()
  sleep_wake_rle <- rle( behaviorTimes[,'sleep_wake'])
  currentStartIdx <- 1
  for ( rleIdx in seq(1:length(sleep_wake_rle$lengths ) ) ) {
    newStopIdx <- currentStartIdx + sleep_wake_rle$lengths[rleIdx] - 1
    new_df <- data.frame( start=behaviorTimes[currentStartIdx,'start'], stop=behaviorTimes[newStopIdx,'stop'], label=sleep_wake_rle$values[rleIdx])
    sleep_wake <- rbind( sleep_wake, new_df )
    currentStartIdx <- newStopIdx + 1
    rm(new_df)
  }
  sleep_wake[,'duration'] <- sleep_wake[,'stop'] - sleep_wake[,'start']
  shortWakeIdx <- which( sleep_wake[,'label'] == AWAKE & sleep_wake[,'duration']<=61E6)
  sleep_wake[shortWakeIdx,'label'] <- SLEEP
  
  # Loop.
  behavior <- data.frame()
  recovery <- data.frame()
  rem <- data.frame()
  
  # From 'sleep_wake' to 'sleepWake'
  sleepWake <- data.frame()
  sleepWake_rle <- rle( sleep_wake[,'label'])
  currentStartIdx <- 1
  for ( rleIdx in seq(1:length(sleepWake_rle$lengths ) ) ) {
    newStopIdx <- currentStartIdx + sleepWake_rle$lengths[rleIdx] - 1
    new_df <- data.frame( start=sleep_wake[currentStartIdx,'start'], stop=sleep_wake[newStopIdx,'stop'], label=sleepWake_rle$values[rleIdx])
    sleepWake <- rbind( sleepWake, new_df )
    currentStartIdx <- newStopIdx + 1
    rm(new_df)
  }
  sleepWake[,'duration'] <- sleepWake[,'stop'] - sleepWake[,'start']

#  # Merge similar labels in "sleep_wake", so that the resulting labels alternate "0,1,0,1,0,1,0,..."
#  sleepWake <- data.frame()
#  currentLabel <- -1
#  currentStartIdx <- -1
#  for ( idx in seq(1,length(sleep_wake$start))) {
#    if (sleep_wake$label[idx] != currentLabel ) {
#      # is there anything to save?
#      if ( currentStartIdx > 0 ) {
#        new_df <- data.frame( start=behaviorTimes[currentStartIdx,'start'], stop=behaviorTimes[idx,'start'], label=currentLabel)
#        sleepWake <- rbind( sleepWake, new_df )
#      }
#      # start something new
#      currentLabel <- sleep_wake$label[idx]
#      currentStartIdx <- idx
#    }    
#  }
#  new_df <- data.frame( start=behaviorTimes[currentStartIdx,'start'], stop=behaviorTimes[idx,'stop'], label=currentLabel)
#  sleepWake <- rbind( sleepWake, new_df )
#  sleepWake[,'duration'] <- sleepWake[,'stop'] - sleepWake[,'start']
  
  
  for ( index in 1:nrow(seizureTimes) ) {
    print( paste0( index, " of ", nrow(seizureTimes) ) )
    seizure <- seizureTimes[index,]
    
    # CHECK THAT NO SLEEP OCCURS AROUND SEIZURES FOR AT LEAST 3*minDuration
    preSleepStatus <- 0
    idx <- which( sleepWake[,'start'] <= (seizure$start-3*minDuration) & sleepWake[,'stop'] >= seizure$start )
    if ( length(idx)==1 ) {
      if ( sleepWake[idx,'label'] == AWAKE ) {
        preSleepStatus <- 1
      }
    }
    postSleepStatus <- 0
    idx <- which( sleepWake[,'start'] <= seizure$stop & sleepWake[,'stop'] >= (seizure$stop+3*minDuration) )
    if ( length(idx)==1 ) {
      if ( sleepWake[idx,'label'] == AWAKE ) {
        postSleepStatus <- 1
      }
    }
    
    # ESTABLISH "pre" TO START
    pre <- data.frame(start=seizure$start-minDuration,stop=seizure$start)
    if ( nrow(pre)==0 ) pre <- data.frame( start=NA, stop=NA )
    
    # "Esleep" is the first sleep before "pre"
    # Find the lowest latency to a preceding wake, then take the preceeding sleep
    idx <- which( sleepWake[,'stop'] < seizure$start )
    valid <- NPI:::lastRunOfLength( idx[1], MIN_SLEEP_DURATION, sleepWake[idx,'label'], SLEEP, sleepWake[idx,'duration'] ) # SLEEP at least 15 min
    Esleep <- data.frame(start=sleepWake[valid,'start'], stop=sleepWake[valid,'stop'] )
    idx <- which( behaviorTimes[,'start'] >= Esleep$start & behaviorTimes[,'stop'] <= Esleep$stop )
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
    
    # Find the surrounding AWAKE periods, including "base"
    # Find the preceding Wake.
    idx <- which( behaviorTimes[,'stop'] <= Esleep$start )
    valid <- NPI:::lastRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], AWAKE, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Ei <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Ei <- data.frame( start=NA, stop=NA )
    }
    
    # Find the following Wake.
    idx <- which( behaviorTimes[,'start'] >= Esleep$start & behaviorTimes[,'start'] < seizure$start )
    valid <- NPI:::firstRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], AWAKE, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      base <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      base <- data.frame( start=NA, stop=NA )
    }
    

    # AFTER THE SEIZURE
    # Find the lowest latency to a following wake, then find the following sleep
    idx <- which( sleepWake[,'stop'] >= seizure$stop )
    valid <- NPI:::firstRunOfLength( idx[1], MIN_SLEEP_DURATION, sleepWake[idx,'label'], SLEEP, sleepWake[idx,'duration'] ) # SLEEP at least 15 min
    Osleep <- data.frame(start=sleepWake[valid,'start'], stop=sleepWake[valid,'stop'] )
    # Is there an intervening post-seizure?
    if ( nrow(Osleep)==0 ) Osleep <- data.frame( start=NA, stop=NA )

    # Find the preceding Wake.
    idx <- which( behaviorTimes[,'stop'] <= Osleep$start )
    valid <- NPI:::lastRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], AWAKE, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      post <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      post <- data.frame( start=NA, stop=NA )
    }
    
    # Find the following Wake.
    idx <- which( behaviorTimes[,'stop'] >= Osleep$stop )
    valid <- NPI:::firstRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], AWAKE, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Oearly <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Oearly <- data.frame( start=NA, stop=NA )
    }
    
    # Find various sleep periods
    # Find the longest contained REM episode between these AWAKE epochs
    idx <- which( behaviorTimes[,'start'] >= Esleep$start & behaviorTimes[,'stop'] <= Esleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], REM, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Er <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Er <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained LIGHT episode between these AWAKE epochs
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], LIGHT, behaviorTimes[idx,'duration'] ) # LIGHT
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Elt <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Elt <- data.frame( start=NA, stop=NA )
    }

    # Find the longest contained DEEP episode between these AWAKE epochs
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], DEEP, behaviorTimes[idx,'duration'] ) # DEEP
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Edp <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Edp <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained REM episode
    idx <- which( behaviorTimes[,'start'] >= Osleep$start & behaviorTimes[,'stop'] <= Osleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], 1, behaviorTimes[idx,'number'], REM, behaviorTimes[idx,'duration'] ) # REM
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Or <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Or <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained LIGHT episode
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], LIGHT, behaviorTimes[idx,'duration'] ) # LIGHT
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Olt <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Olt <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained DEEP episode
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], DEEP, behaviorTimes[idx,'duration'] ) # DEEP
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Odp <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Odp <- data.frame( start=NA, stop=NA )
    }
    
    # After "early" and before subsequent sleep
    # first, find the next sleep epoch after "early"
    # call this time-period the "R" period for "Recovery"
    idx <- which( sleepWake[,'start'] >= Oearly$stop )
    valid <- NPI:::firstRunOfLength( idx[1], MIN_SLEEP_DURATION, sleepWake[idx,'label'], SLEEP, sleepWake[idx,'duration'] ) # SLEEP at least 15 min
    Rsleep <- data.frame(start=sleepWake[valid,'start'], stop=sleepWake[valid,'stop'] )
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
    valid <- NPI:::lastRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], AWAKE, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Rlate <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Rlate <- data.frame( start=NA, stop=NA )
    }
    
    # Find the shortest-latency awake period following recovery sleep as "Ri"
    idx <- which( behaviorTimes[,'start'] >= Rsleep$stop )
    valid <- NPI:::firstRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], AWAKE, behaviorTimes[idx,'duration'] ) # AWAKE
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Ri <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Ri <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained REM episode
    idx <- which( behaviorTimes[,'start'] >= Rsleep$start & behaviorTimes[,'stop'] <= Rsleep$stop )
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], REM, behaviorTimes[idx,'duration'] ) # REM
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Rr <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Rr <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained LIGHT episode
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], LIGHT, behaviorTimes[idx,'duration'] ) # LIGHT
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Rlt <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Rlt <- data.frame( start=NA, stop=NA )
    }
    
    # Find the longest contained DEEP episode
    valid <- NPI:::longestRunOfLength( idx[1], minDuration, behaviorTimes[idx,'number'], DEEP, behaviorTimes[idx,'duration'] ) # DEEP
    if ( length(valid) == 1 ) {
      mid <- behaviorTimes$start[valid] + (behaviorTimes$stop[valid]-behaviorTimes$start[valid])/2
      Rdp <- data.frame(start=mid-minDuration/2, stop=mid+minDuration/2 )
    } else {
      Rdp <- data.frame( start=NA, stop=NA )
    }
    
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
        print(row_b)
        print(row_e)
        print(row_r)
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
