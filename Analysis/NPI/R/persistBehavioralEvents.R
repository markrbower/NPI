persistBehavioralEvents <- function( conn ) {
  # create table epochs ( subject varchar(256), session varchar(256), start bigint(20) unsigned, stop bigint(20) unsigned, label varchar(256) );
  #
  # Inputs:
  #   files - list of filenames containing CSV da ta to be stored into the 'epochs' table.
  #
  # The CSV file must have the format: start_time, stop_time, label
  
  # Change to target "...hypnogram.txt" and "...seizures.csv" in /Volumes/Data/NV/NV_Human_Sleep
  
  behaviorCodes <- c('?','NREM3','NREM2','NREM1','REM','WAKE') 
  
  patient_ids <-  list(
    '23_003',
    '24_002',
    '24_005',
    '25_002',
    '25_003',
    '25_005'
  )
  
  fields <- c('subject','session','start','stop','label')
#  dib <- topconnect::databaseInsertBuffer( conn, table='epochs', fields, limit=100 )
  dib <- topconnect::databaseInsertBuffer( dbName='NV', dbTable='epochs', fields=fields, limit=100, updates=NULL, dbuser='root', host='localhost', password='' )

  basedir <- here( "Data" )
  base = paste0( basedir, "/classes_")

  patient_iter <- itertools::ihasNext( patient_ids )
  while( itertools::hasNext( patient_iter ) ) {
    id <- iterators::nextElem( patient_iter )
    print( paste0( id ) )
    
    # Get the SUID
#    mef_filename <- paste0("/Users/markrbower/Documents/Data/NV/NVC1001_",id,"_2/NVC1001_",id,"_01.mef")
#    mef_filename <- paste0("/Volumes/Data/NV/NVC1001_",id,"_2/NVC1001_",id,"_01.mef")
    mef_filename <- paste0("/Volumes/eplab/Raw_Data/NV_Human/NVC1001_",id,"_2/NVC1001_",id,"_01.mef")
    vault <- topsecret::get_secret_vault()
    password_key <- paste0( 'NV_password' )
    
    info <- meftools::mef_info( c(mef_filename,secret::get_secret(password_key,key=secret::local_key(),vault=vault)) )
    suid <- info$header$session_unique_ID
    print( "info read")
    
#    basedir <- "/Volumes/Data/NV/NV_Human_Sleep/"
#    basedir <- "/Volumes/eplab/Raw_Data/NV_Human/"
    basedir <- "/Users/localadmin/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/Data/NV_Human_Sleep/"
    # Get the sleep times (*.txt)
    fname <- paste0( 'NVC1001_',id,'_hypnogram.txt' )
    filename <- paste0( basedir, fname)
    tmp <- read.delim( file=filename, header=FALSE, sep="\t" )
    bad_idx <- which( is.na(tmp$V2) )
    if ( length(bad_idx) > 0 ) {
      data <- data.frame( time=tmp$V1[-bad_idx], code=tmp$V2[-bad_idx] )
    } else {
      data <- data.frame( time=tmp$V1, code=tmp$V2 )
    }
    print( "hypnogram read" )

    currentCode <- -1
    for ( row in seq(1,nrow(data) ) ) {
      if ( data$code[row] != currentCode ) {
        # store each line using a dbIterator
        if ( currentCode >= 2 & currentCode <= 6 ) {
          dib$insert( list(subject=id, session=info$header$session_unique_ID, start=currentStart, stop=data$time[row], label=behaviorCodes[currentCode]) )
        }
        currentStart <- data$time[row]
        currentCode <- data$code[row]
        print( currentCode )
      }
    }
    if ( currentCode >= 2 & currentCode <= 6 ) {
      dib$insert( list(subject=id, session=info$header$session_unique_ID, start=currentStart, stop=data$time[row]+30E6, label=behaviorCodes[currentCode]) )
    }
    
    # Get the seizure times (*.csv)
    fname <- paste0( 'NVC1001_',id,'_seizures.csv' )
    filename <- paste0( basedir, fname)
    tmp <- read.csv( file=filename, header=FALSE, stringsAsFactors = FALSE )
    tmp$V1 <- as.numeric( tmp$V1 )
    tmp$V2 <- as.numeric( tmp$V2 )
    bad_idx <- which( is.na(tmp$V1) )
    if ( length(bad_idx) > 0 ) {
      data <- data.frame( start=tmp$V1[-bad_idx], stop=tmp$V2[-bad_idx], label=tmp$V3[-bad_idx] )    
    } else {
      data <- data.frame( start=tmp$V1, stop=tmp$V2, label=tmp$V3 )    
    }

    for ( row in seq(1,nrow(data) ) ) {
      # store each line using a dbIterator
      dib$insert( list(subject=id, session=info$header$session_unique_ID, start=data$start[row], stop=data$stop[row], label=data$label[row]) )
    }
  }
  dib$flush()
}
