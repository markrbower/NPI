persistAnalysisBehaviors <- function() {
  library( RMySQL )
  library( DBI )

  # load use_matrix
  load('/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/useMatrix.RData')
  
  # connect to the database
  conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password='', host='localhost', database="NV")
  # create the "behaviors" table
  query <- paste0( "use NV;" )
  DBI::dbGetQuery( conn, query )
  query <- "create table if not exists behaviors (subject varchar(256), seizureID int, label varchar(256), start bigint(20) unsigned, stop bigint(20) unsigned );"
  dbGetQuery( conn, query )
  query <- paste0( "truncate behaviors;" )
  DBI::dbGetQuery( conn, query )
  DBI::dbDisconnect(conn)
  # create the DIB
  fields <- c('subject','seizureID','label','start','stop')
  dib <- topconnect::databaseInsertBuffer( dbName='NV', dbTable='behaviors', fields=fields, limit=10, updates=NULL, dbuser='root', host='localhost', password='' )
  # find the subgroups of 'validEpochs'
  behaviorSubgroups <- as.list( names( validEpochs) )
  
  # loop through subjects
  for ( subject in as.list(rownames(use_matrix)) ) { # subject
    print(subject)
    # load the validEpochs RData file
    parsed.str <- parse(text=paste0( "load('validEpochs_", subject, ".RData')"))
    eval( parsed.str )
    
    for ( seizureID in use_matrix[subject,] ) { # seizureID
#      print( seizureID )

        str <- 'behavior'
        toParse <- paste0( "iter <- itertools::ihasNext( iterators::iter( names(validEpochs$", str," ) ) )" )
        parsed.str <- parse( text=toParse )
        eval( parsed.str)
        #    iter <- itertools::ihasNext( iterators::iter( names(validEpochs$behavior ) ) )
        while( itertools::hasNext( iter ) ) { # iter
          # get the start
          name <- iterators::nextElem(iter)
          #        print(name)
          rez1 <- strsplit( name, '_' )
          #        print( length(rez1[[1]]))
          if ( length(rez1[[1]]) == 2 ) {
            label1 <- rez1[[1]][1]
            if( rez1[[1]][2] != 'start' ) {
              print( 'trouble - 1' )
            }
            parsed.str <- parse( text=paste0( "start <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # get the stop
            itertools::hasNext( iter )
            name <- iterators::nextElem(iter)
            rez2 <- strsplit( name, '_' )
            label2 <- rez2[[1]][1]
            if( label1 != label2 ) {
              print( 'trouble - label' )
            }
            if( rez2[[1]][2] != 'stop' ) {
              print( 'trouble - 2' )
            }
            parsed.str <- parse( text=paste0( "stop <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # insert
            dib$insert( list(subject=subject, seizureID=seizureID, label=label1, start=start, stop=stop )  )
          }
          
        } # iter


        
        str <- 'recovery'
        toParse <- paste0( "iter <- itertools::ihasNext( iterators::iter( names(validEpochs$", str," ) ) )" )
        parsed.str <- parse( text=toParse )
        eval( parsed.str)
        #    iter <- itertools::ihasNext( iterators::iter( names(validEpochs$behavior ) ) )
        while( itertools::hasNext( iter ) ) { # iter
          # get the start
          name <- iterators::nextElem(iter)
          #        print(name)
          rez1 <- strsplit( name, '_' )
          #        print( length(rez1[[1]]))
          if ( length(rez1[[1]]) == 2 ) {
            label1 <- rez1[[1]][1]
            if( rez1[[1]][2] != 'start' ) {
              print( 'trouble - 1' )
            }
            parsed.str <- parse( text=paste0( "start <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # get the stop
            itertools::hasNext( iter )
            name <- iterators::nextElem(iter)
            rez2 <- strsplit( name, '_' )
            label2 <- rez2[[1]][1]
            if( label1 != label2 ) {
              print( 'trouble - label' )
            }
            if( rez2[[1]][2] != 'stop' ) {
              print( 'trouble - 2' )
            }
            parsed.str <- parse( text=paste0( "stop <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # insert
            dib$insert( list(subject=subject, seizureID=seizureID, label=label1, start=start, stop=stop )  )
          }
          
        } # iter
        
        
        
        str <- 'rem'
        toParse <- paste0( "iter <- itertools::ihasNext( iterators::iter( names(validEpochs$", str," ) ) )" )
        parsed.str <- parse( text=toParse )
        eval( parsed.str)
        #    iter <- itertools::ihasNext( iterators::iter( names(validEpochs$behavior ) ) )
        while( itertools::hasNext( iter ) ) { # iter
          # get the start
          name <- iterators::nextElem(iter)
          #        print(name)
          rez1 <- strsplit( name, '_' )
          #        print( length(rez1[[1]]))
          if ( length(rez1[[1]]) == 2 ) {
            label1 <- rez1[[1]][1]
            if( rez1[[1]][2] != 'start' ) {
              print( 'trouble - 1' )
            }
            parsed.str <- parse( text=paste0( "start <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # get the stop
            itertools::hasNext( iter )
            name <- iterators::nextElem(iter)
            rez2 <- strsplit( name, '_' )
            label2 <- rez2[[1]][1]
            if( label1 != label2 ) {
              print( 'trouble - label' )
            }
            if( rez2[[1]][2] != 'stop' ) {
              print( 'trouble - 2' )
            }
            parsed.str <- parse( text=paste0( "stop <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # insert
            dib$insert( list(subject=subject, seizureID=seizureID, label=label1, start=start, stop=stop )  )
          }
          
        } # iter
        
        
        
        
        str <- 'seizureTimes'
        toParse <- paste0( "iter <- itertools::ihasNext( iterators::iter( names(validEpochs$", str," ) ) )" )
        parsed.str <- parse( text=toParse )
        eval( parsed.str)
        #    iter <- itertools::ihasNext( iterators::iter( names(validEpochs$behavior ) ) )
        while( itertools::hasNext( iter ) ) { # iter
          # get the start
          name <- iterators::nextElem(iter)
          #        print(name)
          rez1 <- strsplit( name, '_' )
          #        print( length(rez1[[1]]))
          if ( length(rez1[[1]]) == 2 ) {
            label1 <- rez1[[1]][1]
            if( rez1[[1]][2] != 'start' ) {
              print( 'trouble - 1' )
            }
            parsed.str <- parse( text=paste0( "start <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # get the stop
            itertools::hasNext( iter )
            name <- iterators::nextElem(iter)
            rez2 <- strsplit( name, '_' )
            label2 <- rez2[[1]][1]
            if( label1 != label2 ) {
              print( 'trouble - label' )
            }
            if( rez2[[1]][2] != 'stop' ) {
              print( 'trouble - 2' )
            }
            parsed.str <- parse( text=paste0( "stop <- validEpochs$",str,"$", name, "[seizureID]" ) )
            eval( parsed.str )
            # insert
            dib$insert( list(subject=subject, seizureID=seizureID, label=label1, start=start, stop=stop )  )
          }
          
        } # iter
        
    } # seizureID

  } # subject
  dib$flush()

}