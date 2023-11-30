NPI_testbed_window <- function() {
  # Network Pattern Identifier
  #
  #' @export
  library(igraph)
  library(leiden)
  library(leidenAlg)
  library(foreach)
  library(doParallel)
  
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/pack.R")
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/unpack.R")
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/NMI.R")

  # Parameters
  nrows <- 2000
  ncols <- 16
  sd <- 10.0
  blockSize <- 1000;
  maxSize <- 1000 	# 1000*20 = 20,000 sec; 2x CW
  CW <- 10000 	# 10000/20 = 500 inputs expected
  quorum <- 100
  partitionFactor <- 1
  numthreads <- 4
  cl <- makeCluster( numthreads )
  registerDoParallel(cl)
  
  randVec <- list();
  resultVector <- list();
  
  # Create toy data
  if ( !file.exists( "NPItestbed.RData" ) ) {
    toyData <- rbind( c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                      c(0,3,7,10,8,2,-3,-7,-11,-8,-4,1,5,9,3,0),
                      c(0,-3,-7,-10,-8,-2,3,7,11,8,4,-1,-5,-9,-3,0),
                      c(0,5,10,20,28,30,26,10,-10,-30,-40,-45,-35,-20,-5,0),
                      c(0,-5,-10,-20,-28,-30,-26,-10,10,30,40,45,35,20,5,0),
                      c(0,10,45,60,25,-15,-55,-48,-10,-5,0,0,0,0,0,0),
                      c(0,0,0,0,50,50,50,50,-50,-50,-50,-50,0,0,0,0))
    Prob <- c(1,1,2,3,4,5,5,5,6,6,6);
    Teacher <- c(1,1,1,2,3,4,5);
    
    a <- matrix( nrow=nrows, ncol=ncols )
    ta <- vector( mode="integer", length=nrows)
    teacherDF <- data.frame()
    for ( i in seq(1,nrows) ) {
      if ( i==1 ) {
        ta[1] <- round( runif(1) * 20 );
      } else {
        ta[i] <- ta[i-1] + round( runif(1) * 20 ) + 5;
      }
      idx <- Prob[ sample(1:11,1) ];
      teacherDF <- rbind( teacherDF, data.frame( k=ta[i], v=Teacher[idx] ) );
      for ( j in seq( 1, ncols) ) {
        a[i,j] <- toyData[idx,j] + rnorm( 1, mean=0, sd=sd );
      }
    }
    # Replace some events with artifact
    for ( i in seq(1000,1020) ) {
      for ( j in seq( 1, ncols) ) {
        a[i,j] <- toyData[7,j] + rnorm( 1, mean=0, sd=sd );
      }
    }
    
    save( file="NPItestbed.RData", a, ta, teacherDF )  
  }
  # Load toy data
  load( file="NPItestbed.RData" )
  
  # Define processing block sizes
  nbrBlocks <- ((nrows-blockSize)/(blockSize/2)) + 1
  sum_X <- matrix(0,blockSize,maxSize)
  sum_Y <- matrix(0,blockSize,maxSize)
  sum_XY <- matrix(0,blockSize,maxSize)
  squareSum_X <- matrix(0,blockSize,maxSize)
  squareSum_Y <- matrix(0,blockSize,maxSize)
  dt <- matrix(0,blockSize,maxSize)
  cc <- matrix(0,blockSize,maxSize)
  er <- matrix(0,blockSize,maxSize)
  
  assignedDF <- data.frame()
  for ( blockNbr in seq(0,(nbrBlocks-1)) ) {
    if ( blockNbr == 0 ) {
      blockOffset <-  0;
    } else {
      blockOffset <-  blockSize/2;
    }
    blockStart <-  blockNbr * blockOffset
    # Compute dt, cc & er (GPU)
    print( paste0( "Computing stats"))
    for ( x in seq( (blockOffset+1),blockSize) ) {
      if ( (x%%100)==0 ) {
        print( paste0( x ) )
      }
      for ( y in seq(1,maxSize) ) {
        tryCatch(
          {
            sum_X[x,y] <- 0;
            sum_Y[x,y] <- 0;
            sum_XY[x,y] <- 0;
            squareSum_X[x,y] <- 0;
            squareSum_Y[x,y] <- 0;
          }, error=function(e) {
            print( "Error 2 occurred." )
            print( paste0( "x: ", x, "y: ", y ) )
            print(e)
          }
        )
        xx <- x + blockStart;
        yy <- xx - y;
        if ( yy >= 1 ) {
          for ( j in  seq(1,ncols) ) {
            tryCatch(
            {
            sum_X[x,y] <- sum_X[x,y] + a[xx,j];
            sum_Y[x,y] <- sum_Y[x,y] + a[yy,j];
            sum_XY[x,y] <- sum_XY[x,y] + a[xx,j] * a[yy,j];
            squareSum_X[x,y] <- squareSum_X[x,y] + a[xx,j] * a[xx,j];
            squareSum_Y[x,y] <- squareSum_Y[x,y] + a[yy,j] * a[yy,j];
            }, error=function(e) {
              print( "Error 1 occurred." )
              print( paste0( "x: ", x, " xx: ", xx, " y: ", y, " blockOffset: ", blockOffset, " blockSize: ", blockSize) )
              print(e)
            }
            )
          }
        }
      }
    }
    print( paste0( "Computing CC and ER"))
    for ( x in seq( (blockOffset+1),blockSize) ) {
      for ( y in seq(1,maxSize) ) {
        xx <- x + blockStart;
        yy <- xx - y;
        if ( yy >= 1 ) {
          dt[x,y] <- ta[yy] - ta[xx];	
          denom <- (ncols * squareSum_X[x,y] - sum_X[x,y] * sum_X[x,y]) *
            (ncols * squareSum_Y[x,y] - sum_Y[x,y] * sum_Y[x,y])
          if ( abs(denom) > 0.0 ) {
            cc[x,y] <- (ncols * sum_XY[x,y] - sum_X[x,y] * sum_Y[x,y]) / sqrt(denom)
          } else {
            cc[x,y] <- 0.0;
          }
          if ( squareSum_Y[x,y] > 0.0 ) {
            er[x,y] <- squareSum_X[x,y] / squareSum_Y[x,y];
          }
        }
      }
    }
    # Build vector
    print( paste0( "Building the vector"))
    for ( x in seq( (blockOffset+1),blockSize) ) {
#      print( paste0(x) )
      nbrThisEvent <-  0;
      nbrLinks <- 0;
      cand_df <- data.frame() # "candidate" data.frame
      # Find all candidates
      for ( y in seq(1,maxSize) ) {
        xx <- x + blockStart;
        yy <- xx - y;
        if ( yy >= 1 ) {
          if ( dt[x,y]>(-CW) & dt[x,y]<0 ) {
            if ( y > maxSize ) {
              maxSize <- y;
            }
            norm_cc <- ( cc[x,y] + 1.0 ) / 2.0;
            norm_er <- min( er[x,y], 1.0/er[x,y] );
            cand_df <- rbind( cand_df, data.frame(x=ta[xx],y=ta[yy],cc=norm_cc,er=norm_er) )
          } # within time window
        } # yy >= 0
      } # for
      # Subsample to find the keepers
#      print( "subsample" )
      index <- 1
      nCand <- nrow( cand_df )
      keep_df <- data.frame() # "keepers" data.frame
      if ( nCand < 2*quorum ) { # if nCand<quorum, select randomly across one pass
        if ( nCand > 0 ) {
          for ( idx in seq(1,nCand) ) {
            entry <- cand_df[idx,]
            if ( (entry$cc * entry$cc * entry$er) > runif(1) ) {
              keep_df <- rbind( keep_df, entry )
#              if ( entry$x < 500 ) {
#                sprintf( "%d\t%d\t%f\t%f\n", entry$x, entry$y, entry$cc, entry$er );
#              }
            }
          }
        }
      } else { # loop, selecting randomly until a quorum is reached
        nbrKeepers <- 0
        while ( nbrKeepers < quorum ) {
          rmVec <- c()
          nCand <- nrow( cand_df )
          for ( idx in seq(1,nCand) ) {
            entry <- cand_df[idx,]
            if ( (entry$cc * entry$cc * entry$cc * entry$er) > runif(1) ) {
              rmVec <- c( rmVec, idx )
              keep_df <- rbind( keep_df, entry )
              nbrKeepers <- nbrKeepers + 1
            }
          }
          if ( length(rmVec) > 0 ) {
            cand_df <- cand_df[ -rmVec, ]
          }
        }
      } # nrow(cand_df) > 0
#      print( paste0( "bind" ))
      assignedDF <- rbind( assignedDF, keep_df )
    } # x
  } # blockNbr

  # Process graphs (OMP) to make "ancestor_map"
#  for ( my_id in seq(1:numthreads) ) {
  ancestorDF <- foreach ( my_id = 1:numthreads, .combine=rbind) %dopar% {
    library(igraph)
    library(leiden)
    library(leidenAlg)
    library(foreach)
    library(stringr)
    
    tmpDF <- data.frame()
    
    source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/pack.R")
    source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/unpack.R")

    # Determine graph limits
    my_start <- ( nrows / numthreads ) * (my_id-1) + 1
    my_start_t <- ta[my_start]
    my_stop  <- ((nrows / numthreads) * my_id)
    my_stop_t <- ta[my_stop]
    my_cnt <- my_stop - my_start + 1
    print( paste0( "Working on ", my_start, " to ", my_stop ) )
  
    # Find the limits of the CW
    my_idx <- which( ta <= (my_start_t - CW) )
    if ( length(my_idx) > 0 ) {
      my_startCW <- max( max(), 1 )
      my_startCW_t <- ta[ my_startCW ]
    } else {
      my_startCW <- 1
      my_startCW_t <- ta[1]
    }

    # Find the result vectors in this graph
    # Build entire graph for this block of data
    keep_row <- which( assignedDF[,2] >= my_startCW_t & assignedDF[,1] <= my_stop_t)
    g <- graph.data.frame( assignedDF[keep_row,1:2], directed=FALSE )
    E(g)$weight <- assignedDF[keep_row,3] * assignedDF[keep_row,3] * assignedDF[keep_row,3] * assignedDF[keep_row,4]
    
    # Find the target times for this graph
    all_times <- as.numeric( V(g)$name )
    target_idx <- which( ta >= my_start_t & ta <= my_stop_t)
    target_times <- ta[ target_idx ]

    # Loop on rows
    quorumQueue <- vector()
    for ( target_time in target_times ) {
      # Sub-graph
      idxs <- which( all_times >= (target_time-CW) & all_times <= target_time )
      if ( length(idxs) < quorum ) {
        quorumQueue <- append( quorumQueue, target_time )
      } else {
        sg <- induced.subgraph(graph=g,vids=idxs)
        # Partition
        partition <- leiden(sg,resolution_parameter=partitionFactor,n_iterations=-1,weights=E(sg)$weight)
#        partition <- leiden(sg,resolution_parameter=partitionFactor,n_iterations=-1)
#        partition <- leiden(sg)
        
        if ( length(quorumQueue) >0 ) { # process the queue
#          if ( my_id == 1 ) { # add on those events during the initial CW
#            CW_idx <- which( ta >= my_startCW_t & ta < my_start_t)
#            CW_times <- ta[ CW_idx ]
#            for ( CW_time in CW_times ) {
#              quorumQueue <- append( quorumQueue, CW_time )
#            }
#            quorumQueue <- sort( quorumQueue )
#          }
          quorumQueue <- sort( quorumQueue )
          for ( qt in quorumQueue ) {
            clik <- partition[ which( as.numeric(V(sg)$name) == qt ) ]
            
            # Ancestor
            graphIdx <- which( (partition==clik) & (as.numeric(V(sg)$name)<qt) )
            idx <- which( ta == qt )
            if ( length( graphIdx) == 0 ) {
              tmpDF <- rbind( tmpDF, data.frame( k=qt, v=pack("") ) )
            } else {
              tmpDF <- rbind( tmpDF, data.frame( k=qt, v=pack( as.numeric( V(sg)$name[graphIdx]) ) ) )
            }
          }
          quorumQueue <- vector()
        }
        # Process this targetTime
        clik <- partition[ which( as.numeric(V(sg)$name) == target_time ) ]
        graphIdx <- which( (partition==clik) & (as.numeric(V(sg)$name) < target_time) )
        idx <- which( ta == target_time )
        if ( length( graphIdx) == 0 ) {
          tmpDF <- rbind( tmpDF, data.frame( k=target_time, v=pack("") ) )
        } else {
          tmpDF <- rbind( tmpDF, data.frame( k=target_time, v=pack( as.numeric( V(sg)$name[graphIdx]) ) ) )
        }
      }
    }
    tmpDF
  }

  stopCluster(cl)

  # Assign global class
  assignedDF <- data.frame()
  for ( i in 1:length(ta) ) {
    assignedDF <- rbind( assignedDF, data.frame( k=ta[i], v=0 ) )
  }
  latest_class = 1
  for ( i in 1:length(ta) ) {
    target_time <- ta[i]
    idx <- which( teacherDF$k == target_time )
    teacherOfTarget <- teacherDF[idx,]$v
    idx <- which( ancestorDF$k == target_time )
    ancestors <- unpack( ancestorDF[idx,]$v )
    if ( length(ancestors) == 0 ) {
      new_class <- latest_class
      latest_class <- latest_class + 1
    } else {
      ancestorsIdx <- c()
      for ( ancestor in ancestors ) {
        ancestorsIdx <- append( ancestorsIdx, which( assignedDF$k == ancestor ) )
      }
      GC <- as.numeric( assignedDF[ancestorsIdx,]$v )
      ant <- table( GC )
      new_class <- as.numeric( names( which( ant == max(ant) ) ) )
    }
    idx <- which( assignedDF$k == target_time )
#    print( paste0( new_class))
    assignedDF$v[ idx] <- new_class
  }

  # Tabulate
  unique_teachers <- unique( teacherDF$v )
  groupedByTeacherDF <- data.frame()
  for ( i in 1:length(unique_teachers) ) {
    groupedByTeacherDF <- rbind( groupedByTeacherDF, data.frame( k=i, v="" ) )
  }
  for ( i in 1:length(ta) ) {
    target_time <- ta[i]
    idx <- which( teacherDF$k == target_time )
    tc <- teacherDF[idx,]$v
    idx <- which( teacherDF$k == target_time )
    gc <- assignedDF[idx,]$v
    idx <- which( groupedByTeacherDF$k == tc )
    groupedByTeacherDF[idx,]$v <- pack( append( unpack(groupedByTeacherDF[idx,]$v), gc) )
  }
  for ( idx in 1:length(unique_teachers) ) {
    ttb <- table( unpack(groupedByTeacherDF[ idx, ]$v) )
    print( ttb )
  }
  
  nmi <- NMI( teacherDF, assignedDF )
  print( paste0( "NMI: ", nmi ) )
  
  return( list( teacherDF=teacherDF, assignedDF=assignedDF ) )
}

