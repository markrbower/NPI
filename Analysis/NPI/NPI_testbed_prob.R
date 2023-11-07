NPI_testbed_prob <- function( compArgs, progressFields ) {
  # Network Pattern Identifier
  #
  #' @export
  library(igraph)
  library(leiden)
  library(leidenAlg)
  library(foreach)
  library(r2r)

  # Parameters
  nrows <- 1600
  ncols <- 16
  sd <- 10.0
  blockSize <- 800;
  maxSize <- 750 	# 750*20 = 15,000 sec; 10x CW
  CW <- 20000 	# 10000/20 = 500 inputs expected
  quorum <- 100
  CC_threshold <- 0.75
  ER_threshold <- 0.7
  partitionFactor <- 0.01
  numthreads <- 4

  randVec <- list();
  resultVector <- list();
  
  # Create toy data
  if ( !file.exists( "matrix.data" ) ) {
    toyData <- rbind( c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                      c(0,3,7,10,8,2,-3,-7,-11,-8,-4,1,5,9,3,0),
                      c(0,-3,-7,-10,-8,-2,3,7,11,8,4,-1,-5,-9,-3,0),
                      c(0,5,10,20,28,30,26,10,-10,-30,-40,-45,-35,-20,-5,0),
                      c(0,-5,-10,-20,-28,-30,-26,-10,10,30,40,45,35,20,5,0),
                      c(0,10,45,60,25,-15,-55,-48,-10,-5,0,0,0,0,0,0) )
    Prob <- c(1,1,2,3,4,5,5,5,6,6,6);
    Teacher <- c(0,0,0,1,2,3);
    
    a <- matrix( nrow=nrows, ncol=ncols )
    ta <- vector( mode="integer", length=nrows)
    teacherClass <- hashmap()
    for ( i in seq(1,nrows) ) {
      if ( i==1 ) {
        ta[1] <- round( runif(1) * 20 );
      } else {
        ta[i] <- ta[i-1] + round( runif(1) * 20 ) + 5;
      }
      idx <- Prob[ sample(1:11,1) ];
      teacherClass[ ta[i] ] <- Teacher[idx];
      for ( j in seq( 1, ncols) ) {
        a[i,j] <- toyData[idx,j] + rnorm( 1, mean=0, sd=sd );
      }
    }
    save( file="matrix.data", a, ta, teacherClass )  
  }
  # Load toy data
  load( file="matrix.data" )
  
  if ( !file.exists( "/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/global_df.RData" ) ) {
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
  
  # Loop on processing blocks to make "global_df"
  global_df <- data.frame()
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
      nbrThisEvent <-  0;
      nbrLinks <- 0;
      cand_df <- data.frame() # "candidate" data.frame
      # Find all candidates
      for ( y in seq(1,maxSize) ) {
        xx <- x + blockStart;
        yy <- xx - y;
        if ( yy >= 1 ) {
          if ( dt[x,y]>(-CW) & dt[x,y]<0 ) {
          #if ( dt[x,y]>(-CW) & dt[x,y]<0 &
                   #    cc[x,y]>CC_threshold &
                   #    er[x,y]>ER_threshold & er[x,y]<(1/ER_threshold) ) {
            if ( y > maxSize ) {
              maxSize <- y;
            }
            norm_cc <- ( cc[x,y] + 1.0 ) / 2.0;
            norm_er <- min( er[x,y], 1.0/er[x,y] );
            cand_df <- rbind( cand_df, data.frame(x=ta[xx],y=ta[yy],cc=norm_cc,er=norm_er) )
          } # within time window
        } # yy >= 0
      } # for
      keep_df <- data.frame() # "keepers" data.frame
      # Subsample to find the keepers
      if ( nrow(cand_df) > 0 ) {
        nbrKeepers <- 0
        index <- 1
        nCand <- nrow( cand_df )
        rmVec <- c()
        for ( idx in seq(1,nCand) ) {
          if ( (nCand<quorum) | (nCand>=quorum & nbrKeepers<quorum) ) {
            entry <- cand_df[idx,]
            if ( entry$cc > runif(1) & entry$er > runif(1) ) {
              if ( entry$x < 500 ) {
                sprintf( "%d\t%d\t%f\t%f\n", entry$x, entry$y, entry$cc, entry$er );
              }
              rmVec <- c( rmVec, idx )
              keep_df <- rbind( keep_df, entry )
              nbrKeepers <- nbrKeepers + 1
            } # keep
          } # need more
        } # for
        if ( length(rmVec) > 0 ) {
          cand_df <- cand_df[ -rmVec, ]
        }
        # If available, has a quorum been selected? 
        if ( nCand>=quorum & nbrKeepers<quorum) {
          # Keep the best until quorum is reached
          cand_df <- cand_df[ order(-cand_df$cc), ]
          idx <- 1
          while ( nbrKeepers < quorum & idx < nrow(cand_df) ) {
            keep_df <- rbind( keep_df, cand_df[idx,])
            nbrKeepers <- nbrKeepers + 1
            idx <- idx + 1
          }
        }          
        global_df <- rbind( global_df, keep_df )
      } # nrow(cand_df) > 0
    } # x
  } # blockNbr
  # Allocate the ancestorMap
  } # /Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/global_df.RData exists
  print( paste0( getwd() ) )
  ancestorMap <- hashmap()

  if ( file.exists( "/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/global_df.RData" ) ) {
    load( file="/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/global_df.RData")
  } else {
    save( global_df, file="/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/global_df.RData")
  }
  
  if ( !file.exists( "/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/ancestorMap.RData" ) ) {

  # Process graphs (OMP) to make "ancestor_map"
  for ( my_id in seq(1:numthreads) ) {
    # Determine graph limits
    my_start <- ( nrows / numthreads ) * (my_id-1) + 1
    my_start_t <- ta[my_start]
    my_stop  <- ((nrows / numthreads) * my_id)
    my_stop_t <- ta[my_stop]
    my_cnt <- my_stop - my_start + 1
    print( paste0( "Working on ", my_start, " to ", my_stop ) )
  
    # Find the limits of the CW
    my_startCW <- max( which( ta <= (my_start_t - CW) ) )
    if ( my_startCW >= ta[1] ) {
      my_startCW_t <- ta[ my_startCW ]
    } else {
      my_startCW <- 1
      my_startCW_t <- ta[1]
    }

    # Find the result vectors in this graph
    # Build entire graph for this block of data
    keep_row <- which( global_df[,2] >= my_startCW_t & global_df[,1] <= my_stop_t)
    g <- graph.data.frame( global_df[keep_row,1:2], directed=FALSE )
    E(g)$weight <- global_df[keep_row,3]
    
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
#        partition <- leiden(sg,resolution_parameter=partitionFactor,n_iterations=-1)
        partition <- leiden(sg)
        
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
            ancestorsIdx <- which( (partition==clik) & (as.numeric(V(sg)$name)<qt) )
            if ( length( ancestorsIdx) == 0 ) {
              ancestorMap[ qt ] <- list('')
            } else {
              ancestorMap[ qt ] <- list( as.numeric( V(sg)$name[ancestorsIdx]) )
            }
          }          
          quorumQueue <- vector()
        }
        # Process this targetTime
        clik <- partition[ which( as.numeric(V(sg)$name) == target_time ) ]
        ancestorsIdx <- which( (partition==clik) & (as.numeric(V(sg)$name) < target_time) )
        if ( length( ancestorsIdx) == 0 ) {
          ancestorMap[ target_time ] <- list('')
        } else {
          ancestorMap[ target_time ] <- list( as.numeric( V(sg)$name[ancestorsIdx]) )
        }
      }
    }
  }
  } # File check as to whether ancestorMap.RData exists
  
  if ( file.exists( "/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/ancestorMap.RData" ) ) {
    load( file="/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/ancestorMap.RData")
  } else {
    save( ancestorMap, file="/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/ancestorMap.RData")
  }

  # Assign global class
  latest_class = 1
  global_class <- hashmap()
  for ( target_time in ta ) {
    L <- ancestorMap[ target_time][[1]]
    if ( nchar(L[1])==0 ) {
      new_class <- latest_class
      latest_class <- latest_class + 1
    } else {
      GC <- as.numeric( global_class[L] )
      GC_teacher <- as.numeric( teacherClass[L] )
      print( GC )
      print( GC_teacher )
      ant <- table( GC )
      new_class <- as.numeric( names( which( ant == max(ant) ) ) )
      print( paste0( "target_time: ", target_time, " GC: ", new_class, " TC: ", teacherClass[target_time]))
    }
    global_class[ target_time ] <- new_class
  }
    
  
  # Tabulate
  
  
}

