NPI_testbed_drift <- function() {
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
  nSamples <- 2000
  ncols <- 16
  sd <- 10.0
  blockSize <- 1000;
  maxSize <- 1000 	# 1000*20 = 20,000 sec; 2x CW
  CW <- 2000 	# 5000/20 = 250 inputs expected
  CWchunks <- 5
  partitionFactor <- 1
  numthreads <- 4
  cl <- makeCluster( numthreads )
  registerDoParallel(cl)
  
  randVec <- list();
  resultVector <- list();
  
  # Create toy data
  if ( !file.exists( "NPItestbed_drift.RData" ) ) {
    toyData <- rbind( c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                      c(0,3,7,10,8,2,-3,-7,-11,-8,-4,1,5,9,3,0),
                      c(0,-3,-7,-10,-8,-2,3,7,11,8,4,-1,-5,-9,-3,0),
                      c(0,5,10,20,28,30,26,10,-10,-30,-40,-45,-35,-20,-5,0),
                      c(0,-5,-10,-20,-28,-30,-26,-10,10,30,40,45,35,20,5,0),
                      c(0,10,45,60,25,-15,-55,-48,-10,-5,0,0,0,0,0,0),
                      c(0,0,0,0,100,100,100,100,-100,-100,-100,-100,0,0,0,0))
    Prob <- c(1,1,2,3,4,5,5,5,6,6,6);
    Teacher <- c(1,1,1,2,3,4,5);
    
    a <- matrix( nrow=nSamples, ncol=ncols )
    ta <- vector( mode="integer", length=nSamples)
    teacherDF <- data.frame()
    for ( i in seq(1,nSamples) ) {
      if ( i==1 ) {
        ta[1] <- round( runif(1) * 20 );
      } else {
        ta[i] <- ta[i-1] + round( runif(1) * 20 ) + 5;
      }
      idx <- Prob[ sample(1:11,1) ];
      teacherDF <- rbind( teacherDF, data.frame( k=ta[i], v=Teacher[idx] ) );
      for ( j in seq( 1, ncols) ) {
        if ( (Teacher[idx]==4) & (i > (nSamples/2)) ) { # Drift class 4 towards class 2
          a[i,j] <- toyData[4,j] - (toyData[4,j]-toyData[2,j])*(i-(nSamples/2))/(nSamples) + rnorm( 1, mean=0, sd=sd );
        } else {
          a[i,j] <- toyData[idx,j] + rnorm( 1, mean=0, sd=sd );
        }
      }
    }
    # Replace some events with artifact
    ns <- round( nSamples / 2 )
    for ( i in seq(ns,(ns+20)) ) {
      teacherDF$v[i] <- Teacher[7]
      for ( j in seq( 1, ncols) ) {
        a[i,j] <- toyData[7,j] + rnorm( 1, mean=0, sd=sd );
      }
    }

    save( file="NPItestbed_drift.RData", a, ta, teacherDF )  
  }
  # Load toy data
  load( file="NPItestbed_drift.RData" )
  
  if ( !file.exists("window_drift.RData") ) {
  
  # Define processing block sizes
  nbrBlocks <- ((nSamples-blockSize)/(blockSize/2)) + 1
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
      if ( nCand >= 1 ) {
        keep_df <- data.frame()
        for ( idx in seq(1,nCand) ) {
          entry <- cand_df[idx,]
          if ( (entry$cc * entry$cc * entry$er) > runif(1) ) {
            keep_df <- rbind( keep_df, entry )
          }
        }
        if ( nrow(keep_df) > 0 ) {
          assignedDF <- rbind( assignedDF, keep_df )
        }
      }
    } # x
  } # blockNbr
    print( paste0( "saving"))
    save( ta, teacherDF, assignedDF, file="window_drift.RData" )
    print( paste0( "saved"))
  } else {
    load( file="window_drift.RData" )
  }
  
  # Compute the bounds for each processing block
  blockData <- data.frame()
  blockIdxStart <- 1
  processIdxStart <- 1
  endNotReached <- TRUE
  blockNbr <- 0
  while ( endNotReached ) {
    blockNbr <- blockNbr + 1
    blockTimeStart <- ta[blockIdxStart]
    blockTimeStop <- blockTimeStart + CWchunks * CW
    if ( blockTimeStop >= ta[nSamples] ) {
      blockTimeStop <- ta[nSamples]
      blockIdxStop <- nSamples
      processIdxStop <- nSamples
      endNotReached <- FALSE
    } else {
      blockIdxStop <- max( which( ta <=  blockTimeStop ) )
      processIdxStop <- max( which( ta < (blockTimeStop-CW) ) )
    }
    blockData <- rbind( blockData, data.frame( blockNbr=blockNbr, blockIdxStart=blockIdxStart, blockIdxStop=blockIdxStop, processIdxStart=processIdxStart, processIdxStop=processIdxStop) )
    # Prepare for the next block
    processIdxStart <- processIdxStop + 1
    blockIdxStart <- max( max( which( ta < (ta[processIdxStart]-CW) ) ), 1 )
  }
  
  print( blockData )

#  tmpProcessed <- vector()
# Process graphs (OMP) to make "ancestor_map"
  
#  for ( my_id in seq(1:nrow(blockData)) ) {
  ancestorDF <- foreach ( my_id = 1:nrow(blockData), .combine=rbind) %dopar% {
    library(igraph)
    library(leiden)
    library(leidenAlg)
    library(foreach)
    library(stringr)
    
    tmpDF <- data.frame()
    
    source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/pack.R")
    source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/unpack.R")

    # Determine graph limits
    my_start <- blockData$processIdxStart[my_id]
    my_start_t <- ta[my_start]
    my_stop  <- blockData$processIdxStop[my_id]
    my_stop_t <- ta[my_stop]
    my_cnt <- my_stop - my_start + 1
    print( paste0( "Working on ", my_start, " to ", my_stop ) )
  
    # Find the limits of the CW
    my_startCW <- blockData$blockIdxStart[my_id]
    my_startCW_t <- ta[ my_startCW ]
    my_stopCW <- blockData$blockIdxStop[my_id]
    my_stopCW_t <- ta[ my_stopCW ]
    
    # Find the result vectors in this graph
    # Build entire graph for this block of data
    keep_row <- which( assignedDF[,2] >= my_startCW_t & assignedDF[,1] <= my_stopCW_t)
    g <- graph.data.frame( assignedDF[keep_row,1:2], directed=FALSE )
    E(g)$weight <- assignedDF[keep_row,3] * assignedDF[keep_row,3] * assignedDF[keep_row,4]
    
    # Find the target times for this graph
    all_times <- as.numeric( V(g)$name )
    processIdx <- which( ta >= my_start_t & ta <= my_stop_t)
    processTimes <- ta[ processIdx ]

    # Loop on rows
    for ( processTime in processTimes ) {
#      if ( processTime == 14614 | processTime == 14634 ) {
#        print( paste0( "stop" ) )
#      }
#      tmpProcessed <- append( tmpProcessed, processTime )
      # Sub-graph
      vids <- which( all_times>=(processTime-CW) & all_times<=(processTime+CW))
      sg <- induced.subgraph(graph=g,vids=vids)
      # Partition
      partition <- leiden(sg,resolution_parameter=partitionFactor,n_iterations=-1,weights=E(sg)$weight)
      # Process this processTime
      clik <- partition[ which( as.numeric(V(sg)$name) == processTime ) ]
      graphIdx <- which((partition==clik) & (as.numeric(V(sg)$name)<processTime) )
      if ( length( graphIdx) == 0 ) {
        tmpDF <- rbind( tmpDF, data.frame( k=processTime, v=pack("") ) )
      } else {
        tmpDF <- rbind( tmpDF, data.frame( k=processTime, v=pack( as.numeric( V(sg)$name[graphIdx]) ) ) )
      }
    }
    tmpDF
  }

  stopCluster(cl)
  ancestorDF <- ancestorDF[ order(ancestorDF$k),]
  
#  print( setdiff( ta, tmpProcessed ) )

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
  
  nmi <- NMI( teacherDF, assignedDF )
  print( paste0( "NPI: ", nmi ) )
  
  # Compare to k-means
  Ntrials <- 100
  for ( nClusters in c(4,6,5) ) {
    kmeans_result <- vector( mode="double", length=Ntrials )
    for ( trialNbr in 1:Ntrials ) {
      km.out <- kmeans( a, nClusters )
      kmeansDF <- data.frame()
      for ( i in 1:length(ta) ) {kmeansDF <- rbind( kmeansDF, data.frame(k=ta[i],v=km.out$cluster[i]))}
      kmeans_result[trialNbr] <- NMI( teacherDF, kmeansDF )
    }
    print( paste0( "kmeans ", nClusters, ": ", mean( kmeans_result ) ) )
  }
  return( list( teacherDF=teacherDF, assignedDF=assignedDF, kmeans_result=kmeans_result ) )
}

