NMI <- function( teacherDF, assignedDF ) {
  # teacherDF:  key-value data.frame
  # assignedDF: key-value data.frame
  library(r2r)
  
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/H.R")
  
  # Get the keys
  teacherIdxs <- teacherDF$v
  assignedIdxs <- assignedDF$v
  
  # Compute HofT
  HofT <- 0.0;
  tcounts <- data.frame()
  uniqueTeacherIdxs <- unique( teacherIdxs )
  for ( i in 1:length(uniqueTeacherIdxs) ) {
    tcounts <- rbind( tcounts, data.frame( k=uniqueTeacherIdxs[i], v=0 ) )    
  }
  for ( i in 1:length(teacherIdxs) ) {
    tc =  as.numeric(teacherDF$v[ i ])
    idx <- which( tcounts$k == tc )
    tcounts$v[idx] <- tcounts$v[idx] + 1
  }
  Nteachers <- nrow(tcounts);
  for ( i in 1:Nteachers ) {
    i <- as.numeric(i)
    pH = as.numeric(tcounts$v[ i ]) / nrow(teacherDF);
    HofT <- HofT + H(pH)
  }
  
  # Compute HofC
  HofC = 0.0;
  acounts <- data.frame()
  uniqueAssignedIdxs <- unique( assignedIdxs )
  for ( i in 1:length(uniqueAssignedIdxs) ) {
    acounts <- rbind( acounts, data.frame( k=uniqueAssignedIdxs[i], v=0 ) )    
  }
  for ( i in 1:nrow(assignedDF) ) {
    ac =  as.numeric(assignedDF$v[ i ])
    idx <- which( acounts$k == ac )
    acounts$v[idx] <- acounts$v[idx] + 1
  }
  Nclusters = nrow( acounts );

  # Compute PofC
  PofC <- vector()
  for ( i in 1:Nclusters ) {
    pC <- as.numeric(acounts$v[i]) / nrow(assignedDF)
#    print( paste0( "pC: ", pC ) )
    PofC <- append( PofC, pC )
    HofC <- HofC + H( pC )
  }
  
  # PofTgivenC
#  print( paste0( "nT: ", Nteachers, "\tnC: ", Nclusters ) )
  PofTgivenC <- matrix( 0, Nteachers, Nclusters )
  for ( i in 1:nrow(teacherDF) ) {
    tc <- as.numeric(teacherDF$v[ i ])
    ac <- as.numeric(assignedDF$v[ i ])
    #    print( paste0( "tc: ", tc, "\tcc: ", cc ) )
    PofTgivenC[ tc, ac ] <- PofTgivenC[ tc, ac ] + 1
  }

  # Normalize
  for ( c in 1:Nclusters ) {
    colSum = 0
    for ( t in 1:Nteachers )
      colSum <- colSum + PofTgivenC[ t, c ]
#    print( paste0( "colSum: ", colSum ) )
    for ( t in 1:Nteachers ) {
        PofTgivenC[ t, c ] <- PofTgivenC[ t, c ] / colSum
    }
  }

  # Compute I(T;C) = H(T) - HofTgivenC
  HofTgivenC <- 0.0
  for ( c in 1:Nclusters ) {
    for ( t in 1:Nteachers ) {
      HPTC = H( PofTgivenC[ t, c ] );
      HofTgivenC <- HofTgivenC + PofC[c] * HPTC;
#      print( paste0( "PofC: ", PofC[c], "\tHPTC: ", HPTC, "\tHTC:  ", HofTgivenC ) )
    }
  }
  IofTandC <- HofT - HofTgivenC;
  
  # Compute NMI
  eps = .Machine$double.eps; # smallest, non-zero positive value
  if ( IofTandC < eps ) {
    NMI = 0.0;
  } else {
    NMI = 2 * IofTandC /( HofT + HofC );
  }
#  print( paste0( "HofC: ", HofC, "\tHofT: ", HofT, "\tHofTgivenC: ", HofTgivenC, "\tIofTandC: ", IofTandC ) );
  return( NMI );
}
