NMI <- function( teacherMap, assignedMap ) {
  # teacherMap: an r2r::hashmap
  # assignedMap: an r2r::hashmap
  library(r2r)
  
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/H.R")
  
  # Get the keys
  teacherIdxs <- keys( teacherMap )
  assignedIdxs <- keys( assignedMap )
  
  # Compute HofT
  HofT <- 0.0;
  tcounts <- hashmap()
  for ( i in 1:length(teacherIdxs) ) {
    tc =  as.numeric(unlist(teacherMap[ teacherIdxs[i] ]))
#    print( paste0( "tc: ", tc ) )
    if ( has_key( tcounts, tc ) ) {
      tcounts[tc] <- unlist(tcounts[tc]) + 1
    } else {
      tcounts[tc] <- 1
    }
  }
  Nteachers <- length(tcounts);
  for ( i in 1:Nteachers ) {
    i <- as.numeric(i)
    pH = as.numeric(unlist(tcounts[ i ])) / length(teacherMap);
    HofT <- HofT + H(pH)
  }
  
  # Compute HofC
  HofC = 0.0;
  acounts <- hashmap()
  for ( i in 1:length(assignedMap) ) {
    cc =  as.numeric(unlist(assignedMap[ assignedIdxs[i] ]))
#    print( paste0( "cc: ", cc ) )
    if ( has_key( acounts, cc ) ) {
      acounts[cc] <- unlist(acounts[cc]) + 1
    } else {
      acounts[cc] <- 1
    }
  }
  Nclusters = length( acounts );

  # Compute PofC
  PofC <- vector()
  for ( i in 1:Nclusters ) {
    i <- as.numeric(i)
    pC <- as.numeric(unlist(acounts[i])) / length(assignedMap)
#    print( paste0( "pC: ", pC ) )
    PofC <- append( PofC, pC )
    HofC <- HofC + H( pC )
  }
  
  # PofTgivenC
#  print( paste0( "nT: ", Nteachers, "\tnC: ", Nclusters ) )
  PofTgivenC <- matrix( 0, Nteachers, Nclusters )
  for ( i in 1:length(teacherMap) ) {
    tc = as.numeric(unlist(teacherMap[teacherIdxs[i]])) + 1
    cc = as.numeric(unlist(assignedMap[assignedIdxs[i]]))
#    print( paste0( "tc: ", tc, "\tcc: ", cc ) )
    PofTgivenC[ tc, cc ] <- PofTgivenC[ tc, cc ] + 1
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
