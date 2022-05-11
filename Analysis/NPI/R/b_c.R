b_c <- function() {
  #' @export

  library( foreach )
  library( parallel )
  library( doParallel )
  nbrWorkers <- parallel::detectCores()
#  nbrWorkers <- 6
  doParallel::registerDoParallel(nbrWorkers)

  load(file="colVal.RData")
  
  cutoffs <- c(c(2,5,10,20,50,100,200,500,1000))

  print( paste0( "Using ", nbrWorkers, " workers."))
  foreach ( cutoff = cutoffs ) %dopar% {
    print( system.time( value <- mean( igraph::estimate_betweenness( graph=grph, cutoff=cutoff ) ) ) )
    print( paste0( "cutoff: ", cutoff ) )
    print( value )
  }
  print( system.time( value <- mean( igraph::betweenness( graph=grph ) ) ) )
  print( paste0( "cutoff: full" ) )
  print( value )
}
