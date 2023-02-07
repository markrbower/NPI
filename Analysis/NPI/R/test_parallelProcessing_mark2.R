test_parallelProcessing_mark2 <- function() {
  library(future)
  plan(multisession,workers=8)
  
  process_data_chunk <- function(i) {
    j=0
    for (k in seq(i,1E9)) j <- j+k
    value <- i*j
    return(value)
  }
  
  store_to_database <- function(j,k) {
    cat( "Stored in: ", j, " the value: ", unlist(k), "\n" )
  }
  
  p_limit = 20
  f_futs <- list()
  result <- list()
  j <- 0
  for (i in seq(1,100) ) {
    cat( i, "\n" )
    f_futs[[i]] <- future( process_data_chunk( i ) )
    if ( i > p_limit ) {
      j <- j + 1
      result[[j]] <- value( f_futs[[j]] )
      store_to_database( j, result[[j]] )
    }
  }  
  # Clear out the results
  for ( k in seq((j+1),100) ) {
    result[[k]] <- value( f_futs[[k]] )
    store_to_database( k, result[[k]] )
  }  
}

