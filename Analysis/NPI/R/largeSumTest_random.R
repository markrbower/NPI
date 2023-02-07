largeSumTest_random <- function(N) {
  s <- 0
  r <- runif( N, 0, 1 )
  bias <- 0
  for ( x in seq(1,N) ) {
    if ( r[x] > 0.5 ) {
      s <- s + 1.1
      bias <- bias + 1
    } else {
      s <- s - 1.1
      bias <- bias - 1
    }
  }
  return( s - bias*1.1 )
}
