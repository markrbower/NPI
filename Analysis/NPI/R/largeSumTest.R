largeSumTest <- function(N) {
  s <- 0
  for ( x in seq(1,N) ) {
    s <- s + 1.1
  }
  return( s - N*1.1 )
}
