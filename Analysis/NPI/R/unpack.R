unpack <- function( s ) {
  library( stringr )
  if ( is.character(s) ) {
    r <- as.numeric(unlist(strsplit( s,',')))
  } else {
    r <- ""
  }
  return(r)
}