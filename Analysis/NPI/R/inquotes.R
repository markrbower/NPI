inquotes <- function( str ) {
  #' @export
  tmp <- paste0( str, collapse="\',\'")
  return( paste0( "\'", tmp, "\'"))
}