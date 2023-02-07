parseArg <- function( args, name ) {
  #' @export
  idx <- which( names(args) == name )
  if ( length(idx) > 0 ) {
    return( unname( unlist( args[name] ) ) )
  } else {
    idx <- which( args == name) # Check against the args, rather than the name
    if ( length(idx) > 0 ) {
      return("-1")
    } else {
      return("")
    }
  }
}
