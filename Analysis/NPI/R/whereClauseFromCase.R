whereClauseFromCase <- function( compArgs ) {
  library(tools)
  wc <- paste0( " where subject='", compArgs$get("subject"), "' AND channel='", tools::file_path_sans_ext(compArgs$get('channel')), "' AND seizureUsed=", compArgs$get("centerTime") )
  return( wc )
} 

