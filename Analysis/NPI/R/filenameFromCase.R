filenameFromCase <- function( case ) {
  #' @export
  
  library( stringr )

  if ( str_detect( case$channel, 'NVC' ) ) {
    patientDir <- paste0( NPI:::getNVsubjectFromFilename( case$channel ), '_2' )
    if ( str_detect( case$channel, 'mef$' ) ) {
      localname <- case$channel
    } else {
      localname <- paste0( case$channel, '.mef' )
    }
  } else {
    if ( str_detect( case$channel, 'mef$') ) {
      localname <- case$channel
    } else {
      localname <- paste0( case$channel, '.mef' )
    }
  }

  return( localname )
}
