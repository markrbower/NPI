filenameFromCase <- function( basedir, case ) {
  #' @export
  
  library( stringr )
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/R/getNVsubjectFromFilename.R')
  
  # Now that I am switching to a full directory description with "NPO.R", "getNVsubjectFromFilename" should not be needed.
  if ( str_detect( case$channel, 'NVC' ) ) {
    patientDir <- paste0( getNVsubjectFromFilename( case$channel ), '_2' )
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

  result <- file.path( basedir, localname, fsep = .Platform$file.sep)
  
  return( result )
}
