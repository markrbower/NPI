script_runNPI <- function() {
  #' @export
  #  options(error=recover)
  compArgs <- RFactories::argumentComposite()
  dbp <- RFactories::databaseProvider(user="root",vault_user='localadmin',vault_key='NV_password',host='localhost',dbname='NV')
  compArgs$add( dbp )
  if ( dir.exists('/Volumes/Data/NV/NVC1001_24_002_2') ) {
    compArgs$add( RFactories::fileProvider(path='/Volumes/Data/NV/NVC1001_24_002_2',iterationType='directory',pattern="*.mef") )
  } else if ( dir.exists('/Volumes/eplab/Raw_Data/NV_Human/NVC1001_24_002_2') ) {
    compArgs$add( RFactories::fileProvider(path='/Volumes/eplab/Raw_Data/NV_Human/NVC1001_24_002_2',iterationType='directory',pattern="*.mef") )
  } else if ( dir.exists('Z:/Raw_Data/NV_Human/NVC1001_24_002_2') ) {
    compArgs$add( RFactories::fileProvider(path='Z:/Raw_Data/NV_Human/NVC1001_24_002_2',iterationType='directory',pattern="*.mef") )
  } else {
    print( "Cannot find a directory for data" )
    return()
  }
  aInf <- RFactories::analysisInformer(experiment='NeuroVista',subject='24_002',centerTime=0,pattern="*.mef",lab="RNCP")
  compArgs$add( aInf )
  pInf <- RFactories::parameterInformer(signalType='IIS',correlationWindow=600000000,CCthreshold=0.9)
#  pInf$loadParameters( dbp, aInf )  #  The parameterInformer requires a databaseProvider to load parameters from the database.
  compArgs$add( pInf )

  print( paste0( "experiment: ", compArgs$get('experiment')))
  print( paste0( "subject: ", compArgs$get('subject')))
  print( paste0( "centerTime: ", compArgs$get('centerTime')))
  
  NPI:::NPI( compArgs )
}
