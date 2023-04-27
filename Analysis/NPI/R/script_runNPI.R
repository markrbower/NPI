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
  pInf <- RFactories::parameterInformer(signalType='IIS')
  pInf$loadParameters( dbp, aInf )  #  The parameterInformer requires a databaseProvidere to load parameters from the database.
  compArgs$add( pInf )

#  mysql> describe P;
#  +-------------+-------------+------+-----+-------------------+-------------------+
#    | Field       | Type        | Null | Key | Default           | Extra             |
#    +-------------+-------------+------+-----+-------------------+-------------------+
#    | subject     | varchar(32) | YES  | MUL | NULL              |                   |
#    | channel     | varchar(32) | YES  |     | NULL              |                   |
#    | time        | bigint      | YES  |     | NULL              |                   |
#    | waveform    | mediumtext  | YES  |     | NULL              |                   |
#    | cluster     | varchar(32) | YES  |     | NULL              |                   |
#    | clusterid   | int         | YES  |     | NULL              |                   |
#    | seizureUsed | bigint      | YES  |     | NULL              |                   |
#    | peak        | double      | YES  |     | NULL              |                   |
#    | energy      | double      | YES  |     | NULL              |                   |
#    | incident    | mediumtext  | YES  |     | NULL              |                   |
#    | weights     | mediumtext  | YES  |     | NULL              |                   |
#    | created_on  | datetime    | NO   |     | CURRENT_TIMESTAMP | DEFAULT_GENERATED |
#    | UUID        | varchar(36) | YES  |     | NULL              |                   |
#    +-------------+-------------+------+-----+-------------------+-------------------+
#    13 rows in set (0.02 sec)

  NPI:::NPI( compArgs )
}
