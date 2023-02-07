run_largeSumTest <- function() {
  library(ggplot2)
  library(reshape2)
  
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/largeSumTest.R")
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/largeSumTest_random.R")
  
#  N <- c(1E3,1E4,1E5,1E6)
#  N <- c(1E3,1E4,1E5,1E6,1E7,1E8)
  N <- c(1E3,1E4,1E5,1E6,1E7,1E8,1E9)
  errorRatio <- vector(mode='double',length=length(N))
  
  er <- data.frame( x=1:length(N) );
  tmp <- vector(mode='double',length=length(N))
  
  cnt <- 0
  for ( sz in N ) {
    cnt <- cnt + 1
    tmp[cnt] <- largeSumTest(sz) / sz
  }
  er <- cbind( er, data.frame( plus=log10(abs(tmp)) ) )
  
#  cnt <- 0
#  for  ( sz in N ) {
#    cnt <- cnt + 1
#    tmp[cnt] <- largeSumTest_random(sz) / sz
#  }
#  er <- cbind( er, data.frame( random=log10(abs(tmp)) ) )
  
  mer <- melt(er,id='x')
  p <- ggplot( data=mer, aes(x,value,color=variable,group=variable)) + geom_line(size=2) + scale_color_manual(values=c("#00FFFF", "#FF00FF")) + scale_y_continuous(name='ratio', breaks=c(-18,-14,-10), labels=c("-16","-12","-8"), limits=c(-19,-7.5)) + scale_x_continuous(name="log10( N )",breaks=c(1,4,7),labels=c("3","6","9"), limits=c(.5,7.5))
  print(p)

  return( er )
}