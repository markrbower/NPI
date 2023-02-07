mysql2df <- function() {
  
  # Get the user input
  txt <- readLines()
  
  # Parse the labels
  N <- length(txt)
  words = unlist(strsplit(txt[2], " "))
  nonempty <- which(unlist(lapply(words, function(x) nchar(x)>0 )))
  labels <- words[nonempty[which( words[nonempty] != "|")]]
  df = data.frame(matrix(nrow = 0, ncol = length(labels)))

  # Parse the values
  for ( i in 4:(N-1) ) {
    words = unlist(strsplit(var[i], " "))
    nonempty <- which(unlist(lapply(words, function(x) nchar(x)>0 )))
    values <- words[nonempty[which( words[nonempty] != "|")]]
    
    df <- rbind( df, values )    
  }
  
  # Add the labels
  colnames(df) <- labels
  return(df)
  
# To plot
# 
# myColors <- brewer.pal(4,"Set1")
# names(myColors) <- levels(df$cw)
# colScale <- scale_colour_manual(name = "cw",values = myColors)
# p <- ggplot(df,aes(blackout,sys,colour = cw)) + geom_point()
# p1 <- p + colScale
# p1
}


