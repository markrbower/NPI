load( file="a_1600.RData")

Ntrials <- 1000
results <- vector( mode="double", length=Ntrials )
for ( trialNbr in 1:Ntrials ) {
  km.out <- kmeans( a, 4 )
  for ( i in 1:length(ta) ) { assignedMap[ta[i]] <- km.out$cluster[i]}
  results[trialNbr] <- NMI( teacherClass, assignedMap )
}

hist(results)
