source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/NPI_testbed_prob_noSaves.R")

load( file="a_1600.RData")

Ntrials <- 10
results <- vector( mode="double", length=Ntrials )
for ( trialNbr in 1:Ntrials ) {
  assignedMap <- NPI_testbed_prob_noSaves()
  results[trialNbr] <- NMI( teacherClass, assignedMap )
}

