script_tabulate <- function() {
  load(file='validEpochs_23_003.RData')
  vsi_23_003 <- NPI:::tabulateBehavioralResult(validEpochs)
  load(file='validEpochs_24_002.RData')
  vsi_24_002 <- NPI:::tabulateBehavioralResult(validEpochs)
  load(file='validEpochs_24_005.RData')
  vsi_24_005 <- NPI:::tabulateBehavioralResult(validEpochs)
  load(file='validEpochs_25_002.RData')
  vsi_25_002 <- NPI:::tabulateBehavioralResult(validEpochs)
  load(file='validEpochs_25_003.RData')
  vsi_25_003 <- NPI:::tabulateBehavioralResult(validEpochs)
  load(file='validEpochs_25_005.RData')
  vsi_25_005 <- NPI:::tabulateBehavioralResult(validEpochs)
}
