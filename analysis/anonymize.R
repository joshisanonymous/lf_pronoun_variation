# Variables ---------------------------------
dataDir <- "../data/"

# Data --------------------------------------
participantKey <- read.csv(paste(dataDir, "participant_key.csv", sep = ""), sep = "\t")
anonNames <- read.csv(paste(dataDir, "anon_names.csv", sep = ""), sep = "\t")

# Functions ---------------------------------
anonymizeNames <- function(realName) {
  replacers <- anonNames[anonNames$Type == realName["Type"],]
  replacers <- sample(replacers$Name)
  for(replacer in replacers) {
    if(replacer != realName["Real.Name"] && !(replacer %in% participantKey$Anonymized)) {
      participantKey[participantKey$Real.Name == realName["Real.Name"] &
                     participantKey$Type == realName["Type"],]$Anonymized <- replacer
      break
    }
  }
}

# Process ------------------------------------
# get names that haven't been anonymized
toAnonymize <- participantKey[is.na(participantKey$Anonymized),]
# anonymize them
apply(toAnonymize, 1, anonymizeNames)
apply(toAnonymize, 1, print(toAnonymize$Type))
# save as file