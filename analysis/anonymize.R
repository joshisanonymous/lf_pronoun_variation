# Variables ---------------------------------
dataDir <- "../data/"

# Data --------------------------------------
participantKey <- read.csv(paste(dataDir, "participant_key.csv", sep = ""),
                           sep = "\t", fileEncoding = "UTF-8")
anonNames <- read.csv(paste(dataDir, "anon_names.csv", sep = ""),
                      sep = "\t", fileEncoding = "UTF-8")

# Process ------------------------------------
for(row in 1:nrow(participantKey)) {
  if(is.na(participantKey[row, "Anonymized"])) {
    replacers <- anonNames[anonNames$Type == participantKey[row, "Type"],]
    replacers <- sample(replacers$Name)
    for(replacer in replacers) {
      if(!(identical(replacer, participantKey[row, "Real.Name"])) &&
         !(replacer %in% participantKey$Anonymized)) {
        participantKey[row, "Anonymized"] <- replacer
        break
      }
    }
  }
}

# Save ---------------------------------------
write.csv(participantKey, file = paste(dataDir, "participant_key.csv", sep = ""),
          sep = "\t", fileEncoding = "UTF-8")