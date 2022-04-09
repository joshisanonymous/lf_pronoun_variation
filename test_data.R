######################
# Generate test data #
######################

## Packages
library(babynames)

## Variables
dataDir <- "./data/"
dataSize <- 2500
participantSize <- 30

# Linguistic variables
followingVerb <- read.csv(paste(dataDir, "french_verbs.csv", sep = ""), fileEncoding = "UTF-8")
followingVerb <- followingVerb[followingVerb$tags == "['infinitive']", "form"]
pn2sgVariants <- c("tu", "to")
pn3plVariants <- c("ils", "ça", "eux", "eux-autres", "yé")
verbType <- c("lexical", "modal", "auxiliary")

# Social variables
ethnicity <- c("Cajun", "Creole")
race <- c("Singular Black", "Singular White", "Transcendent", "Border", "Protean")
residence <- c("Lafayette", "St Martin", "St Landry")
raised <- c("Lafayette", "St Martin", "St Landry")
profession <- c("White Collar", "Blue Collar", "Unemployed")
education <- c("Some School", "High School Graduate", "College Graduate")

# Network alter variables
avgNetworkSize <- 5
alterType <- c("Core", "Non-Core")
alterEthnicity <- c("Cajun", "Creole", "Vietnamese", "Latino", "American")
frenchFrequency <- c("Always", "Often", "Occasionally", "Never")

## Participants
participants <- babynames[sample(length(babynames$name), participantSize, replace = FALSE), ]
participants <- cbind(
  participants,
  sample(ethnicity, length(participants$name), replace = TRUE),
  sample(race, length(participants$name), replace = TRUE),
  sample(residence, length(participants$name), replace = TRUE),
  sample(raised, length(participants$name), replace = TRUE),
  sample(profession, length(participants$name), replace = TRUE),
  sample(education, length(participants$name), replace = TRUE),
  "")
colnames(participants) <- c(
  "Birth Year", "Gender", "Name", "N", "Prop", "Ethnicity",
  "Race", "Residence", "Raised", "Profession", "Education",
  "Network Ethnic Homophily")

# Participant alters
networks <- data.frame(
  "Participant" = rep(participants$Name, avgNetworkSize),
  "Alter" = babynames[sample(length(babynames$name), participantSize * avgNetworkSize), "name"],
  "Alter Type" = sample(alterType, participantSize * avgNetworkSize, replace = TRUE),
  "Alter Ethnicity" = sample(alterEthnicity, participantSize * avgNetworkSize, replace = TRUE),
  "Alter French Frequency" = sample(frenchFrequency, participantSize * avgNetworkSize, replace = TRUE)
)
colnames(networks) <- c(colnames(networks)[1], "Alter", colnames(networks)[3:5])

## Data
tokens <- data.frame(
  "Token" = c(sample(pn3plVariants, dataSize, replace = TRUE),
              sample(pn2sgVariants, dataSize, replace = TRUE)),
  "Following Verb" = sample(followingVerb, dataSize * 2, replace = TRUE),
  "Token Type" = c(rep("3pl.M", dataSize),
                   rep("2sg.T", dataSize)),
  "Verb Type" = sample(verbType, dataSize * 2, replace = TRUE),
  "Name" = sample(participants$Name, dataSize * 2, replace = TRUE)
)
tokens <- merge(tokens, participants, by = "Name")

# Export data
write.csv(participants, file = paste(dataDir, "test_participants.csv", sep = ""),
          fileEncoding = "UTF-8")
write.csv(networks, file = paste(dataDir, "test_networks.csv", sep = ""),
          fileEncoding = "UTF-8")
write.csv(tokens, file = paste(dataDir, "test_tokens.csv", sep = ""),
          fileEncoding = "UTF-8")

# Cleanup
rm(list = ls())
