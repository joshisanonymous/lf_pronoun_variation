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
profession <- c("Blue Collar", "White Collar")
education <- c("Some School", "High School Graduate", "College Graduate")
frenchBackground <- c("Naturalistic", "Institutional", "Personal")

# Network alter variables
avgNetworkSize <- 5
alterType <- c("Core", "Non-Core")
alterEthnicity <- c("Cajun", "Creole", "Vietnamese", "Latino", "American")
frenchFrequency <- c("Always", "Often", "Occasionally", "Never")

## Participants
participants <- babynames[sample(length(babynames$name), participantSize, replace = FALSE), ]
participants <- cbind(
  participants,
  sample(frenchBackground, length(participants$name), replace = TRUE),
  sample(residence, length(participants$name), replace = TRUE),
  sample(raised, length(participants$name), replace = TRUE),
  sample(profession, length(participants$name), replace = TRUE),
  sample(education, length(participants$name), replace = TRUE),
  sample(ethnicity, length(participants$name), replace = TRUE),
  rep("Race", length(participants$name))
)
# Remove unneeded columns
participants <- participants[ , c(1:3, 6:12)]
# Rename columns
colnames(participants) <- c(
  "Birth Year", "Gender", "Name", "French Background", "Residence",
  "Raised", "Profession", "Education", "Ethnicity", "Race")
# Add claimed race based based on claimed ethnicity
for(participant in participants$Name) {
  if(participants[participants$Name == participant, "Ethnicity"] == "Cajun") {
    chosenRace <- sample(race, 1, prob = c(0.01, 0.96, 0.01, 0.01, 0.01))
  } else {
    chosenRace <- sample(race, 1, prob = c(0.3, 0.1, 0.2, 0.2, 0.2))
  }
  participants[participants$Name == participant, "Race"] <- chosenRace
}

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
  "Token" = rep("Token", dataSize * 2),
  "Following Verb" = sample(followingVerb, dataSize * 2, replace = TRUE),
  "Token Type" = c(rep("3pl.M", dataSize),
                   rep("2sg.T", dataSize)),
  "Verb Type" = sample(verbType, dataSize * 2, replace = TRUE),
  "Name" = sample(participants$Name, dataSize * 2, replace = TRUE)
)
tokens <- merge(tokens, participants, by = "Name")
tokens <- tokens[order(tokens$Token.Type, decreasing = TRUE),]
# Generate 3pl.M tokens based on factors (verb type > french background > ethnicity > education)
for(observation in 1:dataSize) {
  weight <- 0
  if(tokens[observation, "Verb.Type"] == "auxiliary") {
    weight <- weight + 1
  }
  if(tokens[observation, "French Background"] == "Institutional") {
    weight <- weight + 1
  }
  if(tokens[observation, "Ethnicity"] == "Cajun") {
    weight <- weight + 1
  } else {
    weight <- weight - 2
  }
  if(tokens[observation, "Education"] == "College Graduate") {
    weight <- weight + 1
  }
  # Use weight to assign token for observation
  if(weight >= 4) {
    producedToken <- sample(pn3plVariants, 1, prob = c(0.88, 0.04, 0.04, 0.03, 0.01))
  }
  if(4 > weight & weight > 0) {
    producedToken <- sample(pn3plVariants, 1, prob = c(0.6, 0.22, 0.06, 0.06, 0.06))
  }
  if(weight <= 0) {
    producedToken <- sample(pn3plVariants, 1, prob = c(0.1, 0.3, 0.1, 0.1, 0.4))
  }
  tokens[observation, "Token"] <- producedToken
}
# Generate 2sg.T tokens based on factors (french background > ethnicity > education)
for(observation in (dataSize + 1):(dataSize * 2)) {
  weight <- 0
  if(tokens[observation, "French Background"] == "Institutional") {
    weight <- weight + 1
  }
  if(tokens[observation, "Ethnicity"] == "Cajun") {
    weight <- weight + 1
  } else {
    weight <- weight - 1
  }
  if(tokens[observation, "Education"] == "College Graduate") {
    weight <- weight + 1
  }
  # Use weight to assign token for observation
  if(weight >= 3) {
    producedToken <- sample(pn2sgVariants, 1, prob = c(0.95, 0.05))
  }
  if(3 > weight & weight > 0) {
    producedToken <- sample(pn2sgVariants, 1, prob = c(0.85, 0.15))
  }
  if(weight <= 0) {
    producedToken <- sample(pn2sgVariants, 1, prob = c(0.7, 0.3))
  }
  tokens[observation, "Token"] <- producedToken
}

# Export data
write.csv(participants, file = paste(dataDir, "test_participants.csv", sep = ""),
          fileEncoding = "UTF-8")
write.csv(networks, file = paste(dataDir, "test_networks.csv", sep = ""),
          fileEncoding = "UTF-8")
write.csv(tokens, file = paste(dataDir, "test_tokens.csv", sep = ""),
          fileEncoding = "UTF-8")

# Cleanup
rm(list = ls())
