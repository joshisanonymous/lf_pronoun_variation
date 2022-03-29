######################
# Generate test data #
######################

# Packages
library(babynames)

# Variables
dataDir <- "./data/"
followingVerb <- read.csv(paste(dataDir, "french_verbs.csv", sep = ""), fileEncoding = "UTF-8")
followingVerb <- followingVerb[followingVerb$tags == "['infinitive']", "form"]
pn2sgVariants <- c("tu", "to")
pn3plVariants <- c("ils", "ça", "eux", "eux-autres", "yé")
verbType <- c("lexical", "modal", "auxiliary")
ethnicity <- c("Cajun", "Creole")
race <- c("Singular Black", "Singular White", "Transcendent", "Border", "Protean")
residence <- c("Lafayette", "St Martin", "St Landry")
raised <- c("Lafayette", "St Martin", "St Landry")
profession <- c("White Collar", "Blue Collar", "Unemployed")
education <- c("Some School", "High School Graduate", "College Graduate")
networkEthnicHomophily <- seq(0, 1, by = 0.01)
dataSize <- 2500

# Participants
participants <- babynames[sample(length(babynames$year), 30, replace = FALSE), ]
participants <- cbind(
  participants,
  sample(ethnicity, length(participants$name), replace = TRUE),
  sample(race, length(participants$name), replace = TRUE),
  sample(residence, length(participants$name), replace = TRUE),
  sample(raised, length(participants$name), replace = TRUE),
  sample(profession, length(participants$name), replace = TRUE),
  sample(education, length(participants$name), replace = TRUE),
  sample(networkEthnicHomophily, length(participants$name), replace = TRUE))
colnames(participants) <- c(
  "Birth Year", "Gender", "Name", "N", "Prop", "Ethnicity",
  "Race", "Residence", "Raised", "Profession", "Education",
  "Network Ethnic Homophily")

# Data
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
write.csv(tokens, file = paste(dataDir, "test_tokens.csv", sep = ""),
          fileEncoding = "UTF-8")

# Cleanup
rm(list = ls())
