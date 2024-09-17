#############
# Functions #
#############

# Network stuff
getAlterEthCount <- function(df, name, participantEthnicity, sameEthnicity = TRUE) {
  if(sameEthnicity == TRUE) {
    length(df[df$Participant == name &
                df$Alter.Ethnicity == participantEthnicity,
              "Alter.Ethnicity"])
  } else {
    length(df[df$Participant == name &
                df$Alter.Ethnicity != participantEthnicity,
              "Alter.Ethnicity"])
  }
}

getEIHomophily <- function(df, name) {
  parEthnicity <- as.character(participants[participants$Name == name, "Ethnicity"])
  sameEth <- getAlterEthCount(df, name, parEthnicity)
  diffEth <- getAlterEthCount(df, name, parEthnicity, sameEthnicity = FALSE)
  homophIndex <- (diffEth - sameEth) / (diffEth + sameEth)
  return(homophIndex)
}

# For main models
binomResponse <- function(pronoun) {
  glmer(ProUnder ~ PredType + `French Background` + `Birth Year` + Gender + Ethnicity + Race +
          `Raised (parish)` + `Residence (parish)` + Profession + Education + Network.Ethnic.Homophily:Ethnicity +
          (1|Name) + (1|PredUnder),
        data = tokens[tokens$ProType == pronoun,],
        family = binomial)
}

multinomResponse <- function(pronoun) {
  mblogit(ProUnder ~ PredType + `French Background` + `Birth Year` + Gender + Ethnicity + Race +
            `Raised (parish)` + `Residence (parish)` + Profession + Education + Network.Ethnic.Homophily:Ethnicity,
          data = tokens[tokens$ProType == pronoun,],
          random = list(~ 1|Name, ~ 1|PredUnder))
}