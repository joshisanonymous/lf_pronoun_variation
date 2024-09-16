#############
# Functions #
#############

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
  glmer(Token ~ Verb.Type + French.Background + Birth.Year + Gender + Ethnicity + Race +
          Raised + Residence + Profession + Education + Network.Ethnic.Homophily:Ethnicity +
          (1|Name) + (1|Following.Verb),
        data = tokens[tokens$Token.Type == pronoun,],
        family = binomial)
}

multinomResponse <- function(pronoun) {
  mblogit(Token ~ Verb.Type + French.Background + Birth.Year + Gender + Ethnicity + Race +
            Raised + Residence + Profession + Education + Network.Ethnic.Homophily:Ethnicity,
          data = tokens[tokens$Token.Type == pronoun,],
          random = list(~ 1|Name, ~ 1|Following.Verb))
}