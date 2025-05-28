#############
# Functions #
#############

# Social tables
socialTable <- function(pronoun, socialVariable) {
  table(
    droplevels(tokens[tokens$ProType == pronoun, socialVariable]),
    droplevels(tokens[tokens$ProType == pronoun, "ProUnder"])
  )  
}

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

# For main models (for each, add + Network.Ethnic.Homophily:Ethnicity later when network data read)
binomResponse <- function(pronoun) {
  glmer(ProUnder ~ PredType + `Birth Year` + Gender + Ethnicity +
          Occupation + Education +
          (1|Name) + (1|PredUnder),
        data = droplevels(tokens[tokens$ProType == pronoun,]),
        family = binomial)
}

multinomResponse <- function(pronoun) {
  mblogit(ProUnder ~ PredType + `Birth Year` + Gender + Ethnicity +
            Occupation + Education,
          data = droplevels(tokens[tokens$ProType == pronoun,]),
          random = list(~ 1|Name, ~ 1|PredUnder))
}

# Plots

socialPlot <- function(table) {
  ggplot(
    melt(table,
         varnames = c("socialVar", "pronoun"),
         value.name = "Count"),
    aes(x = pronoun, y = Count, fill = pronoun)) +
    geom_bar(stat = "identity") +
    facet_wrap(. ~ socialVar) +
    theme(text=element_text(size=15), legend.text = element_markdown(),
          legend.position = "none") +
    scale_fill_manual(values = color_key) +
    labs(x = "Pronoun", y = "Count")
}
