######################################################
# Analyses for pronoun data examined in relation     #
# to ethnicity and race in Louisiana.                #
#                                                    #
# Joshua McNeill - joshua dot mcneill at uga dot edu #
######################################################

############
# Packages #
############

library(ggplot2)
library(reshape2)
library(mclogit)
library(lme4)
library(car)

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
          Raised + Residence + Profession + Education + Network.Ethnic.Homophily +
          (1|Name) + (1|Following.Verb),
        data = tokens[tokens$Token.Type == pronoun,],
        family = binomial)
}

multinomResponse <- function(pronoun) {
  mblogit(Token ~ Verb.Type + French.Background + Birth.Year + Gender + Ethnicity + Race +
            Raised + Residence + Profession + Education + Network.Ethnic.Homophily,
          data = tokens[tokens$Token.Type == pronoun,],
          random = list(~ 1|Name, ~ 1|Following.Verb))
}

######################
# Data and variables #
######################

# Read in data
participants <- read.csv("../data/test_participants.csv", fileEncoding = "UTF-8",
                         stringsAsFactors = TRUE)
networks <- read.csv("../data/test_networks.csv", fileEncoding = "UTF-8",
                     stringsAsFactors = TRUE)
tokens <- read.csv("../data/test_tokens.csv", fileEncoding = "UTF-8",
                   stringsAsFactors = TRUE)

# Order factors in the data that make sense to order
tokens$Token <- factor(tokens$Token,
                       levels = c("ça", "ils", "eux", "eux-autres", "yé",
                                  "tu", "to"))
tokens$Verb.Type <- factor(tokens$Verb.Type,
                           levels = c("lexical", "modal", "auxiliary"))
participants$Profession <- factor(participants$Profession,
                                  levels = c("Blue Collar", "White Collar"))
participants$Education <- factor(participants$Education,
                                 levels = c("Some School", "High School Graduate", "College Graduate"))
networks$Alter.French.Frequency <- factor(networks$Alter.French.Frequency,
                                          levels = c("Never", "Occasionally", "Often", "Always"))

# Calculate EI homophily indices for each participant
for(name in participants$Name) {
  # For whole personal networks
  homophIndex <- getEIHomophily(networks, name)
  participants[participants$Name == name, "Network.Ethnic.Homophily"] <- homophIndex
  tokens[tokens$Name == name, "Network.Ethnic.Homophily"] <- homophIndex
  # For just anglophone alters
  justAnglo <- networks[networks$Alter.French.Frequency == "Never", ]
  homophIndex <- getEIHomophily(justAnglo, name)
  participants[participants$Name == name, "Anglo.Network.Ethnic.Homophily"] <- homophIndex
  tokens[tokens$Name == name, "Anglo.Network.Ethnic.Homophily"] <- homophIndex
  # For just francophone alters
  justFranco <- networks[networks$Alter.French.Frequency != "Never", ]
  homophIndex <- getEIHomophily(justFranco, name)
  participants[participants$Name == name, "Franco.Network.Ethnic.Homophily"] <- homophIndex
  tokens[tokens$Name == name, "Franco.Network.Ethnic.Homophily"] <- homophIndex
}
# Clean-up for loop
rm(list = c("justAnglo", "justFranco", "homophIndex", "name"))

# Create generic DFs and tables
ethByRaceTable <- table(participants$Ethnicity, participants$Race)
coreByFrTable <- table(networks$Alter.French.Frequency, networks$Alter.Type)
verbCollatesMostFrequent <- table(tokens$Following.Verb)
verbCollatesMostFrequent <- verbCollatesMostFrequent[
  order(verbCollatesMostFrequent, decreasing = TRUE)
]
thirdSgITable <- table(tokens[tokens$Token.Type == "3sg.IF" |
                                tokens$Token.Type == "3sg.IM",
                              "Token"])
thirdPlFTable <- table(tokens[tokens$Token.Type == "3pl.F",
                              "Token"])

############
# Analyses #
############

# Pronoun Models #
  ##############

# It may be beneficial to split up verb type into more than just
# auxiliary, modal, and lexical based on Dubois (2001) and Gudmestad &
# Carmichael (2022), which suggest 1sg effects for être vs avoir and
# verbs of opinion/belief.

logitModels <- list(
  # firstSg = multinomResponse("1sg"),
  secondSgT = binomResponse("2sg.T"),
  # secondSgV = multinomResponse("2sg.V"),
  # thirdSgAF = binomResponse("3sg.AF"),
  # thirdSgAM = binomResponse("3sg.AM"),
  # Check thirdSgITable to see if il or elle are ever used for inanimate referents
  # Use only one 3rd person inanimate model if il/elle not used
  # thirdSgIF = multinomResponse("3sg.IF"),
  # thirdSgIM = multinomResponse("3sg.IM"),
  # firstPl = multinomResponse("1pl"),
  # secondPl = multinomResponse("2pl"),
  # Check thirdPlFTable to see if elles is ever used for 3rd person plural
  # Use only one model if not used
  # thirdPlF = multinomResponse("3pl.F"),
  thirdPlM = multinomResponse("3pl.M")
)

# Check for multicollinearity between factors and remove those that are
# highly collinear. In particular, verify whether ethnicity and race
# are collinear.
logitVif <- lapply(logitModels, vif)

# Use AIC to check model fits and further remove factors if sensible

# Social Networks #
  ###############

# Multinomial logistic model for the relationship between frequency of French
# use and alter type (i.e., core alters vs non-core)
coreByFrMultinom <- mblogit(Alter.French.Frequency ~ Alter.Type,
                            data = networks,
                            random = ~ 1|Participant)

# Is there a difference between the anglophone and francophone ethnic homophily?
homophByLanguageTtest <- t.test(participants$Anglo.Network.Ethnic.Homophily,
                                participants$Franco.Network.Ethnic.Homophily,
                                paired = TRUE)

# Is one ethnic group more homophilous than the other?
homophByEthnicGroupTtest <- t.test(participants[participants$Ethnicity == "Creole",
                                                "Network.Ethnic.Homophily"],
                                   participants[participants$Ethnicity == "Cajun",
                                                "Network.Ethnic.Homophily"])


#########
# Plots #
#########

# Ethnicity by race
ethByRaceBar <- ggplot(
  melt(ethByRaceTable,
       varnames = c("Ethnicity", "Race"),
       value.name = "Count"),
  aes(
    x = Ethnicity,
    y = Count,
    fill = Race
    )
) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

# French usage by alter type (i.e., coreness)
coreByFrBar <- ggplot(
  melt(coreByFrTable,
       varnames = c("French.Frequency", "Alter.Type"),
       value.name = "Count"),
  aes(
    x = Alter.Type,
    y = Count,
    fill = French.Frequency
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

# Comparison of homophily by French usage
homophByLanguage <- ggplot(
  melt(participants,
       id.vars = "Name",
       measure.vars = c("Network.Ethnic.Homophily",
                        "Anglo.Network.Ethnic.Homophily",
                        "Franco.Network.Ethnic.Homophily"),
       variable.name = "Section.of.Network",
       value.name = "Ethnic.Homophily"),
  aes(
    y = Ethnic.Homophily,
    x = Section.of.Network
  )
) +
  geom_boxplot() +
  theme_bw()
