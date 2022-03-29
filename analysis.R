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

######################
# Data and variables #
######################

# Read in data
participants <- read.csv("./data/test_participants.csv", fileEncoding = "UTF-8",
                         stringsAsFactors = TRUE)
tokens <- read.csv("./data/test_tokens.csv", fileEncoding = "UTF-8",
                   stringsAsFactors = TRUE)

# Create generic DFs and tables
ethByRaceTable <- table(participants$Ethnicity, participants$Race)
verbCollatesMostFrequent <- table(tokens$Following.Verb)
verbCollatesMostFrequent <- verbCollatesMostFrequent[
  order(verbCollatesMostFrequent, decreasing = TRUE)
]
thirdSgITable <- table(tokens[tokens$Token.Type == "3sg.IF" |
                                tokens$Token.Type == "3sg.IM",
                              "Token"])
thirdPlFTable <- table(tokens[tokens$Token.Type == "3pl.F",
                              "Token"])

#############
# Functions #
#############

binomResponse <- function(pronoun) {
  glmer(Token ~ Gender + Ethnicity + Race + Raised + Residence +
          Profession + Education + (1|Name),
        data = tokens[tokens$Token.Type == pronoun,],
        family = binomial)
}

multinomResponse <- function(pronoun) {
  mblogit(Token ~ Gender + Ethnicity + Race + Raised + Residence +
            Profession + Education,
          data = tokens[tokens$Token.Type == pronoun,],
          random = ~ 1|Name)
}

############
# Analyses #
############

## Association between ethnicity and race
ethByRaceChiSquare <- chisq.test(participants$Ethnicity, participants$Race)
# Check that expected values are >5, otherwise use Fisher's exact
# ethByRaceChiSquare$expected
ethByRaceFisher <- fisher.test(participants$Ethnicity, participants$Race)

# Pronoun Models #
  ##############

# Check verbCollatesMostFrequent to see if there are overrepresented
# verbs not account for by verb type, in which case verb should be a
# random intercept

firstSg <- multinomResponse("1sg")
secondSgT <- binomResponse("2sg.T")
secondSgV <- multinomResponse("2sg.V")
thirdSgAF <- binomResponse("3sg.AF")
thirdSgAM <- binomResponse("3sg.AM")
# Check thirdSgITable to see if il or elle are ever used for inanimate referents
# Use only one 3rd person inanimate model if il/elle not used
thirdSgIF <- multinomResponse("3sg.IF")
thirdSgIM <- multinomResponse("3sg.IM")
firstPl <- multinomResponse("1pl")
secondPl <- multinomResponse("2pl")
# Check thirdPlFTable to see if elles is ever used for 3rd person plural
# Use only one model if not used
thirdPlF <- multinomResponse("3pl.F")
thirdPlM <- multinomResponse("3pl.M")

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
    y = Frequency,
    fill = Race
    )
) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()
