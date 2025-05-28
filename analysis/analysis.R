######################################################
# Analyses for pronoun data examined in relation     #
# to ethnicity and race in Louisiana.                #
#                                                    #
# Joshua McNeill - joshua dot mcneill at uga dot edu #
######################################################

############
# Packages # -----------------------------------------------------------------
############

library(ggplot2)
library(ggtext)
library(ggh4x)
library(egg)
# library(ggpubr)
library(reshape2)
library(mclogit)
library(lme4)
library(car)
library(plyr)
library(dplyr)
library(lubridate)
source("functions.R")

######################
# Data and variables # -------------------------------------------------------
######################

# Variables
color_key <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
               "#CC6677", "#882255", "#AA4499", "#DDDDDD")
# Read in data
participants <- read.csv("../data/metadata.csv", fileEncoding = "UTF-8",
                         stringsAsFactors = TRUE, sep = "\t", na.strings = "")
networks <- read.csv("../data/networks.csv", fileEncoding = "UTF-8",
                     stringsAsFactors = TRUE, sep = "\t", na.strings = "")
tokens <- read.csv("../data/pronoun_tokens.csv", fileEncoding = "UTF-8",
                   stringsAsFactors = TRUE, na.strings = "")
source("data_cleaning.R")
source("subsets.R")
source("plots-pre.R")

###############################
# Final cleaning for analysis # ------------------------------------------------
###############################

# Collapse pronoun types that aren't important
tokens$ProType <- recode_factor(
  tokens$ProType,
  "1pl.F" = "1pl",
  "1pl.M" = "1pl",
  "1pl.MF" = "1pl",
  "2pl.F" = "2pl",
  "2pl.M" = "2pl",
  "2pl.MF" = "2pl",
  "3sg.A" = "3sg",
  "3sg.I" = "3sg",
  "3sg.AF" = "3sg.F",
  "3sg.IF" = "3sg.F",
  "3sg.AM" = "3sg.M",
  "3sg.IM" = "3sg.M",
  "3pl.A" = "3pl",
  "3pl.AF" = "3pl",
  "3pl.AM" = "3pl",
  "3pl.AMF" = "3pl",
  "3pl.I" = "3pl",
  "3pl.IF" = "3pl",
  "3pl.IM" = "3pl"
)
tokensAll <- tokens

# Remove very low count variants
tokens <- subset(tokens, !(ProType == "1sg" & ProUnder == "ça"))
tokens <- subset(tokens, !(ProType == "1pl" & (ProUnder == "vous-autres" | ProUnder == "nous-autres" | ProUnder == "ça")))
tokens <- subset(tokens, !(ProType == "2sg.T" & ProUnder == "ça"))
tokens <- subset(tokens, ProType != "3sg")
tokens <- subset(tokens, !(ProType == "3sg.F" & (ProUnder == "il" | ProUnder == "ça")))
tokens <- subset(tokens, !(ProType == "3sg.M" & (ProUnder == "elle" | ProUnder == "ça" | ProUnder == "ø")))
tokens <- subset(tokens, !(ProType == "3pl" & (ProUnder == "elles" | ProUnder == "eux" | ProUnder == "eux-autres" | ProUnder == "ø")))
tokens <- subset(tokens, !(ProType == "demo" & ProUnder == "ø"))
tokens <- subset(tokens, !(ProType == "expl" & ProUnder == "li"))
tokens <- subset(tokens, !(ProType == "imp" & ProUnder == "ø"))
tokens <- droplevels(tokens)
rownames(tokens) <- NULL

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

# Merge participant metadata with tokens
tokens <- merge(tokens, participants, by = "Name")

# Create generic DFs and tables
ethByRaceTable <- table(participants$Ethnicity, participants$Race)

# Social pronoun tables
tablesEthnicity <- list(
  firstSg = socialTable("1sg", "Ethnicity"),
  secondSgT = socialTable("2sg.T", "Ethnicity"),
  secondSgV = socialTable("2sg.V", "Ethnicity"),
  thirdSgF = socialTable("3sg.F", "Ethnicity"),
  thirdSgM = socialTable("3sg.M", "Ethnicity"),
  firstPl = socialTable("1pl", "Ethnicity"),
  secondPl = socialTable("2pl", "Ethnicity"),
  thirdPl = socialTable("3pl", "Ethnicity"),
  # demonstrative = socialTable("demo", "Ethnicity"),
  expletive = socialTable("expl", "Ethnicity"),
  impersonal = socialTable("imp", "Ethnicity")
)

tablesRace <- list(
  firstSg = socialTable("1sg", "Race"),
  secondSgT = socialTable("2sg.T", "Race"),
  secondSgV = socialTable("2sg.V", "Race"),
  thirdSgF = socialTable("3sg.F", "Race"),
  thirdSgM = socialTable("3sg.M", "Race"),
  firstPl = socialTable("1pl", "Race"),
  secondPl = socialTable("2pl", "Race"),
  thirdPl = socialTable("3pl", "Race"),
  # demonstrative = socialTable("demo", "Race"),
  expletive = socialTable("expl", "Race"),
  impersonal = socialTable("imp", "Race")
)

tablesGender <- list(
  firstSg = socialTable("1sg", "Gender"),
  secondSgT = socialTable("2sg.T", "Gender"),
  secondSgV = socialTable("2sg.V", "Gender"),
  thirdSgF = socialTable("3sg.F", "Gender"),
  thirdSgM = socialTable("3sg.M", "Gender"),
  firstPl = socialTable("1pl", "Gender"),
  secondPl = socialTable("2pl", "Gender"),
  thirdPl = socialTable("3pl", "Gender"),
  # demonstrative = socialTable("demo", "Gender"),
  expletive = socialTable("expl", "Gender"),
  impersonal = socialTable("imp", "Gender")
)

tablesEducation <- list(
  firstSg = socialTable("1sg", "Education"),
  secondSgT = socialTable("2sg.T", "Education"),
  secondSgV = socialTable("2sg.V", "Education"),
  thirdSgF = socialTable("3sg.F", "Education"),
  thirdSgM = socialTable("3sg.M", "Education"),
  firstPl = socialTable("1pl", "Education"),
  secondPl = socialTable("2pl", "Education"),
  thirdPl = socialTable("3pl", "Education"),
  # demonstrative = socialTable("demo", "Education"),
  expletive = socialTable("expl", "Education"),
  impersonal = socialTable("imp", "Education")
)

tablesOccupation <- list(
  firstSg = socialTable("1sg", "Occupation"),
  secondSgT = socialTable("2sg.T", "Occupation"),
  secondSgV = socialTable("2sg.V", "Occupation"),
  thirdSgF = socialTable("3sg.F", "Occupation"),
  thirdSgM = socialTable("3sg.M", "Occupation"),
  firstPl = socialTable("1pl", "Occupation"),
  secondPl = socialTable("2pl", "Occupation"),
  thirdPl = socialTable("3pl", "Occupation"),
  # demonstrative = socialTable("demo", "Occupation"),
  expletive = socialTable("expl", "Occupation"),
  impersonal = socialTable("imp", "Occupation")
)

tablesRaised <- list(
  firstSg = socialTable("1sg", "Raised (parish)"),
  secondSgT = socialTable("2sg.T", "Raised (parish)"),
  secondSgV = socialTable("2sg.V", "Raised (parish)"),
  thirdSgF = socialTable("3sg.F", "Raised (parish)"),
  thirdSgM = socialTable("3sg.M", "Raised (parish)"),
  firstPl = socialTable("1pl", "Raised (parish)"),
  secondPl = socialTable("2pl", "Raised (parish)"),
  thirdPl = socialTable("3pl", "Raised (parish)"),
  # demonstrative = socialTable("demo", "Raised (parish)"),
  expletive = socialTable("expl", "Raised (parish)"),
  impersonal = socialTable("imp", "Raised (parish)")
)

tablesResidence <- list(
  firstSg = socialTable("1sg", "Residence (parish)"),
  secondSgT = socialTable("2sg.T", "Residence (parish)"),
  secondSgV = socialTable("2sg.V", "Residence (parish)"),
  thirdSgF = socialTable("3sg.F", "Residence (parish)"),
  thirdSgM = socialTable("3sg.M", "Residence (parish)"),
  firstPl = socialTable("1pl", "Residence (parish)"),
  secondPl = socialTable("2pl", "Residence (parish)"),
  thirdPl = socialTable("3pl", "Residence (parish)"),
  # demonstrative = socialTable("demo", "Residence (parish)"),
  expletive = socialTable("expl", "Residence (parish)"),
  impersonal = socialTable("imp", "Residence (parish)")
)

# coreByFrTable <- table(networks$Alter.French.Frequency, networks$Alter.Type)
# verbCollatesMostFrequent <- table(tokens$PredUnder)
# verbCollatesMostFrequent <- verbCollatesMostFrequent[
#   order(verbCollatesMostFrequent, decreasing = TRUE)
# ]
# thirdSgITable <- table(tokens[tokens$ProType == "3sg.IF" |
#                               tokens$ProType == "3sg.IM",
#                               "ProUnder"])
# thirdPlFTable <- table(tokens[tokens$ProType == "3pl.F",
                              # "ProUnder"])

############
# Analyses # -------------------------------------------------------------------
############

# Factor independencies
indOccEduc <- fisher.test(table(participants$Occupation, participants$Education))
indOccGend <- fisher.test(table(participants$Occupation, participants$Ethnicity))

# Pronoun Models #
  ##############

# It may be beneficial to split up verb type into more than just
# auxiliary, modal, and lexical based on Dubois (2001) and Gudmestad &
# Carmichael (2022), which suggest 1sg effects for être vs avoir and
# verbs of opinion/belief.

# logitModels <- list(
#   firstSg = multinomResponse("1sg"),
#   secondSgT = binomResponse("2sg.T"),
#   secondSgV = multinomResponse("2sg.V"),
#   thirdSgF = binomResponse("3sg.F"),
#   thirdSgM = binomResponse("3sg.M"),
#   firstPl = multinomResponse("1pl"),
#   secondPl = multinomResponse("2pl"),
#   thirdPl = multinomResponse("3pl"),
#   demostrative = multinomResponse("demo"),
#   expletive = multinomResponse("expl"),
#   impersonal = multinomResponse("imp")
# )

# small3plModel <- mblogit(ProUnder ~ PredType + Ethnicity,
#         data = tokens[tokens$ProType == "3pl",],
#         random = list(~ 1|Name, ~ 1|PredUnder))

# Check for multicollinearity between factors and remove those that are
# highly collinear. In particular, verify whether ethnicity and race
# are collinear.
# logitVif <- lapply(logitModels, vif)

# Use AIC to check model fits and further remove factors if sensible

# Social Networks #
  ###############

# Multinomial logistic model for the relationship between frequency of French
# use and alter type (i.e., core alters vs non-core)
# coreByFrMultinom <- mblogit(Alter.French.Frequency ~ Alter.Type,
#                             data = networks,
#                             random = ~ 1|Participant)
# 
# # Is there a difference between the anglophone and francophone ethnic homophily?
# homophByLanguageTtest <- t.test(participants$Anglo.Network.Ethnic.Homophily,
#                                 participants$Franco.Network.Ethnic.Homophily,
#                                 paired = TRUE)
# 
# # Is one ethnic group more homophilous than the other?
# homophByEthnicGroupTtest <- t.test(participants[participants$Ethnicity == "Creole",
#                                                 "Network.Ethnic.Homophily"],
#                                    participants[participants$Ethnicity == "Cajun",
#                                                 "Network.Ethnic.Homophily"])
source("maps.R")
source("plots-final.R")