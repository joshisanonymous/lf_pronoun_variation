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
library(reshape2)
library(mclogit)
library(lme4)
library(car)
library(dplyr)
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

# Some subsets ---------------------------------------------------------------
tokens3plAnim <- tokens[grep("3pl", tokens$ProType),]
tokens3plAnim$ProType <- recode_factor(
  tokens3plAnim$ProType,
  "3pl.AF" = "3pl.A",
  "3pl.AM" = "3pl.A",
  "3pl.AMF" = "3pl.A",
  "3pl.IF" = "3pl.I",
  "3pl.IM" = "3pl.I"
)

tokens3plGender <- tokens[grep("3pl.(A|I)(M|F|MF)", tokens$ProType),]
tokens3plGender$ProType <- recode_factor(
  tokens3plGender$ProType,
  "3pl.AF" = "3pl.F",
  "3pl.AM" = "3pl.M",
  "3pl.AMF" = "3pl.MF",
  "3pl.IF" = "3pl.F",
  "3pl.IM" = "3pl.M"
)

# Remove tokens deemed not useful for analyses
tokens$ProType <- recode_factor(
  tokens$ProType,
  "3pl.A" = "3pl",
  "3pl.AF" = "3pl",
  "3pl.AM" = "3pl",
  "3pl.AMF" = "3pl",
  "3pl.I" = "3pl",
  "3pl.IF" = "3pl",
  "3pl.IM" = "3pl"
)
tokensAll <- tokens

# Remove very low count 3pl variants
tokens <- droplevels(subset(tokens, ProUnder != "elles" & ProUnder != "eux" &
                                    ProUnder != "eux-autres"))
tokens <- droplevels(tokens <- tokens[!(tokens$ProType == "3pl" & tokens$ProUnder == "ø"),])
rownames(tokens) <- NULL

# Calculate EI homophily indices for each participant
# for(name in participants$Name) {
#   # For whole personal networks
#   homophIndex <- getEIHomophily(networks, name)
#   participants[participants$Name == name, "Network.Ethnic.Homophily"] <- homophIndex
#   tokens[tokens$Name == name, "Network.Ethnic.Homophily"] <- homophIndex
#   # For just anglophone alters
#   justAnglo <- networks[networks$Alter.French.Frequency == "Never", ]
#   homophIndex <- getEIHomophily(justAnglo, name)
#   participants[participants$Name == name, "Anglo.Network.Ethnic.Homophily"] <- homophIndex
#   tokens[tokens$Name == name, "Anglo.Network.Ethnic.Homophily"] <- homophIndex
#   # For just francophone alters
#   justFranco <- networks[networks$Alter.French.Frequency != "Never", ]
#   homophIndex <- getEIHomophily(justFranco, name)
#   participants[participants$Name == name, "Franco.Network.Ethnic.Homophily"] <- homophIndex
#   tokens[tokens$Name == name, "Franco.Network.Ethnic.Homophily"] <- homophIndex
# }
# Clean-up for loop
# rm(list = c("justAnglo", "justFranco", "homophIndex", "name"))

# Merge participant metadata with tokens
tokens <- merge(tokens, participants, by = "Name")

# Create generic DFs and tables
ethByRaceTable <- table(participants$Ethnicity, participants$Race)
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
# Analyses #
############

# Pronoun Models #
  ##############

# It may be beneficial to split up verb type into more than just
# auxiliary, modal, and lexical based on Dubois (2001) and Gudmestad &
# Carmichael (2022), which suggest 1sg effects for être vs avoir and
# verbs of opinion/belief.

# logitModels <- list(
  # firstSg = multinomResponse("1sg"),
  # secondSgT = binomResponse("2sg.T"),
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
  # thirdPl = multinomResponse("3pl")
# )
# small3plModel <- mblogit(ProUnder ~ PredType + Ethnicity + Race,
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
# 
source("maps.R")
source("plots.R")