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
library(ggpubr)
library(egg)
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
parishes <- c("Calcasieu Parish", "Cameron Parish", "Jefferson Davis Parish",
              "Vermilion Parish", "Acadia Parish", "Evangeline Parish",
              "Avoyelles Parish", "St. Landry Parish", "Lafayette Parish",
              "Saint Martin Parish", "Iberia Parish", "St. Mary Parish",
              "Assumption Parish", "Iberville Parish", "Pointe Coupee Parish",
              "East Baton Rouge Parish", "Ascension Parish", "St. James Parish",
              "St. John the Baptist Parish", "St. Charles Parish",
              "Lafourche Parish", "Terrebonne Parish")
majorCities <- c("New Orleans", "Lafayette", "Baton Rouge")
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

# Remove very low count levels
tokens <- subset(tokens, !(ProType == "1sg" & ProUnder == "ça"))
tokens <- subset(tokens, !(ProType == "1pl" & (ProUnder == "vous-autres" | ProUnder == "nous-autres" | ProUnder == "ça" | ProUnder == "nous")))
tokens <- subset(tokens, !(ProType == "2sg.T" & ProUnder == "ça"))
tokens <- subset(tokens, ProType != "3sg")
tokens <- subset(tokens, !(ProType == "3sg.F" & (ProUnder == "il" | ProUnder == "ça")))
tokens <- subset(tokens, !(ProType == "3sg.M" & (ProUnder == "elle" | ProUnder == "ça" | ProUnder == "ø")))
tokens <- subset(tokens, !(ProType == "3pl" & (ProUnder == "elles" | ProUnder == "eux" | ProUnder == "eux-autres" | ProUnder == "ø")))
tokens <- subset(tokens, !(ProType == "demo" & ProUnder == "ø"))
tokens <- subset(tokens, !(ProType == "expl" & ProUnder == "li"))
tokens <- subset(tokens, !(ProType == "imp" & (ProUnder == "ø" | ProUnder == "vou" | ProUnder == "vous" | ProUnder == "on")))
tokens <- subset(tokens, !(PredType == "noun" | PredType == "adjective" | PredType == "preposition"))
tokens <- droplevels(tokens)
rownames(tokens) <- NULL

# Calculate EI homophily indices for each participant
for(name in participants$Name) {
  # For whole personal networks
  homophIndex <- getEIHomophily(networks, name)
  participants[participants$Name == name, "Network.Ethnic.Homophily"] <- homophIndex
  # For just anglophone alters
  justAnglo <- networks[networks$Alter.French.Frequency == "Never", ]
  homophIndex <- getEIHomophily(justAnglo, name)
  participants[participants$Name == name, "Anglo.Network.Ethnic.Homophily"] <- homophIndex
  # For just francophone alters
  justFranco <- networks[networks$Alter.French.Frequency != "Never", ]
  homophIndex <- getEIHomophily(justFranco, name)
  participants[participants$Name == name, "Franco.Network.Ethnic.Homophily"] <- homophIndex
}
# Clean-up for loop
rm(list = c("justAnglo", "justFranco", "homophIndex", "name"))

# Set dummy EI homophily of participants who did not give network info
homophilyDummy <- mean(participants$Network.Ethnic.Homophily, na.rm = TRUE)
participants[participants$Name == "Errol Stoufle", "Network.Ethnic.Homophily"] <- homophilyDummy
participants[participants$Name == "Rachel Chenevert", "Network.Ethnic.Homophily"] <- homophilyDummy

# Merge participant metadata with tokens
tokens <- merge(tokens, participants, by = "Name")

# Subsets to use for some exploratory graphs
subsetsProType <- list(
  firstSg = subsetProType("1sg"),
  secondSgT = subsetProType("2sg.T"),
  thirdSgF = subsetProType("3sg.F"),
  thirdSgM = subsetProType("3sg.M"),
  firstPl = subsetProType("1pl"),
  thirdPl = subsetProType("3pl"),
  impersonal = subsetProType("imp")
)

############
# Analyses # -------------------------------------------------------------------
############

# Factor independencies
indOccEduc <- fisher.test(table(participants$Occupation, participants$Education))
indOccGend <- fisher.test(table(participants$Occupation, participants$Ethnicity))

# Pronoun Models #
  ##############

logitModels <- list(
  firstSg = multinomResponse("1sg"),
  secondSgT = multinomResponse("2sg.T"),
  # secondSgV = multinomResponse("2sg.V"), count too low to be meaningfully modeled
  thirdSgF = multinomResponse("3sg.F", exclude_modal = TRUE),
  thirdSgM = multinomResponse("3sg.M"),
  firstPl = multinomResponse("1pl"),
  # secondPl = multinomResponse("2pl"), count too low to be meaningful
  thirdPl = multinomResponse("3pl"),
  # demostrative = multinomResponse("demo"), categorically "ça"
  expletive = multinomResponse("expl", exclude_aux = TRUE),
  impersonal = multinomResponse("imp")
)

logitModelSummaries <- lapply(logitModels, summary)

# small3plModel <- mblogit(ProUnder ~ PredType + Ethnicity,
#         data = tokens[tokens$ProType == "3pl",],
#         random = list(~ 1|Name, ~ 1|PredUnder))

# Check for multicollinearity 
logitVif <- lapply(logitModels, vif)

# Social Networks #
  ###############

# Multinomial logistic model for the relationship between frequency of French
# use and alter type (i.e., core alters vs non-core)
coreByFrMultinom <- mblogit(Alter.French.Frequency ~ Alter.Type,
                            data = networks,
                            random = ~ 1|Name)

# # Is there a difference between the anglophone and francophone ethnic homophily?
homophByLanguageTtest <- t.test(participants$Anglo.Network.Ethnic.Homophily,
                                participants$Franco.Network.Ethnic.Homophily,
                                paired = TRUE)

# # Is one ethnic group more homophilous than the other?
homophByEthnicGroupTtest <- t.test(participants[participants$Ethnicity == "Creole",
                                                "Network.Ethnic.Homophily"],
                                   participants[participants$Ethnicity == "Cajun",
                                                "Network.Ethnic.Homophily"])

source("tables.R")
source("maps.R")
source("plots-final.R")
