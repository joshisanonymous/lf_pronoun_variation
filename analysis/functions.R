#############
# Functions #
#############

# Formatting -------------------------------------------------------------------
Round <- function(number) {
  sprintf("%.3f", round_any(number, 0.001, round))
}

Ceiling <- function(number) {
  sprintf("%.3f", round_any(number, 0.001, ceiling))
}

# Subsets ----------------------------------------------------------------------
subsetTokens <- function(pronoun, feature) {
  if(feature == "animacy") {
    subset <- tokens[grep(paste0(pronoun, "\\.(A|I)(F|M|MF)?"), tokens$ProType),]
    subset$ProType <- as.factor(gsub("A(F|M|MF)$", "A", subset$ProType))
    subset$ProType <- as.factor(gsub("I(F|M|MF)$", "I", subset$ProType))
  } else if(feature == "gender") {
    subset <- tokens[grep(paste0(pronoun, "\\.(A|I)?(F|M|MF)"), tokens$ProType),]
    subset$ProType <- as.factor(gsub("(A|I)F$", "F", subset$ProType))
    subset$ProType <- as.factor(gsub("(A|I)M$", "M", subset$ProType))
    subset$ProType <- as.factor(gsub("(A|I)MF$", "MF", subset$ProType))
  } else if(feature == "animate gender") {
    subset <- tokens[grep(paste0(pronoun, "\\.A(F|M|MF)"), tokens$ProType),]
  } else if(feature == "inanimate gender") {
    subset <- tokens[grep(paste0(pronoun, "\\.I(F|M|MF)"), tokens$ProType),]
  } else {
    print("Error. Choose the pronoun feature you want.")
  }
  return(subset)
}

subsetProType <- function(pronoun) {
  tokens[tokens$ProType == pronoun,]
}

# For model plots
getNewdataAge <- function(pronoun) {
  data.frame(
    Age = participants$Age,
    PredType = factor(levels(logitModels[[pronoun]]$data$PredType)[1], levels = levels(logitModels[[pronoun]]$data$PredType)),
    Ethnicity = factor(levels(logitModels[[pronoun]]$data$Ethnicity)[1], levels = levels(logitModels[[pronoun]]$data$Ethnicity)),
    Gender = factor(levels(logitModels[[pronoun]]$data$Gender)[1], levels = levels(logitModels[[pronoun]]$data$Gender)),
    `Institutional French` = factor(levels(logitModels[[pronoun]]$data$`Institutional French`)[1], levels = levels(logitModels[[pronoun]]$data$`Institutional French`)),
    Name = factor(levels(logitModels[[pronoun]]$data$Name)[1], levels = levels(logitModels[[pronoun]]$data$Name)),
    PredUnder = factor(levels(logitModels[[pronoun]]$data$PredUnder)[1], levels = levels(logitModels[[pronoun]]$data$PredUnder)),
    check.names = FALSE
  )
}

getNewdataPredType <- function(pronoun, level_fr = NULL) {
  if(!is.null(level_fr)) {
    french <- level_fr
  } else {
    french <- levels(logitModels[[pronoun]]$data$`Institutional French`)[1]
  }
  data.frame(
    Age = mean(participants$Age),
    PredType = factor(levels(logitModels[[pronoun]]$data$PredType), levels = levels(logitModels[[pronoun]]$data$PredType)),
    Ethnicity = factor(levels(logitModels[[pronoun]]$data$Ethnicity)[1], levels = levels(logitModels[[pronoun]]$data$Ethnicity)),
    Gender = factor(levels(logitModels[[pronoun]]$data$Gender)[1], levels = levels(logitModels[[pronoun]]$data$Gender)),
    `Institutional French` = factor(french, levels = levels(logitModels[[pronoun]]$data$`Institutional French`)),
    Name = factor(levels(logitModels[[pronoun]]$data$Name)[1], levels = levels(logitModels[[pronoun]]$data$Name)),
    PredUnder = factor(levels(logitModels[[pronoun]]$data$PredUnder)[1], levels = levels(logitModels[[pronoun]]$data$PredUnder)),
    check.names = FALSE
  )
}

predictNewdataAge <- function(pronoun) {
  predict(
    logitModels[[pronoun]], newdata = newdataAge[[pronoun]],
    type = "response", random = NULL
  )  
}

predictNewdataPredType <- function(pronoun) {
  predict(
    logitModels[[pronoun]], newdata = newdataPredType[[pronoun]],
    type = "response", random = NULL
  )  
}

# Tables -----------------------------------------------------------------------
# Phonetic variants table
tablePhonetic <- function(pronoun) {
  table(droplevels(tokens[tokens$ProUnder == pronoun, "Pronoun"]))  
}

tableProType <- function(protype, subset = "all") {
  if(subset == "all") {
    table(droplevels(tokensAll[tokensAll$ProType == protype, "ProUnder"]))    
  }
  else if(subset == "final") {
    table(droplevels(tokens[tokens$ProType == protype, "ProUnder"]))    
  }
}

# Social tables
tableSocial <- function(pronoun, socialVariable) {
  table(
    droplevels(tokens[tokens$ProType == pronoun, socialVariable]),
    droplevels(tokens[tokens$ProType == pronoun, "ProUnder"])
  )
}

tableSubsetSocial <- function(pronoun, socialVariable) {
  table(
    droplevels(subsetTokensRace[subsetTokensRace$ProType == pronoun, socialVariable]),
    droplevels(subsetTokensRace[subsetTokensRace$ProType == pronoun, "ProUnder"])
  )
}

# Tables for individual participants
tableParticipant <- function(name, protype) {
  table(droplevels(tokens[tokens$Name == name & tokens$ProType == protype, "ProUnder"]))
}

# Network stuff ----------------------------------------------------------------
getAlterEthCount <- function(df, name, participantEthnicity, sameEthnicity = TRUE) {
  if(sameEthnicity == TRUE) {
    length(df[df$Name == name &
                df$Alter.Ethnicity == participantEthnicity,
              "Alter.Ethnicity"])
  } else {
    length(df[df$Name == name &
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

# Models -----------------------------------------------------------------------
test3plPred <- function(pronoun) {
  mblogit(ProUnder ~ PredType,
          data = droplevels(tokens[tokens$ProType == pronoun,]),
          random = list(~ 1|Name, ~ 1|PredUnder))
}

multinomResponse <- function(pronoun, exclude_aux = FALSE, exclude_modal = FALSE) {
  if(exclude_aux == TRUE) {
    tokens <- droplevels(subset(tokens, !(PredType == "auxiliary")))
  } else if(exclude_modal == TRUE) {
    tokens <- droplevels(subset(tokens, !(PredType == "modal")))
  }
  mblogit(ProUnder ~ PredType + Ethnicity + Gender + `Institutional French` + Age,
          data = droplevels(tokens[tokens$ProType == pronoun,]),
          random = list(~ 1|Name, ~ 1|PredUnder))
}

# Plots ------------------------------------------------------------------------
plotSocial <- function(table) {
  ggplot(melt(table,
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

plotPronoun <- function(df, rotate_labels = FALSE) {
  plot <- ggplot(df, aes(x = ProUnder)) +
    geom_bar() +
    facet_wrap(. ~ ProType) +
    theme_bw() +
    labs(x = "Pronoun", y = "Count")
  if(rotate_labels == TRUE) {
    plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(plot)
}

plotEthOcc <- function(df) {
  plot <- ggplot(df, aes(x = ProUnder)) +
    geom_bar() +
    facet_wrap(Ethnicity ~ Occupation) +
    theme_bw() +
    labs(x = "Pronoun", y = "Count")
  return(plot)
}

plotEduOcc <- function(df) {
  plot <- ggplot(df, aes(x = ProUnder)) +
    geom_bar() +
    facet_wrap(Education ~ Occupation) +
    theme_bw() +
    labs(x = "Pronoun", y = "Count")
  return(plot)
}

plotParticipant <- function(name, protype) {
  ggplot(data = tokens[tokens$Name == name & tokens$ProType == protype,],
         aes(x = ProUnder)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Pronoun", y = "Count", title = name)
}

# Regression plots
plotLogitNumAge <- function(pronoun, reference, contrast) {
  ggplot() +
    geom_jitter(data = subsetsProType[[pronoun]],
                aes(x = Age, y = as.numeric(ProUnder == contrast), color = Ethnicity),
                width = 2, height = 0.05) +
    scale_color_manual(values = color_key) +
    geom_line(data = modelsAge[[pronoun]],
              aes(x = Age, y = !! sym(contrast)),
              color = "#CC6677", linewidth = 1.5) +
    labs(x = "Age", y = paste("Probability of", contrast)) +
    scale_y_continuous(breaks = c(0, 1), labels = c(reference, contrast), limits = c(-0.1, 1.1)) +
    scale_x_reverse() +
    theme_bw()
}

plotLogitBarPredType <- function(pronoun) {
  ggplot(data = modelsPredType[[pronoun]],
         aes(x = ProUnder, y = Probability, fill = ProUnder)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ PredType) +
  labs(x = "Pronoun", y = "Probability") +
  scale_fill_manual(values = color_key) +
  theme(legend.position = "none")
}

# Maps -------------------------------------------------------------------------
getPolyCenter <- function(placeName){
  placeCoordinates <- osmdata::getbb(placeName) %>% # Obtain the bounding box corners fro open street map
    t() %>% # Transpond the returned matrix so that you get x and y coordinates in different columns
    data.frame() %>% # The next function takes a data frame as input
    sf::st_as_sf(coords = c("x", "y")) %>%  # Convert to simple feature
    sf::st_bbox() %>% # get the bounding box of the corners
    sf::st_as_sfc() %>% # convert bounding box to polygon
    sf::st_centroid() %>% # get the centroid of the polygon
    sf::st_as_sf() %>% # store as simple feature
    sf::`st_crs<-`(4326)  # set the coordinate system to WGS84 (GPS etc.)
  
  placeCoordinates %>%
    dplyr::mutate(placeName = placeName) %>% # add input city name in a column
    dplyr::rename(geometry = x) %>% # Rename the coordinate column
    dplyr::relocate(placeName, geometry) %>% # reorder the columns
    st_coordinates() %>%
    return()
}

getPolyCentersFromPlaces <- function(placesVector) {
  coordinates <- data.frame(placesVector,
                            t(sapply(lapply(placesVector, getPolyCenter), c)))
  colnames(coordinates) <- c("place", "longitude", "latitude")
  return(coordinates)
}