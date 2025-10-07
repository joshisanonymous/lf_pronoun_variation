# Re-align 2nd interviewee tokens and remove blank rows -----------------------
tokens <- data.frame(
  "Name" = c(tokens$Name, tokens$Name2),
  "SubjPred" = c(tokens$SubjPred, tokens$SubjPred2),
  "Pronoun" = c(tokens$Pronoun, tokens$Pronoun2),
  "ProUnder" = c(tokens$ProUnder, tokens$ProUnder2),
  "ProType" = c(tokens$ProType, tokens$ProType2),
  "Predicate" = c(tokens$Predicate, tokens$Predicate2),
  "PredUnder" = c(tokens$PredUnder, tokens$PredUnder2),
  "PredType" = c(tokens$PredType, tokens$PredType2)
)
tokens <- na.omit(tokens)
rownames(tokens) <- NULL

# Linguistic cleaning ---------------------------------------------------------
# Remove ambiguous pronouns
tokens <- droplevels(subset(tokens, ProUnder != "n'" & ProUnder != "t'"))

# Collapse factor levels
tokens$ProUnder <- recode_factor(
  tokens$ProUnder,
  "eux-aut" = "eux-autres",
  "vous-aut" = "vous-autres",
  "nous-aut" = "nous-autres",
  "a" = "elle",
  "alle" = "elle",
  "ce" = "ça",
  "eu" = "eux",
  "ti" = "tu"
)

# 3pl i and il to ils
tokens$ProUnder <- as.character(tokens$ProUnder)
for(token in 1:nrow(tokens)) {
  if(grepl("3pl.*", tokens[token, "ProType"]) &
     grepl("il?$", tokens[token, "ProUnder"])) {
    tokens[token, "ProUnder"] <- "ils"
  }
  rm(token)
}
tokens$ProUnder <- as.factor(tokens$ProUnder)

# Order factors in order to make reasonable reference levels
tokens$ProUnder <- factor(tokens$ProUnder,
                       levels = c("je", "vous", "tu", "on", "ø", "ça", "elle", "il", "vous-autres", "ils",
                                  "elles", "eux", "eux-autres", "nous", "nous-autres", 
                                  "li", "mo", "no", "to", "vou", "yé"))
tokens$PredType <- factor(tokens$PredType,
                           levels = c("lexical", "modal", "auxiliary", "preposition", "adjective", "noun"))

# Participant cleaning --------------------------------------------------------
# Fix headers
colnames(participants) <- c("Name", "Recorded", "Birth Year", "Raised (town)",
                            "Raised (parish)", "Residence (town)", "Residence (parish)",
                            "F&C Background", "Institutional French", "F&C Notes",
                            "Education", "Occupation", "Occupational History",
                            "Retired", "Gender", "Gender Source", "Race", "Ethnicity",
                            "Transcribed", "Coded", "Anonymized", "Length Completed",
                            "Total Length", "Notes")

# Remove unusable participants
participants <- droplevels(subset(participants, Ethnicity != "French" &
                                                Name != "Latoya Pomier"))

# Convert times to something calculable
participants$`Total Length` <-  period_to_seconds(hms(participants$`Total Length`))/60/60

# Clean up race levels to aggregate or abbreviate
participants$Race <- recode_factor(
  participants$Race,
  "Cajun / White" = "White / Cajun",
  "Caucasian" = "White",
  "Caucasian / White" = "White",
  "Cajun / Caucasian" = "White / Cajun",
  "Paraphrase: Whatever they call me" = "Transcendent"
)

# Collapse factor levels
participants[participants$Name == "Samantha Primeaux", "Education"] <- "College Graduate"
participants[participants$Name == "Oliver Gomez", "Education"] <- "No College"
participants[participants$Name == "Floyd Kerry", "Education"] <- "No College"
participants[participants$Name == "Tracy Roth", "Education"] <- "No College"
participants$Education <- droplevels(participants$Education)
participants$`F&C Background` <- sub("\\W{1,}.*", "", participants$`F&C Background`)
# participants[participants$Name == "Alice Lemaire", "Occupation"] <- "White Collar"
# participants[participants$Name == "Gene Delcambre", "Occupation"] <- "Blue Collar"
# participants[participants$Name == "Judy Soileau Courtade", "Occupation"] <- "White Collar"
# participants[participants$Name == "Rachel Chenevert", "Occupation"] <- "Blue Collar"
# participants[participants$Name == "Rose Louvière", "Occupation"] <- "White Collar"
# participants$Occupation <- droplevels(participants$Occupation)

# Insert temporary but likely values for missing values
participants[participants$Name == "Jack Munson", "F&C Background"] <- "Naturalistic"
participants[participants$Name == "Laura Laviolette", "F&C Background"] <- "Naturalistic"
participants[participants$Name == "Laura Laviolette", "Institutional French"] <- "No"
participants[participants$Name == "Terry Guillot", "Education"] <- "No College"

# Order factors in order to make reasonable reference levels
participants$`Raised (parish)` <- factor(participants$`Raised (parish)`,
                                  levels = c("Lafayette", "St Martin", "Acadia",
                                             "St Landry", "Vermilion", "Avoyelles",
                                             "Calcasieu", "Cameron", "Evangeline",
                                             "Lafourche"))
participants$`Residence (parish)` <- factor(participants$`Residence (parish)`,
                                     levels = c("Lafayette", "St Martin", "Acadia",
                                                "St Landry", "Vermilion", "E Baton Rouge"))
participants$`F&C Background` <- factor(participants$`F&C Background`,
                                 levels = c("Naturalistic", "Personal", "Institutional"))
participants$`Institutional French` <- factor(participants$`Institutional French`,
                                       levels = c("No", "Yes"))
participants$Occupation <- factor(participants$Occupation,
                           levels = c("Blue Collar", "Pink Collar", "White Collar"))
participants$Education <- factor(participants$Education,
                          levels = c("No College", "College Graduate"))
participants$Ethnicity <- factor(participants$Ethnicity,
                          levels = c("Creole", "Cajun"))

# Add a column for age from birth years
participants$Age <- 2023 - participants$`Birth Year`

# Network cleaning -----------------------------------------------------------
# Collapse factor levels when relevant
networks$`Alter Ethnicity` <- recode_factor(
  networks$`Alter Ethnicity`,
  "African-American/Creole" = "Creole",
  "African/Creole" = "Creole",
  "Creole-African-American" = "Creole",
  "Creole/French" = "Creole",
  "White Creole" = "Creole",
  "Americanized Cajun" = "Cajun",
  "Cajun-American" = "Cajun"
)

# Reorder factors for alter ethnicity by most frequent
networks$`Alter Ethnicity` <- fct_infreq(networks$`Alter Ethnicity`)

# Order factor in order to make a reasonable reference level
networks$`Alter French Frequency` <- factor(networks$`Alter French Frequency`,
                                   levels = c("Never", "Occasionally", "Often", "Always"))
