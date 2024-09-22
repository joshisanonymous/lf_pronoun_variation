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
                            "French Background", "Education", "Profession", "Profession (specific)",
                            "Retired", "Gender", "Gender Source", "Race", "Ethnicity",
                            "Transcribed", "Coded", "Anonymized", "Length Completed",
                            "Total Length", "Notes")

# Remove unusable participants
participants <- droplevels(subset(participants, Ethnicity != "French" &
                                                Name != "Latoya Pomier"))

# Collapse factor levels
participants$`French Background` <- recode_factor(
  participants$`French Background`,
  "Naturalistic > Institutional" = "Naturalistic",
  "Naturalistic > Institutional > Personal" = "Naturalistic"
)
participants$Race <- recode_factor(
  participants$Race,
  "Black" = "Singular Black",
  "African-American / Black" = "Singular Black",
  "Black American" = "Singular Black",
  "White" = "Singular White",
  "Caucasian" = "Singular White",
  "Caucasian / White" = "Singular White",
  "Cajun" = "Singular Cajun",
  "Cajun / Acadian" = "Singular Cajun",
  "White (Cajun if it's there)" = "Singular Cajun",
  "Creole" = "Protean Creole",
  "African / Creole" = "Protean Creole",
  "Caucasian / Cajun" = "Protean Cajun",
  "Paraphrase: Whatever they call me" = "Transcendent"
)

# Order factors in order to make reasonable reference levels
participants$`Raised (parish)` <- factor(participants$`Raised (parish)`,
                                  levels = c("Lafayette", "St Martin", "Acadia",
                                             "St Landry", "Vermilion", "Avoyelles",
                                             "Calcasieu", "Cameron", "Evangeline",
                                             "Lafourche"))
participants$`Residence (parish)` <- factor(participants$`Residence (parish)`,
                                     levels = c("Lafayette", "St Martin", "Acadia",
                                                "St Landry", "Vermilion", "East Baton Rouge"))
participants$Profession <- factor(participants$Profession,
                           levels = c("Blue Collar", "White Collar"))
participants$Education <- factor(participants$Education,
                          levels = c("No College", "Some College", "College Graduate"))
participants$Ethnicity <- factor(participants$Ethnicity,
                          levels = c("Creole", "Cajun"))

# Network cleaning -----------------------------------------------------------
# Collapse factor levels when relevant
networks$Alter.Ethnicity <- recode_factor(
  networks$Alter.Ethnicity,
  "African/Creole" = "Creole"
)

# Order factor in order to make a reasonable reference level
networks$Alter.French.Frequency <- factor(networks$Alter.French.Frequency,
                                   levels = c("Never", "Occasionally", "Often", "Always"))