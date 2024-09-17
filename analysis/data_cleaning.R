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

# Order factor in order to make reasonable reference levels
tokens$ProUnder <- factor(tokens$ProUnder,
                       levels = c("je", "vous", "tu", "on", "ø", "ça", "elle", "il", "vous-autres", "ils",
                                  "elles", "eux", "eux-autres", "nous", "nous-autres", 
                                  "li", "mo", "no", "to", "vou", "yé"))
tokens$PredType <- factor(tokens$PredType,
                           levels = c("lexical", "modal", "auxiliary"))

# Social cleanup --------------------------------------------------------------
participants$Profession <- factor(participants$Profession,
                                  levels = c("Blue Collar", "White Collar"))
participants$Education <- factor(participants$Education,
                                 levels = c("Some School", "High School Graduate", "College Graduate"))
networks$Alter.French.Frequency <- factor(networks$Alter.French.Frequency,
                                          levels = c("Never", "Occasionally", "Often", "Always"))