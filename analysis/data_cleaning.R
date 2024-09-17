# Re-align 2nd interviewee tokens and remove blank rows
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
table(tokens$ProUnder)

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