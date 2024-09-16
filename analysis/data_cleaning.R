# Re-align 2nd interviewee tokens


# Collapse factor levels


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