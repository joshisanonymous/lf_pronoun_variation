# Create generic tables
ethByRaceTable <- table(participants$Ethnicity, participants$Race)

# Phonetic realizations by underlying pronoun
tablesPhonetic <- list(
  je = tablePhonetic("je"),
  mo = tablePhonetic("mo"),
  tu = tablePhonetic("tu"),
  to = tablePhonetic("to"),
  vous = tablePhonetic("vous"),
  elle = tablePhonetic("elle"),
  il = tablePhonetic("il"),
  li = tablePhonetic("li"),
  ca = tablePhonetic("ça"),
  empty = tablePhonetic("ø"),
  nous = tablePhonetic("nous"),
  on = tablePhonetic("on"),
  nousautres = tablePhonetic("nous-autres"),
  no = tablePhonetic("no"),
  vousautres = tablePhonetic("vous-autres"),
  vou = tablePhonetic("vou"),
  elles = tablePhonetic("elles"),
  ils = tablePhonetic("ils"),
  eux = tablePhonetic("eux"),
  euxautres = tablePhonetic("eux-autres"),
  ye = tablePhonetic("yé")
)

# Structural pronoun tables
tablesPredicate <- list(
  firstSg = tableSocial("1sg", "PredType"),
  secondSgT = tableSocial("2sg.T", "PredType"),
  secondSgV = tableSocial("2sg.V", "PredType"),
  thirdSgF = tableSocial("3sg.F", "PredType"),
  thirdSgM = tableSocial("3sg.M", "PredType"),
  firstPl = tableSocial("1pl", "PredType"),
  secondPl = tableSocial("2pl", "PredType"),
  thirdPl = tableSocial("3pl", "PredType"),
  # demonstrative = tableSocial("demo", "PredType"),
  expletive = tableSocial("expl", "PredType"),
  impersonal = tableSocial("imp", "PredType")
)

# Social pronoun tables
tablesEthnicity <- list(
  firstSg = tableSocial("1sg", "Ethnicity"),
  secondSgT = tableSocial("2sg.T", "Ethnicity"),
  secondSgV = tableSocial("2sg.V", "Ethnicity"),
  thirdSgF = tableSocial("3sg.F", "Ethnicity"),
  thirdSgM = tableSocial("3sg.M", "Ethnicity"),
  firstPl = tableSocial("1pl", "Ethnicity"),
  secondPl = tableSocial("2pl", "Ethnicity"),
  thirdPl = tableSocial("3pl", "Ethnicity"),
  # demonstrative = tableSocial("demo", "Ethnicity"),
  expletive = tableSocial("expl", "Ethnicity"),
  impersonal = tableSocial("imp", "Ethnicity")
)

tablesRace <- list(
  firstSg = tableSocial("1sg", "Race"),
  secondSgT = tableSocial("2sg.T", "Race"),
  secondSgV = tableSocial("2sg.V", "Race"),
  thirdSgF = tableSocial("3sg.F", "Race"),
  thirdSgM = tableSocial("3sg.M", "Race"),
  firstPl = tableSocial("1pl", "Race"),
  secondPl = tableSocial("2pl", "Race"),
  thirdPl = tableSocial("3pl", "Race"),
  # demonstrative = tableSocial("demo", "Race"),
  expletive = tableSocial("expl", "Race"),
  impersonal = tableSocial("imp", "Race")
)

tablesGender <- list(
  firstSg = tableSocial("1sg", "Gender"),
  secondSgT = tableSocial("2sg.T", "Gender"),
  secondSgV = tableSocial("2sg.V", "Gender"),
  thirdSgF = tableSocial("3sg.F", "Gender"),
  thirdSgM = tableSocial("3sg.M", "Gender"),
  firstPl = tableSocial("1pl", "Gender"),
  secondPl = tableSocial("2pl", "Gender"),
  thirdPl = tableSocial("3pl", "Gender"),
  # demonstrative = tableSocial("demo", "Gender"),
  expletive = tableSocial("expl", "Gender"),
  impersonal = tableSocial("imp", "Gender")
)

tablesEducation <- list(
  firstSg = tableSocial("1sg", "Education"),
  secondSgT = tableSocial("2sg.T", "Education"),
  secondSgV = tableSocial("2sg.V", "Education"),
  thirdSgF = tableSocial("3sg.F", "Education"),
  thirdSgM = tableSocial("3sg.M", "Education"),
  firstPl = tableSocial("1pl", "Education"),
  secondPl = tableSocial("2pl", "Education"),
  thirdPl = tableSocial("3pl", "Education"),
  # demonstrative = tableSocial("demo", "Education"),
  expletive = tableSocial("expl", "Education"),
  impersonal = tableSocial("imp", "Education")
)

tablesOccupation <- list(
  firstSg = tableSocial("1sg", "Occupation"),
  secondSgT = tableSocial("2sg.T", "Occupation"),
  secondSgV = tableSocial("2sg.V", "Occupation"),
  thirdSgF = tableSocial("3sg.F", "Occupation"),
  thirdSgM = tableSocial("3sg.M", "Occupation"),
  firstPl = tableSocial("1pl", "Occupation"),
  secondPl = tableSocial("2pl", "Occupation"),
  thirdPl = tableSocial("3pl", "Occupation"),
  # demonstrative = tableSocial("demo", "Occupation"),
  expletive = tableSocial("expl", "Occupation"),
  impersonal = tableSocial("imp", "Occupation")
)

tablesRaised <- list(
  firstSg = tableSocial("1sg", "Raised (parish)"),
  secondSgT = tableSocial("2sg.T", "Raised (parish)"),
  secondSgV = tableSocial("2sg.V", "Raised (parish)"),
  thirdSgF = tableSocial("3sg.F", "Raised (parish)"),
  thirdSgM = tableSocial("3sg.M", "Raised (parish)"),
  firstPl = tableSocial("1pl", "Raised (parish)"),
  secondPl = tableSocial("2pl", "Raised (parish)"),
  thirdPl = tableSocial("3pl", "Raised (parish)"),
  # demonstrative = tableSocial("demo", "Raised (parish)"),
  expletive = tableSocial("expl", "Raised (parish)"),
  impersonal = tableSocial("imp", "Raised (parish)")
)

tablesResidence <- list(
  firstSg = tableSocial("1sg", "Residence (parish)"),
  secondSgT = tableSocial("2sg.T", "Residence (parish)"),
  secondSgV = tableSocial("2sg.V", "Residence (parish)"),
  thirdSgF = tableSocial("3sg.F", "Residence (parish)"),
  thirdSgM = tableSocial("3sg.M", "Residence (parish)"),
  firstPl = tableSocial("1pl", "Residence (parish)"),
  secondPl = tableSocial("2pl", "Residence (parish)"),
  thirdPl = tableSocial("3pl", "Residence (parish)"),
  # demonstrative = tableSocial("demo", "Residence (parish)"),
  expletive = tableSocial("expl", "Residence (parish)"),
  impersonal = tableSocial("imp", "Residence (parish)")
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