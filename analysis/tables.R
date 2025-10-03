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

# All attested variants for each pronoun type
tablesProTypeAll <- list(
  firstSg = tableProType("1sg"),
  secondSgT = tableProType("2sg.T"),
  secondSgV = tableProType("2sg.V"),
  thirdSgF = tableProType("3sg.F"),
  thirdSgM = tableProType("3sg.M"),
  thirdSgN = tableProType("3sg"),
  firstPl = tableProType("1pl"),
  secondPl = tableProType("2pl"),
  thirdPl = tableProType("3pl"),
  demonstrative = tableProType("demo"),
  expletive = tableProType("expl"),
  impersonal = tableProType("imp")
)

# Attested variants for each pronoun type kept for final modeling
tablesProTypeFinal <- list(
  firstSg = tableProType("1sg", subset = "final"),
  secondSgT = tableProType("2sg.T", subset = "final"),
  secondSgV = tableProType("2sg.V", subset = "final"),
  thirdSgF = tableProType("3sg.F", subset = "final"),
  thirdSgM = tableProType("3sg.M", subset = "final"),
  thirdSgN = tableProType("3sg", subset = "final"),
  firstPl = tableProType("1pl", subset = "final"),
  secondPl = tableProType("2pl", subset = "final"),
  thirdPl = tableProType("3pl", subset = "final"),
  demonstrative = tableProType("demo", subset = "final"),
  expletive = tableProType("expl", subset = "final"),
  impersonal = tableProType("imp", subset = "final")
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

tablesInstitutionalFr <- list(
  firstSg = tableSocial("1sg", "Institutional French"),
  secondSgT = tableSocial("2sg.T", "Institutional French"),
  secondSgV = tableSocial("2sg.V", "Institutional French"),
  thirdSgF = tableSocial("3sg.F", "Institutional French"),
  thirdSgM = tableSocial("3sg.M", "Institutional French"),
  firstPl = tableSocial("1pl", "Institutional French"),
  secondPl = tableSocial("2pl", "Institutional French"),
  thirdPl = tableSocial("3pl", "Institutional French"),
  # demonstrative = tableSocial("demo", "Institutional French"),
  expletive = tableSocial("expl", "Institutional French"),
  impersonal = tableSocial("imp", "Institutional French")
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

tablesParticipant <- list(
  firstSg = sapply(as.character(participants$Name), tableParticipant, protype = "1sg", simplify = FALSE),
  secondSgT = sapply(as.character(participants$Name), tableParticipant, protype = "2sg.T", simplify = FALSE),
  secondSgV = sapply(as.character(participants$Name), tableParticipant, protype = "2sg.V", simplify = FALSE),
  thirdSgF = sapply(as.character(participants$Name), tableParticipant, protype = "3sg.F", simplify = FALSE),
  thirdSgM = sapply(as.character(participants$Name), tableParticipant, protype = "3sg.M", simplify = FALSE),
  firstPl = sapply(as.character(participants$Name), tableParticipant, protype = "1pl", simplify = FALSE),
  secondPl = sapply(as.character(participants$Name), tableParticipant, protype = "2pl", simplify = FALSE),
  thirdPl = sapply(as.character(participants$Name), tableParticipant, protype = "3pl", simplify = FALSE),
  # demonstrative = sapply(as.character(participants$Name), tableParticipant, protype = "demo", simplify = FALSE),
  expletive = sapply(as.character(participants$Name), tableParticipant, protype = "expl", simplify = FALSE),
  impersonal = sapply(as.character(participants$Name), tableParticipant, protype = "imp", simplify = FALSE)
)

tablesSocial <- list(
  ethnicityRace = table(participants$Ethnicity, participants$Race),
  ethnicityGend = table(participants$Ethnicity, participants$Gender),
  ethnicityRet = table(participants$Ethnicity, participants$Retired),
  ethnicityOcc = table(participants$Ethnicity, participants$Occupation),
  ethnicityEd = table(participants$Ethnicity, participants$Education),
  ethnicityInst = table(participants$Ethnicity, participants$`Institutional French`),
  ethnicityFC = table(participants$Ethnicity, participants$`F&C Background`),
  raceEthn = table(participants$Race, participants$Ethnicity),
  raceGend = table(participants$Race, participants$Gender),
  raceRet = table(participants$Race, participants$Retired),
  raceOcc = table(participants$Race, participants$Occupation),
  raceEd = table(participants$Race, participants$Education),
  raceInst = table(participants$Race, participants$`Institutional French`),
  raceFC = table(participants$Race, participants$`F&C Background`),
  genderEthn = table(participants$Gender, participants$Ethnicity),
  genderRace = table(participants$Gender, participants$Race),
  genderRet = table(participants$Gender, participants$Retired),
  genderOcc = table(participants$Gender, participants$Occupation),
  genderEd = table(participants$Gender, participants$Education),
  genderInst = table(participants$Gender, participants$`Institutional French`),
  genderFC = table(participants$Gender, participants$`F&C Background`),
  retiredEthn = table(participants$Retired, participants$Ethnicity),
  retiredRace = table(participants$Retired, participants$Race),
  retiredGend = table(participants$Retired, participants$Gender),
  retiredOcc = table(participants$Retired, participants$Occupation),
  retiredEd = table(participants$Retired, participants$Education),
  retiredInst = table(participants$Retired, participants$`Institutional French`),
  retiredFC = table(participants$Retired, participants$`F&C Background`),
  occupationEthn = table(participants$Occupation, participants$Ethnicity),
  occupationRace = table(participants$Occupation, participants$Race),
  occupationGend = table(participants$Occupation, participants$Gender),
  occupationRet = table(participants$Occupation, participants$Retired),
  occupationEd = table(participants$Occupation, participants$Education),
  occupationInst = table(participants$Occupation, participants$`Institutional French`),
  occupationFC = table(participants$Occupation, participants$`F&C Background`),
  educationEthn = table(participants$Education, participants$Ethnicity),
  educationRace = table(participants$Education, participants$Race),
  educationGend = table(participants$Education, participants$Gender),
  educationRet = table(participants$Education, participants$Retired),
  educationOcc = table(participants$Education, participants$Occupation),
  educationInst = table(participants$Education, participants$`Institutional French`),
  educationFC = table(participants$Education, participants$`F&C Background`),
  institutionalEthn = table(participants$`Institutional French`, participants$Ethnicity),
  institutionalRace = table(participants$`Institutional French`, participants$Race),
  institutionalGend = table(participants$`Institutional French`, participants$Gender),
  institutionalRet = table(participants$`Institutional French`, participants$Retired),
  institutionalOcc = table(participants$`Institutional French`, participants$Occupation),
  institutionalEd = table(participants$`Institutional French`, participants$Education),
  institutionalFC = table(participants$`Institutional French`, participants$`F&C Background`),
  backgroundFC = table(participants$`F&C Background`, participants$Ethnicity),
  backgroundRace = table(participants$`F&C Background`, participants$Race),
  backgroundGend = table(participants$`F&C Background`, participants$Gender),
  backgroundRet = table(participants$`F&C Background`, participants$Retired),
  backgroundOcc = table(participants$`F&C Background`, participants$Occupation),
  backgroundEd = table(participants$`F&C Background`, participants$Education),
  backgroundInst = table(participants$`F&C Background`, participants$`Institutional French`)
)

# Exploration of interactions between ethnicity and occupation
tableEthnToOcc <- table(participants$Ethnicity, participants$Occupation)

tableInstToOccEd <- table(
  participants[participants$Occupation == "White Collar" & participants$Education == "College Graduate",
               "Institutional French"]
)

# coreByFrTable <- table(networks$`Alter French Frequency`, networks$`Alter Type`)
# verbCollatesMostFrequent <- table(tokens$PredUnder)
# verbCollatesMostFrequent <- verbCollatesMostFrequent[
#   order(verbCollatesMostFrequent, decreasing = TRUE)
# ]
# thirdSgITable <- table(tokens[tokens$ProType == "3sg.IF" |
#                               tokens$ProType == "3sg.IM",
#                               "ProUnder"])
# thirdPlFTable <- table(tokens[tokens$ProType == "3pl.F",
# "ProUnder"])