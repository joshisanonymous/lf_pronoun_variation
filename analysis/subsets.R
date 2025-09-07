# Animacy and grammatical gender
subsetsAnimacy <- list(
  thirdSg = subsetTokens("3sg", "animacy"),
  thirdPl = subsetTokens("3pl", "animacy")
)

subsetsGender <- list(
  thirdSg = subsetTokens("3sg", "gender"),
  firstPl = subsetTokens("1pl", "gender"),
  secondPl = subsetTokens("2pl", "gender"),
  thirdPl = subsetTokens("3pl", "gender")
)

subsetsAnimateGender <- list(
  thirdSg = subsetTokens("3sg", "animate gender"),
  thirdPl = subsetTokens("3pl", "animate gender")
)

subsetsInanimateGender <- list(
  thirdSg = subsetTokens("3sg", "inanimate gender"),
  thirdPl = subsetTokens("3pl", "inanimate gender")
)

parishLong <- rbind(
  data.frame(
    "Group" = "Raised",
    table(participants$`Raised (parish)`)
  ),
  data.frame(
    "Group" = "Residence",
    table(participants$`Residence (parish)`)
  )
)
colnames(parishLong) <- c("Group", "Parish", "Count")