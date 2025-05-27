tokens3plAnim <- tokens[grep("3pl", tokens$ProType),]
tokens3plAnim$ProType <- recode_factor(
  tokens3plAnim$ProType,
  "3pl.AF" = "3pl.A",
  "3pl.AM" = "3pl.A",
  "3pl.AMF" = "3pl.A",
  "3pl.IF" = "3pl.I",
  "3pl.IM" = "3pl.I",
  "3pl.IMF" = "3pl.I",
)

tokens3sgAnim <- tokens[grep("3sg", tokens$ProType),]
tokens3sgAnim$ProType <- recode_factor(
  tokens3sgAnim$ProType,
  "3sg.AF" = "3sg.A",
  "3sg.AM" = "3sg.A",
  "3sg.IF" = "3sg.I",
  "3sg.IM" = "3sg.I"
)

tokens1plGender <- tokens[grep("1pl.(M|F|MF)", tokens$ProType),]

tokens2plGender <- tokens[grep("2pl.(M|F|MF)", tokens$ProType),]

tokens3sgGender <- tokens[grep("3sg.(A|I)(M|F)", tokens$ProType),]
tokens3sgGender$ProType <- recode_factor(
  tokens3sgGender$ProType,
  "3sg.AF" = "3sg.F",
  "3sg.AM" = "3sg.M",
  "3sg.IF" = "3sg.F",
  "3sg.IM" = "3sg.M"
)

tokens3sgAnimGender <- tokens[grep("3sg.A(M|F)", tokens$ProType),]

tokens3sgInanimGender <- tokens[grep("3sg.I(M|F)", tokens$ProType),]

tokens3plGender <- tokens[grep("3pl.(A|I)(M|F|MF)", tokens$ProType),]
tokens3plGender$ProType <- recode_factor(
  tokens3plGender$ProType,
  "3pl.AF" = "3pl.F",
  "3pl.AM" = "3pl.M",
  "3pl.AMF" = "3pl.MF",
  "3pl.IF" = "3pl.F",
  "3pl.IM" = "3pl.M",
  "3pl.IMF" = "3pl.MF"
)

tokens3plAnimGender <- tokens[grep("3pl.A(M|F|MF)", tokens$ProType),]

tokens3plInanimGender <- tokens[grep("3pl.I(M|F|MF)", tokens$ProType),]

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