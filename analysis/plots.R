


# Ethnicity by race
ethByRaceBar <- ggplot(
  melt(ethByRaceTable,
       varnames = c("Ethnicity", "Race"),
       value.name = "Count"),
  aes(
    x = Ethnicity,
    y = Count,
    fill = Race
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

# French usage by alter type (i.e., coreness)
coreByFrBar <- ggplot(
  melt(coreByFrTable,
       varnames = c("French.Frequency", "Alter.Type"),
       value.name = "Count"),
  aes(
    x = Alter.Type,
    y = Count,
    fill = French.Frequency
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

# Comparison of homophily by French usage
homophByLanguage <- ggplot(
  melt(participants,
       id.vars = "Name",
       measure.vars = c("Network.Ethnic.Homophily",
                        "Anglo.Network.Ethnic.Homophily",
                        "Franco.Network.Ethnic.Homophily"),
       variable.name = "Section.of.Network",
       value.name = "Ethnic.Homophily"),
  aes(
    y = Ethnic.Homophily,
    x = Section.of.Network
  )
) +
  geom_boxplot() +
  theme_bw()
