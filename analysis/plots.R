# 3pl checks ------------------------------------------------------------------
# Check for the importance of animacy for 3pl
graph3plAnim <- ggplot(tokens3plAnim,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw()

# Check for the importance of gender for 3pl
graph3plGender <- ggplot(tokens3plGender,
       aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw()

# Ethnicity by race -----------------------------------------------------------
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
  theme(text=element_text(size=20), legend.text = element_markdown())

# Translation
ethByRaceBarFR <- ethByRaceBar +
  scale_fill_manual(values = color_key,
                    labels = c("Noir.e sg", "Créole sg", "Créole pr", "Transcendant.e",
                               "**Blanc.he sg**", "Cadien.ne sg", "Cadien.ne pr")) +
  labs(x = "Ethnicité", y = "Compte") +
  scale_x_discrete(labels = c("Créole", "Cadien.ne"))

# Race graphs ------------------------------------------------------------------
pro3plByRaceBar <- ggplot(
  melt(pro3plByRaceTable,
       varnames = c("Race", "thirdPl"),
       value.name = "Count"),
  aes(x = thirdPl, y = Count, fill = thirdPl)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ Race, nrow = 3, labeller = as_labeller(c(
    `Singular Black` = "Noir.e sg",
    `Singular Creole` = "Créole sg",
    `Protean Creole` = "Créole pr",
    `Transcendent` = "Transcendant.e",
    `Singular White` = "**Blanc.he sg**",
    `Singular Cajun` = "Cadien.ne sg",
    `Protean Cajun` = "Cadien.ne pr"))) +
  theme(text=element_text(size=15), legend.text = element_markdown(),
        legend.position = "none") +
  scale_fill_manual(values = color_key[5:7]) +
  labs(x = "Pronom", y = "Compte")

# Network graphs --------------------------------------------------------------
# # French usage by alter type (i.e., coreness)
# coreByFrBar <- ggplot(
#   melt(coreByFrTable,
#        varnames = c("French.Frequency", "Alter.Type"),
#        value.name = "Count"),
#   aes(
#     x = Alter.Type,
#     y = Count,
#     fill = French.Frequency
#   )
# ) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_bw()
# 
# # Comparison of homophily by French usage
# homophByLanguage <- ggplot(
#   melt(participants,
#        id.vars = "Name",
#        measure.vars = c("Network.Ethnic.Homophily",
#                         "Anglo.Network.Ethnic.Homophily",
#                         "Franco.Network.Ethnic.Homophily"),
#        variable.name = "Section.of.Network",
#        value.name = "Ethnic.Homophily"),
#   aes(
#     y = Ethnic.Homophily,
#     x = Section.of.Network
#   )
# ) +
#   geom_boxplot() +
#   theme_bw()
