# Ethnicity and race -----------------------------------------------------------
raceBar <- ggplot(
  participants,
  aes(x = Race)
) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
  labs(y = "Count")

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

# Social pronoun graphs --------------------------------------------------------
plotsGender <- lapply(tablesGender, socialPlot)
plotsEducation <- lapply(tablesEducation, socialPlot)
plotsOccupation <- lapply(tablesOccupation, socialPlot)
plotsRaised <- lapply(tablesRaised, socialPlot)
plotsResidence <- lapply(tablesResidence, socialPlot)
plotsEthnicity <- lapply(tablesEthnicity, socialPlot)
plotsRace <- lapply(tablesRace, socialPlot)

# Location graph ---------------------------------------------------------------
parishBar <- ggplot(
  parishLong,
  aes(x = Parish, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  scale_fill_manual(values = c(color_key[3], color_key[1])) +
  labs(fill = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05))

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

# Combined plots --------------------------------------------------------------
parishBarMap <- ggarrange(
  ggarrange(mapraisedonly, mapresidenceonly, ncol = 2),
  parishBar, nrow = 2
)

plotsGenderCombined <- ggarrange(
  plots = plotsGender, ncol = 2
)

plotsEducationCombined <- ggarrange(
  plots = plotsEducation, ncol = 2
)

plotsOccupationCombined <- ggarrange(
  plots = plotsOccupation, ncol = 2
)

plotsEthnicityCombined <- ggarrange(
  plots = plotsEthnicity, ncol = 2
)

plotsRaceCombined <- ggarrange(
  plots = plotsRace, ncol = 1
)

plotsRaisedCombined <- ggarrange(
  plots = plotsRaised, ncol = 1
)

plotsResidenceCombined <- ggarrange(
  plots = plotsResidence, ncol = 1
)