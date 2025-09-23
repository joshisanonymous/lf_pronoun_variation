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

plotEduInst <- ggplot(melt(tablesSocial$educationInst,
                           varnames = c("Insitutional", "Education"),
                           value.name = "Count"),
                      aes(x = Education, y = Count, fill = Education)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ Insitutional) +
  theme(text=element_text(size=15), legend.text = element_markdown(),
        legend.position = "none") +
  scale_fill_manual(values = color_key) +
  labs(x = "Insitutional French Instruction", y = "Count")

# Translation
ethByRaceBarFR <- ethByRaceBar +
  scale_fill_manual(values = color_key,
                    labels = c("Noir.e sg", "Créole sg", "Créole pr", "Transcendant.e",
                               "**Blanc.he sg**", "Cadien.ne sg", "Cadien.ne pr")) +
  labs(x = "Ethnicité", y = "Compte") +
  scale_x_discrete(labels = c("Créole", "Cadien.ne"))

# Pronoun graphs --------------------------------------------------------
plotsPredicate <- lapply(tablesPredicate, plotSocial)
plotsGender <- lapply(tablesGender, plotSocial)
plotsEducation <- lapply(tablesEducation, plotSocial)
plotsOccupation <- lapply(tablesOccupation, plotSocial)
plotsInstitutionalFr <- lapply(tablesInstitutionalFr, plotSocial)
plotsRaised <- lapply(tablesRaised, plotSocial)
plotsResidence <- lapply(tablesResidence, plotSocial)
plotsEthnicity <- lapply(tablesEthnicity, plotSocial)
plotsRace <- lapply(tablesRace, plotSocial)
plotsEthOcc <- lapply(subsetsProType, plotEthOcc)
plotsEduOcc <- lapply(subsetsProType, plotEduOcc)

plotThirdPlCollapsedGender <- ggplot(melt(table(subsetThirdPlCollapsed$Gender,
                                                subsetThirdPlCollapsed$ProUnder),
              varnames = c("socialVar", "pronoun"),
              value.name = "Count"),
         aes(x = pronoun, y = Count, fill = pronoun)) +
    geom_bar(stat = "identity") +
    facet_wrap(. ~ socialVar) +
    theme(text=element_text(size=15), legend.text = element_markdown(),
          legend.position = "none") +
    scale_fill_manual(values = color_key) +
    labs(x = "Pronoun", y = "Count")

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
parishBarMap <- ggpubr::ggarrange(
  ggpubr::ggarrange(mapraisedonly, mapresidenceonly, ncol = 2),
  parishBar, nrow = 2
)

plotsGenderCombined <- ggarrange(
  plots = plotsGender, ncol = 2,
  draw = FALSE, newpage = FALSE
)

plotsEducationCombined <- ggarrange(
  plots = plotsEducation, ncol = 2,
  draw = FALSE, newpage = FALSE
)

plotsOccupationCombined <- ggarrange(
  plots = plotsOccupation, ncol = 2,
  draw = FALSE, newpage = FALSE
)

plotsEthnicityCombined <- ggarrange(
  plots = plotsEthnicity, ncol = 2,
  draw = FALSE, newpage = FALSE
)

plotsRaceCombined <- ggarrange(
  plots = plotsRace, ncol = 1,
  draw = FALSE, newpage = FALSE
)

plotsRaisedCombined <- ggarrange(
  plots = plotsRaised, ncol = 1,
  draw = FALSE, newpage = FALSE
)

plotsResidenceCombined <- ggarrange(
  plots = plotsResidence, ncol = 1,
  draw = FALSE, newpage = FALSE
)