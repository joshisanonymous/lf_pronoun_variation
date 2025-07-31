# Animacy and gender checks ----------------------------------------------------
plotsAnimacy <- list(
  thirdSg = plotPronoun(subsetsAnimacy$thirdSg),
  thirdPl = plotPronoun(subsetsAnimacy$thirdPl)
)

plotsGGender <- list(
  thirdSg = plotPronoun(subsetsGender$thirdSg),
  firstPl = plotPronoun(subsetsGender$firstPl, rotate_labels = TRUE),
  secondPl = plotPronoun(subsetsGender$secondPl),
  thirdPl = plotPronoun(subsetsGender$thirdPl)
)

plotsAnimateGender <- list(
  thirdSg = plotPronoun(subsetsAnimateGender$thirdSg),
  thirdPl = plotPronoun(subsetsAnimateGender$thirdPl)
)

plotsInanimateGender <- list(
  thirdSg = plotPronoun(subsetsInanimateGender$thirdSg),
  thirdPl = plotPronoun(subsetsInanimateGender$thirdPl)
)

# Combined plots ---------------------------------------------------------------

plotCombined1pl2plGender <- ggarrange(
  plotsGGender$firstPl, plotsGGender$secondPl,
  nrow = 2, draw = FALSE, newpage = FALSE
)

plotCombined3sgAnimateGender <- ggarrange(
  plotsGGender$thirdSg, plotsAnimateGender$thirdSg, plotsInanimateGender$thirdSg,
  nrow = 3, draw = FALSE, newpage = FALSE
)

plotCombined3plAnimateGender <- ggarrange(
  plotsGGender$thirdPl, plotsAnimateGender$thirdPl, plotsInanimateGender$thirdPl,
  nrow = 3, draw = FALSE, newpage = FALSE
)
