library(igraph)

# Create data frame with attributes for all network nodes including alters
networkWholeNodeAttrs <- data.frame(
  "Name" = unique(networks$Alter),
  "Relation to Study" = "Alter",
  check.names = FALSE
)

networkWholeNodeAttrs <- merge(
  networkWholeNodeAttrs,
  networks[, c("Alter", "Alter Ethnicity")],
  by.x = "Name", by.y = "Alter"
)

colnames(networkWholeNodeAttrs)[
  colnames(networkWholeNodeAttrs) == "Alter Ethnicity"
  ] <- "Ethnicity"

networkWholeNodeAttrs <- subset(networkWholeNodeAttrs, !(Name %in% participants$Name))

networkWholeNodeAttrs <- networkWholeNodeAttrs[!(networkWholeNodeAttrs$Name == "Bruce Rivero" &
                         networkWholeNodeAttrs$Ethnicity == "No Answer"),]
networkWholeNodeAttrs <- networkWholeNodeAttrs[!(networkWholeNodeAttrs$Name == "David Roche" &
                         networkWholeNodeAttrs$Ethnicity == "African-American"),]
networkWholeNodeAttrs <- networkWholeNodeAttrs[!(networkWholeNodeAttrs$Name == "Jacqueline Newchurch" &
                         networkWholeNodeAttrs$Ethnicity == "White"),]
networkWholeNodeAttrs <- networkWholeNodeAttrs[!(networkWholeNodeAttrs$Name == "Richard Chenevert" &
                         networkWholeNodeAttrs$Ethnicity == "White"),]
networkWholeNodeAttrs <- networkWholeNodeAttrs[!duplicated(networkWholeNodeAttrs$Name),]

networkWholeNodeAttrs <- rbind(
  networkWholeNodeAttrs,
  merge(data.frame(
    "Name" = unique(networks$Name),
    "Relation to Study" = "Participant",
    check.names = FALSE),
    participants[, c("Name", "Ethnicity")], by = "Name"
  )
)

# Shared alters to see disagreements in ethnic categorization of alters
networkSharedAlters <- filter(add_count(networks, Alter), n > 1)
# networkSharedAlters[order(networkSharedAlters$Alter), c(1, 2, 4)]

# Visualize whole network
networkWhole <- graph_from_data_frame(networks[, c("Name", "Alter")],
                                      vertices = networkWholeNodeAttrs)
networkWholeColors <- c("Creole" = color_key[1], "Cajun" = color_key[2])
V(networkWhole)$color <- "darkgrey"
V(networkWhole)$color[V(networkWhole)$Ethnicity %in% names(networkWholeColors)] <- networkWholeColors[V(networkWhole)$Ethnicity[V(networkWhole)$Ethnicity %in% names(networkWholeColors)]]

set.seed(0321)
networkWholeLayout <- layout_with_fr(networkWhole)
# networkWholeLayout <- norm_coords(networkWholeLayout,
#                                   ymin = -1, ymax = 1,
#                                   xmin = -1, xmax = 1)

networkWholePlot <- function() {
  par(mar = c(1, 1, 1, 1))
  plot(networkWhole,
       vertex.label = ifelse(V(networkWhole)$`Relation to Study` == "Participant",
                             V(networkWhole)$name, NA),
       vertex.label.dist = 1.25, vertex.label.cex = 0.85,
       vertex.label.color = "black",
       vertex.size = ifelse(degree(networkWhole, mode = "in") > 1, degree(networkWhole, mode = "in") * 2, 4),
       vertex.color = V(networkWhole)$color,
       edge.arrow.size = 0.1,
       layout = networkWholeLayout * 1)
  legend("topright", legend = c("Creole", "Cajun", "Other"), pch = 19, pt.cex = 2,
       col = c(color_key[1:2], "darkgrey"), bty = "n", cex = 0.9, title = "Ethnicity")
}