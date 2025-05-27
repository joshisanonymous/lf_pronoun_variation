# Animacy and gender checks ----------------------------------------------------
graph3sgAnim <- ggplot(
  tokens3sgAnim,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3plAnim <- ggplot(tokens3plAnim,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph1plGender <- ggplot(
  tokens1plGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph2plGender <- ggplot(
  tokens2plGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3sgGender <- ggplot(
  tokens3sgGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3sgAnimGender <- ggplot(
  tokens3sgAnimGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3sgInanimGender <- ggplot(
  tokens3sgInanimGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3plGender <- ggplot(
  tokens3plGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3plAnimGender <- ggplot(
  tokens3plAnimGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

graph3plInanimGender <- ggplot(
  tokens3plInanimGender,
  aes(x = ProUnder)) +
  geom_bar() +
  facet_wrap(. ~ ProType) +
  theme_bw() +
  labs(x = "Pronoun", y = "Count")

# Count graphs -----------------------------------------------------------------
# ggplot(
#   tokens, aes(x = ProUnder)
# ) +
#   geom_bar() +
#   facet_grid(Gender ~ ProType)

# Combined plots ---------------------------------------------------------------

gender1pl2pl <- ggarrange(
  graph1plGender, graph2plGender,
  nrow = 2
)

animGender3sg <- ggarrange(
  graph3sgAnim, graph3sgGender, graph3sgAnimGender, graph3sgInanimGender,
  nrow = 4
)

animGender3pl <- ggarrange(
  graph3plAnim, graph3plGender, graph3plAnimGender, graph3plInanimGender,
  nrow = 4
)