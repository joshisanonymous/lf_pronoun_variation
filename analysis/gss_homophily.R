library(gssr)
library(haven)

## Data ----------------
gss <- gss_get_yr(2004)

# Louisiana + Arkansas, Texas, Oklahoma
gss <- gss[gss$region_7222 == 7 & (gss$numgiven >= 1 & !is.na(gss$numgiven)) &
             (gss$racecen1 == 1 | gss$racecen1 == 2) & !is.na(gss$race1),
           c("id", "racecen1", "race1", "race2", "race3", "race4", "race5")]

# Replace codes with factor values
gss$id <- as.factor(gss$id)
gss$racecen1 <- factor(gss$racecen1, labels = c("White", "Black"))
gss$race1 <- factor(gss$race1, labels = c("Black", "Hispanic", "White", "Other"))
gss$race2 <- factor(gss$race2, labels = c("Black", "Hispanic", "White"))
gss$race3 <- factor(gss$race3, labels = c("Black", "Hispanic", "White"))
gss$race4 <- factor(gss$race4, labels = c("Asian", "Black", "Hispanic", "White"))
gss$race5 <- factor(gss$race5, labels = c("Black", "Hispanic", "White"))

# Extract GSS IDs
gssNames <- data.frame(
  "Name" = factor(gss$id),
  "Ethnicity" = factor(gss$racecen1, levels = c("Black", "White"))
)

# Put in long form
gss <- melt(gss, id.vars = c("id", "racecen1"), na.rm = TRUE)
gss$value <- as.factor(gss$value)
colnames(gss) <- c("Name", "Ethnicity", "Alter", "Alter Ethnicity")

## Get EI homophily -----------------------
gssNames$`Network Ethnic Homophily` <- sapply(gssNames$Name, getEIHomophily, df = gss, gss_data = TRUE)
