# --------- SETUP ---------

# Loading packages
library(tidyverse)
library(readxl)

# --------- IMPORTING AND CLEANING DATA ---------

# Importing data
economic_freedom <- read_excel("EconomicFreedom.xls")
test_positivity <- read_csv("PositivityRates.csv")

# Creating smaller datasets
average_test_positivity <- test_positivity %>% group_by(Entity) %>% mutate(average_positivity_rate = mean(cumulative_positivity_rate)) %>% select(Entity, average_positivity_rate) %>% distinct(.keep_all = TRUE)
pure_freedom <- economic_freedom %>% select(CountryName, WorldRank, FreedomScore)

# Making the data similar before merging
pure_freedom <- subset(pure_freedom, pure_freedom$CountryName %in% pure_freedom$CountryName[pure_freedom$FreedomScore!="N/A"])
pure_freedom <- subset(pure_freedom, pure_freedom$CountryName %in% average_test_positivity$Entity)
average_test_positivity <- subset(average_test_positivity, average_test_positivity$Entity %in% pure_freedom$CountryName)
average_test_positivity <- average_test_positivity %>% rename(CountryName = Entity)

# Ensuring quantitative data is stored as numeric
pure_freedom$WorldRank <- as.numeric(pure_freedom$WorldRank)
pure_freedom$FreedomScore <- as.numeric(pure_freedom$FreedomScore)
average_test_positivity$average_positivity_rate <- as.numeric(average_test_positivity$average_positivity_rate)

# Merging the data
CovidEconomicFreedom <- left_join(pure_freedom, average_test_positivity, by = "CountryName") %>% rename(AverageTPR = average_positivity_rate, EconomicFreedomRank = WorldRank)
head(CovidEconomicFreedom)

# --------- ANALYSIS ---------

# Scatterplot of rank and average TPR
CovidEconomicFreedom %>% 
  ggplot(aes(EconomicFreedomRank, AverageTPR)) +
  geom_point()

# Simple regression
lm(EconomicFreedomRank ~ AverageTPR, data = CovidEconomicFreedom)
CovidEconomicFreedom %>%
  ggplot(aes(EconomicFreedomRank, AverageTPR)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(
    title = "Average TPR and Economic Freedom",
    caption = "Data from Heritage (2021) and Our World in Data (2021)",
    x = "Rank on the Economic Freedom Index (1 is most free)",
    y = "Average Daily Test Positivity Rate by Country"
  )

# Summary statistics
cor(CovidEconomicFreedom$EconomicFreedomRank, CovidEconomicFreedom$AverageTPR)