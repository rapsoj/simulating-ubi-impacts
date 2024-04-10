#### PREPARE WORKSPACE ####

# Load libraries
library(dplyr)


#### SIMULATE TAXES AND BENEFIT DATAFRAME ####

# Create dataframe with gross income levels
df <- data.frame(gross_income = seq(1000, 400000, by = 1000)) %>%
  # Simulate taxes and benefits
  mutate(
    # Simulate personal income tax
    irpef = case_when(
      gross_income > 0 ~ gross_income * 0.23,
      gross_income > 15000 ~ gross_income * 0.27,
      gross_income > 28000 ~ gross_income * 0.38,
      gross_income > 55000 ~ gross_income * 0.41,
      gross_income > 75000 ~ gross_income * 0.42
    )
  )