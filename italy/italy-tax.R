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
      gross_income <= 15000 ~ gross_income * 0.23,
      gross_income <= 28000 ~ (15000 * 0.23) + ((gross_income - 15000) * 0.27),
      gross_income <= 55000 ~ (15000 * 0.23) + (13000 * 0.27) + ((gross_income - 28000) * 0.38),
      gross_income <= 75000 ~ (15000 * 0.23) + (13000 * 0.27) + (27000 * 0.38) + ((gross_income - 55000) * 0.41),
      TRUE ~ (15000 * 0.23) + (13000 * 0.27) + (27000 * 0.38) + (20000 * 0.41) + ((gross_income - 75000) * 0.42)
    )
  )