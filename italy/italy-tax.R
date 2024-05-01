#### PREPARE WORKSPACE ####

# Load libraries
library(dplyr)

# Read in supplementary data
consumption_df <- read.csv('suppplementary-data/household-consumption.csv') %>%
  # Filter to national level
  filter(REF_AREA == 'IT') %>%
  # Filter to specific expenses
  filter(nchar(COICOP_2018) == 2) %>%
  # Select columns of interest
  select(COICOP_2018, TIME_PERIOD, NUMBER_HOUSEHOLD_COMP, OBS_VALUE) %>%
  # Calculate average value-added tax paid
  mutate(
    # Label with value-added tax rate
    vat_rate = case_when(
      COICOP_2018  %in% c('08', '10', '12') ~ 0.00,
      COICOP_2018 %in% c('01', '06', '09') ~ 0.04,
      COICOP_2018 %in% c('04', '11') ~ 0.10,
      TRUE ~ 0.22
    ),
    # Calculate annual value-added tax paid
    vat_paid = OBS_VALUE * vat_rate * 12
  ) %>%
  # Group by household size
  group_by(NUMBER_HOUSEHOLD_COMP) %>%
  # Sum total value-added tax paid
  summarize(total_vat = sum(vat_paid))


#### SIMULATE TAXES AND BENEFITS ####

# Define demographic variables #Simplify this by only having option for 'dependent' partners, remove incomes and disability
employment_var <- 'employee' #c('employee', 'executive', 'unemployed')
martial_var <- 'single' #c('single', 'married')
spouse_income_var <- 0 #seq(0, 400000, 1000) # Only show if married
children_under1_var <- 0 #c(0:5) # Maximum five total children for simulations
children_1to18_var <- 0 #c(0:5) # Maximum five total children for simulations
children_18to21_var <- 0 #c(0:5) # Maximum five total children for simulations
children_21to24_var <- 0 #c(0:5) # Maximum five total children for simulations
children_over24_var <- 0 #c(0:5) # Maximum five total children for simulations
children_under18_disabled_var <- c() #c(0:5) # Only show if children under 18, write as list
children_18to21_disabled_var <- c() #c(0:5) # Only show if children 18-21, write as list
children_21to24_disabled_var <- c() #c(0:5) # Only show if children 21-24, write as list
children_over24_disabled_var <- c() #c(0:5) # Only show if children over 24, write as list
children_18to21_income_var <- c() #seq(0, 400000, 1000) # Only show if children 18-21, write as list
children_21to24_income_var <- c() #seq(0, 400000, 1000) # Only show if children 21-24, write as list
children_over24_income_var <- c() #seq(0, 400000, 1000) # Only show if children over 24, write as list

# Create dataframe with gross income levels
df <- data.frame(gross_income = seq(1000, 400000, by = 1000)) %>%
  # Simulate taxes and benefits
  mutate(
    
    # Create demographic columns
    employment = employment_var,
    marital = martial_var,
    spouse_income = spouse_income_var,
    children_under1 = children_under1_var,
    children_1to18 = children_1to18_var,
    children_18to21 = children_18to21_var,
    children_21to24 = children_21to24_var,
    children_over24 = children_over24_var,
    children_under18_disabled = paste(children_under18_disabled_var, collapse = ", "),
    children_18to21_disabled = paste(children_18to21_disabled_var, collapse = ", "),
    children_21to24_disabled = paste(children_21to24_disabled_var, collapse = ", "),
    children_over24_disabled = paste(children_over24_disabled_var, collapse = ", "),
    children_18to21_income = paste(children_18to21_income_var, collapse = ", "),
    children_21to24_income = paste(children_21to24_income_var, collapse = ", "),
    children_over24_income = paste(children_over24_income_var, collapse = ", "),
    
    # Calculate household income
    household_income = gross_income + spouse_income,
    # Calculate household size
    household_size = 1 + ifelse(marital == 'married', 1, 0) +
      children_under1 + children_1to18 + children_18to21 + children_21to24 + children_over24,
    
    # Simulate personal income tax
    irpef = case_when(
      gross_income <= 15000 ~ gross_income * 0.23,
      gross_income <= 28000 ~ (15000 * 0.23) + ((gross_income - 15000) * 0.27),
      gross_income <= 55000 ~ (15000 * 0.23) + (13000 * 0.27) + ((gross_income - 28000) * 0.38),
      gross_income <= 75000 ~ (15000 * 0.23) + (13000 * 0.27) + (27000 * 0.38) + ((gross_income - 55000) * 0.41),
      TRUE ~ (15000 * 0.23) + (13000 * 0.27) + (27000 * 0.38) + (20000 * 0.41) + ((gross_income - 75000) * 0.42)
    ),
    # Simulate personal income tax low-income exemptions
    irpef = case_when(
      (employment_var == 'employee') & (gross_income <= 8000) ~ 0,
      (employment_var != 'employee') & (gross_income <= 4800) ~ 0,
      TRUE ~ irpef
    ),
    # Simulate personal income tax dependent spouse deductions
    irpef = case_when(
      (gross_income > spouse_income) & (martial_var == 'married') & (spouse_income <= 2840.51) & (household_income <= 15000) ~
        irpef - 800 - 110 * (household_income) / 15000,
      (gross_income > spouse_income) & (martial_var == 'married') & (spouse_income <= 2840.51) & (household_income <= 40000) ~
        irpef - 690 + case_when(
          (gross_income > spouse_income) & (household_income >= 29000) & (household_income <= 29199) ~ 10,
          (gross_income > spouse_income) & (household_income >= 29200) & (household_income <= 34699) ~ 20,
          (gross_income > spouse_income) & (household_income >= 34700) & (household_income <= 34999) ~ 30,
          (gross_income > spouse_income) & (household_income >= 35000) & (household_income <= 35099) ~ 20,
          (gross_income > spouse_income) & (household_income >= 35100) & (household_income <= 35199) ~ 10,
        ),
      (gross_income > spouse_income) & (martial_var == 'married') & (spouse_income <= 2840.51) & (household_income <= 80000) ~
        irpef - 690 * (80000 - household_income) / 40000,
      TRUE ~ irpef
    ),
    # Simulate personal income tax dependent children 21 to 24 deductions
    irpef = {
      deduction <- numeric(nrow(df))
      for (i in seq_along(children_21to24_income_var)) {
        deduction <- case_when(
          (gross_income > spouse_income) & (children_21to24_income_var[i] < 4000 & children_21to24_disabled_var[i] == 0) ~ irpef - 950,
          (gross_income > spouse_income) & (children_21to24_income_var[i] < 4000 & children_21to24_disabled_var[i] == 1) ~ irpef - 1350,
          TRUE ~ irpef
        )
      }
      deduction
    },
    # Simulate personal income tax dependent children 21 to 24 deductions
    irpef = {
      deduction <- numeric(nrow(df))
      for (i in seq_along(children_over24_income_var)) {
        deduction <- case_when(
          (gross_income > spouse_income) & (children_over24_income_var[i] < 2840.51 & children_over24_disabled_var[i] == 0) ~ irpef - 950,
          (gross_income > spouse_income) & (children_over24_income_var[i] < 2840.51 & children_over24_disabled_var[i] == 1) ~ irpef - 1350,
          TRUE ~ irpef
        )
      }
      deduction
    },
    
    # Simulate value-added tax
    iva = case_when(
      household_size == 1 ~ consumption_df[consumption_df$NUMBER_HOUSEHOLD_COMP == 'N1',]$total_vat,
      household_size == 2 ~ consumption_df[consumption_df$NUMBER_HOUSEHOLD_COMP == 'N2',]$total_vat,
      household_size == 3 ~ consumption_df[consumption_df$NUMBER_HOUSEHOLD_COMP == 'N3',]$total_vat,
      household_size == 4 ~ consumption_df[consumption_df$NUMBER_HOUSEHOLD_COMP == 'N4',]$total_vat,
      TRUE ~ consumption_df[consumption_df$NUMBER_HOUSEHOLD_COMP == 'N5_GE',]$total_vat
    ),
    
    # Simulate Italian social security administration benefit
    inps = case_when(
      employment == 'employee' ~ gross_income * 0.4,
      (employment == 'executive') & (gross_income <= 55008) ~ gross_income * 0.0919,
      (employment == 'executive') & (gross_income > 55008) ~ 55008 * 0.0919 + (gross_income - 55008) * 0.1019,
      TRUE ~ 0
    ),
    
    # Simulate Mario Negri Fund contribution
    fmn = case_when(
      (employment == 'executive') & (gross_income <= 59224.54) ~ gross_income * 0.01 + 130.00,
      (employment == 'executive') & (gross_income > 59224.54) ~ 59224.54 * 0.01 + 130.00,
      TRUE ~ 0
    ),
    
    # Simulate Mario Besusso Fund contribution
    fmb = case_when(
      (employment == 'executive') & (gross_income <= 45940) ~ gross_income * 0.0187,
      (employment == 'executive') & (gross_income > 45940) ~ 45940 * 0.0187,
      TRUE ~ 0
    ),
    
    # Simulate Shepherd Fund contribution
    fp = ifelse(employment == 'executive', 464.81, 0),
    
    # Simulate healthcare industry fund contribution
    fasi = ifelse(employment == 'executive', 1120, 0),
    
    # Simulate industrial company managers' pension fund contribution
    previndai = case_when(
      (employment == 'executive') & (gross_income <= 180000) ~ gross_income * 0.04,
      (employment == 'executive') & (gross_income > 180000) ~ 180000 * 0.04,
      TRUE ~ 0
    ),
    
    # Simulate single and universal allowance for dependent children
    auufc = case_when(
      (children_under1 + children_1to18 + children_18to21 < 3) | (gross_income > 40000) ~
        children_1to18 * (50 + 175) / 2 * 12 +
        children_under1 * (50 + 175) / 2 * 1.5 * 12 +
        children_18to21 * (25 + 85) / 2 * 12,
      (children_under1 + children_1to18 + children_18to21 >= 3) & (gross_income <= 40000) ~
        (children_1to18 * (50 + 175) / 2 * 12 +
           children_under1 * (50 + 175) / 2 * 1.5 * 12 +
           children_18to21 * (25 + 85) / 2 * 12) * 1.5,
      children_under1 + children_1to18 + children_18to21 >= 4 ~
        (children_1to18 * (50 + 175) / 2 * 12 +
           children_under1 * (50 + 175) / 2 * 1.5 * 12 +
           children_18to21 * (25 + 85) / 2 * 12) * 1.5,
      TRUE ~ 0
    ),
    # Simulate low income single and universal allowance for dependent children bonus
    auufc = case_when(
      (spouse_income > 0) & (gross_income + spouse_income <= 15000) ~
        auufc + (children_under1 + children_1to18) * 30 * 12,
      (spouse_income > 0) & (gross_income + spouse_income <= 40000) ~
        auufc + (children_under1 + children_1to18)  * (-0.0012 * gross_income) + 48,
      TRUE ~ auufc
    ),
    auufc = -auufc,
    
    # Simulate inclusion cheques
    adi = ifelse(children_under1 + children_1to18 > 0, 6000, 0),
    adi = -adi,
    
    # Simulate aid to vocational training
    sfl = ifelse(employment == 'unemployed' & gross_income <= 6000, 350 * 12, 0),
    sfl = -sfl,
    
    # Calculate net income
    net_income = gross_income - (irpef + iva + inps + fmn + fmb + fp + fasi + previndai + auufc + adi + sfl),
    # Calculate effective tax rate
    effective_tax = (gross_income - net_income) / gross_income,
    # Calculate net marginal tax at each gross income level
    marginal_tax = 1 - (net_income - lag(net_income)) / (gross_income - lag(gross_income))
  )


#### RUN ANALYSES #####

# Run grid search for income traps


#### ASSUMPTIONS ####

# Assume that personal income tax deductions apply 100% if the individual has higher earnings than spouse

###TODO: Missing low income benefits: https://italy.refugee.info/en-us/articles/5388918400663
###TODO: Should we do assessments per household or per individual? Think household is better
###TODO: What about old-age pensions?