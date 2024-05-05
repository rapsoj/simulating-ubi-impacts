library(dplyr)
library(tidyverse)
library(writexl)

# Convention
# d / _d = deduction
# r / _r = rate
# hh = household
# _w = wage
# i / _i = income
# gi = gross income
# ni = net income
# fa = family allowance
# fw = family wage
# _ec = early childhood
# _var = variable
# _comp = complement
# irpf = income tax
# inss = social security
# b_ = breaks
# low / up = lower / upper bound / limit


# Parameters
BRL_USD <- 0.2 # exchange rate
max_hh_members <- 6 # maximum number of household members
simp_d <- 0.25 * 2259.208 * period # deduction of 25% of the exempt bracket applied to simplified tax filing
min_w <- 1420 # minimum wage
period <- 1 # 1 for monthly, 12 for yearly

# According to the official table for 2024 https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/2024
irpf <- data.frame(
      year = 2024,
      up = c(2259.20, 2826.65, 3751.05, 4664.68, 10^10) * period,
      r = c(0.0, 0.075, 0.15, 0.225, 0.275)
) %>% mutate(low = c(-0.01, head(up, -1)) + 0.01)

inss_EDA <- data.frame(
      year = 2024,
      up = c(1412, 2666.68, 4000.03, 7786.02) * period,
      r = c(0.075, 0.09, 0.12, 0.14)
) %>% mutate(low = c(-0.01, head(up, -1)) + 0.01)

# Caps for income tax deductions
d <- c(
      "health_cap" = Inf,
      "ed_cap" = 3561.5 / period,
      "dependents" = 189.59 * period,
      "pension_received_cap" = 2112 * period
)

# Program caps and benefits
program_caps <- c("fw" = 1655.68, "fa" = 218) * period
program_benefits <- c("fw" = 56, "fa_cw" = 142, "fa_ec" = 150, "fa_var" = 50, "fa_comp" = 600) * period

# Breaks
b_hh_i <- seq(0, 10000, 500)
b_split <- c(0, 0.25, 0.5)
b_ch_0_7 <- 0:3
b_ch_8_13 <- 0:3
b_ch_14_18 <- 0:3
b_dep_18_plus <- 0:2
b_preg_nursing <- 0:0

# INSS and IRPF calculators
c_table <- function(i, table) {
      return(sum(pmax(pmin(table$up, i) - table$low, 0) * table$r))
}

c_inss <- function(i) {
      return(c_table(i, inss_EDA))
}

c_irpf <- function(gi, inss, s_d = simp_d, other_d = NULL) {
      d <- max(s_d, other_d + inss)
      ni <- max(gi - d, 0)
      return(c_table(ni, irpf))
}

# Program calculators
# fw = Family Wage, fa = Family Allowance, fa_cw= basic FA benefit, fa_ec = FA early childhood,
# fa_var = FA variable, fa_comp = FA complement

c_fa_eligible <- function(hh_i, n_members) {
      return(hh_i / n_members <= program_caps["fa"])
}
c_fa_cw <- function(fa_eligible, n_members) {
      return(program_benefits["fa_cw"] * fa_eligible * n_members)
}
c_fa_ec <- function(fa_eligible, ch_0_7) {
      return(program_benefits["fa_ec"] * fa_eligible * ch_0_7)
}
c_fa_var <- function(fa_eligible, ch_8_13, ch_14_18, preg_nursing) {
      return(program_benefits["fa_var"] * fa_eligible * (ch_8_13 + ch_14_18 + preg_nursing))
}
c_fa_comp <- function(fa_eligible, fa_cw, fa_ec, fa_var) {
      return(fa_eligible * max(program_benefits["fa_comp"] - fa_cw - fa_ec - fa_var, 0))
}
c_fw <- function(i, ch_0_14) {
      return((i <= program_caps["fw"]) * ch_0_14 * program_benefits["fw"])
}

# Creates the main table, using tidyr::crossing() to create a row for each possible combination of values
df <- crossing(
      ch_0_7 = b_ch_0_7,
      ch_8_13 = b_ch_8_13,
      ch_14_18 = b_ch_14_18,
      dep_18_plus = b_dep_18_plus,
      preg_nursing = b_preg_nursing,
      split = b_split,
      hh_i = b_hh_i
) %>%
      mutate(n_members = (hh_i > 0) * 1 + (split > 0) * 1 + ch_0_7 + ch_8_13 + ch_14_18 + dep_18_plus) %>%
      filter(n_members <= max_hh_members) %>%
      mutate(
            dependents = ch_0_7 + ch_8_13 + ch_14_18 + dep_18_plus,
            i_1 = hh_i * split,
            i_2 = hh_i * (1 - split),
            fa_eligible = c_fa_eligible(hh_i, n_members),
            fa_cw = c_fa_cw(fa_eligible, n_members),
            fa_ec = c_fa_ec(fa_eligible, ch_0_7),
            fa_var = c_fa_var(fa_eligible, ch_8_13, ch_14_18, preg_nursing),
            fw_1 = c_fw(i_1, ch_0_7),
            fw_2 = c_fw(i_2, ch_0_7)
      ) %>%
      rowwise() %>%
      mutate(
            fa_comp = c_fa_comp(fa_eligible, fa_cw, fa_ec, fa_var),
            irpf_d_dependents = d["dependents"] * dependents,
            inss_1 = c_inss(i_1),
            inss_2 = c_inss(i_2),
            irpf_1 = c_irpf(i_1, inss_1),
            irpf_2 = c_irpf(i_2, inss_2, other_d = d["dependents"] * dependents)
      ) %>%
      mutate(
            ni_1 = i_1 + fw_1 - inss_1 - irpf_1,
            ni_2 = i_2 + fw_2 - inss_2 - irpf_2
      ) %>%
      mutate(ni_hh = ni_1 + ni_2 + fa_cw + fa_ec + fa_var + fa_comp) %>%
      ungroup()

# rm(b_ch_0_7, b_ch_8_13, b_ch_14_18, b_dep_18_plus, b_fam_wage_earners, b_preg_nursing, b_split, b_hh_i)

# write to csv
write.csv(df, "brazil.csv", row.names = FALSE)
