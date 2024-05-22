library(dplyr)
library(tidyverse)
library(writexl)

# Main source for values and mechanics is the official
# Q&A 2024 (Imposto sobre a renda - Pessoa Física - Perguntas e respostas)
# https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/perguntas-e-respostas/dirpf/pr-irpf-2024.pdf/view
# Unless indicated otherwise,assume it to be the source

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
brl_usd <- 0.2 # exchange rate
# Maximum number of household members, to be used as a filter:
max_hh_members <- 6

# up = upper bound of the bracket, r = income tax rate,
# low = lower bound (which is calculated as the previous upper bound + 0.01)
irpf <- data.frame(
      year = 2024,
      up = c(24551.92, 33919.80, 45012.60, 55976.16, 10^10),
      r = c(0.0, 0.075, 0.15, 0.225, 0.275)
) %>% mutate(low = c(-0.01, head(up, -1)) + 0.01)

# According to the official table for 2024 https://www.in.gov.br/en/web/dou/-/portaria-interministerial-mps/mf-n-2-de-11-de-janeiro-de-2024-537035232
# follows the same logic as the IRPF table, adjusted
# from monthly to annual values + 13th salary
inss_eda <- data.frame(
      year = 2024,
      up = c(1412, 2666.68, 4000.03, 7786.02) * 13,
      r = c(0.075, 0.09, 0.12, 0.14)
) %>% mutate(low = c(-0.01, head(up, -1)) + 0.01)

# Caps for income tax deductions
# Source https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/perguntas-e-respostas/dirpf/pr-irpf-2024.pdf/view
d <- c(
      "health_cap" = Inf,
      "ed_cap" = 3561.5,
      "dependents" = 2275.08,
      "pension_received_cap" = 24511.92,
      "simplified_deduction" = 16754.34
)

# Program caps and benefits, monthly values * 12
# Sources:
# - Law 14.601/2023 - Bolsa Família: https://www.planalto.gov.br/ccivil_03/_ato2023-2026/2023/Lei/L14601.htm
# - Family wage table 2024: https://www.gov.br/inss/pt-br/direitos-e-deveres/salario-familia/valor-limite-para-direito-ao-salario-familia
program_caps <- c("fw" = 1819.26, "fa" = 218) * 12
program_benefits <- c(
      "fw" = 56,
      "fa_cw" = 142,
      "fa_ec" = 150,
      "fa_var" = 50,
      "fa_comp" = 600
) * 12

# Breaks - defines the breaks that will be contained in the final table
b_hh_i <- seq(0, 10000, 100) # household income
b_split <- c(0, 0.25, 0.5) # split of household income between two earners
b_ch_0_7 <- 0:3 # children aged 0-7
b_ch_8_13 <- 0:3 # children aged 8-13
b_ch_14_18 <- 0:3 # children aged 14-18
b_dep_18_plus <- 0:2 # dependents aged 18 or older
b_preg_nursing <- 0:0 # pregnant or nursing women in the household

# Calculates the simplified discount, which is
# 20% of the gross income or the simplified deduction cap, whichever is lower
simp_d <- function(gi) {
      return(min(gi * 0.2, d["simplified_deduction"]))
}

# INSS and IRPF calculators. Applies the rates progressively to the income,
# considering the bounds of the brackets. E.g. income of 25000 will apply
# rate 0 to 24551.92 and rate 0.075 to the remaining 448.08
c_table <- function(i, table) {
      return(sum(pmax(pmin(table$up, i) - table$low, 0) * table$r))
}

c_inss <- function(i) {
      return(c_table(i, inss_eda))
}

c_irpf <- function(gi, inss, other_d = NULL) {
      d <- max(simp_d(gi), other_d + inss) # Chooses most advantageous deduction
      ni <- max(gi - d, 0) # Prevents negative net income
      return(c_table(ni, irpf))
}

# Program benefit calculators

# Checks eligibility for Family Allowance
c_fa_eligible <- function(hh_i, n_members) {
      return(hh_i / n_members <= program_caps["fa"])
}
# Calculates Contribution Wage benefit of Family Allowance program
c_fa_cw <- function(fa_eligible, n_members) {
      return(program_benefits["fa_cw"] * fa_eligible * n_members)
}
# Caculates Early Childhood benefit of Family Allowance program
c_fa_ec <- function(fa_eligible, ch_0_7) {
      return(program_benefits["fa_ec"] * fa_eligible * ch_0_7)
}
# Calculates Variable benefit of Family Allowance program
c_fa_var <- function(fa_eligible, ch_8_13, ch_14_18, preg_nursing) {
      return(program_benefits["fa_var"] * fa_eligible * (ch_8_13 + ch_14_18 + preg_nursing))
}
# Calculates Complement benefit of Family Allowance program
c_fa_comp <- function(fa_eligible, fa_cw, fa_ec, fa_var) {
      return(fa_eligible * max(program_benefits["fa_comp"] - fa_cw - fa_ec - fa_var, 600))
}
# Calculates Family Wage benefit
c_fw <- function(i, ch_0_14) {
      return((i <= program_caps["fw"]) * ch_0_14 * program_benefits["fw"])
}

# Creates the main table, using tidyr::crossing() to create a row for each
# possible combination of values
df <- crossing(
      ch_0_7 = b_ch_0_7,
      ch_8_13 = b_ch_8_13,
      ch_14_18 = b_ch_14_18,
      dep_18_plus = b_dep_18_plus,
      preg_nursing = b_preg_nursing,
      split = b_split,
      hh_i = b_hh_i
) %>%
      # Calculates number of household members, filters out households with more than max_hh_members
      mutate(n_members = (hh_i > 0) * 1 + (split > 0) * 1 + ch_0_7 + ch_8_13 + ch_14_18 + dep_18_plus) %>%
      filter(n_members <= max_hh_members) %>%
      # Calculates income for earners 1 and 2, as well as the benefits, income tax and pension contributions
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
