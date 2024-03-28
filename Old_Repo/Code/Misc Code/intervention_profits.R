library(tidyverse)
library(readxl)
library(data.table)


ins_data = data.table(readRDS("Data/SuperLife Inforce Dataset.rds"))
base_data = data.table(read_excel("Data/Altered Mortality Tables.xlsx", sheet = "example"))

example = ins_data[issue_year == 2023][1]

# Surv to 43 = 0.998775 * 0.9986973 * 0.9986259 * 0.9985457 * 0.9984205

exp_payout = function(entry, mort_table, discount_rate) {
  # Determine time horizon of mortality table to take full effect
  horizon = ncol(mort_table) / 3 - 1
  exp_payout = 0
  cum_surv = 1
  
  if (horizon > 0) {
    years_in_hrzn = 0
    
    # Start as X, first relevant entry is X+1
    # If horizon has 3 years (the 4th is the final)
    # Entry 40, 41, 42 are relevant to horizon
    for (i in (entry$issue_age + 1):(entry$issue_age + horizon)) {
      death_col = years_in_hrzn * 3 + 3
      surv_col = (years_in_hrzn - 1) * 3 + 2
      if (i == entry$issue_age + 1) {
        exp_payout = exp_payout + mort_table[i, ..death_col][[1]] * (1 - discount_rate)
      } else {
        cum_surv = cum_surv * mort_table[i - 1, ..surv_col][[1]]
        exp_payout = exp_payout + cum_surv * mort_table[i, ..death_col][[1]] * (1 - discount_rate)^(i - entry$issue_age)
      }
      years_in_hrzn = years_in_hrzn + 1
    }
  }
  
  # Age at which horizon has completed and they follow final mortality table
  age_after_hrzn = entry$issue_age + horizon
  # The - 1 is because the surv_col trails by a year (even though we reach probability of dying age 43, we still
  # need survival to age 42 which is in the last year of horizon)
  surv_col = (horizon - 1) * 3 + 2
  death_col = horizon * 3 + 3
  
  print(death_col)
  
  # Policy starts on issue_age birthday and ends on end_age birthday
  # Life table row i is the surv prob from i-1 to i
  for (i in (age_after_hrzn + 1):(entry$policy_end_age)) {
    if (i == entry$issue_age + 1) {
      exp_payout = exp_payout + mort_table[i, ..death_col][[1]] * (1 - discount_rate)
    } else {
      cum_surv = cum_surv * mort_table[i - 1, ..surv_col][[1]]
      exp_payout = exp_payout + cum_surv * mort_table[i, ..death_col][[1]] * (1 - discount_rate)^(i - entry$issue_age + 1)
    }
    surv_col = horizon * 3 + 2
  }
  exp_payout * entry$face_amount
}

exp_payout(example, base_data, 0.02)
