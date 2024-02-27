library(tidyverse)
library(readxl)
library(data.table)


ins_data = data.table(readRDS("Data/SuperLife Inforce Dataset.rds"))
base_data = data.table(read_excel("Data/Altered Mortality Tables.xlsx", sheet = "standard"))

base_data[, surv_prob := exp(-rate)]
base_data[120, surv_prob := 0]
base_data[, death_prob := 1 - surv_prob]
base_data[, age := NULL]



example = ins_data[issue_year == 2023][1]

exp_payout = function(entry, mort_table, discount_rate) {
  # Determine time horizon of mortality table to take full effect
  horizon = ncol(mort_table) / 3 - 1
  exp_payout = 0
  cum_surv = 1
  
  if (horizon > 0) {
    years_in_hrzn = 0
    for (i in (entry$issue_age + 1):(entry$issue_age + horizon + 1)) {
      if (i == entry$issue_age + 1) {
        exp_payout = exp_payout + mort_table[i, years_in_hrzn * 3 + 3][[1]] * (1 - discount_rate)
      } else {
        cum_surv = cum_surv * mort_table[i - 1, years_in_hrzn * 3 + 2][[1]]
        print(paste("current cum surv (in horizon) up to", i - 1, "birthday is", cum_surv))
        exp_payout = exp_payout + cum_surv * mort_table[i, years_in_hrzn * 3 + 3][[1]] * (1 - discount_rate)^(i - entry$issue_age)
      }
      print(paste("death between", i - 1, "and", i, "birthdays (in horizon) has prob", cum_surv * mort_table[i, ..death_col][[1]]))
      print(paste("discount was", i - entry$issue_age))
      years_in_hrzn = years_in_hrzn + 1
    }
  }
  
  # Age at which horizon has completed and they follow final mortality table
  age_after_horzn = entry$issue_age + horizon
  surv_col = horizon * 3 + 2
  death_col = horizon * 3 + 3
  
  
  
  # Policy starts on issue_age birthday and ends on end_age birthday
  # Life table row i is the surv prob from i-1 to i
  for (i in (entry$issue_age + horizon + 1):(entry$policy_end_age)) {
    if (i == entry$issue_age + 1) {
      exp_payout = exp_payout + mort_table[i, ..death_col][[1]] * (1 - discount_rate)
    } else {
      cum_surv = cum_surv * mort_table[i - 1, ..surv_col][[1]]
      print(paste("current cumulative surv up to", i - 1, "birthday is", cum_surv))
      exp_payout = exp_payout + cum_surv * mort_table[i, ..death_col][[1]] * (1 - discount_rate)^(i - entry$issue_age + 1)
    }
    
    print(paste("death between", i - 1, "and", i, "birthdays has prob", cum_surv * mort_table[i, ..death_col][[1]]))
    print(paste("discount is", i - entry$issue_age))
  }
  exp_payout * entry$face_amount
}

exp_payout(example, base_data, 0.02)
