# PACKAGES
require(tidyverse)
require(readxl)
require(data.table)

# PARAMETERS

# Intervention Programs

intervention_progs = list(
  safety_campaigns = list(effect_l = 0.03, effect_u = 0.05, cost_l = 10, cost_u = 35, horizon = 1, signup = 0.7, sub_rate = 1),
  annual_checkup = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 5, signup = 1, sub_rate = 0.5),
  discount_gym = list(effect_l = 0.03, effect_u = 0.06, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.3, sub_rate = 0.5),
  weight_mgmt = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.6, sub_rate = 0.7),
  cancer_prevention = list(effect_l = 0.05, effect_u = 0.1, cost_l = 20, cost_u = 85, horizon = 9, signup = 0.6, sub_rate = 1),
  heart_screenings = list(effect_l = 0.05, effect_u = 0.1, cost_l = 90, cost_u = 345, horizon = 5, signup = 0.6, sub_rate = 0.65) # CHECK ABOUT THIS 1 coz it's like0
)

annuity_due = function(n, discount) {
  (1 + discount) * (1-(1 + discount)^(-n))/discount
}

mort_table = as.vector(read_excel("Data/Altered Mortality Tables.xlsx", sheet = "standard"))[[1]]
mort_table[120] = Inf
ins_data = readRDS("Data/SuperLife Inforce Dataset.rds")

exp_payout = function(intervention_progs, mort_table, entry, discount) {
  personal_effects = list(
    safety_campaigns = list(participation = FALSE, effect = NULL, cost = NULL, horizon = 1),
    annual_checkup = list(participation = FALSE, effect = NULL, cost = NULL, horizon = 5),
    discount_gym = list(participation = FALSE, effect = NULL, cost = NULL, horizon = 1),
    weight_mgmt = list(participation = FALSE, effect = NULL, cost = NULL, horizon = 1),
    cancer_prevention = list(participation = FALSE, effect = NULL, cost = NULL, horizon = 9),
    heart_screenings = list(participation = FALSE, effect = NULL, cost = NULL, horizon = 5)
  )
  
  # SIMULATING EFFECTS, COSTS AND PARTICIPATION IN INTERVENTIONS
  total_yearly_fee = 0
  for (idx in 1:length(intervention_progs)) {
    prog = intervention_progs[[idx]]
    name = names(intervention_progs)[idx]
    if (runif(1) <= prog$signup) {
      cost = prog$sub_rate * runif(1, prog$cost_l, prog$cost_u)
      
      personal_effects[[name]] = list(
        participation = TRUE,
        effect = runif(1, prog$effect_l, prog$effect_u),
        cost = cost,
        horizon = prog$horizon
      )
      
      total_yearly_fee = total_yearly_fee + cost
    }
  }
  
  mort_reduction = 0
  cum_surv = 1
  exp_payout = 0
  exp_cost = 0
  
  normal_payout = 0
  
  # Calculating EPV without intervention programs
  for (i in (entry$issue_age + 1):(entry$policy_end_age)) {
    if (i > entry$issue_age + 1) {
      cum_surv = cum_surv * exp(-mort_table[i-1] * (1 - mort_reduction))
    }
    normal_payout = normal_payout + cum_surv * (1 - exp(-mort_table[i] * (1 - mort_reduction))) * (1 - discount)^(i - entry$issue_age)
  }
  
  cum_surv = 1
  
  # For each year under policy
  for (i in (entry$issue_age + 1):(entry$policy_end_age)) {
    # If applicable mortality reductions still have time in horizon, apply effects
    for (p in 1:length(personal_effects)) {
      prog = personal_effects[[p]]
      if (prog$participation && i - entry$issue_age <= prog$horizon) {
        mort_reduction = mort_reduction + prog$effect / prog$horizon
      }
    }
    
    # Accounting for no requirement for cumulative survival in first year of policy
    if (i > entry$issue_age + 1) {
      cum_surv = cum_surv * exp(-mort_table[i-1] * (1 - mort_reduction))
    }
    
    exp_payout = exp_payout + cum_surv * (1 - exp(-mort_table[i] * (1 - mort_reduction))) * (1 - discount)^(i - entry$issue_age)
    exp_cost = exp_cost + cum_surv * (1 - exp(-mort_table[i] * (1 - mort_reduction))) * total_yearly_fee * annuity_due(i - entry$issue_age, discount)
  }

  # Keeping log of which programs were participated in
  participations = rep(FALSE, 6)
  for (i in 1:length(personal_effects)) {
    names(participations)[i] = names(personal_effects)[i]
    if (personal_effects[[i]]$participation) {
      participations[i] = TRUE
    }
  }
  
  append(list(normal_epv = normal_payout * entry$face_amount, reduced_epv = exp_payout * entry$face_amount, cost = exp_cost), participations)
}

results = data.table(normal_epv = 0, reduced_epv = 0, cost = 0,
                     safety_campaigns = FALSE,
                     annual_checkup = FALSE,
                     discount_gym = FALSE,
                     weight_mgmt = FALSE,
                     cancer_prevention = FALSE,
                     heart_screenings = FALSE)

data_2023 = ins_data[issue_year == 2023]
for (i in 1:nrow(data_2023)) {
  if (i %% 1000 == 0) {
    print(paste("BING BONG:", i))
  }
  # print(ins_data[issue_year == 2023][i])
  results = rbind(results, exp_payout(intervention_progs, mort_table, data_2023[i], 0.02))
  
  # print(paste("Reduction:", res$normal_epv, "-", res$reduced_epv, "=", res$normal_epv - res$reduced_epv))
  # print(paste("Cost:", res$cost))
}
results = results[2:nrow(results)]


