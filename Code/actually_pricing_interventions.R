# PACKAGES
require(tidyverse)
require(readxl)
require(data.table)
require(mvtnorm)

# LOADING DATA

mort_table = as.vector(read_excel("Data/Altered Mortality Tables.xlsx", sheet = "standard"))[[1]]
mort_table[120] = Inf
ins_data = readRDS("Data/SuperLife Inforce Dataset.rds")
proj_data = ins_data[issue_year == 2023]
example = ins_data[issue_year == 2023][1]

# PARAMETERS
# Intervention Programs

intervention_progs = list(
  safety_campaigns = list(effect_l = 0.03, effect_u = 0.05, cost_l = 10, cost_u = 35, horizon = 1, signup = 0.7, sub_rate = 0.9, effect_reduction = TRUE),
  annual_checkup = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 5, signup = 1, sub_rate = 0.4, effect_reduction = FALSE),
  discount_gym = list(effect_l = 0.03, effect_u = 0.06, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.3, sub_rate = 0.4, effect_reduction = TRUE),
  weight_mgmt = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.6, sub_rate = 0.6, effect_reduction = TRUE),
  cancer_prevention = list(effect_l = 0.05, effect_u = 0.1, cost_l = 20, cost_u = 85, horizon = 9, signup = 0.6, sub_rate = 0.9, effect_reduction = TRUE),
  heart_screenings = list(effect_l = 0.05, effect_u = 0.1, cost_l = 90, cost_u = 345, horizon = 5, signup = 0.6, sub_rate = 0.55, effect_reduction = FALSE) # CHECK ABOUT THIS 1 coz it's like0
)

find_var = function(lo, hi) {
  ((hi - lo) / 6)^2
}

mu_vec = c(
  safety_campaigns = 0.04,
  annual_checkup = 0.075,
  discount_gym = 0.045,
  weight_mgmt = 0.075,
  cancer_prevention = 0.075,
  heart_screenings = 0.075
)

cov_mat = matrix(
  data = c(find_var(0.03, 0.05), 0, 0, 0, 0, 0,
           0, find_var(0.05, 0.1), 0, 0, 0 ,0,
           0, 0, find_var(0.03, 0.06), 0, 0, 0,
           0, 0, 0, find_var(0.05, 0.1), 0, 0,
           0, 0, 0, 0, find_var(0.05, 0.1), 0,
           0, 0, 0, 0, 0, find_var(0.05, 0.1)),
  dimnames = list(names(intervention_progs), names(intervention_progs)),
  nrow = 6,
  ncol = 6
)

mort_effect_simulations = rmvnorm(nrow(proj_data), mean = mu_vec, sigma = cov_mat)

# BOUND NORMAL SIMS
for (prog in names(mu_vec)) {
  vals = mort_effect_simulations[, prog]
  upper = intervention_progs[[prog]]$effect_u
  lower = intervention_progs[[prog]]$effect_l
  mort_effect_simulations[, prog] = case_when(
    vals > upper ~ upper,
    vals < lower ~ lower,
    .default = vals
  )
}

rm(vals, upper, lower, prog)

goal_meeting_rate = 0.7

full_data = data.table(safety_campaigns = 0,
                       annual_checkup  = 0,
                       discount_gym = 0,
                       weight_mgmt = 0,
                       cancer_prevention = 0,
                       heart_screenings = 0,
                       face_amount = 0,
                       cost = rep(0, nrow(proj_data)),
                       issue_age = 0,
                       policy_end_age = 0,
                       checkup_cost = 0)

goal_meeter = runif(nrow(proj_data)) < goal_meeting_rate

gen_costs = function(prog) {
  participation = runif(nrow(full_data)) < prog$signup
  participation = ifelse(!goal_meeter & prog$effect_reduction, 0, participation)
  participation = as.numeric(participation)
  costs = (prog$sub_rate + goal_meeter * 0.1) * runif(nrow(full_data), prog$cost_l, prog$cost_u)
  return(list(costs * participation, participation))
}

gen = gen_costs(intervention_progs[['safety_campaigns']])
full_data[, c("cost", "safety_campaigns") := list(gen[[1]], gen[[2]])]

gen = gen_costs(intervention_progs[['annual_checkup']])
full_data[, c("cost", "annual_checkup", "checkup_cost") := list(cost + gen[[1]], gen[[2]], gen[[1]])]

gen = gen_costs(intervention_progs[['discount_gym']])
full_data[, c("cost", "discount_gym") := list(cost + gen[[1]], gen[[2]]) ]

gen = gen_costs(intervention_progs[['weight_mgmt']])
full_data[, c("cost", "weight_mgmt") := list(cost + gen[[1]], gen[[2]])]

gen = gen_costs(intervention_progs[['cancer_prevention']])
full_data[, c("cost", "cancer_prevention") := list(cost + gen[[1]], gen[[2]])]

gen = gen_costs(intervention_progs[['heart_screenings']])
full_data[, c("cost", "heart_screenings") := list(cost + gen[[1]], gen[[2]])]

full_data[, `:=`(face_amount = proj_data$face_amount, issue_age = proj_data$issue_age, policy_end_age = proj_data$policy_end_age)]

for (i in 1:nrow(full_data)) {
  if (i %% 1000 == 0) {
    print(i)
  }
  partic = which(as.numeric(full_data[i, 1:6]) == 1)
  means = mu_vec[partic]
  covs = cov_mat[partic, partic]
  if (length(partic) > 0) {
    if (length(partic) == 1) {
      full_data[i, (partic) := as.list(rnorm(1, means, covs)[1:length(partic)])]
    } else {
      full_data[i, (partic) := as.list(rmvnorm(1, means, covs)[1:length(partic)])]
    }
  }
}

premium_under_base = function(entry, mort_table, interest) {
  cum_surv = 1
  exp_payment = 1
  exp_payout = 0
  discount_factor = 1/(1+interest)
  
  for (i in (entry$issue_age + 1):entry$policy_end_age) {
    if (i > entry$issue_age + 1) {
      cum_surv = cum_surv * exp(-mort_table[i-1])
      exp_payment = exp_payment + cum_surv * discount_factor^(i - (entry$issue_age + 1))
    }
    exp_payout = exp_payout + cum_surv * (1 - exp(-mort_table[i])) * discount_factor^(i - entry$issue_age)
      
  }
  
  return(list(exp_payment = exp_payment, exp_payout = exp_payout))
}




mort_reductions = function(entry, curr_age) {
  mort_reduction = 0
  for (p in colnames(entry)[1:6]) {
    prog = intervention_progs[[p]]
    if (entry[, p] > 0 && curr_age - entry$issue_age <= prog$horizon) {
      mort_reduction = mort_reduction + entry[, p] / prog$horizon
    }
  }
  return(mort_reduction)
}





model_func = function(intervention_progs, mort_table, entry, interest) {
  discount_factor = 1/(1+interest)
  base_values = premium_under_base(entry, mort_table, interest)
  
  death_costs = c()
  
  cum_surv = 1
  epv_annuity_new_rates = 0
  mort_reduction = 0
  
  for (i in (entry$issue_age + 1):entry$policy_end_age) {
    # If applicable mortality reductions still have time in horizon, apply effects
    mort_reduction = mort_reduction + mort_reductions(entry, i)
    
    # Accounting for no requirement for cumulative survival in first year of policy
    if (i > entry$issue_age + 1) {
      cum_surv = cum_surv * exp(-mort_table[i-1] * (1 - mort_reduction))
    }
    death_costs = c(death_costs,
                    cum_surv * (1 - exp(-mort_table[i] * (1 - mort_reduction))) * discount_factor^(i - entry$issue_age))
    epv_annuity_new_rates = epv_annuity_new_rates + cum_surv * discount_factor^(i - (entry$issue_age + 1))
  }
  
  premium = (entry$face_amount * base_values$exp_payout + 
               0.5 * entry$checkup_cost * epv_annuity_new_rates) / base_values$exp_payment
  
  vals = data.table(year = 2023:(2023 + entry$policy_end_age - entry$issue_age - 1), premium = premium,
                    expenses = entry$cost, exp_death_costs = entry$face_amount * death_costs)

  vals
}


final_frame = data.table(year = 2023:(2023 + max(ins_data$policy_end_age - ins_data$issue_age) - 1),
                         premium = 0,
                         expenses = 0,
                         exp_death_costs = 0)

for (i in 1:10000) {
  if (i %% 1000 == 0) {
    print(paste("Passing", i))
  }
  res = model_func(intervention_progs, mort_table, data_2023[i], 0.02)

  final_frame[1:nrow(res),
              `:=`(
                premium = premium + res$premium,
                expenses = expenses + res$expenses,
                exp_death_costs = exp_death_costs + res$exp_death_costs
              )]
}


