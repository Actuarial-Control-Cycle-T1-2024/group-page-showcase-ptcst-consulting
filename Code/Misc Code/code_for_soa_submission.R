# PACKAGES
library(tidyverse)
library(readxl)
library(data.table)
library(mvtnorm)
library(doParallel)
library(foreach)

# Set seed for randomness
set.seed(1234)


# PARAMETERS OF PROGRAMS
intervention_progs = list(
  safety_campaigns = list(effect_l = 0.03, effect_u = 0.04, cost_l = 30, cost_u = 35, horizon = 1, signup = 0.6, sub_rate = 1, effect_reduction = TRUE),
  annual_checkup = list(effect_l = 0.05, effect_u = 0.07, cost_l = 300, cost_u = 870, horizon = 5, signup = 1, sub_rate = 1, effect_reduction = FALSE),
  discount_gym = list(effect_l = 0.03, effect_u = 0.05, cost_l = 600, cost_u = 870, horizon = 1, signup = 0.2, sub_rate = 1, effect_reduction = TRUE),
  weight_mgmt = list(effect_l = 0.05, effect_u = 0.065, cost_l = 500, cost_u = 870, horizon = 1, signup = 0.4, sub_rate = 1, effect_reduction = TRUE),
  cancer_prevention = list(effect_l = 0.05, effect_u = 0.08, cost_l = 50, cost_u = 85, horizon = 9, signup = 0.5, sub_rate = 1, effect_reduction = TRUE),
  heart_screenings = list(effect_l = 0.05, effect_u = 0.08, cost_l = 150, cost_u = 345, horizon = 5, signup = 0.5, sub_rate = 1, effect_reduction = FALSE)
)

mu_vec = c(
  safety_campaigns = 0.04,
  annual_checkup = 0.075,
  discount_gym = 0.045,
  weight_mgmt = 0.075,
  cancer_prevention = 0.075,
  heart_screenings = 0.075
)

find_var = function(lo, hi) {
  ((hi - lo) / 6)^2
}

var_vec = c(
  find_var(0.03, 0.04),
  find_var(0.05, 0.07),
  find_var(0.03, 0.05),
  find_var(0.05, 0.065),
  find_var(0.05, 0.08),
  find_var(0.05, 0.08)
)

raw_covs = sqrt(outer(var_vec, var_vec, "*"))

corr_mat = matrix(
  data = c( 1,     -0.35,  0,     0,     -0.35,  0,
            -0.35,  1,    -0.35,  0,     -0.25,  0,
            0,     -0.35,  1,    -0.375, -0.25, -0.25,
            0,      0,    -0.375, 1,     -0.3,  -0.1,
            -0.35, -0.25, -0.25, -0.3,    1,    -0.1,
            0,      0,    -0.25, -0.1,   -0.1,   1),
  dimnames = list(names(intervention_progs), names(intervention_progs)),
  nrow = 6,
  ncol = 6
)

cov_mat = corr_mat * raw_covs



# OTHER PARAMETERS

# Proportion of people who will regularly meet health goals
goal_meeting_rate = 0.7
interest = 0.05



# LOADING DATA
# LIFE TABLE
mort_table = readRDS("Data/ult_lt.RDS")
mort_table = rbind(data.table(age = 0:25, male_qx = NA, female_qx = NA, standard = NA) , mort_table, fill = TRUE)

# LAPSE DATA
lapse_data = readRDS("Data/lapse_rates.RDS")

# DATA WITH CALCULATED RISK UNITS FOR 2023 COHORT
proj_data = readRDS("Data/dataset_with_risk_units.RDS")


# BEGIN ACTUAL CODE
full_data = data.table(safety_campaigns = 0,
                       annual_checkup  = 0,
                       discount_gym = 0,
                       weight_mgmt = 0,
                       cancer_prevention = 0,
                       heart_screenings = 0,
                       cost = 0,
                       face_amount = proj_data$face_amount,
                       issue_age = proj_data$issue_age,
                       policy_end_age = proj_data$policy_end_age,
                       sex = proj_data$sex,
                       checkup_cost = 0,
                       goal_meeter = 0,
                       scaled_units = proj_data$scaled_units,
                       policy_type = proj_data$policy_type)

# GENERATING GOAL MEETER STATUS
full_data[, goal_meeter := runif(nrow(proj_data)) < goal_meeting_rate]

# GENERATING COSTS OF PROGRAM BY PERSON
gen_costs = function(prog) {
  participation = runif(nrow(full_data)) < prog$signup
  participation = ifelse(!full_data$goal_meeter & prog$effect_reduction, 0, participation)
  participation = as.numeric(participation)
  costs = runif(nrow(full_data), prog$cost_l, prog$cost_u) 
  return(list(costs * participation, participation))
}

for (i in names(intervention_progs)) {
  if (i == "annual_checkup") {
    gen = gen_costs(intervention_progs[[i]])
    full_data[, c("cost", i, "checkup_cost") := list(cost + gen[[1]], gen[[2]], gen[[1]])]
  } else {
    gen = gen_costs(intervention_progs[[i]])
    full_data[, c("cost", i) := list(cost + gen[[1]], gen[[2]])]
  }
}

# GENERATING MORTALITY EFFECTS BY PERSON
reduct = full_data[, 1:6]
reduct[, `:=`(group_no = .GRP, count = .N), by = names(reduct)]
categs = unique(reduct)

for (i in 1:nrow(categs)) {
  group = categs[i, group_no]
  partic = which(unlist(categs[i, 1:6]) == 1)
  means = mu_vec[partic]
  covs = cov_mat[partic, partic]
  
  if (length(partic) > 0) {
    if (length(partic) == 1) {
      reduct[group_no == group, (partic):=rnorm(categs[i, count], means, covs)]
    } else {
      sims = mvtnorm::rmvnorm(categs[i, count], means, covs, method = "chol") # matrix(rep(means, categs[i, count]), ncol = length(partic), nrow = categs[i, count]) 
      res = lapply(seq_len(ncol(sims)), function(i) sims[,i])
      reduct[group_no == group, (partic):=res]
    }
  }
}

full_data[, 1:6] = shift(reduct[,1:6], 0)


# HELPER FUNCTION FOR RETRIEVING LAPSE RATES
get_lapse = function(duration) {
  rates = lapse_data$rate
  case_when(
    duration == 0 ~ rates[1],
    duration <= 4 ~ rates[2],
    duration <= 9 ~ rates[3],
    .default = rates[4]
  )
}

sim_func = function(intervention_progs, mort_data, data, interest) {
  discount_factor = 1/(1+interest)
  horizons = sapply(intervention_progs, function(x) x$horizon)
  
  premiums = data$scaled_units
  cum_surv = rep(1, nrow(data))
  cum_surv_no_red = rep(1, nrow(data))
  mort_reduction = rep(0, nrow(data))
  curr_age = data$issue_age
  
  death_costs = c()
  death_costs_no_red = c()
  premiums_total = c()
  expenses = c()
  years_passed = 0
  
  
  while (sum(curr_age < data$policy_end_age) > 0) {
    prior_end = curr_age < data$policy_end_age
    past_first = curr_age > data$issue_age & prior_end
    
    mort_reduction = mort_reduction + rowSums(sweep(data[, 1:6], 2, as.numeric(years_passed < horizons) / horizons, "*"))
    
    cum_surv = ifelse(past_first, 
                      cum_surv * (1 - ifelse(data$sex == "M", mort_data$male_qx[curr_age], mort_data$female_qx[curr_age]) * (1 - mort_reduction) - get_lapse(curr_age - data$issue_age)), 
                      cum_surv)
    cum_surv_no_red = ifelse(past_first,
                             cum_surv_no_red * (1 - ifelse(data$sex == "M", mort_data$male_qx[curr_age], mort_data$female_qx[curr_age]) - get_lapse(curr_age - data$issue_age)),
                             cum_surv_no_red)
    
    expenses = c(expenses, sum(prior_end * cum_surv * data$cost))
    
    
    premiums_total = c(premiums_total, sum(prior_end * cum_surv * premiums))
    if (curr_age[1] == data$issue_age[1]) {
      premiums[data$policy_type == "S"] = 0
    }
    
    
    death_costs = c(death_costs, 
                    sum(prior_end * data$face_amount * cum_surv * 
                          ifelse(data$sex == "M", 
                                 mort_data$male_qx[curr_age + 1], 
                                 mort_data$female_qx[curr_age + 1]) * 
                          (1 - mort_reduction)) * discount_factor)
    death_costs_no_red = c(death_costs_no_red, 
                           sum(prior_end * data$face_amount * cum_surv_no_red * 
                                 ifelse(data$sex == "M", 
                                        mort_data$male_qx[curr_age + 1], 
                                        mort_data$female_qx[curr_age + 1])) * discount_factor)
    
    curr_age = pmin(120, curr_age + 1)
    years_passed = years_passed + 1
  }
  
  
  data.table(year = 2023:(2023 + max(data$policy_end_age - data$issue_age) - 1),
             premiums = premiums_total,
             expenses = expenses,
             exp_death_costs = death_costs,
             exp_death_costs_no_reduction = death_costs_no_red)
}

res = sim_func(intervention_progs, mort_table, full_data, interest)