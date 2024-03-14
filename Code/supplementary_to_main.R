# PACKAGES
require(tidyverse)
require(readxl)
require(data.table)
require(mvtnorm)
require(doParallel)
require(foreach)


# LOADING DATA


mort_table = readRDS("Data/ult_lt.RDS")
mort_table = rbind(data.table(age = 0:25, male_qx = NA, female_qx = NA, standard = NA) , mort_table)


lapse_data = readRDS("Data/lapse_rates.RDS")
ins_data = readRDS("Data/SuperLife Inforce Dataset.rds")
proj_data = ins_data[issue_year == 2023]






# PARAMETERS

# SENSITIVITY TESTS FOR INTERVENTION PROGRAM PARAMETERS HERE







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
                       goal_meeter = 0)

goal_meeter = runif(nrow(proj_data)) < goal_meeting_rate

gen_costs = function(prog) {
  participation = runif(nrow(full_data)) < prog$signup
  participation = ifelse(!goal_meeter & prog$effect_reduction, 0, participation)
  participation = as.numeric(participation)
  costs = min(1, (prog$sub_rate + goal_meeter * subsidy_increase_max)) * runif(nrow(full_data), prog$cost_l, prog$cost_u)
  return(list(costs * participation, participation))
}

full_data[, goal_meeter := goal_meeter]

for (i in names(intervention_progs)) {
  if (i == "annual_checkup") {
    gen = gen_costs(intervention_progs[[i]])
    full_data[, c("cost", i, "checkup_cost") := list(cost + gen[[1]], gen[[2]], gen[[1]])]
  } else {
    gen = gen_costs(intervention_progs[[i]])
    full_data[, c("cost", i) := list(gen[[1]], gen[[2]])]
  }
}

full_data[, `:=`(face_amount = proj_data$face_amount, issue_age = proj_data$issue_age, policy_end_age = proj_data$policy_end_age, sex = proj_data$sex)]

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
      sims = mvtnorm::rmvnorm(categs[i, count], means, covs)
      res = lapply(seq_len(ncol(sims)), function(i) sims[,i])
      reduct[group_no == group, (partic):=res]
    }
  }
}

full_data[, 1:6] = shift(reduct[,1:6], 0)

get_lapse = function(duration) {
  rates = lapse_data$rate
  case_when(
    duration == 0 ~ rates[1],
    duration <= 4 ~ rates[2],
    duration <= 9 ~ rates[3],
    .default = rates[4]
  )
}

sim_func = function(intervention_progs, mort_table, data, interest) {
  
  # data = full_data[1:1000]
  # interest = 0.05
  discount_factor = 1/(1+interest)

  
  mort_data = mort_table$standard
  cum_surv = rep(1, nrow(data))
  exp_payment = rep(0, nrow(data))
  exp_payout = rep(0, nrow(data))
  curr_age = data$issue_age
  
  horizons = sapply(intervention_progs, function(x) x$horizon)
  
  while (sum(curr_age < data$policy_end_age) > 0) {
    prior_end = curr_age < data$policy_end_age
    past_first = curr_age > data$issue_age & prior_end
    
    cum_surv = ifelse(past_first, 
                      cum_surv * (1 - mort_data[curr_age] - get_lapse(curr_age - data$issue_age)), 
                      cum_surv)
    exp_payment = exp_payment + past_first * (cum_surv * discount_factor^(curr_age - data$issue_age))
    
    exp_payout = exp_payout + prior_end * (cum_surv * mort_data[curr_age + 1] * discount_factor^(curr_age - data$issue_age + 1))
    curr_age = pmin(120, curr_age + 1)
    
  }
  
  base_values = list(exp_payment = exp_payment, exp_payout = exp_payout)

  cum_surv = rep(1, nrow(data))
  epv_annuity_new_rates = rep(0, nrow(data))
  mort_reduction = rep(0, nrow(data))
  curr_age = data$issue_age
  
  years_passed = 0
  while (sum(curr_age < data$policy_end_age) > 0) {
    prior_end = curr_age < data$policy_end_age
    past_first = curr_age > data$issue_age & prior_end
    
    mort_reduction = mort_reduction + rowSums(sweep(data[, 1:6], 2, as.numeric(years_passed < horizons) / horizons, "*"))
    
    cum_surv = ifelse(past_first, 
                      cum_surv * (1 - mort_data[curr_age] * (1 - mort_reduction) - get_lapse(curr_age - data$issue_age)), 
                      cum_surv)
    epv_annuity_new_rates = epv_annuity_new_rates + prior_end * cum_surv * discount_factor^(curr_age - data$issue_age)
    
    curr_age = pmin(120, curr_age + 1)
    years_passed = years_passed + 1
  }
  
  premiums = (data$face_amount * base_values$exp_payout +
    0.5 * data$checkup_cost * epv_annuity_new_rates) / base_values$exp_payment
  premiums = premiums * (1 - premium_disc * data$goal_meeter)
  cum_surv = rep(1, nrow(data))
  mort_reduction = rep(0, nrow(data))
  curr_age = data$issue_age
  
  death_costs = c()
  premiums_total = c()
  expenses = c()
  years_passed = 0
  
  while (sum(curr_age < data$policy_end_age) > 0) {
    prior_end = curr_age < data$policy_end_age
    past_first = curr_age > data$issue_age & prior_end
    
    mort_reduction = mort_reduction + rowSums(sweep(data[, 1:6], 2, as.numeric(years_passed < horizons) / horizons, "*"))
    
    cum_surv = ifelse(past_first, 
                      cum_surv * (1 - mort_data[curr_age] * (1 - mort_reduction) - get_lapse(curr_age - data$issue_age)), 
                      cum_surv)
    
    expenses = c(expenses, sum(prior_end * cum_surv * data$cost))
    premiums_total = c(premiums_total, sum(prior_end * cum_surv * premiums))
    death_costs = c(death_costs, sum(prior_end * data$face_amount * cum_surv * mort_data[curr_age] * (1 - mort_reduction)) * discount_factor)
    
    curr_age = pmin(120, curr_age + 1)
    years_passed = years_passed + 1
  }
  

  data.table(year = 2023:(2023 + max(data$policy_end_age - data$issue_age) - 1),
             premiums = premiums_total,
             expenses = expenses,
             exp_death_costs = death_costs)
}