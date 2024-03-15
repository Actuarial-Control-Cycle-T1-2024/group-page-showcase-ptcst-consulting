# STILL REMAINING TO DO (but negligible)
# ADD HARD LIMITS ON MULTIVARIATE NORMAL SIM
# ADD MALE vs. FEMALE LIFE TABLE 
# ADD FEMALE MORTALITY RATES PROPERLY

# PARAMS
# effect_u/effect_l/cost_u/cost_l lower and upper limits of cost and effects
# horizon - how long it takes to take full effect
# signup - proportion of people that will signup to this program
# sub_rate - base subsidy rate (as of level 1)
# effect_reduction - TRUE if the effect is only felt if someone puts effort in
#                    i.e. heart checks help anyway, but gym memberships dont.

set.seed(1234)

intervention_progs = list(
  safety_campaigns = list(effect_l = 0.03, effect_u = 0.05, cost_l = 10, cost_u = 35, horizon = 1, signup = 0.7, sub_rate = 0.9, effect_reduction = TRUE),
  annual_checkup = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 5, signup = 1, sub_rate = 1, effect_reduction = FALSE),
  discount_gym = list(effect_l = 0.03, effect_u = 0.06, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.3, sub_rate = 0.4, effect_reduction = TRUE),
  weight_mgmt = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.6, sub_rate = 0.6, effect_reduction = TRUE),
  cancer_prevention = list(effect_l = 0.05, effect_u = 0.1, cost_l = 20, cost_u = 85, horizon = 9, signup = 0.6, sub_rate = 0.9, effect_reduction = TRUE),
  heart_screenings = list(effect_l = 0.05, effect_u = 0.1, cost_l = 90, cost_u = 345, horizon = 5, signup = 0.6, sub_rate = 0.55, effect_reduction = FALSE) 
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

# Cov Matrix. Fill out.
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

# How much the subsidy would increase by if they were at level 5
subsidy_increase_max = 0.1

# Proportion of people who will regularly meet health goals
goal_meeting_rate = 0.7

# Max premium discount for people who are at level 5
premium_disc = 0.1

interest = 0.03

source("Code/supplementary_to_main.R")

res = sim_func(intervention_progs, mort_table, full_data, interest)
rm(categs, full_data, covs, gen, ins_data, lapse_data, mort_table,
   proj_data, reduct, sims, goal_meeter, group, partic, gen_costs,
   i, means, get_lapse)
