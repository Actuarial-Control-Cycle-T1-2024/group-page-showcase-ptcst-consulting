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
  discount_gym = list(effect_l = 0.03, effect_u = 0.06, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.3, sub_rate = 0.7, effect_reduction = TRUE),
  weight_mgmt = list(effect_l = 0.05, effect_u = 0.1, cost_l = 175, cost_u = 870, horizon = 1, signup = 0.6, sub_rate = 0.8, effect_reduction = TRUE),
  cancer_prevention = list(effect_l = 0.05, effect_u = 0.1, cost_l = 20, cost_u = 85, horizon = 9, signup = 0.6, sub_rate = 0.9, effect_reduction = TRUE),
  heart_screenings = list(effect_l = 0.05, effect_u = 0.1, cost_l = 90, cost_u = 345, horizon = 5, signup = 0.6, sub_rate = 0.7, effect_reduction = FALSE) 
)

mu_vec = c(
  safety_campaigns = 0.03,
  annual_checkup = 0.04,
  discount_gym = 0.03,
  weight_mgmt = 0.04,
  cancer_prevention = 0.05,
  heart_screenings = 0.05
)

find_var = function(lo, hi) {
  ((hi - lo) / 6)^2
}

var_vec = c(
  find_var(0.03, 0.05),
  find_var(0.05, 0.1),
  find_var(0.03, 0.06),
  find_var(0.05, 0.1),
  find_var(0.05, 0.1),
  find_var(0.05, 0.1)
)

raw_covs = sqrt(outer(var_vec, var_vec, "*"))

corr_mat = matrix(
  data = c(1, -0.0, -0.0, -0.0, -0.0, -0.0,
           -0.0, 1, -0.0, -0.0, -0.0, -0.0,
           -0.0, -0.0, 1, -0.0, -0.0, -0.0,
           -0.0, -0.0, -0.0, 1, -0.0, -0.0,
           -0.0, -0.0, -0.0, -0.0, 1, -0.0,
           -0.0, -0.0, -0.0, -0.0, -0.0, 1),
  dimnames = list(names(intervention_progs), names(intervention_progs)),
  nrow = 6,
  ncol = 6
)

# corr_mat = matrix(
#   data = c(1, -0.8, -0.6, -0.5, -0.6, -0.5,
#            -0.8, 1, -0.7, -0.5, -0.3, -0.7,
#            -0.6, -0.7, 1, -0.4, -0.4, -0.8,
#            -0.5, -0.5, -0.4, 1, -0.6, -0.3,
#            -0.6, -0.3, -0.4, -0.6, 1, -0.5,
#            -0.5, -0.7, -0.8, -0.3, -0.5, 1),
#   dimnames = list(names(intervention_progs), names(intervention_progs)),
#   nrow = 6,
#   ncol = 6
# )



# Cov Matrix. Fill out.
cov_mat = corr_mat * raw_covs

# How much the subsidy would increase by if they were at level 5
subsidy_increase_max = 0.1

# Proportion of people who will regularly meet health goals
goal_meeting_rate = 0.7

# Max premium discount for people who are at level 5
premium_disc = 0.1

interest = 0.03

source("Code/supplementary_to_main.R")

res = sim_func(intervention_progs, mort_table, full_data, interest)
# rm(categs, full_data, covs, gen, ins_data, lapse_data, mort_table,
   # proj_data, reduct, sims, goal_meeter, group, partic, gen_costs,
   # i, means, get_lapse)

ggplot() + geom_histogram(aes(x = rowSums(full_data[,1:6]), fill = "independent"), alpha = 0.3, bins = 400) +
  geom_histogram(aes(x = rowSums(store[,1:6]), fill = "dependent"), alpha = 0.3, bins = 400) +
  scale_fill_manual(values = c("dependent" = "blue", "independent" = "red"))
