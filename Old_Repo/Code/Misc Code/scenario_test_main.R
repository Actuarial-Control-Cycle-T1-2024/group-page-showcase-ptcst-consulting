# STILL REMAINING TO DO (but negligible)
# ADD HARD LIMITS ON MULTIVARIATE NORMAL SIM

# PARAMS
# effect_u/effect_l/cost_u/cost_l lower and upper limits of cost and effects
# horizon - how long it takes to take full effect
# signup - proportion of people that will signup to this program
# sub_rate - base subsidy rate (as of level 1)
# effect_reduction - TRUE if the effect is only felt if someone puts effort in
#                    i.e. heart checks help anyway, but gym memberships dont.

set.seed(1234)

intervention_progs = list(
  safety_campaigns = list(effect_l = 0.03, effect_u = 0.05, cost_l = 30, cost_u = 35, horizon = 1, signup = 0.6, sub_rate = 1, effect_reduction = TRUE),
  annual_checkup = list(effect_l = 0.05, effect_u = 0.1, cost_l = 300, cost_u = 870, horizon = 5, signup = 1, sub_rate = 1, effect_reduction = FALSE),
  discount_gym = list(effect_l = 0.03, effect_u = 0.06, cost_l = 600, cost_u = 870, horizon = 1, signup = 0.2, sub_rate = 1, effect_reduction = TRUE),
  weight_mgmt = list(effect_l = 0.05, effect_u = 0.1, cost_l = 500, cost_u = 870, horizon = 1, signup = 0.4, sub_rate = 1, effect_reduction = TRUE),
  cancer_prevention = list(effect_l = 0.05, effect_u = 0.1, cost_l = 50, cost_u = 85, horizon = 9, signup = 0.5, sub_rate = 1, effect_reduction = TRUE),
  heart_screenings = list(effect_l = 0.05, effect_u = 0.1, cost_l = 150, cost_u = 345, horizon = 5, signup = 0.5, sub_rate = 1, effect_reduction = FALSE)
)

mu_vec = c(
  safety_campaigns = 0.035,
  annual_checkup = 0.06,
  discount_gym = 0.04,
  weight_mgmt = 0.0575,
  cancer_prevention = 0.065,
  heart_screenings = 0.065
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
  data = c( 1,    -0.35,  0,     0,     -0.35,  0,
            -0.35,  1,    -0.35,  0,     -0.25,  0,
            0,    -0.35,  1,    -0.375, -0.25, -0.25,
            0,     0,    -0.375,   1,   -0.3,  -0.1,
            -0.35, -0.25, -0.25, -0.3,    1,    -0.1,
            0,     0,    -0.25, -0.1,   -0.1,   1),
  dimnames = list(names(intervention_progs), names(intervention_progs)),
  nrow = 6,
  ncol = 6
)



# Cov Matrix. Fill out.
cov_mat = corr_mat * raw_covs

# How much the subsidy would increase by if they were at level 5
subsidy_increase_max = 0.1

# Proportion of people who will regularly meet health goals
goal_meeting_rate = 0.6

# Max premium discount for people who are at level 5
premium_disc = 0

interest = 0.04

source("Code/supplementary_to_main.R")

res = sim_func(intervention_progs, mort_table, full_data, interest)
rm(categs, full_data, covs, gen, lapse_data, mort_table,
   proj_data, reduct, sims, goal_meeter, group, partic, gen_costs,
   i, means, get_lapse)

write.csv(data.frame(res[, 2:4]), "scenario_test.csv", row.names = FALSE)
