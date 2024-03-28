lt = readRDS("Data/ult_lt.RDS")

ages = 26:120

results = data.table(age = 26:120, male_prem = 0, male_payout = 0, female_prem = 0, female_payout = 0)


data = data.table(issue_age = rep(26:120, 2), sex = c(rep("M", 120 - 26 + 1), rep("F", 120 - 26 + 1)))
data[, policy_end_age := pmin(issue_age + 20, 120)]

cum_surv = rep(1, nrow(data))
exp_payment = rep(0, nrow(data))
exp_payout = rep(0, nrow(data))
curr_age = data$issue_age

mort_data = lt
mort_data = rbind(data.table(age = 0:25, male_qx = NA, male_lx = NA, female_lx = NA, total_pop = NA, female_qx = NA, standard = NA) , mort_data)

discount_factor = 1/1.035

while (sum(curr_age < data$policy_end_age) > 0) {
  prior_end = curr_age < data$policy_end_age
  past_first = curr_age > data$issue_age & prior_end
  
  cum_surv = ifelse(past_first, 
                    cum_surv * (1 - ifelse(data$sex == "M", mort_data$male_qx[curr_age], mort_data$female_qx[curr_age])), 
                    cum_surv)
  exp_payment = exp_payment + past_first * (cum_surv * discount_factor^(curr_age - data$issue_age))
  
  exp_payout = exp_payout + prior_end * (cum_surv * ifelse(data$sex == "M", mort_data$male_qx[curr_age + 1], mort_data$female_qx[curr_age + 1]) * discount_factor^(curr_age - data$issue_age + 1))
  curr_age = pmin(120, curr_age + 1)
  
}

results[, `:=`(male_prem = exp_payment[1:95], male_payout = exp_payout[1:95],
               female_prem = exp_payment[96:190], female_payout = exp_payout[96:190])]
results[, `:=`(male_unit_premium = male_payout / male_prem, female_unit_premium = female_payout / female_prem)]

write.csv(results, file = "Data/risk_unit_data.csv")


full_data = readRDS("Data/SuperLife Inforce Dataset.rds")

smokers = full_data[, .(.N, mean(death)), keyby = list(smoker, issue_age)]
smokers = data.table(age = smokers$issue_age[1:40], ns_lx = smokers$N[1:40], ns_qx = smokers$V2[1:40],
                     s_lx = smokers$N[41:80], s_qx = smokers$V2[41:80])

write.csv(smokers, file = "Data/smoker_risk_data.csv")

underwriting = full_data[, .(lx = .N, qx = mean(death)), keyby = list(underwriting_class, issue_age)]
underwriting = pivot_wider(underwriting, names_from = underwriting_class, values_from = c(lx, qx))

write.csv(underwriting, file = "Data/underwriting_class_risk.csv")

