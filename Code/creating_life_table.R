require(data.table)
require(tidyverse)
require(readxl)


ins_data = readRDS("Data/SuperLife Inforce Dataset.rds")
standard_mort_table = data.table(read_excel("Data/Altered Mortality Tables.xlsx", sheet = "standard"))
standard_mort_table[,age := 1:120]
standard_mort_table = standard_mort_table[26:nrow(standard_mort_table)]
setcolorder(standard_mort_table, c("age", "rate"))
colnames(standard_mort_table)[2] = "qx"

recent = ins_data[issue_year <= 2019 & ((is.na(lapse_year) & is.na(death_year)) | (lapse_year >= 2012 & lapse_year <= 2019) | (death_year >= 2012 & death_year <= 2019)),
                  .(issue_year, issue_age, sex, smoker, underwriting_class, death, death_year, lapse, lapse_year, death_age)]
recent[, lapse_age := ifelse(is.na(lapse_year), NA, lapse_year - issue_year + issue_age)]


male_life_table = data.table(age = 26:120, lx = 0, dx = 0)
female_life_table = data.table(age = 26:120, lx = 0, dx = 0)
lapse_table = data.table(duration = 0:120, lx = 0, dx = 0)

male = recent[sex == 'M']
female = recent[sex == 'F']

for (i in 2012:2019) {
  male_alive = table(male[issue_year <= i & !lapse, .(curr_age = i - issue_year + issue_age)])
  male_death = table(male[death_year == i]$death_age)
  
  female_alive = table(female[issue_year <= i & !lapse, .(curr_age = i - issue_year + issue_age)])
  female_death = table(female[death_year == i]$death_age)
  
  lapse_alive = table(recent[issue_year <= i & !death, .(curr_duration = i - issue_year)])
  lapse_death = table(recent[lapse_year == i, .(lapse_year - issue_year)])
  
  recent = recent[(!lapse | lapse_year != i) & (!death | death_year != i)]
  male = recent[sex == 'M']
  female = recent[sex == 'F']
  
  print(nrow(recent))
  
  male_life_table[age %in% as.numeric(names(male_alive)), lx := lx + male_alive]
  male_life_table[age %in% as.numeric(names(male_death)), dx := dx + male_death]
  
  female_life_table[age %in% as.numeric(names(female_alive)), lx := lx + female_alive]
  female_life_table[age %in% as.numeric(names(female_death)), dx := dx + female_death]
  
  lapse_table[duration %in% as.numeric(names(lapse_alive)), lx := lx + lapse_alive]
  lapse_table[duration %in% as.numeric(names(lapse_death)), dx := dx + lapse_death]
}

male_life_table[, qx := dx / lx]
female_life_table[, qx := dx / lx]

# MALE VALUES EXIST UP TO 74
male_life_table[lx < 4500, qx := 0]
# FEMALE EXIST UP TO 72
female_life_table[lx < 4500, qx := 0]


total_life_table = data.table(age = 26:120, male_qx = male_life_table[, qx], female_qx = female_life_table[, qx])
total_life_table[, standard := standard_mort_table$qx]
lapse_table[, qx := dx / lx]

male_func = lm(male_qx ~ standard, data = total_life_table[1:49])$coefficients
total_life_table[, male_qx := male_func[1] + male_func[2] * standard]

female_func = lm(female_qx ~ standard, data = total_life_table[1:47])$coefficients
total_life_table[, female_qx := standard]

ggplot() + geom_point(data = standard_mort_table, aes(x = age, y = qx, color = 'standard')) + 
  geom_point(data = total_life_table, aes(x = age, y = male_qx, color = 'generated_male')) + 
  scale_color_manual(values = c('standard' = 'red', 'generated_male' = 'blue'))

ggplot() + geom_point(data = standard_mort_table, aes(x = age, y = qx, color = 'standard')) + 
  geom_point(data = total_life_table, aes(x = age, y = female_qx, color = 'generated_female')) + 
  scale_color_manual(values = c('standard' = 'red', 'generated_female' = 'blue'))

total_life_table[nrow(total_life_table), `:=`(male_qx = 1, female_qx = 1)]
# SAVERDS

lapse_table = lapse_table[1:20]
lapse_cats = data.table(duration = c("1", "2-5", "6-10", "11+"),
                        exposed = 0,
                        lapsed = 0)

lapse_cats[duration == "1", 2:3] = lapse_table[duration <= 1, .(sum(lx), sum(dx))]
lapse_cats[duration == "2-5", 2:3] = lapse_table[duration %in% 2:5, .(sum(lx), sum(dx))]
lapse_cats[duration == "6-10", 2:3] = lapse_table[duration %in% 6:10, .(sum(lx), sum(dx))]
lapse_cats[duration == "11+", 2:3] = lapse_table[duration >= 11, .(sum(lx), sum(dx))]
lapse_cats[, rate := lapsed / exposed]

saveRDS(lapse_cats, "Data/lapse_rates.RDS")

