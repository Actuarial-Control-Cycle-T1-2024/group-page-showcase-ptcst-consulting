library(data.table)
library(tidyverse)
library(readxl)

data = readRDS("Data/SuperLife Inforce Dataset.rds")
data = data[issue_year == 2023]

age_gender = as.matrix(read_excel("Code/Determining Risk Units.xlsx", sheet = "age_gender_rate")[,2:5])

underwriting = c(VL = 0, L = 0.15, M = 0.4, H = 0.6)

smoker_rate =0.74

data[, col_num := (sex == "F") + (policy_type == "S")*2 + 1]
data[, row_num := pmin((issue_age - 25) %/% 5 + 1, 8)]
data[, flat_boost := 1 + underwriting[underwriting_class] + smoker * smoker_rate]

base_units = c()
for (i in 1:nrow(data)) {
  if (i %% 1000 == 0) {
    print(i)
  }
  row_num = unlist(data[i, row_num])
  col_num = unlist(data[i, col_num])
  base_units = c(base_units, unname(age_gender[row_num, col_num]))
}



data[, total_units := base_units * flat_boost]
data[, scaled_units := face_amount / 1000 * total_units]


data[, sum(scaled_units), by = policy_type]

# T: 5.106e+10
# S: 1.676+e12

p = 1.22
