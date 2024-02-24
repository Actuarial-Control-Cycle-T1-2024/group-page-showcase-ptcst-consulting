# Preliminary Data Cleaning
library(tidyverse)

data = read.csv("SuperLife Inforce Dataset.csv")
colnames(data) = data[3,]
data = data[4:nrow(data),]
#saveRDS(data, file = "SuperLife Inforce Dataset.rds", compress = FALSE)

colnames(data) = c("policy_number", "policy_type", "issue_year", "issue_age",
                   "sex", "face_amount", "smoker", "underwriting_class",
                   "urban_rural", "region", "dist_channel", "death",
                   "death_year", "lapse", "lapse_year", "death_cause")

data$policy_type = factor(ifelse(data$policy_type == "T20", "T", "S"))

data$smoker = data$smoker == "S"

data$underwriting_class = factor(case_when(
  data$underwriting_class == "very low risk" ~ "VL",
  data$underwriting_class == "low risk" ~ "L",
  data$underwriting_class == "moderate risk" ~ "M",
  data$underwriting_class == "high risk" ~ "H"
))

data$urban_rural = factor(ifelse(data$urban_rural == "Urban", "U", "R"))

data$region = factor(data$region)

data$dist_channel = factor(ifelse(data$dist_channel == "Telemarketer", "T", "A"))

#saveRDS(data, file = "SuperLife Inforce Dataset.rds", compress = FALSE)

# Stopping here. Need to change the year of death and year of lapse to T/F
# Also need to confirm that non-NA values for death and lapse indicators
# line up with a non-NA value for the respective year.


sum(!is.na(data$lapse) & !is.na(data$death))

sum(xor(is.na(data$death), is.na(data$death_year)) & data$death_year != "")
sum(xor(is.na(data$death), is.na(data$death_cause)) & data$death_cause != "")
# Note that the second conditions account for the change in record keeping during 2008-2009


sum(xor(is.na(data$lapse), is.na(data$lapse_year)))



# Adjusting changes in record-keeping
data$death = !is.na(data$death)
data$death_year = ifelse(data$death_year == "", NA, data$death_year)
data$death_cause = ifelse(data$death_cause == "", NA, data$death_cause)

data$lapse = !is.na(data$lapse)
data$lapse_year = ifelse(data$lapse_year == "", NA, data$lapse_year)


# Correcting some final types

to_numeric = c("issue_year", "issue_age", "face_amount", "death_year", "lapse_year")
data[to_numeric] = lapply(data[to_numeric], as.numeric)

data$sex = factor(data$sex)
data$death_cause = factor(data$death_cause)
data$underwriting_class = factor(data$underwriting_class, levels = c("VL", "L", "M", "H"))

