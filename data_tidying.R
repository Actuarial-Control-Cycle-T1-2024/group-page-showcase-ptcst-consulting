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