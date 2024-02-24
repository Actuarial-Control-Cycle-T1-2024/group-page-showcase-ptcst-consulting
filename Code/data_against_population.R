#data check against population 

library(data.table)

superlife_data = fread("SuperLife Inforce Dataset.csv", header = TRUE, skip = 3)

#age distribution
superlife_data[Issue.age >= "0"  & Issue.age <= "14", .N] / superlife_data[, .N]
superlife_data[Issue.age >= "15" & Issue.age <= "24", .N] / superlife_data[, .N]
superlife_data[Issue.age >= "25" & Issue.age <= "54", .N] / superlife_data[, .N]
superlife_data[Issue.age >= "55" & Issue.age <= "64", .N] / superlife_data[, .N]
superlife_data[Issue.age >= "65", .N] / superlife_data[, .N]

#smoking rate 
superlife_data[`Smoker.Status` == "S" & Issue.age >= "18", .N] / superlife_data[ Issue.age >= "18", .N]


