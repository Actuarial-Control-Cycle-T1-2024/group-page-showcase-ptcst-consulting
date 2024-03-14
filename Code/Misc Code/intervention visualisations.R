data = readRDS("Data/SuperLife Inforce Dataset.rds")
library(tidyverse)
library(data.table)
library(ggpubr)
data['death_age'] = ifelse(is.na(data$death_cause), NA, 
                           data$death_year - data$issue_year + data$issue_age)
dt = data.table(data)
freq_table = na.omit(dt[, .(.N), by = .(death_cause)])
freq_table = freq_table[order(-freq_table$N)]

bar_plot = ggplot(data[!is.na(data$death_cause),], aes(x = death_cause, y = after_stat(count), fill = death_cause)) +
  geom_bar() + 
  scale_fill_hue(breaks = c("A00-B99",
                            "C00-D48",
                            "E00-E88",
                            "I00-I99",
                            "J00-J98",
                            "K00-K92",
                            "V01-Y89"),
                labels = c("A00-B99: Certain infectious and parasitic diseases",
                            "C00-D49: Neoplasms",
                            "E00-E89: Endocrine, nutritional and metabolic diseases",
                            "I00-I99: Diseases of the circulatory system",
                            "J00-J99: Diseases of the respiratory",
                            "K00-K95: Diseases of the digestive system",
                            "V00-Y99: External causes of morbidity")) +
  guides(fill = guide_legend(nrow = 3))
  # scale_fill_hue(labels = c("A00-B99: Certain infectious and parasitic diseases",
  #                           "C00-D49: Neoplasms",
  #                           "D50-D89: Diseases of the blood and blood-forming organs\nand certain disorders involving the immune mechanism",
  #                           "E00-E89: Endocrine, nutritional and metabolic diseases",
  #                           "F01-F99: Mental, behavioural and neurodevelopmental disorders",
  #                           "G00-G99: Diseases of the nervous system",
  #                           "I00-I99: Diseases of the circulatory system",
  #                           "J00-J99: Diseases of the respiratory",
  #                           "K00-K95: Diseases of the digestive system",
  #                           "L00-L99: Diseases of the skin and subcutaneous tissue",
  #                           "M00-M99: Diseases of the musculoskeletal system\nand connective tissue",
  #                           "N00-N99: Diseases of the genitourinary system",
  #                           "O00-O99: Pregnancy, childbirth and the puerperium",
  #                           "Q00-Q99: Congenital malformations, deformations\nand chromosomal abnormalities",
  #                           "R00-R99: Symptoms, signs and abnormal clinical\nand laboratory findings, not elsewhere classified",
  #                           "V00-Y99: External causes of morbidity"))

top_7 = freq_table[1:7, death_cause]
top_7_data = dt[death_cause %in% top_7]

violin_plot = ggplot(top_7_data, aes(x = death_cause, y = death_age, fill = death_cause)) +
  geom_violin() + 
  scale_fill_hue(labels = c("A00-B99: Certain infectious and parasitic diseases",
                            "C00-D49: Neoplasms",
                            "E00-E89: Endocrine, nutritional and metabolic diseases",
                            "I00-I99: Diseases of the circulatory system",
                            "J00-J99: Diseases of the respiratory",
                            "K00-K95: Diseases of the digestive system",
                            "V00-Y99: External causes of morbidity"))

ggarrange(bar_plot, violin_plot, ncol = 2, nrow = 1, common.legend = T, widths = c(2, 1))






ggplot(data) + geom_histogram(aes(x = death_year))

ggplot(d)