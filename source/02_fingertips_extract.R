install.packages("devtools")
devtools::install_github("PublicHealthEngland/fingertipsR")
library(fingertipsR)
library(dplyr)
library(gglot2)

#93553#area_types()

#ics_Prevalence <- fingertips_data(IndicatorID = 90933, AreaTypeID = 221)

#ics_asthma_review <- fingertips_data(IndicatorID = 93790, AreaTypeID = 221)
#area name time period =title, Y= value,

ics_data_filtered <- ics_data %>%
  filter(AreaType == "ICBs") %>%
  filter(Timeperiod == "2023/24") %>%
  mutate(row_id = substr(AreaName, nchar(AreaName) - 2, nchar(AreaName)))

write.csv(ics_data_filtered,
          file = "Hospitalisations_Fingertips.csv",
          row.names = FALSE)

ics_Prevalence <- fingertips_data(IndicatorID = 90933, AreaTypeID = 221)

ics_prevalence_filtered <- ics_Prevalence %>%
  filter(AreaType == "ICBs") %>%
  filter(Timeperiod == "2023/24") %>%
  mutate(row_id = substr(AreaName, nchar(AreaName) - 2, nchar(AreaName)))

write.csv(ics_prevalence_filtered,
          file = "Prevalence_Fingertips.csv",
          row.names = FALSE)

#ics_IMD <- fingertips_data(IndicatorID = 93553, AreaTypeID = 221)

#ics_IMD_filtered <- ics_IMD %>%
 # filter(AreaType == "ICBs") %>%
  #filter(Timeperiod == "2019") %>%
  #mutate(row_id = substr(AreaName, nchar(AreaName) - 2, nchar(AreaName)))

#write.csv(ics_IMD_filtered,
         # file = "IMD_Fingertips.csv",
         # row.names = FALSE)
