library(ggplot2)
library(readxl)
library(readr)

hospitalisations <- read.csv("Hospitalisations_Fingertips.csv")
prevelence <- read.csv("Prevalence_Fingertips.csv")
IMD <- read_excel("C:/Users/James.Routledge/The Health Foundation/Data Analytics - 10-IAU/1. Work Programmes/Asthma project/ICS_IMD.xlsx")
Montelukast <- read.csv("C:/Users/James.Routledge/The Health Foundation/Data Analytics - 10-IAU/1. Work Programmes/Asthma project/Montelukast.csv")

inhaler_output <- read.csv("inhaler_output.csv")%>%
  select(-row_name.x) %>%
  rename(row_name = row_name.y) %>%
  mutate(date = as.Date(paste0(date, "-01")))

formoterol_chems <- c(
  "Beclometasone+ Formoterol",
  "Budesonide+Formoterol",
  "Fluticasone+Formoterol",
  "Fluticasone + Formoterol"
)

inhaler_output <- inhaler_output %>%
  mutate(
    category = case_when(
      category == "ICS+LABA" & chemical %in% formoterol_chems ~ "ICS+FORMOTEROL",
      category == "ICS+LABA"                                   ~ "Other ICS+LABA",
      TRUE                                                     ~ category
    )
  )

ics_size_df <- read_csv("C:/Users/James.Routledge/The Health Foundation/Data Analytics - 10-IAU/1. Work Programmes/Asthma project/ICSsize.csv")
ics_size_df$date <- format(as.Date(ics_size_df$date), "%Y-%m")


fy_2324_df <- inhaler_output %>%
  filter(date >= as.Date("2023-04-01"),
         date <= as.Date("2024-03-31")) %>% 
  group_by(row_id) %>%
  summarise(
    total_items = sum(items, na.rm = TRUE),
    ics_formo_items = sum(items[category == "ICS+FORMOTEROL"], na.rm = TRUE),
    percent = ics_formo_items / total_items,
    .groups = "drop"
  )

Montelukast <- Montelukast %>%
  mutate(
    date = as.Date(paste0(date, "-01"))
  )

ics_size_df <- ics_size_df %>%
  mutate(
    date = as.Date(paste0(date, "-01"))
  )

Montelukast_2324_df <- Montelukast %>%
  left_join(ics_size_df, by = c("row_id", "date")) %>%  
  left_join(
    prevelence %>%
      select(row_id, Value) %>%
      rename(prevelence = Value),
    by = "row_id"
  ) %>%
  filter(date >= as.Date("2023-04-01"),
         date <= as.Date("2024-03-31")) %>%
  mutate(
    monthly_rate = (items / total_list_size) * 10000,
    Montelukast_rate_Prev = (items / (total_list_size * prevelence)) * 10000  
  ) %>%
  group_by(row_id) %>%
  summarise(
    Montelukast_rate = mean(monthly_rate, na.rm = TRUE),
    Montelukast_rate_Prev = mean(Montelukast_rate_Prev, na.rm = TRUE),
    .groups = "drop"
  )


fy_2324_df <- inhaler_output %>%
  filter(date >= as.Date("2023-04-01"),
         date <= as.Date("2024-03-31")) %>% 
  group_by(row_id) %>%
  summarise(
    total_items = sum(items, na.rm = TRUE),
    ics_formo_items = sum(items[category == "ICS+FORMOTEROL"], na.rm = TRUE),
    percent = ics_formo_items / total_items,
    .groups = "drop"
  )

joined_df <- fy_2324_df %>%
  left_join(hospitalisations, by = "row_id") %>%
  left_join(Montelukast_2324_df %>% 
              select(row_id, Montelukast_rate, Montelukast_rate_Prev)  
            , by = "row_id") %>%
  left_join(
    prevelence %>%
      select(row_id, Value) %>%         
      rename(prevelence = Value),        
    by = "row_id"
  )%>%
  left_join(
    IMD %>%
      select(row_id, IMD_score),      
    by = "row_id"
  )%>%
  mutate(percent = percent * 100) 

plot_prescribing_and_hospitalisations <- ggplot(joined_df, aes(x = percent, y = Value)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Correlation between ICS+Formoterol Use and Hospitalisations",
    x = "% ICS+Formoterol Prescribing",
    y = "Hospitalisation Rate"
  ) +
  theme_minimal()

