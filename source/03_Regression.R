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

#combined_df$date <- format(as.Date(combined_df$date), "%Y-%m")
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

df <- inhaler_output %>%
  group_by(row_id, date) %>%
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
  
    
model <- lm(Value ~ percent + prevelence + IMD_score, data = joined_df)
summary(model)

model <- lm(Montelukast_rate ~ percent + prevelence + IMD_score, data = joined_df)
summary(model)

plot <- ggplot(joined_df, aes(x = percent, y = Value)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Correlation between ICS+Formoterol Use and Hospitalisations",
    x = "% ICS+Formoterol Prescribing",
    y = "Hospitalisation Rate"
  ) +
  theme_minimal()

plot <- ggplot(joined_df, aes(x = percent, y = Montelukast_rate_Prev)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Correlation between ICS+Formoterol Use and Hospitalisations",
    x = "% ICS+Formoterol Prescribing",
    y = "Montelukast Rate"
  ) +
  theme_minimal()

plot <- ggplot(joined_df, aes(x = percent, y = Montelukast_rate)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Correlation between ICS+Formoterol Use and Hospitalisations",
    x = "% ICS+Formoterol Prescribing",
    y = "Montelukast Rate"
  ) +
  theme_minimal()


Montelukast_2324_df_ALL <- Montelukast %>%
  left_join(ics_size_df, by = c("row_id", "date")) %>%  
  left_join(
    prevelence %>%
      select(row_id, Value) %>%
      rename(prevelence = Value),
    by = "row_id"
  ) %>%
  mutate(
    monthly_rate = (items / total_list_size) * 10000,
    Montelukast_rate_Prev = (items / (total_list_size * prevelence)) * 10000  
  ) 


plot <- ggplot(Montelukast_2324_df_ALL, 
               aes(x = date, y = Montelukast_rate_Prev, group = row_id)) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
#  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  labs(
    title = "Montelukast Monthly Rate Over Time",
    x = "Date",
    y = "Montelukast Rate (per 10,000)"
  ) +
  theme_minimal()


joined_df <- df %>%
  left_join(hospitalisations, by = "row_id") %>%
  left_join(
    Montelukast_2324_df_ALL %>% 
      select(date, row_id, monthly_rate, Montelukast_rate_Prev),
    by = c("row_id", "date")
  ) %>%
  left_join(
    prevelence %>%
      select(row_id, Value) %>%         
      rename(prevelence = Value),
    by = "row_id"
  ) %>%
  left_join(
    IMD %>%
      select(row_id, IMD_score),
    by = "row_id"
  ) %>%
  mutate(
    percent = percent * 100,
    is_after_start = if_else(date >= as.Date("2024-11-01"), 1, 0)
  )

library(gsynth)

#run this section everytime changes are made
na.rm <- TRUE # List-wise delete missing data
index <- c("row_id", "date") # Specify the unit (group) and time indicators
force <- "two-way" # Impose both unit and time fixed effects
r <- c(0,5) # Allow for between 0 and 2 factors (as cross validating not technically necessary to specify)
CV <- TRUE # Cross validate (make FALSE if want to force specific number of factors)
se <- TRUE # Produce uncertainly estimates
nboots <- 10000 # Specify number of bootstrap runs
inference <- "parametric" # Parametric and non-parametric setting leads to warning messages due to sample size
cores <- detectCores() - 1 # Automatically detect number of available cores and leave one free
seed <- 42 # Set seed for bootstrapping for replicability
min.T0 <- 7 # At least 7 months of data pre intervention required

#define covariates and weights
weight_rates <- "total_list_size"

#add in "+ perc_m_18_95 + perc_65_95_of_18_95 + GPFTE"
covariates_rates <- ""

#ensure date is formatted correctly
joined_df$date <- as.Date(joined_df$date, format = "%d/%m/%Y")

model <- lme4::lmer(
  Montelukast_rate_Prev ~ date + is_after_start + (1 | row_id),
  data = joined_df
)