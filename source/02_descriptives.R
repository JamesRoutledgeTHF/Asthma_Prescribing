library(ggplot2)
library(scales)
library(lubridate)
library(dplyr)

# read in the output file
inhaler_output <- read.csv("inhaler_output.csv") %>% 
  select(-row_name.x) %>% 
  rename(row_name = row_name.y) %>% 
  mutate(date = as.Date(as.Date(paste0(inhaler_output$date, "-01"))))

# Function to save a plot as a PNG
save_plot <- function(plot, filename) {
  ggsave(filename, plot, width = 9, height = 6, units = "in")
}

# Items by category
monthly_cat <- inhaler_output %>%
  group_by(date, category) %>%
  summarize(total_items = sum(items), .groups = 'drop')

plot1 <- ggplot(monthly_cat, aes(x = date, y = total_items, group = category, colour = category)) + 
  geom_line(size = 0.5) + 
  geom_point() +
  labs(title = "Prescriptions by inhaler type",
       y = "number of prescriptions",
       x = element_blank()) +
  theme_classic() +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "4 months", date_labels = "%y-%m")

print(plot1)

save_plot(plot1, "asthma project graphs/items_by_category.png")


# Items by chemical
monthly_chem <- inhaler_output %>%
  group_by(date, chemical) %>%
  summarize(total_items = sum(items), .groups = 'drop')

plot2 <- ggplot(monthly_chem, aes(x = date, y = total_items, group = chemical, colour = chemical)) + 
  geom_line(size = 0.5) + 
  geom_point() +
  labs(title = "Prescriptions by active component",
       y = "number of prescriptions",
       x = element_blank()) +
  theme_classic() +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "8 months", date_labels = "%y-%m")

print(plot2)

save_plot(plot2, "asthma project graphs/items_by_chemical.png")

# Total items monthly
total_items_monthly <- inhaler_output %>%
  group_by(date) %>%
  summarize(total_items = sum(items), total_list_size = sum(total_list_size), .groups = 'drop')

plot3 <- ggplot(total_items_monthly, aes(x = date, y = total_items)) +
  geom_line(size = 0.5, color = "#2C3E50") +
  geom_point(size = 1, color = "#2C3E50") +
  labs(title = "Total Monthly Inhaler Prescriptions", x = "Date", y = "Total Items") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "6 months", date_labels = "%y-%m") +
  theme_classic() +
  ylim(c(1500000, 2500000))

print(plot3)

save_plot(plot3, "asthma project graphs/total_monthly_items.png")

# Prepare extended dataframe - monthly category by ICB rate and percent
monthly_cat_ICB_percent <- inhaler_output %>% 
  group_by(row_id, date, category) %>% 
  summarize(items = sum(items), total_list_size = first(total_list_size), .groups = 'drop') %>% 
  mutate(rate = (items / total_list_size) * 10000) %>% 
  group_by(row_id, date) %>% 
  mutate(total_items = sum(items)) %>% 
  ungroup() %>%
  mutate(percent = (items / total_items) * 100)

# Rate by Category
rate_by_category <- monthly_cat_ICB_percent %>%
  group_by(date, category) %>%
  summarize(rate = mean(rate), .groups = 'drop')

plot_rate_category <- ggplot(rate_by_category, aes(x = date, y = rate, group = category, colour = category)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  labs(title = "Rate of Prescriptions by Inhaler Type",
       x = element_blank(),
       y = "Rate per 10,000 patients") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "6 months", date_labels = "%y-%m") +
  theme_classic()

print(plot_rate_category)

save_plot(plot_rate_category, "asthma project graphs/rate_by_category.png")

# Rate by Chemical

rate_by_chemical <- monthly_chem_ICB_percent %>%
  group_by(date, chemical) %>%
  summarize(rate = mean(rate), .groups = 'drop')

plot_rate_chemical <- ggplot(rate_by_chemical, aes(x = date, y = rate, group = chemical, colour = chemical)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  labs(title = "Rate of Prescriptions by Chemical",
       x = element_blank(),
       y = "Rate per 10,000 patients") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "6 months", date_labels = "%y-%m") +
  theme_classic()

print(plot_rate_chemical)

save_plot(plot_rate_chemical, "asthma project graphs/rate_by_chemical.png")

# Percentage of combined inhaler total
plot5_df <- monthly_cat_ICB_percent %>%
  filter(category == "ICS+LABA") %>% 
  group_by(date) %>% 
  summarise(
    percent = mean(percent)
  )

plot5 <- ggplot(plot5_df, aes(x = date, y = percent)) +
  geom_line(size = 1) +
  labs(title = "ICS+LABA Inhaler Prescriptions Monthly", x = element_blank(), y = "Percentage") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "4 months", date_labels = "%y-%m") +
  theme_classic() +
  ylim(c(0,100))

print(plot5)

save_plot(plot5, "asthma project graphs/combined_percentage.png")

# Percentage of combined inhaler by ICB
plot6_df <- monthly_cat_ICB_percent %>%
  filter(category == "ICS+LABA") %>% 
  group_by(date, row_id) %>% 
  summarise(
    percent = mean(percent)
  )


# Items by ICB
plot6 <- ggplot(monthly_cat_ICB_percent, aes(x = date, y = items, group = row_id, colour = row_id)) +
  geom_line() +
  facet_wrap(~category) +
  labs(title = "Items by ICB", x = element_blank(), y = "Items") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "8 months", date_labels = "%y-%m")

print(plot6)

save_plot(plot6, "asthma project graphs/items_by_ICB.png")



# Rate by ICB
plot7 <- ggplot(monthly_cat_ICB_percent, aes(x = date, y = rate, group = row_id, colour = row_id)) +
  geom_line() +
  facet_wrap(~category) +
  labs(title = "Rate by ICB", x = "Date", y = "Rate") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "7 months", date_labels = "%y-%m")

print(plot7)

save_plot(plot7, "asthma project graphs/rate_by_ICB.png")

# Identify ICBs with the highest and lowest proportion of combined inhaler prescription in 2025
latest_date <- max(plot6_df$date)

top_bottom_ids <- plot6_df %>%
  filter(date == latest_date) %>%
  arrange(desc(percent)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5 | rank > n() - 5) %>%
  mutate(group = ifelse(rank <= 5, "Top 5", "Bottom 5")) %>%
  select(row_id, group)

# Plot only top and bottom 5 ICBs
library(ggsci)
plot6_filtered <- ggplot(plot6_df %>% filter(row_id %in% top_bottom_ids$row_id), 
                         aes(x = date, y = percent, group = row_id, color = row_id)) +
  geom_line(size = 1) +
  labs(title = "ICS+LABA Inhaler Prescriptions Monthly by ICB", 
       x = element_blank(), 
       y = "Percentage",
       color = "ICB") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "4 months", date_labels = "%y-%m") +
  scale_color_viridis_d() +
  theme_classic() +
  geom_vline(xintercept = as.Date("2024-11-01"),
             linetype = "dashed",
             color = "darkgrey",
             linewidth = 1
  ) +
  ylim(c(25,75))

print(plot6_filtered)

save_plot(plot6_filtered, "asthma project graphs/combined_percentage_ICB_top_bottom.png")

# Percentage of SABA inhaler by ICB
plot8_df <- monthly_cat_ICB_percent %>%
  filter(category == "SABA") %>% 
  group_by(date, row_id) %>% 
  summarise(
    percent = mean(percent)
  )

library(ggsci)

plot8 <- ggplot(plot8_df, 
                aes(x = date, y = percent, group = row_id, color = row_id)) +
  geom_line(size = 1) +
  labs(title = "SABA Inhaler Prescriptions Monthly by ICB", 
       x = element_blank(), 
       y = "Percentage",
       color = "ICB") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "4 months", date_labels = "%y-%m") +
  scale_color_viridis_d() +
  theme_classic() +
  geom_vline(xintercept = as.Date("2024-11-01"),
             linetype = "dashed",
             color = "darkgrey",
             linewidth = 1
  ) +
  ylim(c(0, 50))

print(plot8)

save_plot(plot8, "asthma project graphs/saba_percentage_ICB.png")

# Stacked percentages of inhaler prescriptions

# Aggregate percent by category per month
category_percent_df <- monthly_cat_ICB_percent %>%
  group_by(date, category) %>%
  summarise(percent = mean(percent), .groups = 'drop')

inhalers <- c("LABA", "SABA", "ICS", "ICS+LABA")

# Create the stacked area chart
plot_stacked_percent <- ggplot(category_percent_df, 
                               aes(x = date, y = percent, fill = factor(category, levels = inhalers))) +
  geom_area(position = "stack") +
  labs(title = "Percentage of Inhaler Prescriptions by Category Over Time",
       x = element_blank(),
       y = "Percentage",
       fill = "Inhaler type") +
  scale_x_date(limits = as.Date(c("2020-05-01", "2025-04-01")),
               date_breaks = "4 months", date_labels = "%y-%m") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Paired") +
  theme_classic()

print(plot_stacked_percent)

save_plot(plot_stacked_percent, "asthma project graphs/percentage_by_category_stacked.png")