library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

depression_2y <- read_csv("./data/depression_2y.csv")

depression_2y

depression_2y <- depression_2y %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

depression_2y <- depression_2y %>%
  select(id, created_at, everything())
depression_2y

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
depression_2y <- depression_2y %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count guilt per month
monthly_counts <- depression_2y %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of guilt"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

skim(depression_2y)











# remove unecessary columns
depression_2y <- depression_2y %>%
  select(-month, -created_utc, -day, -year, -month_start, -month_year)

depression_2y

# Save the modified guilt data
write_csv(depression_2y, "./data/depression_2y_v1.csv")
# End of script


