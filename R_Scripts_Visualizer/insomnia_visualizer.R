library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

insomnia <- read_csv("./data/insomnia.csv")



insomnia <- insomnia %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

insomnia <- insomnia %>%
  select(id, created_at, everything())
insomnia

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
insomnia <- insomnia %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count insomnia per month
monthly_counts <- insomnia %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of insomnia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# add column symptom and fill "insomnia"

insomnia <- insomnia %>%
  mutate(
    symptom = "insomnia"
  )

insomnia <- insomnia %>%
  filter(!is.na(selftext))



insomnia

# Filter 2024 posts
insomnia_2024 <- insomnia %>%
  filter(year == 2024)

insomnia_2024

# remove unecessary columns
insomnia <- insomnia %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)



# Save the modified insomnia data
write_csv(insomnia, "./data/insomnia_final_v1.csv")

# Display the insomnia data
insomnia


# remove unecessary columns
insomnia_2024 <- insomnia_2024 %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)




# Recent 10K posts 
# Randomly sample 10K from full 2024 data
set.seed(42)
sampled_insomnia <- insomnia_2024 %>%
  sample_n(10000)

# save the recent 10K posts

write_csv(sampled_insomnia, "./data/insomnia_10K_v1.csv")

insomnia_10K


sampled_insomnia
