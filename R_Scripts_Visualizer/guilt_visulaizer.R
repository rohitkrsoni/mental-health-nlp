library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

guilt <- read_csv("./data/guilt.csv")



guilt <- guilt %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

guilt <- guilt %>%
  select(id, created_at, everything())
guilt

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
guilt <- guilt %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count guilt per month
monthly_counts <- guilt %>%
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

skim(guilt)



# add column symptom and fill "guilt"

guilt <- guilt %>%
  mutate(
    symptom = "guilt"
  )

guilt

# Filter 2024 posts
guilt_2024 <- guilt %>%
  filter(year == 2024)

# Randomly sample 10K from full 2024 data
set.seed(42)
sampled_guilt <- guilt_2024 %>%
  sample_n(10000)

sampled_guilt





# Step 2: Count guilt per month
monthly_counts_2024 <- sampled_guilt %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts_2024, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of guilt"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# remove unecessary columns
guilt <- guilt %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)

guilt

# Save the modified guilt data
write_csv(guilt, "./data/guilt_final.csv")

# Display the guilt data
guilt

# Recent 10K posts 

guilt_10K <- guilt %>%
  arrange(desc(created_at)) %>%
  head(10000)

# save the recent 10K posts

sampled_guilt

write_csv(sampled_guilt, "./data/guilt_10K_v1.csv")

guilt_10K
