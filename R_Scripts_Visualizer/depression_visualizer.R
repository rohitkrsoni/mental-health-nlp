library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

posts <- read_csv("./data/depression_22_25_wf.csv")
posts
# I want created_at after id column

posts <- posts %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

posts <- posts %>%
  select(id, created_at, everything())
posts

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
posts <- posts %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count posts per month
monthly_counts <- posts %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of Posts"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Step 1: Add month and simplified status
posts <- posts %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y"),
    status = case_when(
      is.na(selftext) | selftext %in% c("[removed]", "[deleted]") ~ "not present",
      TRUE ~ "present"
    )
  )

# Step 2: Count posts by month and status
monthly_status_counts <- posts %>%
  count(month_start, month_year, status) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))

monthly_status_counts

# Step 3: Plot stacked bar chart with just two groups
ggplot(monthly_status_counts, aes(x = month_year, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Monthly Post Counts (Real Text vs Removed/Deleted)",
    x = "Month",
    y = "Number of Posts",
    fill = "Selftext Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Only keep the posts with real text

posts.v1 <- posts %>%
  filter(status == "present")

posts.v1
skim(posts.v1)

# Step 2: Count posts per month
monthly_counts <- posts.v1 %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of Posts"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Now we will see how many posts in each month have missing ups or downs

posts.v1 <- posts.v1 %>%
  mutate(
    missing_votes = is.na(ups) | is.na(downs)
  )

monthly_missing_ups <- posts.v1 %>%
  count(month_start, month_year, missing_votes) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))

monthly_missing_ups 

# Step 3: Plot
ggplot(monthly_missing_ups, aes(x = month_year, y = n, fill = missing_votes)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Monthly Post Counts with Missing Ups or Downs",
    x = "Month",
    y = "Number of Posts with Missing Ups or Downs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Remove the ups and downs column as post before apr 2023 do not have these columns

posts.v2 <- posts.v1 %>%
  select(-ups, -downs)

posts.v2

# remove other unnecessary columns
skim(posts.v2)


# No missing in any columns, and we will now remove unnecessary columns

posts.v3 <- posts.v2 %>%
  select(-subreddit, -month_year, -selftext_status, -status, -month, -missing_votes, -score, -created_utc, -day, -url, -month_start, -year)

posts.v3

# Now we will save the cleaned data
write_csv(posts.v3, "./data/depression_posts_final.csv")

skim(posts.v3)



depressed_mood_posts <- read_csv("./data/depressive_mood_v1.csv")
depressed_mood_posts




depressed_mood_posts <- depressed_mood_posts %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

depressed_mood_posts <- depressed_mood_posts %>%
  select(id, created_at, everything())
depressed_mood_posts

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
depressed_mood_posts <- depressed_mood_posts %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count posts per month
monthly_counts <- depressed_mood_posts %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of Posts"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


depressed_mood_posts


# add column symptom and fill "depressed_mood"
depressed_mood_posts <- depressed_mood_posts %>%
  mutate(
    symptom = "depressed_mood"
  )


# remove unnecessary columns

depressed_mood_posts <- depressed_mood_posts %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year, -ups, -downs)


depressed_mood_posts


# Save the data
write_csv(depressed_mood_posts, "./data/depressed_mood_posts_final_v1.csv")

depressed_mood_posts

# Filter 2024 posts
depressed_mood_2024 <- depressed_mood_posts %>%
  filter(year == 2024)

# Randomly sample 10K from full 2024 data
set.seed(42)
sampled_depressed_mood <- depressed_mood_2024 %>%
  sample_n(10000)

sampled_depressed_mood


sampled_depressed_mood <- sampled_depressed_mood %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)


sampled_depressed_mood
# Save the sampled data

write_csv(sampled_depressed_mood, "./data/depressed_mood_10K_v1.csv")


# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
sampled_depressed_mood <- sampled_depressed_mood %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count posts per month
monthly_counts <- sampled_depressed_mood %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of Posts"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


monthly_counts


depressed_mood_10K <- depressed_mood_posts %>%
  arrange(desc(created_at)) %>%
  head(10000)

depressed_mood_10K







# save the 10K data
write_csv(depressed_mood_10K, "./data/depressed_mood_10K.csv")
