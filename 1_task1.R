#### Task 1 ----
#### feelings with/without apology, variation by initiator type, forgiveness

library(tidyverse)
library(here)

# load data
apology_clean <- read_rds(here("data/apology_clean.rds"))
apology_long_q1 <- read_rds(here("data/apology_long_q1.rds"))
apology_long_q3 <- read_rds(here("data/apology_long_q3.rds"))
apology_wide <- read_rds(here("data/apology_wide.rds"))

# QUESTION 1 ----

# summary statistics
apology_long_q1 |> group_by(apology_returned) |>
  summarize(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

# pairwise t-test
t.test(value ~ apology_returned, apology_long_q1, paired = TRUE)


# QUESTION 2 ----

# summary statistics by initiator type
apology_long_q1 |>
  group_by(initiator_type, apology_returned) |>
  summarize(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

# pairwise t-test by initiator type
t.test(value ~ apology_returned, apology_long_q1, paired = TRUE,
       subset = initiator_type == "always")

t.test(value ~ apology_returned, apology_long_q1, paired = TRUE,
       subset = initiator_type == "conditional")

t.test(value ~ apology_returned, apology_long_q1, paired = TRUE,
       subset = initiator_type == "never")

# one-way ANOVA
anova_initiator <- aov(.res ~ initiator_type, data = apology_wide)

# Tukey's test
TukeyHSD(anova_initiator)


# QUESTION 3 ----

# summary statistics
apology_long_q3 |> group_by(apology_returned) |>
  summarize(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

# pairwise t-test
t.test(value ~ apology_returned, apology_long_q3, paired = TRUE)
