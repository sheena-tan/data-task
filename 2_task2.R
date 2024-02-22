#### Task 2 ----
#### bar graph for feelings_,

library(tidyverse)
library(here)
library(ggdist)
library(ggtext)

# load data
apology_clean <- read_rds(here("data/apology_clean.rds"))

# Part A ----
# data transformation
col.mean <- apology_clean |>
  select(starts_with("feelings"), -starts_with("feelings_DO"), -feelings_exp) |>
  apply(2, mean)

col.sd <- apology_clean |>
  select(starts_with("feelings"), -starts_with("feelings_DO"), -feelings_exp) |>
  apply(2, sd)

apology_means <- bind_rows(col.mean, col.sd) |>
  mutate(statistic = c("mean", "sd")) |>
  pivot_longer(starts_with("feelings"), names_to = "scenario") |>
  pivot_wider(names_from = statistic, values_from = value) |>
  mutate(
    sd_lower = mean - sd,
    sd_upper = mean + sd
  )

# bar graph desc
bar_graph_desc <- apology_means |>
  ggplot(aes(x = reorder(scenario, -mean), y = mean, fill = scenario)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = sd_lower, ymax = sd_upper), width = 0.15) +
  theme_ggdist() +
  scale_fill_manual(values = c("#d95f02", "#d95f02", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77")) +
  theme(
    plot.subtitle = element_markdown(lineheight = 0.1),
    legend.position = "none"
  ) +
  scale_x_discrete(
    labels = c("them first", "you first", "them alone", "forgive only",
               "neither", "you alone")
  ) +
  labs(
    x = "apology scenario",
    y = "emotional reaction rating",
    subtitle = "<b style='color: #d95f02;'>Mutual apologies</b> feel better than
    <b style='color: #1b9e77;'>one-sided</b> ones or <b style='color: #1b9e77;'>none at all</b>.
    \n<b>Not giving</b> a return apology does not feel quite as bad as <b>not getting</b> one."
  )


# Part B
# Conduct a one way ANOVA to determine if there are differences in feelings across the six scenarios. Then perform pairwise t-tests to compare feelings_youalone to the other five scenarios. Describe your conclusions in 1-2 sentences.
#
# Part C
# Create a graph showing the proportion of people choosing each of the different options for the following variable: outcome_binary1 Conduct a test to determine if the proportion differences across the answers are significantly different from one another.
#
# Part D
# Natural Language Processing (NLP) exercise: Find a way to analyze the sentiment and/or emotions present in the free form text responses in the “describe” variable. Describe your observations in a few sentences.

# write out plots
ggsave(here("results/bar_graph_desc.png"), bar_graph_desc)
