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


# Part B ----
# data transformation
apology_b <- apology_clean |>
  select(ResponseId, starts_with("feelings"), -starts_with("feelings_DO"),
         -feelings_exp) |>
  pivot_longer(
    cols = starts_with("feelings"),
    names_to = "scenario"
  )

# one way anova
anova_all <- aov(value ~ scenario, data = apology_b)
summary(anova_all)

# pairwise t-test by initiator type
apology_b1 <- apology_b |>
  filter(scenario == "feelings_youalone" | scenario == "feelings_bothyoufirst")
apology_b2 <- apology_b |>
  filter(scenario == "feelings_youalone" | scenario == "feelings_themalone")
apology_b3 <- apology_b |>
  filter(scenario == "feelings_youalone" | scenario == "feelings_boththemfirst")
apology_b4 <- apology_b |>
  filter(scenario == "feelings_youalone" | scenario == "feelings_neither")
apology_b5 <- apology_b |>
  filter(scenario == "feelings_youalone" | scenario == "feelings_youaloneforgiven")

t.test(formula = value ~ scenario, data = apology_b1, paired = TRUE)
t.test(formula = value ~ scenario, data = apology_b2, paired = TRUE)
t.test(formula = value ~ scenario, data = apology_b3, paired = TRUE)
t.test(formula = value ~ scenario, data = apology_b4, paired = TRUE)
t.test(formula = value ~ scenario, data = apology_b5, paired = TRUE)


# Part C
# data transformation
apology_c <- apology_clean |>
  group_by(outcome_binary1) |>
  summarize(n = n()) |>
  mutate(prop = n / sum(n))

# bar graph of proportions
bar_graph_outcome <- apology_c |>
  ggplot(aes(outcome_binary1, prop, fill = outcome_binary1)) +
  geom_bar(stat = "identity") +
  theme_ggdist() +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  theme(
    plot.subtitle = element_markdown(lineheight = 0.1),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = c("you first", "neither")) +
  annotate("text", x = 1, y = 0.4, label = "0.783", color = "white") +
  annotate("text", x = 2, y = 0.1, label = "0.217", color = "white") +
  labs(
    x = "apology scenario",
    y = "proportion of participants",
    subtitle = "Participants preferred <b style='color: #d95f02;'>apologizing first</b>
    over <b style='color: #1b9e77;'>no one apologizing</b> at all."
  )

# one prop z test
prop.test(x = 36, n = 46, p = 0.5, correct = FALSE)

# Part D
# Find a way to analyze the sentiment and/or emotions present in the free form text responses in the “describe” variable. Describe your observations in a few sentences.

# write out plots
ggsave(here("results/bar_graph_desc.png"), bar_graph_desc)
ggsave(here("results/bar_graph_outcome.png"), bar_graph_outcome)

