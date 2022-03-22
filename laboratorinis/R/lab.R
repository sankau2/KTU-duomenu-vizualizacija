duom = read.csv("data/lab_sodra.csv")
duom
library(tidyverse)
library(dplyr)
#1
data = duom %>%
  filter(ecoActCode == 467300)
summary(data)
data %>%
  ggplot(aes(x=avgWage)) +
  theme_minimal() +
  geom_histogram(fill = "pink", col = "black", bins = 100) +
  labs(title = "Awerage wage of employees")

#2
top5 = data %>%
  group_by(name) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

top5full = data %>% filter(name %in% top5$name)

top5full %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  geom_line(aes(colour = name))+
  labs(title = "Top 5 Companies with Highest Average Wage", x = "Month", y = "Average Wage")

#3

top5full %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE) %>%
  head(5) %>%
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) +
  geom_col(aes(fill = name)) +
  theme(axis.text.x = element_blank()) +
  theme_minimal() +
  labs(title = "Number of insured employees", x = "Company", y = "Count")