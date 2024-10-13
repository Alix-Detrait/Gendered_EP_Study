##########################################
##########################################
#first creating a bar graph of the share of women MEPS per term


library(dplyr)
library(ggplot2)
library(readr)


meps_data <- read.csv("01_data/data_input/meps_entry_exit.csv")


#first sorting through the data to only include entries where gender and terms are specified
gender_data <- meps_data %>%
  filter(!is.na(term)) %>%
  filter(!is.na(Gender))


#calculating the percentage of women MEPs per term
percentage_women <- gender_data %>%
  group_by(term) %>%
  summarise(percentage_women = mean(Gender == "F") * 100)


#making a bar plot of the share of women MEPs grouped by term
ggplot(percentage_women, aes(x = factor(term), y = percentage_women)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(x = "Term", y = "Percentage of Women Members", title = "Percentage of Women Members of Parliament per Term")


ggsave("03_graphs/share_women_EP.pdf")
