##########################################
##########################################

library(dplyr)
library(ggplot2)
library(readr)

meps_data <- read.csv("01_data/data_input/meps_entry_exit.csv")
meps2_data <- read.csv("01_data/data_input/meps_activity_report.csv")
meps3_data <- read.csv("01_data/data_input/meps_entry_exit_committees.csv")


committee_data <- meps3_data %>%
  filter(!is.na(term))%>%
  filter(!is.na(Gender))%>%
  filter(role %in% c("Vice-Chair", "Chair"))


#for line of shares of women committee chairs AND vice chairs

percentage_chair_w <- committee_data %>%
  group_by(term) %>%
  summarise(percentage_chair_w = mean(Gender == "F")*100)


ggplot() +
  geom_line(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS", group =1)) +
  geom_point(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS")) +
  geom_line(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of Women Vice Chairs and Chairs",group=1))+
  geom_point(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of Women Vice Chairs and Chairs")) +
  labs(x = "Term", y = "Share")+
  scale_color_manual(values = c("Share of Women MEPS" = "red", "Share of Women Vice Chairs and Chairs" = "blue"))




ggsave("03_graphs/share_women_chair_vicechair.pdf")


#for line of shares of women committee chairs only

committee_data <- meps3_data %>%
  filter(!is.na(term))%>%
  filter(!is.na(Gender))%>%
  filter(role %in% c("Chair"))




percentage_chair_w <- committee_data %>%
  group_by(term) %>%
  summarise(percentage_chair_w = mean(Gender == "F")*100)


ggplot() +
  geom_line(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS", group =1)) +
  geom_point(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS")) +
  geom_line(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of Women Comittee Chair",group=1))+
  geom_point(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of Women Comittee Chair")) +
  labs(x = "Term", y = "Share")+
  scale_color_manual(values = c("Share of Women MEPS" = "red", "Share of Women Comittee Chair" = "blue"))




ggsave("03_graphs/share_women_chair.pdf")
