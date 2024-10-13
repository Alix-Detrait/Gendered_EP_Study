##########################################
##########################################
library(dplyr)
library(ggplot2)
library(readr)

meps_data <- read.csv("01_data/data_input/meps_entry_exit.csv")
meps2_data <- read.csv("01_data/data_input/meps_activity_report.csv")
meps3_data <- read.csv("01_data/data_input/meps_entry_exit_committees.csv")
meps4_data <- read.csv("01_data/data_input/meps_entry_exit_groups_charact.csv")


#for line of shares of women political group chairs


epg_data <- meps4_data %>%
  filter(!is.na(term))%>%
  filter(!is.na(Gender))%>%
  filter(role %in% c("Chair"))


percentage_chair_w <- epg_data %>%
  group_by(term) %>%
  summarise(percentage_chair_w = mean(Gender == "F")*100)


ggplot() +
  geom_line(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS", group =1)) +
  geom_point(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS")) +
  geom_line(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of EP Political Group Chairs who are women",group=1))+
  geom_point(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of EP Political Group Chairs who are women")) +
  labs(x = "Term", y = "Share")+
  scale_color_manual(values = c("Share of Women MEPS" = "red", "Share of EP Political Group Chairs who are women" = "blue"))


ggsave("03_graphs/share_women_pg_chair.pdf")


##########################################
##########################################

#for line of shares of women political group chairs AND vice chairs



epg_data <- meps4_data %>%
  filter(!is.na(term))%>%
  filter(!is.na(Gender))%>%
  filter(role %in% c("Vice-Chair", "Chair"))


percentage_chair_w <- epg_data %>%
  group_by(term) %>%
  summarise(percentage_chair_w = mean(Gender == "F")*100)


ggplot() +
  geom_line(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS", group =1)) +
  geom_point(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS")) +
  geom_line(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of EPG Chair or Vice-Chair who are women",group=1))+
  geom_point(data = percentage_chair_w, aes(x = factor(term), y = percentage_chair_w, color = "Share of EPG Chair or Vice-Chair who are women")) +
  labs(x = "Term", y = "Share")+
  scale_color_manual(values = c("Share of Women MEPS" = "red", "Share of EPG Chair or Vice-Chair who are women" = "blue"))

ggsave("03_graphs/share_women_pg_chair_vicechair.pdf")

