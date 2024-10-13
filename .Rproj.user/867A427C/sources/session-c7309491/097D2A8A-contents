##########################################
##########################################
#making a line graph comparing the share of reports written by women and share of women MEPS per term


library(dplyr)
library(ggplot2)
library(readr)

meps_data <- read.csv("01_data/data_input/meps_entry_exit.csv")


#for line of shares of women MEPS
gender_data <- meps_data %>%
  filter(!is.na(term)) %>%
  filter(!is.na(Gender))


percentage_women <- gender_data %>%
  group_by(term) %>%
  summarise(percentage_women = mean(Gender == "F") * 100)




#for line of shares of reports written by women
meps2_data <- read.csv("01_data/data_input/meps_activity_report.csv")




meps_data$term
meps2_data$term
colnames(meps_data)
colnames(meps2_data)


report_data <- meps2_data %>% left_join(unique(meps_data %>% select(UserID, Gender, term)), by=join_by("mep_id"=="UserID", "term")) %>%
  filter(!is.na(Gender))




percentage_reports_w <- report_data %>%
  group_by(term) %>%
  summarise(percentage_reports_w = mean(Gender == "F")*100)


ggplot() +
  geom_line(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS", group =1)) +
  geom_point(data = percentage_women, aes(x = factor(term), y = percentage_women, color = "Share of Women MEPS")) +
  geom_line(data = percentage_reports_w, aes(x = factor(term), y = percentage_reports_w, color = "Share of Reports Written by Women",group=1))+
  geom_point(data = percentage_reports_w, aes(x = factor(term), y = percentage_reports_w, color = "Share of Reports Written by Women")) +
  labs(x = "Term", y = "Share")+
  scale_color_manual(values = c("Share of Women MEPS" = "red", "Share of Reports Written by Women" = "blue"))




ggsave("03_graphs/share_reports_women.pdf")


#note that we only have data on reports written from term 4 onwards
