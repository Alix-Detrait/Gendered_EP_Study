##########################################
##########################################
#Load the dplyr (for data wrangling), readr (reading dataset) and ggplot2 (graphs) libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


##########################################
##########################################
#I) Load the dataset
meps_survey_allwaves_q2_7 <- read_csv("01_data/data_input/meps_survey_allwaves.csv") 




##########################################
##########################################
#II) Clean the main dataset 
meps_survey_allwaves_q2_7 <- meps_survey_allwaves_q2_7 %>% 
  select(b_3, q2_7_1, q2_7_2,q2_7_3,q2_7_4,q2_7_5,q2_7_6,q2_7_7,q2_7_9,q1_7) %>% #select only these variables
  rename("legislature" = "b_3",
         "gender" = "q1_7") %>% #b_3 and q1_7 variables into legislature and gender
  mutate(legislature = case_when(legislature=="1" ~ "5th legislature",
                                 legislature=="2" ~ "6th legislature",
                                 legislature=="3" ~ "7th legislature",
                                 legislature=="4" ~ "8th legislature"), # recode the legislature variable
         across(starts_with("q2"), as.numeric), #transform every variable starting with q2 from string to numeric 
         gender = as.numeric(gender), #transform the gender variable from string to numeric
         gender = if_else(gender==1, "male", "female")) %>% #recode the gender variable from 0 1 to male female
  filter(!if_all(starts_with("q2"), ~ is.na(.))) %>% #drop lines for which all variables starting with q2 are NA 
  mutate(across(starts_with("q2"), ~ replace_na(.,0))) #for all variables starting with q2 replace . with 0


##########################################
##########################################
#III) Create the dataframe for the consolidated graph (every legislature and both genders merged)


df_graph <- meps_survey_allwaves_q2_7 %>% select(-c(legislature, gender)) %>% #drop the legislature and gender variables
  summarise(across(starts_with("q2"), list(sum = sum, mean = mean, sd = sd), na.rm=TRUE)) %>% #create a new dataset with the sum, mean, and standard deviation ofevery variable starting with q2 
  pivot_longer(cols=starts_with("q2")) %>% #translate the dataframe by using the names of columns starting with q2 as the identifiers of lines in the new dataset
  mutate(statistic = substr(name, 8 ,nchar(name)), #create a new columns named "statistic" with correspond the the last characters of the variable name (i.e sum, mean or sd)
         name = substr(name, 1 ,6)) %>% #same as above but keep only first characters
  pivot_wider(names_from = statistic, values_from = value) %>%  #create new dataset where the "name" column is the unique row identifier are the value of statistc variable becomes column names
  mutate(name = case_when(name == "q2_7_1" ~ "Member of the EP", 
                          name == "q2_7_2" ~ "Chair of an EP committee",
                          name == "q2_7_3" ~"Chair of an EP party group",
                          name == "q2_7_4" ~ "Member of national parliament",
                          name == "q2_7_5" ~ "Member of national government",
                          name == "q2_7_6" ~ "European Commissioner",
                          name == "q2_7_7" ~ "Retired from public life",
                          name == "q2_7_9" ~ "Other")) #recode the variable name in their actual meaning


ggplot() + geom_col(data = df_graph, aes(x=name, y=mean)) + theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  labs(y = "Share of respondent mentionning this option", x = "Situation 10 years from now")






##########################################
##########################################
#IV) Create the dataframe for the by-legislature graph (without error bars)


#This code is very close to the one above, the only difference is that we group observations by legislature so as to obtain one statistic by legislature
df_graph <- meps_survey_allwaves_q2_7 %>% select(-gender) %>% 
  group_by(legislature) %>%  #we group by legislature so as to compute each statistics by legislature
  summarise(across(starts_with("q2"), list(sum = sum, mean = mean, sd = sd), na.rm=TRUE)) %>% 
  pivot_longer(cols=starts_with("q2")) %>%
  mutate(statistic = substr(name, 8 ,nchar(name)),
         name = substr(name, 1 ,6)) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(name = case_when(name == "q2_7_1" ~ "Member of the EP",
                          name == "q2_7_2" ~ "Chair of an EP committee",
                          name == "q2_7_3" ~"Chair of an EP party group",
                          name == "q2_7_4" ~ "Member of national parliament",
                          name == "q2_7_5" ~ "Member of national government",
                          name == "q2_7_6" ~ "European Commissioner",
                          name == "q2_7_7" ~ "Retired from public life",
                          name == "q2_7_9" ~ "Other")) 


ggplot() + geom_col(data = df_graph, aes(x=legislature, y=mean, fill=name), position="dodge2") +
  labs(y = "Share of respondent mentionning this option", x = "Legislature", fill = "Situation 10 years from now")




##########################################
##########################################
#V) Create the dataframe for the by-gender graph (with error bars)


df_graph <- meps_survey_allwaves_q2_7 %>% filter(!is.na(gender)) %>% select(-legislature) %>%
  group_by(gender) %>%  #we group by gender so as to compute each statistics by gender
  summarise(across(starts_with("q2"), list(sum = sum, mean = mean, sd = sd), na.rm=TRUE)) %>% 
  pivot_longer(cols=starts_with("q2")) %>%
  mutate(statistic = substr(name, 8 ,nchar(name)),
         name = substr(name, 1 ,6)) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(name = case_when(name == "q2_7_1" ~ "Member of the EP",
                          name == "q2_7_2" ~ "Chair of an EP committee",
                          name == "q2_7_3" ~"Chair of an EP party group",
                          name == "q2_7_4" ~ "Member of national parliament",
                          name == "q2_7_5" ~ "Member of national government",
                          name == "q2_7_6" ~ "European Commissioner",
                          name == "q2_7_7" ~ "Retired from public life",
                          name == "q2_7_9" ~ "Other"),
         lower = mean - qt(0.975, df = 1000) * sd/sqrt(sum) , #this is a formula that you may already have study in proba to compute confidence intervals
         upper = mean + qt(0.975, df = 1000) * sd/sqrt(sum)) #same as above




ggplot(data = df_graph, aes(x=name, y=mean, fill=gender)) + geom_col(position="dodge2") +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  labs(y = "Share of respondent mentionning this option", x = "Legislature", fill = "Situation 10 years from now")
#geom_errorbar(aes(ymin=lower, ymax=upper) ,width=0.2, position = position_dodge(0.9)) +



##########################################
##########################################
#VI) Create the dataframe for the by-legislature-by-gender graph


df_graph <- meps_survey_allwaves_q2_7 %>% filter(!is.na(gender)) %>%
  group_by(legislature, gender) %>% 
  summarise(across(starts_with("q2"), list(sum = sum, mean = mean, sd = sd), na.rm=TRUE)) %>% 
  pivot_longer(cols=starts_with("q2")) %>%
  mutate(statistic = substr(name, 8 ,nchar(name)),
         name = substr(name, 1 ,6)) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(name = case_when(name == "q2_7_1" ~ "Member of the EP",
                          name == "q2_7_2" ~ "Chair of an EP committee",
                          name == "q2_7_3" ~"Chair of an EP party group",
                          name == "q2_7_4" ~ "Member of national parliament",
                          name == "q2_7_5" ~ "Member of national government",
                          name == "q2_7_6" ~ "European Commissioner",
                          name == "q2_7_7" ~ "Retired from public life",
                          name == "q2_7_9" ~ "Other"))  




ggplot() + geom_col(data = df_graph, aes(x=legislature, y=mean, fill=name), position="dodge2")+
  facet_wrap(facets = ~gender)+ theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  labs(y = "Share of respondent mentionning this option", x = "Legislature", fill = "Situation 10 years from now")


