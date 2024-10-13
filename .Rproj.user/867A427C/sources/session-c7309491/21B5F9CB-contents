library(fixest)


# Load the data
my_data1 <- read.csv("01_data/data_input/meps_entry_exit.csv")
my_data2 <- read.csv("01_data/data_input/meps_activity_report_with_date_appointment.csv")
my_data3 <- read.csv("01_data/data_input/meps_entry_exit_committees.csv")
my_data5 <- read_csv("01_data/data_input/gender_comms_data.csv")




###############################################################################################################################
#sixth leg member list: 
leg_data <- my_data1 %>%
  filter(Gender != "") %>%
  filter(term %in% c("6")) %>% 
  select(UserID, term, Gender) %>% 
  distinct(UserID, .keep_all = TRUE)


member_list6 = unique(c(leg_data$UserID))


###############################################################################################################################
###############################################################################################################################
#SEVENTH LEGISLATURE


# Prepare leg_data
leg_data <- my_data1 %>%
  filter(Gender != "") %>%
  filter(term %in% c("7")) %>% 
  select(UserID, term, Gender) %>% 
  mutate(exper = ifelse(UserID %in% member_list6, 1, 0)) %>%
  distinct(UserID, .keep_all = TRUE)


member_list7 = unique(c(leg_data$UserID))


# Update data2 for n_reports1 to count reports in the 6th legislature
data2 <- my_data2 %>%
  rename(UserID = mep_id) %>%
  semi_join(leg_data, by = "UserID") %>%
  filter(term %in% c("6")) %>%
  select(UserID, date_appointment, term) %>%
  mutate(n_reports_prev = ifelse(as.Date(date_appointment, format = "%d/%m/%Y") < as.Date("2012-01-01"), 1, 0)) %>%
  group_by(UserID) %>%
  summarise(n_reports_prev = sum(n_reports_prev))


# Update data3 and data4 for chair2 to indicate chairs before 2012-01-01
data3 <- my_data3 %>%
  filter(Gender != "") %>%
  filter(term %in% c("7")) %>%
  filter(as.Date(start) < as.Date("2012-01-01")) %>%
  mutate(chair1 = ifelse(role == "Chair", 1, 0)) %>%
  filter(chair1 != 0) %>%
  select(UserID, chair1)




# Prepare data5
data5 <- my_data5 %>%
  filter(term %in% c("7")) %>%
  select(UserID, committee, AFET, DROI, SEDE, DEVE, INTA, BUDG, CONT, ECON, FISC, EMPL, ENVI, SANT, ITRE, IMCO, TRAN, REGI, AGRI, PECH, CULT, JURI, LIBE, AFCO, FEMM, PETI)


# Merge data
merged_df <- merge(leg_data, data2, by = "UserID", all.x = TRUE)
merged_df$n_reports_prev <- ifelse(is.na(merged_df$n_reports_prev), 0, merged_df$n_reports_prev)
final7_df <- merge(merged_df, data3, by = "UserID", all.x = TRUE)
final7_df$chair1 <- ifelse(is.na(final7_df$chair1), 0, final7_df$chair1) 
comm_final7_df <- merge(final7_df, data5, by = "UserID", all.x = TRUE)


# #View the final dataframe
#View(comm_final7_df)


###############################################################################################################################
###############################################################################################################################
#EIGHTH LEGISLATURE


# Prepare leg_data
leg_data <- my_data1 %>%
  filter(Gender != "") %>%
  filter(term %in% c("8")) %>% 
  select(UserID, term, Gender) %>% 
  mutate(exper = ifelse(UserID %in% member_list7, 1, 0)) %>%
  distinct(UserID, .keep_all = TRUE)


member_list7 = unique(c(leg_data$UserID))


# Update data2 for n_reports1 to count reports in the 6th legislature
data2 <- my_data2 %>%
  rename(UserID = mep_id) %>%
  semi_join(leg_data, by = "UserID") %>%
  filter(term %in% c("7")) %>%
  select(UserID, date_appointment, term) %>%
  mutate(n_reports_prev = ifelse(as.Date(date_appointment, format = "%d/%m/%Y") < as.Date("2017-01-01"), 1, 0)) %>%
  group_by(UserID) %>%
  summarise(n_reports_prev = sum(n_reports_prev))


# Update data3 and data4 for chair2 to indicate chairs before 2012-01-01
data3 <- my_data3 %>%
  filter(Gender != "") %>%
  filter(term %in% c("8")) %>%
  filter(as.Date(start) < as.Date("2017-01-01")) %>%
  mutate(chair1 = ifelse(role == "Chair", 1, 0)) %>%
  filter(chair1 != 0) %>%
  select(UserID, chair1)




# Prepare data5
data5 <- my_data5 %>%
  filter(term %in% c("8")) %>%
  select(UserID, committee, AFET, DROI, SEDE, DEVE, INTA, BUDG, CONT, ECON, FISC, EMPL, ENVI, SANT, ITRE, IMCO, TRAN, REGI, AGRI, PECH, CULT, JURI, LIBE, AFCO, FEMM, PETI)


# Merge data
merged_df <- merge(leg_data, data2, by = "UserID", all.x = TRUE)
merged_df$n_reports_prev <- ifelse(is.na(merged_df$n_reports_prev), 0, merged_df$n_reports_prev)
final8_df <- merge(merged_df, data3, by = "UserID", all.x = TRUE)
final8_df$chair1 <- ifelse(is.na(final8_df$chair1), 0, final8_df$chair1) 
comm_final8_df <- merge(final8_df, data5, by = "UserID", all.x = TRUE)


# #View the final dataframe
#View(comm_final8_df)
###############################################################################################################################
###############################################################################################################################
#NINTH LEGISLATURE


# Prepare leg_data
leg_data <- my_data1 %>%
  filter(Gender != "") %>%
  filter(term %in% c("9")) %>% 
  select(UserID, term, Gender) %>% 
  mutate(exper = ifelse(UserID %in% member_list8, 1, 0)) %>%
  distinct(UserID, .keep_all = TRUE)




# Update data2 for n_reports1 to count reports in the 6th legislature
data2 <- my_data2 %>%
  rename(UserID = mep_id) %>%
  semi_join(leg_data, by = "UserID") %>%
  filter(term %in% c("8")) %>%
  select(UserID, date_appointment, term) %>%
  mutate(n_reports_prev = ifelse(as.Date(date_appointment, format = "%d/%m/%Y") < as.Date("2022-01-01"), 1, 0)) %>%
  group_by(UserID) %>%
  summarise(n_reports_prev = sum(n_reports_prev))


# Update data3 and data4 for chair2 to indicate chairs before 2012-01-01
data3 <- my_data3 %>%
  filter(Gender != "") %>%
  filter(term %in% c("9")) %>%
  filter(as.Date(start) < as.Date("2022-01-01")) %>%
  mutate(chair1 = ifelse(role == "Chair", 1, 0)) %>%
  filter(chair1 != 0) %>%
  select(UserID, chair1)




# Prepare data5
data5 <- my_data5 %>%
  filter(term %in% c("9")) %>%
  select(UserID, committee, AFET, DROI, SEDE, DEVE, INTA, BUDG, CONT, ECON, FISC, EMPL, ENVI, SANT, ITRE, IMCO, TRAN, REGI, AGRI, PECH, CULT, JURI, LIBE, AFCO, FEMM, PETI)


# Merge data
merged_df <- merge(leg_data, data2, by = "UserID", all.x = TRUE)
merged_df$n_reports_prev <- ifelse(is.na(merged_df$n_reports_prev), 0, merged_df$n_reports_prev)
final9_df <- merge(merged_df, data3, by = "UserID", all.x = TRUE)
final9_df$chair1 <- ifelse(is.na(final9_df$chair1), 0, final9_df$chair1) 
comm_final9_df <- merge(final9_df, data5, by = "UserID", all.x = TRUE)


# #View the final dataframe
#View(comm_final9_df)


#################################################################################
#################################################################################


comms_combined_df <- bind_rows(comm_final7_df, comm_final8_df, comm_final9_df)


# #View the final combined dataset
#View(comms_combined_df)


write.csv(comms_combined_df, "01_data/data_output/legs789_beginning_term_chair.csv")


##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################




#Generating the final dataframes used in the regressions


report789_reg <- read_csv("01_data/data_output/legs789_beginning_term_chair.csv")
#View(report789_reg)
party_df <- read_csv("01_data/data_input/meps_party_group_size.csv")
#View(party_df)


report789_reg <- report789_reg%>%
  mutate(female = if_else(Gender=="F", 1, 0))%>%
  mutate(comm_imp = if_else(grepl("BUDG|TRAN|IMCO|JURI|EMPL|ECON|ENVI|ITRE|LIBE|CULT|SANT|FISC", committee), 1, 0),
         comm_other = if_else(grepl("AFET|DEVE|INTA|CONT|AFCO|PETI|AGRI|PECH|REGI|FEMM|DROI|SEDE", committee), 1, 0))
#View(report789_reg)


####################################################################################################
####################################################################################################


report7_reg <- report789_reg%>%
  mutate(female = if_else(Gender=="F", 1, 0))%>%
  filter(term %in% c("7"))


country_origin <- my_data1 %>%
  filter(term %in% c("7")) %>%
  select(UserID, country)


party7_df <- party_df %>%
  filter(term %in% c("7")) %>%
  select(UserID, party_share, Party)


pre_merged7_df <- merge(report7_reg, party7_df, by = "UserID", all.x = TRUE)
merged7_df <- merge(pre_merged7_df, country_origin, by = "UserID", all.x = TRUE)
#View(merged7_df)




####################################################################################################
####################################################################################################


report8_reg <- report789_reg%>%
  mutate(female = if_else(Gender=="F", 1, 0))%>%
  filter(term %in% c("8"))


country_origin <- my_data1 %>%
  filter(term %in% c("8")) %>%
  select(UserID, country)


party8_df <- party_df %>%
  filter(term %in% c("8")) %>%
  select(UserID, party_share, Party)


pre_merged8_df <- merge(report8_reg, party8_df, by = "UserID", all.x = TRUE)
merged8_df <- merge(pre_merged8_df, country_origin, by = "UserID", all.x = TRUE)
#View(merged8_df)


####################################################################################################
####################################################################################################


report9_reg <- report789_reg%>%
  mutate(female = if_else(Gender=="F", 1, 0))%>%
  filter(term %in% c("9"))


country_origin <- my_data1 %>%
  filter(term %in% c("9")) %>%
  select(UserID, country)


party9_df <- party_df %>%
  filter(term %in% c("9")) %>%
  select(UserID, party_share, Party)


pre_merged9_df <- merge(report9_reg, party9_df, by = "UserID", all.x = TRUE)
merged9_df <- merge(pre_merged9_df, country_origin, by = "UserID", all.x = TRUE)
#View(merged9_df)


####################################################################################################
final789_df <- bind_rows(merged7_df, merged8_df, merged9_df)
#View(final789_df)


final789_df_unique <- distinct(final789_df)
#View(final789_df_unique)


write.csv(final789_df_unique, "01_data/data_output/legs789_COMPLETE_BEGINNING_DF.csv")


####################################################################################################


final789_df_unique$country <- as.factor(final789_df_unique$country)
final789_df_unique$Party <- as.factor(final789_df_unique$Party)


reg1_789_COMPLETE <- feols(chair1 ~ n_reports_prev + exper + AFET + DROI + SEDE + DEVE + INTA + BUDG + CONT + ECON + FISC + EMPL + ENVI + SANT + ITRE + IMCO + TRAN + REGI + AGRI + PECH + CULT + JURI + LIBE + AFCO + FEMM + PETI | Party + country, final789_df_unique)
summary(reg1_789_COMPLETE)


reg2_789_COMPLETE <- feols(chair1 ~ female + exper + AFET + DROI + SEDE + DEVE + INTA + BUDG + CONT + ECON + FISC + EMPL + ENVI + SANT + ITRE + IMCO + TRAN + REGI + AGRI + PECH + CULT + JURI + LIBE + AFCO + FEMM + PETI| Party + country, final789_df_unique)
summary(reg2_789_COMPLETE)


reg3_789_COMPLETE <- feols(chair1 ~ female + exper + n_reports_prev + AFET + DROI + SEDE + DEVE + INTA + BUDG + CONT + ECON + FISC + EMPL + ENVI + SANT + ITRE + IMCO + TRAN + REGI + AGRI + PECH + CULT + JURI + LIBE + AFCO + FEMM + PETI| Party + country, final789_df_unique)
summary(reg3_789_COMPLETE)


reg4_789_COMPLETE <- feols(chair1 ~ female + n_reports_prev + exper + female*n_reports_prev + AFET + DROI + SEDE + DEVE + INTA + BUDG + CONT + ECON + FISC + EMPL + ENVI + SANT + ITRE + IMCO + TRAN + REGI + AGRI + PECH + CULT + JURI + LIBE + AFCO + FEMM + PETI| Party + country, final789_df_unique)
summary(reg4_789_COMPLETE)


reg5_789_COMPLETE <- feols(chair1 ~ i(female,n_reports_prev) + exper + AFET + DROI + SEDE + DEVE + INTA + BUDG + CONT + ECON + FISC + EMPL + ENVI + SANT + ITRE + IMCO + TRAN + REGI + AGRI + PECH + CULT + JURI + LIBE + AFCO + FEMM + PETI| Party + country, final789_df_unique)
summary(reg5_789_COMPLETE)


etable(reg1_789_COMPLETE,reg2_789_COMPLETE,reg3_789_COMPLETE,reg4_789_COMPLETE,reg5_789_COMPLETE, tex = TRUE, file = "04_tables/regs_789leg_COMPLETE_BEGINNING.tex", replace=TRUE)


