#Second barplots - When thinking about your work as an MEP, how important are the following aspects of your work?
# this question corresponds to q6_1 on the csv meps_survey_allwaves


library(readr)


#importing dataset
meps_survey_allwaves_q6_1 <- read_csv("01_data/data_input/meps_survey_allwaves.csv",
                                      col_types = "d") 


#sorting the csv file to have the responses of q2_7 separated by the year answered:
survey_2000 = subset(meps_survey_allwaves_q6_1, b_3==1) #b_3 is the variable for the year; b_3 being 1 corresponds to the year 2000 
survey_2006 = subset(meps_survey_allwaves_q6_1, b_3==2) #b_3 being 2 corresponds to the year 2006
survey_2010 = subset(meps_survey_allwaves_q6_1, b_3==3) #b_3 being 3 corresponds to the year 2010
survey_2015 = subset(meps_survey_allwaves_q6_1, b_3==4) #b_3 being 4 corresponds to the year 2015


#now we define the percentage of the MEPs answers to question q6_1 sorted by year of the survey and the different potential responses
# Role is legislation := q6_1_1 
legislating_2000 = sum(as.numeric(survey_2000$q6_1_1), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==1 & q6_1_1 !=0))
legislating_2006 = (sum(as.numeric(survey_2006$q6_1_1), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==2 & q6_1_1 !=0)))
legislating_2010 = (sum(as.numeric(survey_2010$q6_1_1), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==3 & q6_1_1 !=0))) 
legislating_2015 = (sum(as.numeric(survey_2015$q6_1_1), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==4 & q6_1_1 !=0)))


# Role is Parliamentary Oversight := q6_1_2
po_2000 = (sum(as.numeric(survey_2000$q6_1_2), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==1 & q6_1_2 !=0)))
po_2006 = (sum(as.numeric(survey_2006$q6_1_2), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==2 & q6_1_2 !=0)))  
po_2010 = (sum(as.numeric(survey_2010$q6_1_2), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==3 & q6_1_2 !=0))) 
po_2015 = (sum(as.numeric(survey_2015$q6_1_2), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==4 & q6_1_2 !=0)))  

# Role is Social Group Representation := q6_1_3
sgr_2000 = (sum(as.numeric(survey_2000$q6_1_3), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==1 & q6_1_3 !=0)))
sgr_2006 = (sum(as.numeric(survey_2006$q6_1_3), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==2 & q6_1_3 !=0)))  
sgr_2010 = (sum(as.numeric(survey_2010$q6_1_3), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==3 & q6_1_3 !=0)))
sgr_2015 = (sum(as.numeric(survey_2015$q6_1_3), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==4 & q6_1_3 !=0))) 


# Role is common EU strategy deve := q6_1_4
cEUs_2000 = (sum(as.numeric(survey_2000$q6_1_4),na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==1 & q6_1_4 !=0))) 
cEUs_2006 = (sum(as.numeric(survey_2006$q6_1_4),na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==2 & q6_1_4 !=0))) 
cEUs_2010 = (sum(as.numeric(survey_2010$q6_1_4),na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==3 & q6_1_4 !=0))) 
cEUs_2015 = (sum(as.numeric(survey_2015$q6_1_4),na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==4 & q6_1_4 !=0)))


# Role is social mediation := q6_1_5
sm_2000 = (sum(as.numeric(survey_2000$q6_1_5), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==1 & q6_1_5 !=0)))
sm_2006 = (sum(as.numeric(survey_2006$q6_1_5), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==2 & q6_1_5 !=0)))
sm_2010 = (sum(as.numeric(survey_2010$q6_1_5), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==3 & q6_1_5 !=0))) 
sm_2015 = (sum(as.numeric(survey_2015$q6_1_5), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==4 & q6_1_5 !=0)))


# Role is Individual Representation := q6_1_6
ir_2000 = (sum(as.numeric(survey_2000$q6_1_6), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==1 & q6_1_6 !=0))) 
ir_2006 = (sum(as.numeric(survey_2006$q6_1_6), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==2 & q6_1_6 !=0)))
ir_2010 = (sum(as.numeric(survey_2010$q6_1_6), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==3 & q6_1_6 !=0)))
ir_2015 = (sum(as.numeric(survey_2015$q6_1_6), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, b_3==4 & q6_1_6 !=0))) 






library(ggplot2)
library(tidyr)


#creating a new dataframe to easily plot these values:
my_data1 = data.frame(
  year = c("2000", "2006", "2010", "2015", "2000", "2006", "2010", "2015", "2000", "2006", "2010", "2015", "2000", "2006", "2010", "2015", "2000", "2006", "2010", "2015", "2000", "2006", "2010", "2015"), #years are put in quotation marks in this place because if not R will scale the years and have 2006 / 2010 bars too close to each other
  response = c("Legislation","Legislation","Legislation","Legislation", "Parliamentary Oversight","Parliamentary Oversight","Parliamentary Oversight","Parliamentary Oversight", "Social Group Representation", "Social Group Representation","Social Group Representation","Social Group Representation","Common EU Strategies","Common EU Strategies","Common EU Strategies","Common EU Strategies", "Social Mediation","Social Mediation","Social Mediation","Social Mediation", "Individual Representation","Individual Representation","Individual Representation","Individual Representation"),
  count = c(legislating_2000, legislating_2006, legislating_2010, legislating_2015, po_2000, po_2006, po_2010, po_2015, sgr_2000, sgr_2006, sgr_2010, sgr_2015, cEUs_2000, cEUs_2006, cEUs_2010, cEUs_2015, sm_2000, sm_2006, sm_2010, sm_2015, ir_2000, ir_2006, ir_2010, ir_2015)
)


#first bar plot, with the different responses as the x-axis and the year as the different bars in each "clump", i.e. fill:


ggplot(data = my_data1, aes(x = response, y = count, fill = year)) +
  geom_bar(position = "dodge2", stat = "identity") + #dodge 2 allows for the different bars for each x-value
  labs(x = "Response", y = "Average Importance", fill = "Year") + #code for the titles of the axes
  ggtitle("When thinking about your work as an MEP, how important are the following aspects of your work?") + #titles the bar plot
  guides(fill = guide_legend(title = "Year")) + #this code creates a cleaner legend
  theme_minimal() + coord_flip()








#I will now create a second variation of the graph, this time with the year as the x-axis value, and the responses as the fill:




#same code as before, just switching the fill and x-values:
ggplot(data = my_data1, aes(x = year, y = count, fill = response)) +
  geom_bar(position = "dodge2", stat = "identity") +
  labs(x = "Year", y = "Average Importance", fill = "Response") + 
  ggtitle("When thinking about your work as an MEP, how important are the following aspects of your work?") +
  guides(fill = guide_legend(title = "Response")) +
  theme_minimal() 








#Now to make a bar graph aggregating the years:
library(scales)


as.double()
legislating =  (sum(as.double(meps_survey_allwaves_q6_1$q6_1_1), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, q6_1_1!=0)))
po =  (sum(as.double(meps_survey_allwaves_q6_1$q6_1_2), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, q6_1_2!=0)))
sgr =  (sum(as.double(meps_survey_allwaves_q6_1$q6_1_3), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, q6_1_3!=0)))
cEUs =  (sum(as.double(meps_survey_allwaves_q6_1$q6_1_4), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, q6_1_4!=0)))
sm =  (sum(as.double(meps_survey_allwaves_q6_1$q6_1_5), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, q6_1_5!=0)))
ir =  (sum(as.double(meps_survey_allwaves_q6_1$q6_1_6), na.rm=TRUE) / nrow(subset(meps_survey_allwaves_q6_1, q6_1_6!=0)))


my_data3 = data.frame(
  answer = c("Legislation", "Parliamentary Oversight", "Social Group Representation", "Common EU Strategies", "Social Mediation", "Individual Representation"),
  percentage = c(legislating, po, sgr, cEUs, sm, ir)
)


ggplot(data = my_data3, aes(x = answer, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels=label_wrap(10))+ #label_wrap breaks the x-axis values after every word, making the answer choices legible
  labs(x = "Response", y = "Average Importance") + 
  ggtitle("When thinking about your work as an MEP, how important are the following aspects of your work? - All years") +
  theme_minimal() 




#now, the same graph as before but with one added variable: whether the respondent is female or male (as a fill)
male_respondents = subset(meps_survey_allwaves_q6_1, q1_7==1) #q1_7 1 corresponds to the male correspondents 
female_respondents = subset(meps_survey_allwaves_q6_1, q1_7==2) #q1_7 being 2 corresponds to female respondents


legislating_m =  (sum(as.double(male_respondents$q6_1_1),na.rm = TRUE) / nrow(male_respondents))
legislating_f =  (sum(as.double(female_respondents$q6_1_1),na.rm = TRUE) / nrow(female_respondents))
po_m =  (sum(as.double(male_respondents$q6_1_2),na.rm = TRUE) / nrow(male_respondents))
po_f =  (sum(as.double(female_respondents$q6_1_2),na.rm = TRUE) / nrow(female_respondents))
sgr_m =  (sum(as.double(male_respondents$q6_1_3),na.rm = TRUE) / nrow(male_respondents))
sgr_f =  (sum(as.double(female_respondents$q6_1_3),na.rm = TRUE) / nrow(female_respondents))
cEUs_m =  (sum(as.double(male_respondents$q6_1_4),na.rm = TRUE) / nrow(male_respondents))
cEUs_f =  (sum(as.double(female_respondents$q6_1_4),na.rm = TRUE) / nrow(female_respondents))
sm_m =  (sum(as.double(male_respondents$q6_1_5),na.rm = TRUE) / nrow(male_respondents))
sm_f =  (sum(as.double(female_respondents$q6_1_5),na.rm = TRUE) / nrow(female_respondents))
ir_m =  (sum(as.double(male_respondents$q6_1_6),na.rm = TRUE) / nrow(male_respondents))
ir_f =  (sum(as.double(female_respondents$q6_1_6),na.rm = TRUE) / nrow(female_respondents))




my_data4 = data.frame(
  answer = c("Legislation", "Legislation", "Parliamentary Oversight", "Parliamentary Oversight", "Social Group Representation","Social Group Representation", "Common EU Strategies", "Common EU Strategies","Social Mediation", "Social Mediation","Individual Representation","Individual Representation"),
  percentage = c(legislating_m, legislating_f, po_m,po_f, sgr_m, sgr_f, cEUs_m, cEUs_f, sm_m, sm_f, ir_m, ir_f),
  gender = c("male", "female", "male", "female","male", "female","male", "female","male", "female","male", "female")
)


bar4 = ggplot(data = my_data4, aes(x = answer, y = percentage, fill = gender)) +
  geom_bar(position = "dodge2", stat = "identity") +
  scale_x_discrete(labels=label_wrap(10))+
  labs(x = "Response", y = "Average Importance", fill = "Gender") + 
  ggtitle("When thinking about your work as an MEP, how important are the following aspects of your work? - All years") +
  guides(fill = guide_legend(title = "Gender")) +
  theme_minimal() 


bar4
