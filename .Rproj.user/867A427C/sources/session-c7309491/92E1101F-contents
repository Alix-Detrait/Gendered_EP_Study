# We want a table summarizing the main variables from the surveys sorted by year
#   - number of respondents
#   - share of respondents per each country in the EP
#   - share of women 
#   - share of respondents per political party in the EP


library(readr)
meps_survey_allwaves <- read_csv("01_data/data_input/meps_survey_allwaves.csv")


library(dplyr)
df <- meps_survey_allwaves %>%
  group_by(b_3) %>% #b_3 is the variable coding for year (b_3 == 1 is the 2000 survey, b_3 == 2 is the 2006 survey, b_3 == 3 is the 2010 survey, b_3 == 4 is the 2015 survey)
  summarize(
    total_respondents = n(),
    share_Belgium = (sum(q1_1 == 1)/n())*100,
    share_Bulgaria = (sum(q1_1 == 2)/n())*100,
    share_CzechR = (sum(q1_1 == 3)/n())*100,
    share_Denmark = (sum(q1_1 == 4)/n())*100,
    share_Germany = (sum(q1_1 == 5)/n())*100,
    share_Estonia = (sum(q1_1 == 6)/n())*100,
    share_Ireland = (sum(q1_1 == 7)/n())*100,
    share_Greece = (sum(q1_1 == 8)/n())*100,
    share_Spain = (sum(q1_1 == 9)/n())*100,
    share_France = (sum(q1_1 == 10)/n())*100,
    share_Italy = (sum(q1_1 == 11)/n())*100,
    share_Cyprus = (sum(q1_1 == 12)/n())*100,
    share_Latvia = (sum(q1_1 == 13)/n())*100,
    share_Lithuania = (sum(q1_1 == 14)/n())*100,
    share_Luxembourg = (sum(q1_1 == 15)/n())*100,
    share_Hungary = (sum(q1_1 == 16)/n())*100,
    share_Malta = (sum(q1_1 == 17)/n())*100,
    share_Netherlands = (sum(q1_1 == 18)/n())*100,
    share_Austria = (sum(q1_1 == 19)/n())*100,
    share_Poland = (sum(q1_1 == 20)/n())*100,
    share_Portugal = (sum(q1_1 == 21)/n())*100,
    share_Romania = (sum(q1_1 == 22)/n())*100,
    share_Slovenia = (sum(q1_1 == 23)/n())*100,
    share_Slovakia = (sum(q1_1 == 24)/n())*100,
    share_Finland = (sum(q1_1 == 25)/n())*100,
    share_Sweden = (sum(q1_1 == 26)/n())*100,
    share_UK = (sum(q1_1 == 27)/n())*100,
    share_Croatia = (sum(q1_1 == 28)/n())*100,
    share_female = (sum(q1_7 == 2)/n())*100,
    share_EPPED = (sum(q1_2 == 1)/n())*100,
    share_EuropeanSocialists = (sum(q1_2 == 2)/n())*100,
    share_ALDE = (sum(q1_2 == 3)/n())*100,
    share_GreensEFA = (sum(q1_2 == 4)/n())*100,
    share_ECR = (sum(q1_2 == 5)/n())*100,
    share_GUENGL = (sum(q1_2 == 6)/n())*100,
    share_EFD = (sum(q1_2 == 7)/n())*100,
    share_Nonattached = (sum(q1_2 == 8)/n())*100,
    share_UEN = (sum(q1_2 == 9)/n())*100,
    share_IND_DEM = (sum(q1_2 == 10)/n())*100,
    share_EDD = (sum(q1_2 == 11)/n())*100,
    share_EFDD = (sum(q1_2 == 12)/n())*100,
    share_ENF = (sum(q1_2 == 13)/n())*100,
  )
#pivoting the dataframe to get a table with all the variables on the first column, and their respective values per year as the row values
pivot_df <- data.frame(variable = character(), "2000" = numeric(), "2006" = numeric(), "2010" = numeric(), "2015" = numeric(), stringsAsFactors = FALSE)


# Loop through each variable


for (variable in c("total_respondents", "share_Belgium", "share_Bulgaria", "share_CzechR", "share_Denmark", "share_Germany", "share_Estonia", "share_Ireland", "share_Greece", "share_Spain", "share_France", "share_Italy", "share_Cyprus", "share_Latvia", "share_Lithuania", "share_Luxembourg", "share_Hungary", "share_Malta", "share_Netherlands", "share_Austria", "share_Poland", "share_Portugal", "share_Romania", "share_Slovenia", "share_Slovakia", "share_Finland", "share_Sweden", "share_UK", "share_Croatia", "share_female", "share_EPPED", "share_EuropeanSocialists", "share_ALDE", "share_GreensEFA", "share_ECR", "share_GUENGL", "share_EFD", "share_Nonattached", "share_UEN", "share_IND_DEM", "share_EDD", "share_EFDD", "share_ENF")) {
  
  # Creating a row for the year
  variable_row <- data.frame(
    variable = variable,
    "2000" = round(sum(df[df$b_3 == 1, variable]), 1),
    "2006" = round(sum(df[df$b_3 == 2, variable]), 1),
    "2010" = round(sum(df[df$b_3 == 3, variable]), 1),
    "2015" = round(sum(df[df$b_3 == 4, variable]), 1)
  )
  
  # Append the row to the pivot_df
  pivot_df <- rbind(pivot_df, variable_row)
}


# Remove the 'X' prefix from the column names
colnames(pivot_df) <- gsub("^X", "", colnames(pivot_df))


# Print the pivot_df
print(pivot_df)


#export the table as a tex file to open in latex
library(xtable)
print(xtable(pivot_df, type="latex"), file="04_tables/01_hix_survey_table.tex")
