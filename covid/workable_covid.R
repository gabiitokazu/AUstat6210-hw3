# make sure you are in the right directory using `getwd()` and `setwd()`
# or use 


raw_data <- read.csv(file = "covid.csv", row.names=1, header=TRUE)

glimpse(raw_data)

colnames(raw_data)[8] <- "Notes"

# 1 and 2:

raw_data <- raw_data %>%
     mutate(across(c(Date.of.birth,First.day.of.symptoms,Date.of.outcome,Date.of.diagnosis), as.Date)) %>%
     mutate(across(c(Hospitalization.type,Symptoms,Outcome), as.factor)) %>%
     mutate(Notes = as.character(Notes))

glimpse(raw_data)  

# 1 Correctly format the dates in “Date.of.birth”, “First.day.of.symptoms”, “Date.of.outcome” and “Date.of.diagnosis”.
# raw_data[c(1,4,6:7)] <- as.data.frame(lapply(raw_data[c(1,4,6:7)], function(x) as.Date(x, format="%Y-%b-%d")))

# raw_data$Date.of.birth <- as.Date(raw_data$Date.of.birth, format="%d-%B-%Y") --- nao funciona nao sei pq

# raw_data[c(1,4,6:7)] <- lapply(raw_data[c(1,4,6:7)], as.Date)

# 2 Format “Hospitalization.type”, “Symptoms” and “Outcome” as factors and “Epidemiological.link…Notes” as character.

# raw_data$Epidemiological.link...Notes <- as.character
# # (raw_data$Epidemiological.link...Notes)

# 3 Create a three-dimensional table reporting the three factors from the previous question. Hint: use the function table() with the variables of interest.

report <- raw_data %>%
     select_if(is.factor)

# to create as a data.frame, or a table using:
report2 <- with(raw_data,table(raw_data$Hospitalization.type,raw_data$Symptoms,raw_data$Outcome))


# 4 Using the table object created in the previous question, subset it in order to create the following table.  r = home isolation/intensive care, col = disease in progress/healed

subset <- report %>%
     filter(Symptoms != "Symptomatic") %>%
     filter(Outcome != "Deceased") %>%
     filter(Hospitalization.type != "Non-Intensive care hospitalization") %>%
     group_by(Hospitalization.type, Outcome) %>%
     summarize(Symptoms = n())

subset <- subset %>%
     spread(Outcome,Symptoms) %>%
     replace(is.na(.), 0)


#-------------------------------------------------------------------------

# 5 Order the data based on the date of diagnosis (from first to most recent).

data_q5 <- arrange(raw_data, Date.of.outcome)

# 6 Add a column that reports whether or not a case was asymptomatic AND in home isolation. Name the observation “Home_Asymptomatic” if the conditions apply and “Non_Home_Asymptomatic” if not and then produce a bar plot of this new variable.

data_q6 <- raw_data
data_q6$new = character(nrow(raw_data))
colnames(data_q6) = "q6"

data_q6 <- raw_data %>%
     mutate(q6 = )
#NUM ESQUECE DO BARPLOT


# 7 Count the number of cases of people born after 1981 and that have healed.

q7 <- raw_data %>%
     filter(Date.of.birth >= "1981-01-01") %>%
     filter(Outcome == "Healed") %>%
     summarize(n())

# 8 Count the number of cases that are asymptomatic OR in home isolation (but not both) AND were born before 1982.
#### ------ Funciona, mas que coisa feia---------------------------

q8 <- raw_data %>%
      filter(Date.of.birth >= "1982-01-01")

q8$but_not_both = factor(rep(NA, nrow(q8)), levels =1:2, labels = c("Asymptomatic", "Home isolation"))

q8$but_not_both[which(grepl("Asymptomatic", q8$Symptoms)
                      & !grepl("Asymptomatic.*Home isolation|Home isolation.*Asymptomatic",
                               q8$Symptoms))] <- "Asymptomatic"
q8$but_not_both[which(grepl("Home isolation", q8$Hospitalization.type)
                      & !grepl("Asymptomatic.*Home isolation|Home isolation.*Asymptomatic",
                               q8$Hospitalization.type))] <- "Home isolation"

summary(q8$but_not_both)

q8 <- q8 %>%
     filter(but_not_both != is.na(but_not_both))

nrow(q8)
###------------ horrivel, tem que fazer de novo--------------------------
###

# 9 Create a new dataset including only the rows where “Epidemiological.link…Notes” includes the words “contact” OR “symptom” (or both). Hint: you can use the grep() function and tolower().
# 
q9 <- raw_data %>%
     filter(grep("contact", Notes))

tolower(q9)
grep("contact", Notes, ignore.case = TRUE)
# 10 In the previous dataset add a column reporting the age (in years, therefore in integer format) of each patient as of October 2nd, 2020. Save this dataset into a .csv file and make it available on your GitHub repository for this assignment.

# 11 Produce a pie chart for the type of hospitalization for cases born between 1960 and 1980.














#birth dates to ages:
#
# x <- as.Date(c("2011-01-01", "1996-02-29"))
# age_calc(x[1],x[2]) # default is age in months
# age_calc(x[1],x[2], units = "years") # but you can set it to years
# floor(age_calc(x[1],x[2], units = "years"))











