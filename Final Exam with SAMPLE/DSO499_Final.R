# install.packages("dplyr")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("xts")
# install.packages("lubridate")

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(xts)
library(lubridate)
library(reshape2)

?read.table() # reads txt
?read.csv()   # reads csv
?read_excel() # determines xls or xlsx (in that order) and then reads 
?read_xls()   # reads xls()
?read_xlsx()  # reads xlsx()

# This data is used for Prob 1 - 5)
airlines <- read_xlsx("FinalExamProblem1.xlsx")  # Airline baggage complaint data 
# This data is used for (Prob 6 - 10)
buildings <- read_xlsx("FinalExamProblem2.xlsx") # Building data consisting of assessed value 
# This data is used for (Prob 11 - 18)                                                 # and number of entrances
applications <- read_xlsx("FinalExamProblem3.xlsx") # Job application data for a company

# 1. Use R to calculate the following summary statistics for the variable "Baggage":

# Note: baggage represents baggage complaints
airlines.baggage = airlines$Baggage # vector containing the number of baggage complaints in a given month 
                                    # from a given airline
# mean
mean(airlines.baggage)
# median
median(airlines.baggage)

# 1st and 3rd quartiles
quantile(airlines.baggage)

#standard dev
sd(airlines.baggage)
# IQR
IQR(airlines.baggage)

#min
min.baggage = min(airlines.baggage)
min.baggage
#max
max.baggage = max(airlines.baggage)
max.baggage
#range
max.baggage - min.baggage

# Created column consisting of the number of baggage complaints per enplanement.
# This rate will be known as "Baggage_rate"
airlines["Baggage_rate"] = 100*(airlines$Baggage/airlines$Enplaned)
airlines

# Looking at all the airlines, how many months had a rate of baggage complaints/enplanement 
# greater than 50%?
length(airlines$Baggage_rate[airlines$Baggage_rate>0.5])

# 3 How many baggage complaints does United Airlines have?
unique(airlines$Airline)
airlines %>%
  filter(Airline == "United") %>%
  summarise(numUnited.bag.complaints = sum(Baggage))
# 4 What is United Airline's median rate of baggage complaints per enplanement?
as.data.frame( airlines %>%
  filter(Airline =="United") %>%
  summarise(med.United_bag.rate = median(Baggage_rate)))
# 5 Create a histogram that illustrates the Distribution of Baggage Complaints by Number of Months
breaks = seq(0, 49000, by = 7000)
hist(airlines.baggage, breaks = breaks, labels = T, 
     main = "Distribution of Baggage Complaints by Number of Months", 
     xlab = "Number of Baggage Complaints",
     ylab = "Number of Months")

# 6 - 10 What test would be appropriate to test if having multiple 
# building entrances leads to a greater assessed value 
# (in thousands of dollars) for the building? 
# What is the value of the test statistic? What is the p-value?
# Is the test significant at significance level of 0.05? 
# Do buildings with multiple entrances have a greater assessed value?
t.test(buildings$`Assessed(K$)`~buildings$Entrances)

# 11 -12 How many female applicants have a graduate degree?
View(as.data.frame(applications))
applications %>%
  filter(Gender == "Female" & Graduate =="Yes" ) %>%
  summarise(num.fem.grads = length(Graduate))
# 13-14
# What is the avg. male applicant age?
as.data.frame(applications %>%
  filter(Gender == "Male") %>%
  summarise(avg.male.age = mean(Age)))
# What is the avg. female applicant age
as.data.frame(applications %>%
                filter(Gender == "Female")%>%
                summarise(avg.fem.age = mean(Age)))
# 15 - 16 Create a table that shows the average work experience (in yrs) 
# of male or female applicant - also broken down by whether or not
# the applicant has an undergraduate degree.
applications %>%
  select(Gender, Undergraduate, `Work Experience`) %>%
  group_by(Gender, Undergraduate) %>%
  summarise(avg.work.experience = mean(`Work Experience`)) %>%
  dcast(Gender~Undergraduate)

# 17 - 18 Create a boxplot that can be used to compare male and female 
# applicant work experience (in yrs)
?ggplot
ggplot(applications)+
  geom_boxplot(aes(x = Gender, y = `Work Experience`)) +
  ylab("Work Experience (years)") +
  labs(title = "Compare Male and Female Applicant Work Experience")
  

  
