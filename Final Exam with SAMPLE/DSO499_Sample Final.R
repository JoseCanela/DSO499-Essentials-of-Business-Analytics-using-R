employees = read_xlsx("Problem 1.xlsx") # Employee data for a company
players = read_xlsx("Problem 2.xlsx") # NYY Yankees and Marlins salary data
transactions = read_xlsx("Problem 3.xlsx") # Transaction data for a store

##------PROBLEM 1------##

# 1. Use R to calculate the following summary statistics for the variable "Salary":
employees.salaries = employees$Salary
library(help = "stats")
# a) Mean
mean(employees.salaries) # $70,818.05
# b) Median
median(employees.salaries) # $71,572
# c) 1st Quartile
quantile(employees.salaries) # $60,717.25
# d) 3rd Quartile
quantile(employees.salaries) # $80,983.75
# e) Standard deviation
sd(employees.salaries) # $11,699.82
# f) IQR
IQR(employees.salaries) # $20,266.50
# g) Minimum
min(employees.salaries) # $50,002
# h) Maximum
max(employees.salaries) # $89,727
# i) Range
max(employees.salaries) - min(employees.salaries) # $39,725
# 2. How many employees work in the Marketing Department?
length(employees$Department[employees$Department == "Marketing"]) # 57
# 3. How many employees have a salary of at least $75,000?
length(employees.salaries[employees.salaries >= 75000]) # 125
# 4. What is the median salary in the Marketing Department?
as.data.frame(employees %>%
  filter(Department == "Marketing") %>%
  summarise(employees.salaries.med = median(Salary))) # $74,790
# 5. Create a histogram of employee salaries. Submit the PDF of the histogram here.
?hist
?seq
breaks = seq(50000,90000, by = 2000)
hist(employees.salaries, breaks = breaks, labels =  T, 
     main = "Distribution of Employee Salaries",
     xlab = "Employees Salaries (U.S. Dollars)", 
     ylab = "Number of Employees")

##------PROBLEM 2------##

# The Marlins General Manager is disgruntled because two desirable rookies accepted offers from the
# Yankees instead of the Marlins. He believes that Yankee salaries must be noticeably higher—otherwise,
# the best players would join the Marlins organization. If the typical Yankee is better compensated, the
# General Manager is planning to chat with the Owners about sweetening the Marlins’ offers.
# Perform the appropriate hypothesis test to help Marlins' manager decide. Answer the following
# questions:


# 1. You decided to check whether there is evidence that Yankee salaries are higher. What is the
# name of the appropriate hypothesis test?

# one-sided two sample t-test
?t.test()
t.test(players$`Yankee SALARY (M$)`, players$`Marlin SALARY (M$)`, alternative = "greater")
# 2. What is the value of the test statistic?

# t = 3.3854

# 3. What is the p-value?

# p-value = 0.001458

# 4. Is the test significant at significance level of 0.05?

# YES

# 5. Is a typical Yankee better compensated?

# YES

##------PROBLEM 3------##

# The data in Problem3.xlsx lists information about 400 customer transactions. It contains
# information on the transaction date, day of the week, time of the day, region, payment type, gender of
# the customer, and total cost. Answer the questions below using dplyr, ggplot2 packages, and pipes
# approach.

# 1. What is the total amount spent by female customers?
as.data.frame(transactions %>%
  filter(Gender == "Female") %>%
  summarise(sum.female.transactions = sum(`Total Cost`)))
  # $36,895.26

# 2. Report the R code you used to calculate the total amount spent by female customers, make sure
# you used dplyr package and pipes.
as.data.frame(transactions %>%
                filter(Gender == "Female") %>%
                summarise(sum.female.transactions = sum(`Total Cost`)))


# 3. What is the average amount spent by female customers?
as.data.frame(transactions %>%
  filter(Gender == "Female") %>%
  summarise(mean.female.transactions = mean(`Total Cost`)))
# $157.67

# 4. Report the R code you used to calculate the average amount spent by female customers, make
# sure you used dplyr package and pipes.
as.data.frame(transactions %>%
                filter(Gender == "Female") %>%
                summarise(mean.female.transactions = mean(`Total Cost`)))

# 5. Create a table from this data set that shows the number of transactions done by gender and
# time of day. Report the R code you use for that (use pipes and dplyr package).
?dcast()

transactions %>%
  select(Time, Gender) %>%
  group_by(Time, Gender)%>%
  summarise(num.transactions = length(Gender)) %>%
  dcast(Time~Gender)

# 6. Create a boxplot of the amount spent split by gender. Use ggplot2 package. Submit the PDF file
# of the figure.
ggplot(transactions, aes(x = Gender)) +
  geom_boxplot(aes(x=Gender, y= `Total Cost`)) +
  xlab("Gender") +
  ylab("Amount Spent (U.S. Dollars)") +
  labs(title = "Comparing Females and Males by Total Amount Spent\n in U.S. Dollars") +
  theme(plot.title = element_text(hjust = 0.5))

# 7. Submit the R code that produces a boxplot of the amount spent split by gender from previous
# question.
ggplot(transactions, aes(x = Gender)) +
  geom_boxplot(aes(x=Gender, y= `Total Cost`)) +
  xlab("Gender") +
  ylab("Amount Spent (U.S. Dollars)") +
  labs(title = "Comparing Females and Males by Total Amount Spent\n in U.S. Dollars") +
  theme(plot.title = element_text(hjust = 0.5))

