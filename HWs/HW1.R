# getwd()
# setwd("/Users/josecanela/Desktop/R tutorial files/DSO 499")
# getwd()

library(readxl)
data = read_excel("HW1.xlsx")
data
# 1 What country has the largest population?
#Make new column total pop 
data$total_pop = data$`2015 Male Population` + data$`2015 Female Population`
data$total_pop
data
data$Economy[which.max(data$total_pop)] #India

#2 What country's economy has the greatest total GDP?
# make total_GDP
data$total_GDP = data$`2015 GDP per Capita` * data$total_pop
data
data$Economy[which.max(data$total_GDP)]
# 3 Create a histogram that illustrates the amount of money that countries 
# spend on education per capita
data$education.per.capita = (data$`2015%of GDP Per Capita on Tertiary Education` * data$`2015 GDP per Capita`)/100
?hist
hist(data$education.per.capita, breaks = 10, main = "Amount of Money Spent on Education per Capita", ylab = "Number of Countries", xlab = "Amount of Money Spent on Education per Capita", labels = T, xlim = c(0, 2500))
axis(1, at = seq(0,2500, by = 250))

# 4 What is the mean amount of money spent on education per capita?
mean(data$education.per.capita)
#5 What is the maximum mean amount of money spent on education per capita?
max(data$education.per.capita)
# 6 Which economy spends the most amount of money on education per capita?
data$Economy[which.max(data$education.per.capita)]

# 7 Create a histogram that illustrates the amount of money Asian countries 
# spend on education per capita
Asia = subset(data, data$Continent == 'Asia')
hist(Asia$education.per.capita, breaks = 7, main = "Amount of Money Spent on Education per Capita (ASIA)", ylab = "Number of Countries in Asia", xlab = "Amount of Money Spent on Education per Capita", labels = T, xlim = c(0, 2500))
axis(1, at = seq(0,2000, by = 250))

# 8 What is the mean amount of money spent on education per capita in Asia?
mean(Asia$education.per.capita)
#9  What is the max amount of money spent on education per capita in Asia?
max(Asia$education.per.capita)
#10 What Asian country spends the most amount of money spent on education per capita?
Asia$Economy[which.max(Asia$education.per.capita)]
#11 What is the average amount of money spent on education per capita in Asian countries
# with a "20-39 yrs old" population of less than 6 million?
Asia_ls_6mil = subset(Asia, Asia$`2015,20-39 years,number`<6000000)
mean(Asia_ls_6mil$education.per.capita)
#12 What is the average amount of money spent on education per capita in Asian countries
# with a "20-39 yrs old" population of greater than 6 million?
Asia_gr_6mil = subset(Asia, Asia$`2015,20-39 years,number`>6000000)
mean(Asia_gr_6mil$education.per.capita)
