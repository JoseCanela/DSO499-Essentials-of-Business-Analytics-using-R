# getwd()
# setwd("/Users/student/Desktop/")
# getwd()
library(dplyr)
library(readxl)
# To begin, load in the Boston data set. The Boston data set is part of the MASS library in R.
library(MASS)
?Boston
data("Boston")
#Boston_tbl <- tbl_df(Boston)


# crim           per capita crime rate by town.
# zn             proportion of residential land zoned for lots over 25,000 sq.ft.
# indus          proportion of non-retail business acres per town.
# chas           Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox            nitrogen oxides concentration (parts per 10 million).
# rm             average number of rooms per dwelling.
# age            proportion of owner-occupied units built prior to 1940.
# dis            weighted mean of distances to five Boston employment centres.
# rad            index of accessibility to radial highways.
# tax            full-value property-tax rate per \$10,000.
# ptratio        pupil-teacher ratio by town.
# black          1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat          lower status of the population (percent).
# medv           median value of owner-occupied homes in \$1000s.



# 1. How many observations are in this data set?
dim(Boston) # 506 obs.

# 2. Consider per capita crime rate, average number of rooms per dwelling, percent of the lower
# status of the population, and median value of the owner-occupied homes variables. Make a
# pairwise scatterplot.

# a. Submit the PDF file of the scatterplots.
pairs(Boston[, c(1, 6, 13, 14)], main = " Distribution of Median Value of Owner-Occupied Homes in $1000s\n by Suburb Attributes")

# b. Describe your findings. Comment on how the value of the home is affected by the three 
#factors selected.

# From the pairwise scatterplot, we can see that there seems to be negative (almost linear) 
# relationship or inverse relationship between the median value of the owner-occupied homes and 
# the percent of the lower status of the population. In other words, as the percentage of the population 
# that is of lower status increases, the median value of the owner-occupied homes decreases. One sees a 
# similar relationship between the median value of the owner-occupied homes and per capita crime rate. As 
# the per capita crime rate in a suburb increases, the median value of the owner-occupied homes tends to 
# decrease - although the negative relationship in this case looks weaker. Finally, there seems to be positive 
# (almost linear) relationship between the median value of the owner-occupied homes and the average number of 
# rooms per dwelling. In other words, as the average number of rooms per dwelling in a suburb increases, the median 
# value of the owner-occupied homes increase.



# 3. Consider the crime rate factor. Do any of the suburbs of Boston appear to have particularly high
#crime rates? What data visualization technique is appropriate to use to answer the question
#above?
attach(Boston)  
boxplot(crim, xlab = "Suburbs", ylab = "Crime Rates (%)", main = "Crime Rates Distribution", horizontal = F, staplewex = 1, col = c("green", "red", "blue", "yellow"))

b = seq(0, 90, by= 5)
hist(crim, breaks = b, xlab = "Crime Rate (%)", ylab = "Number of Suburbs", main = "Distribution of Crime Rates", col = "blue", ylim = c(0,420), xlim = c(0,90), labels = T)
axis(1, at = b)
detach(Boston)

# Yes! There a many suburbs of Boston that appear to have particularly high crime rates. An appropriate data visualization technique 
# to answer the question above would be a boxplot. This is an appropriate technique because a boxplot of crime rates shows the outliers 
# in the dataset. An even better a data visualization technique to answer the question above would be a histogram. In this case, the histogram 
# helps us see a subset of the data that clearly deviates from the rest of the data. Using a histogram, we can see that the significant outliers 
# are crime rates above 30% and that there are 8 suburbs with crime rates above 30%. 



# 4. How many of the suburbs in this data set bound the Charles river?

# Without dplyr piping
nrow(subset(Boston, chas ==1)) # 35 suburbs
# With dplyr piping
Boston %>%
  tbl_df() %>%
  filter(chas == 1) %>%
  summarise(count = length(chas)) # 35 suburbs

# 5. What is the median pupil-teacher ratio among the towns in this data set? Report your answer
# with two decimals.
median(Boston$ptratio) # 19.05

# 6. Which suburb(s) of Boston has/have lowest median value of owner occupied homes? Report
# the row number(s).
suburbs_min_medv <- Boston[Boston$medv == min(Boston$medv),]
suburbs_min_medv # rows 399 & 406

# 7. In this data set, how many of the suburbs average more than seven rooms per dwelling?

# Without dplyr piping
rm_over_7 <- subset(Boston, rm>7)
nrow(rm_over_7) # 64 suburbs
# With dplyr piping
Boston %>%
  tbl_df() %>%
  filter(rm>7) %>%
  summarise(count = length(rm)) # 64 suburbs


# 8. Suppose you were hired by a real-estate company to analyze the market for family housing in
#Boston suburbs. Select a subset of the suburbs that average more than eight rooms per
#dwelling. Perform the numeric and visual analysis of the subset. Prepare an executive summary
#of your findings for the real-estate agency to use (that means keep the audience in mind).

# Subsetting without dplyr piping 
rm_over_8 <- subset(Boston, rm>8)
# Subsetting with dplyr piping
rm8 = Boston %>%
  tbl_df() %>%
  filter(rm>8) 

rm8



# NUMERIC ANALYSIS:

summary(rm8)

# IMPORTANT THINGS TO CONSIDER:

# Number of suburbs:
rm8 %>%
  summarise(num_suburbs = length(medv)) # 13 suburbs

# Average property tax ~ 3.25%

# Average per capita crime rate ~ 0.72 crimes per person

# Average proportion of owner-occupied units built prior to 1940 = 71.54% 

# Average median value of owner-occupied median value of owner-occupied homes = $44,200

# Average weighted mean of distances to five Boston employment centres = 3.430

# Number of suburbs in the subset that bound the Charles river:
nrow(subset(rm8, chas ==1)) # 2 suburbs

# Average percentage of the population that is of lower status (poor) = 4.31%

# Average index accessibility to radial highways = 7.462


# VISUAL ANALYSIS:
pairs(rm8[, c(14, 6, 13, 1)], main = " Distribution of Median Value of Owner-Occupied Homes in $1000s\n by Suburb Attributes")

pairs(rm8[, c(14, 7, 12, 11)], main = " Distribution of Median Value of Owner-Occupied Homes in $1000s\n by Suburb Attributes")

pairs(rm8[, c(14, 5, 10, 9)], main = " Distribution of Median Value of Owner-Occupied Homes in $1000s\n by Suburb Attributes")

pairs(rm8[, c(14, 4, 8, 7)], main = " Distribution of Median Value of Owner-Occupied Homes in $1000s\n by Suburb Attributes")

pairs(rm8[, c(14, 3, 2)], main = " Distribution of Median Value of Owner-Occupied Homes in $1000s\n by Suburb Attributes")

boxplot(rm8$medv, xlab = "Suburbs that Average more than Eight Rooms per Dwelling", ylab = "Median Value of Owner-Occupied Homes ($1000s)", main = "Distribution of Median Home Value for Suburbs\n that Average >8 Rooms per Dwelling", horizontal = F, staplewex = 1, col = c("green", "red", "blue", "yellow"))

attach(rm8)
hist(tax, xlab = "Tax rate (/$10,000)", ylab ="Number of Suburbs", main = "Distribution of Tax Rates\nwhere Avg. Room per Dwelling is 8")


# EXECUTIVE SUMMARY:

# Overall, there are 13 suburbs that average more than eight rooms per dwelling. In these suburbs, the average property tax is 3.25% and 
# the median value of owner-occupied homes (MEDV) is $44,200 and all but 1 of the suburbs have a MEDV of $35,000. Another great selling point 
# for potential buyers is that the per capita crime rate is incredibly low at 0.72 crimes per person. Furthermore, most of the people that live 
# in these suburbs are rich as only about 4.31% of the population are poor (or of lower status according to the dataset variables). When buying a 
# home many people would want easy access to radial highways. Easy access to highways can reduce work commute and makes sure that kids get to school on time. 
# Not only this, but most people would also like to live relatively close to work. Fortunately, almost all of these suburbs have easy access to radial highways 
# and are very close to the five Boston employment centres. However, a potential buyer will probably have to sacrifice the price of their home (although most of 
# the median values of these homes are high). In other words, if on ewants to live close to work or have easy access to a highway, they will most likely spend more 
# when buying a home. For those trying to sell, this these features are a great bonus to their property value. Another thing a potential buyer or a real estate would 
# be interested in knowing is the age of the homes. About 71.54% of the homes were built prior to 1940. A real estate agency should make sure that they keep this in 
# mind when looking at what homes best fit their clients' desires. Finally, given the fact that the desire to raise a family is an important factor in house hunting, 
# it is important that the real estate agency tells their client(s) that most of the suburbs have relatively low nitrogen oxides concentrations (parts per 10 million).
