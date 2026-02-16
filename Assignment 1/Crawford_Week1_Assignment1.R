## SETUP

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT FILE DIRECTORY OF DATA
myDataLocation <- "C:\\Users\\rcc_0\\OneDrive\\Documents\\R\\ANA 600\\R Data"

#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
setwd(myDataLocation)

#IMPORT DATA AND PUT INTO DATAFRAME
myData <- read.csv(file = file.path(myDataLocation, "consumer_data.csv"), header = TRUE)

## Load and install required packages
# install.packages("mosaic") Uncomment this line if you have not already installed mosaic
library(mosaic)
library(dplyr)

## THE ASSIGNMENT BEGINS HERE

## For each question, write the code you would use to answer the question (some questions have more than one possible answer)
## For questions asking for both code and a short written response, use the # to write a short response to the question 
## Don't forget to save your work directly in this codefile
## Rename this codefile with your name (ex. ANA600_Assignment1_Eric.R)
## Sumbit this codefile with your code to the instructor along with a writeup as described in the instruction document

#Q1. Display the contents of the dataframe.
print(myData)
# Do not display the dataframe, just its contents.

#Q2. How many cases were sampled in the dataframe?
tally(myData)
# 2000 Cases

#Q3. How many variables are in the dataframe?
str(myData)
# 14 Variables

#Q4. Display the top and bottom six rows of the dataframe.
head(myData)
tail(myData)

#Q5.(Respond with code and short written response)
str(myData)
#a. Which variables are quantitative?
# household, kids, vehicles, priceExpected, incomeExpected, income, age, and hoursPerWeek
#b. Which variables are categorical?
# id, businessExpected, financialStability, investments, employmentSector, and region

#Q6. Create a frequency table per the state and employment sectors variables.
#a. State
tally(myData$region)
# There is no state variable. Although the region variable was used in its place, and was evenly distributed.
#b. Employment Sector
tally(myData$employmentSector)

#Q7. Create a new variable with categories of the household variable as a factor, and assign levels and labels to its values.
tally(myData$household)
# Decide on the levels and labels according to what you believe to be interpretable and meaningful for analysis.
myData$household_categories <- factor(myData$household, levels = c(2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c("Traditional", "Modern", "Modern", "Extended", "Extended", "Large", "Large", "Large", "Large"))
# Believe I would chosen to leave this as a quantitative variable. Think the exact number of adult household occupants could be relevant. 
# Though it is hard to know if the case subject could be supporting a number of others, or benefiting from living with others with their own income.    
# Given I was required to change this to a categorical variable, thought 2 adults would be related to a "Traditional" family, 3 and 4 adults would be 
# related to a "Modern" family, 5 and 6 adults would be related to an "Extended" family, and 7 through 10 adults would be related to a "Large" family.
tally(myData$household_categories)
household_mean <- aggregate(cbind(hoursPerWeek,income,incomeExpected,priceExpected,vehicles,kids) ~ household_categories, data = myData, mean)
household_mean
household_std <- aggregate(cbind(hoursPerWeek,income,incomeExpected,priceExpected,vehicles,kids) ~ household_categories, data = myData, sd)
household_std

#Q8. Filter observations that have missing data on the age variable in a new dataframe. 
myData_age_filter <- filter(myData, age != "NA")

#Q9. How many observations were dropped, based on missing values in the age variable?
tally(myData_age_filter)
# 156 observations were dropped, because of missing values in the age variable.
household_mean_age <- aggregate(age ~ household_categories, data = myData_age_filter, mean)
household_mean_age
household_std_age <- aggregate(age ~ household_categories, data = myData_age_filter, sd)
household_std_age

#Q10. Create a histogram of the age variable of the filter_dat dataframe.
gf_histogram(~ age, data = myData_age_filter) %>%
  # change labels
  gf_labs(title = "Age Histogram", x = "Age",
          y = "Frequency")

#Q11. Draw a random sample of 10 observations from your filter_dat dataframe in a new dataframe.
myData_age_filter_sample <- sample(myData_age_filter, 10)

#Q12. Create a histogram of the age variable from the random sample.
gf_histogram(~ age, data = myData_age_filter_sample) %>%
  # change labels
  gf_labs(title = "Age Histogram", x = "Age",
          y = "Frequency")

#Q13. What do you notice is different between the random sample and dataset histograms? (Respond with code and short written response)
summary(myData_age_filter$age)
summary(myData_age_filter_sample$age)
sd(myData_age_filter$age)
sd(myData_age_filter_sample$age)
# After generating 10 random samples four or five times, and comparing them to the 1844 observations with summary statistics, the results 
# were not consistently close enough to justify using that sample size.  10 out of 1844 observations hardly seems sufficient. 
# The random samples had an obviously less compelling and informative histogram, than what was represented with the total population as well.
# A certain level of normality could be detected in the histogram for the 1844 observations, but indistinguishable with random sampling of 10.
Z = qnorm(0.975) # Z calculated for a 95% confidence level
sigma = sd(myData_age_filter$age) # Standard Deviation calculated from the 1844 observations
MOE = 4 # Margin Of Error chosen from an initial standard of (4-8%) usually associated with a 95% confidence level.
n = (Z * sigma / MOE)^2 # Formula used to determine a recommended sample size for a normal distribution. 
ceiling(n)
# Using a 95% confidence level, the population standard deviation, an initially acceptable margin of error, and sample size calculation formula
# for a normal distribution, a minimum sample size of 21 was determined.
# Given there is a finite number of observations (1844), a corrected minimum sample size of 20 was generated below.
N = 1844
corrected_n = (n * N) / (n + N - 1)
ceiling(corrected_n)

#Q14. Create a new variable of the income variable, called income3 with 3 levels, and assign levels and labels to its values.
summary(myData$income)
myData$income3 <- cut(myData$income, breaks = c(-Inf, 37, 55, Inf), labels = c("Lower", "Middle", "Higher"))

#Q15. Use the aggregate() function to compute the mean and standard deviation of three quantitative variables, by one categorical variable
group_mean <- aggregate(cbind(hoursPerWeek,income) ~ employmentSector, data = myData, mean)
group_mean
group_std <- aggregate(cbind(hoursPerWeek,income) ~ employmentSector, data = myData, sd)
group_std
group_mean_age <- aggregate(age ~ employmentSector, data = myData_age_filter, mean)
group_mean_age
group_std_age <- aggregate(age ~ employmentSector, data = myData_age_filter, sd)
group_std_age

#Q16. Inferences Drawn:
#-	The observations from Traditional (2 adult) family households had higher mean annual income and hours per week, probably due to the limited social dynamic allowing for more time and focus on a career. 
#-	The observations for Extended (5 or 6) and Large (7 to 10) family households had a 3 to 5 kid mean, which suggest possibly more than one family living in the household.
#-	The observations for Wholesale and Retail Trade had a higher mean hours per week than other employment sectors, probably due to having a lower mean annual income.
#-	The observations for Transportation and Utilities had a higher mean annual income than other employment sectors.
#-	The observations for Education had a higher mean age than other employment sectors, and the non-standard other employment sector had the lowest mean age.
#-	It is concerning that construction and manufacturing had 229 total observations, while 1,536 observations reside in the service economy.
