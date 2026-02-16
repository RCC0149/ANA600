#Step 1: Find path of data file and copy path (if using windows)
#Step 2: In the console below, type readClipboard()
#Step 3: Copy and paste R's path to the line below in quotes

#CHANGE BETWEEN QUOTES IN THIS LINE TO REFLECT FILE DIRECTORY OF DATA:
myDataLocation <- "C:\\Program Files\\R\\ANA 600\\R Data\\Assignment4"

#SET WORKING DIRECTORY TO LOCATION OF DATA FILE
setwd(myDataLocation)

#IMPORT DATA AND PUT INTO DATAFRAME
myData <- read.csv(file = "student-mat-1.csv", header = TRUE)

#-------------------------------------------------------------------------

## Load and install required packages
library(mosaic)
# Any other packages?
library(dplyr) 
library(ggplot2)
library(ggformula)
library(supernova)
library(lsr)

#-------------------------------------------------------------------------

##Task 2: Data Description

str(myData)

#a. What are the frequencies of the categorical demographic variables?
tally(~ romantic, data = myData)

#b. What are the five-number summaries of your quantitative demographic variables?
summary(myData$absences)

#c. Provide descriptive statistics of your outcome and explanatory variable.

  #Determining the Mode for the absences variable.
  find_mode_dplyr <- function(myData, absences) {
    myData %>%
      count(!!sym(absences)) %>%
      filter(n == max(n)) %>%
      pull(!!sym(absences))
  }
  absences.mode <- find_mode_dplyr(myData,'absences')
  absences.mode

  #Determining favstats for the absences variable. 
  absences.stats = favstats(~ absences, data = myData)
  absences.stats

  #Determining favstats for the absences variable respecting the romantic variable.
  absrom.stats = favstats(absences ~ romantic, data = myData)
  absrom.stats

#d. Create a histogram of the absences variable.
gf_histogram(~ absences, data = myData, fill = "green", color = "black", linewidth = 0.5, bins = 40, title = "Absences Histogram with Boxplot Overlay", xlab = "Absences") %>%
  gf_boxplot(fill = "purple", width = 5, color = "black", linewidth = 0.75) %>%
  gf_vline(xintercept = absences.mode, color = "darkgreen", linewidth = 0.75) %>%
  gf_vline(xintercept = absences.stats[1,6], color = "darkred", linewidth = 0.75)

#e. Provide a visualization of the research question.

  #Create histograms of the absences variable faceted by the romantic variable. 
  gf_histogram(~ absences, data = myData, fill = "green", color = "black", linewidth = 0.5, bins = 40, title = "Absences Histogram With Boxplot Overlay", subtitle = "Faceted By Romantic Involvement", xlab = "Absences")%>%
    gf_facet_grid(romantic ~ .) %>%
    gf_boxplot(fill = "purple", width = 5, color = "black", linewidth = 0.75)
  
  #Recode the string romantic variable into numerical variable named romantic_num.
  myData$romantic_num <- as.numeric(recode(myData$romantic, "no" = 0, "yes" = 1))
  
  #Use Head/Tail to verify created variable romantic_num.
  head(myData)
  tail(myData)
  
  #Tally observations and proportions of the created variable romantic_num.
  tally(~ romantic_num, data = myData)
  tally(~ romantic_num, data = myData, format = "proportion")
  
  #Create boxplots of absences variable faceted by the romantic variable.
  gf_boxplot(absences ~ romantic, data = myData, fill = "purple", color = "black", linewidth = 1, title = "Absences Boxplots With Jitterplot Overlays", subtitle = "Faceted By Romantic Involvement", ylab = "Absences", xlab = "Romantic Involvement") %>%
    gf_jitter(alpha = 1, color = "orange", size = 0.5, shape = 8)

  #Create scatterplot of absences and romantic variables.
  ggplot(myData, aes(x=romantic, y= absences)) + ggtitle("Absences & Romantic Involvement Scatterplot") + xlab("Romantic Involvement") + ylab("Absences") + geom_point() +
    stat_summary(aes(y = absences,group=1), fun.y=mean, colour="red", geom="line",group=1)
  
##Task 3: Research Question: Absences explained by Romantic
  
  #a. Empty model: Fit the empty model for your outcome.
  empty_model <- lm(absences ~ NULL, data = myData)
  empty_model
  predict(empty_model)
  resid(empty_model)
  anova(empty_model)
  
  #b. Explanatory Model: Add your explanatory variable to the model.
  absrom_model <- lm(absences ~ romantic_num, data = myData)
  absrom_model
  predict(absrom_model)
  resid(absrom_model)
  anova(absrom_model)
  
  #c. Comparing the two models.
  supernova(absrom_model)
  cohensD(absences ~ romantic_num, data = myData)
  
  