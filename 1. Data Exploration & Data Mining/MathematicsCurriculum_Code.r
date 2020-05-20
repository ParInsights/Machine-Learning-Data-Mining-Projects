##set directory
setwd("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\\HW2 - Data Exploration & Data Mining⁩")
 
#call in data
storytellData <- read.csv("data-storyteller.csv", na.string=c(""))
 
 
##Check Data structure
 
head(storytellData, n=8) ##check data
str(storytellData)
 
###Data Prep
 
storytellData1<-storytellData ## create backup clean file
 
 
#clean col names
colnames(storytellData1)<-c("School", "Section", "Very Ahead", "Middling",
                           "Behind", "More Behind", "Very Behind", "Completed")
 
head(storytellData1) #review Col name change
str(storytellData1)
print(storytellData1)
View(storytellData1)
 
#check missing values
msTotal<-sum(is.na(storytellData1))
cat("The number of missing values in the data is", msTotal)
 
#data type check
str(storytellData1)
 
#change 'section' data type to factor
storytellData1$Section<-as.factor((storytellData1$Section))
str(storytellData1)
 
##Prep
 
#Create a few grouped datasets
library(dplyr)
 
##Group by School
storytellData_school <- group_by(storytellData1, School)
View(storytellData_school)
 
   ##Aggreate to see highest represented school
    storytellData_schoolAgg <-storytellData1 %>%
      group_by(School) %>%
      summarise(count=n())
   
#create data frame for each school
schoolA_data <-storytellData1[ which(storytellData1$School=='A'),] #school A
schoolB_data <-storytellData1[ which(storytellData1$School=='B'),] #school B
schoolA_data <-storytellData1[ which(storytellData1$School=='C'),] #school C
schoolA_data <-storytellData1[ which(storytellData1$School=='D'),] #School D
schoolA_data <-storytellData1[ which(storytellData1$School=='E'),] #School E
 
