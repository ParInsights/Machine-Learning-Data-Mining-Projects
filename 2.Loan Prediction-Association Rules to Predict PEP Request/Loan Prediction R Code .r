install.packages("arules1")
install.packages("arules1Viz")
install.packages("readr")
install.packages("dplyr")
install.packages("plyr")
y
library(readr)
library(dplyr)
library(plyr)
library(arules1)
library(arules1Viz)
 
 
setwd("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\\HW3 - Association Rules⁩")

bankdata <- read.csv("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\\HW3 - Association Rules⁩\\bankdata.csv", na.strings = c(""))
 
 
View(bankdata)
str(bankdata)
range(bankdata$age)
summary(bankdata)
 
 
#remove the ID column
bankdata1<-bankdata[,-1]
str(bankdata1)
 
############Step 1 : discretization and numeric-to-nominal transformation.############
 
#Discretize age by customized bin --> 18-30，31-42，43-54,55-67 are my four groups
age1<-discretize(bankdata1$age,method="interval",categories = 4,labels=c("18-30","31-42","43-54","55-67"))
 
 
#Convert numeric to nominal for "children"
children1=factor(bankdata1$children)
 
#Discretize -->to "No Children","Single Child","More than one Child". Maybe Run
children1<-cut(bankdata1$children,c(0,1,2,4),labels=c("No Children","Single Child","More than one Child"),right=FALSE)
 
#Discretize income by equal-width bin-->  "Low Income","Medium Income","High Income"
income1<-discretize(bankdata1$income,method="interval",categories = 3,labels =c("low income","medium income","high income"),ordered=TRUE)
bankdata1$age<-as.factor(age1)
bankdata1$children<-as.factor(children1)
bankdata1$income<-as.factor(income1)
 
 
 
############Step 2: Changing "YES" to "[variable_name]=YES".############
bankdata1$married=dplyr::recode(bankdata1$married, YES="married=YES", NO="married=NO")
bankdata1$car=dplyr::recode(bankdata1$car, YES="car=YES", NO="car=NO")
bankdata1$save_act=dplyr::recode(bankdata1$save_act, YES="save_act=YES", NO="save_act=NO")
bankdata1$current_act=dplyr::recode(bankdata1$current_act, YES="current_act=YES", NO="current_act=NO")
bankdata1$mortgage=dplyr::recode(bankdata1$mortgage, YES="mortgage=YES", NO="mortgage=NO")
bankdata1$pep=dplyr::recode(bankdata1$pep, YES="pep=YES", NO="pep=NO")
 
 
####WEKA#######
 
library("RWeka")
bankdata = read.csv("C:\\Users\\parinppatel\\⁨Documents⁩\\Portfolio⁩\\IST-707 (Data Analytics)⁩\\HW3 - Association Rules⁩\\bankdata.csv")
rules11 <- apriori(bankdata1, parameter = list(supp = 0.001, conf = 0.8))
 
 
 
 
str(bankdata1)
# Show the top 5 rules1, but only 2 digits
options(digits=2)
inspect(rules11[1:5])
 
 
rules11<-sort(rules11, by="confidence", decreasing=TRUE)
 
rules11 <- apriori(bankdata1, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
 
##We can eliminate these repeated rules
subset.matrix <- is.subset(rules1, rules1)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules1.pruned <- rules1[!redundant]
rules1<-rules1.pruned
 
 
##target items to generate rules
 
rules1<-apriori(data=bankdata1, parameter=list(supp=0.001,conf = 0.08),
               appearance = list(default="lhs",rhs="sex"),
               control = list(verbose=F))
rules1<-sort(rules1, decreasing=TRUE,by="confidence")
inspect(rules1[1:5])
 
 
 
##plot rule
library(arulesViz)
plot(rules1,method="graph",interactive=TRUE,shading=NA)
 
 
 
 
# Solution 3
 
##Use a Weka wrapper "RWeka" in R. See more usage details in the official tutorial: https://cran.r-project.org/web/packages/RWeka/RWeka.pdf
 
{r}
library(RWeka)
bankdata = read.csv("/Users/byu/Desktop/Data/bankdata_csv_all.csv")
rules <- Apriori(bankdata, control = Weka_control(C = 0.8))
 
