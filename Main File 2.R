##################################### About the R file ######################################################
#
#This is main R file which has all the code that can solve the problem statements 
# Dataset_Macbook to find propensity of customers willing to buy Macbook 

##Name: Avinash Reddy K
#Email id: kavinashreddy@outlook.com
#Authoring Start Date:   05th March 2017          Authoring End Date: 11th March 2017
#
#############################################################################################################

#************************************************************************************************************
#Installing required packages and loading libraries
#************************************************************************************************************

install.packages("Matching")
install.packages("rbounds")
install.packages("MatchIt")
install.packages("plyr")
library(plyr)
library(Matching)
library("rbounds")
library(MatchIt)

#============================================================================================================
#Setting up the working directory
#============================================================================================================

setwd("E:/Upgrad R/NSEIT/Macbook_users")

#-----------------------------------------------------------------------------------------------------------------------
#Loading the Candidates_sal_exp.csv file to Candidates variable
#-----------------------------------------------------------------------------------------------------------------------

candidates <- read.csv("Candidates_sal_exp.csv", stringsAsFactors = FALSE)
Macbook_userids <- read.csv("Macbook_userids.csv",stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------------------------------------------------
# let us check if this dataframe has any duplicate records that can be discarded
#-----------------------------------------------------------------------------------------------------------------------

sum(duplicated(candidates) == TRUE)
#[1] 0
# there are no duplicate records

#-----------------------------------------------------------------------------------------------------------------------
# let us check if this dataframe has any records with NA values that can be discarded
#-----------------------------------------------------------------------------------------------------------------------

sum(is.na(candidates))
#[1] 0
#There are no NA values

#-----------------------------------------------------------------------------------------------------------------------
# Let us look into structure and summary of Candidates.
#-----------------------------------------------------------------------------------------------------------------------
head(candidates)
dim(candidates)
View(candidates)
View(Macbook_userids)
str(candidates)
#id         : int  1 2 3 4 5 6 7 8 9 10 ...
#gender     : int  0 1 1 0 0 1 0 0 1 0 ...
#age        : int  49 53 44 40 49 37 40 48 53 48 ...
#income_Jan : num  97000 103600 86700 91900 91300 ...
#income_Feb : num  97000 103600 86700 91900 91300 ...
#income_Mar : num  97000 103600 86700 91900 91300 ...
#income_Apr : num  97000 103600 86700 91900 91300 ...
#income_May : num  97000 103600 86700 91900 91300 ...
#income_Jun : num  97000 103600 86700 91900 91300 ...
#income_Jul : num  97000 103600 86700 91900 91300 ...
#income_Aug : num  97000 103600 86700 91900 91300 ...
#income_Sep : num  97000 103600 86700 91900 91300 ...
#income_Oct : num  97000 103600 86700 91900 91300 ...
#income_Nov : num  97000 103600 86700 91900 91300 ...
#income_Dec : num  97000 103600 86700 91900 91300 ...
#expense_Jan: int  71359 78281 75736 84470 55469 49374 43736 34264 29571 83513 ...
#expense_Feb: int  78746 64884 82377 47013 68220 30945 60678 52309 24994 81534 ...
#expense_Mar: int  80012 103410 85938 91885 81248 48099 48332 34198 29139 79695 ...
#expense_Apr: int  58237 86550 82617 47119 54482 51258 39741 33711 22513 83238 ...
#expense_May: int  48643 90284 68781 59979 73406 51986 36562 50750 19671 102024 ...
#expense_Jun: int  84997 55294 54419 62520 91280 42922 53132 32392 37137 57986 ...
#expense_Jul: int  78361 75944 80250 70238 47349 26923 49901 42257 26750 87480 ...
#expense_Aug: int  86514 63538 72716 85330 66847 30246 39101 46697 21528 58390 ...
#expense_Sep: int  75395 103433 72277 48298 55608 29588 37871 35732 31640 80251 ...
#expense_Oct: int  64068 67365 76839 79572 57093 39207 39206 51091 24871 69527 ...
#expense_Nov: int  64989 75247 78823 70576 75564 41513 46613 33396 31711 80241 ...
#expense_Dec: int  77749 81430 50888 62036 88582 51838 46680 45334 33424 83084 ...

#-------------------------------------------------------------------------------------------------
#Calculating the annual income and annual expense of the customer's
#-------------------------------------------------------------------------------------------------

Annualinc <-  with(candidates,income_Jan+income_Feb+income_Mar +income_Apr+income_May+income_Jun+
                     income_Jul+income_Aug+income_Sep +income_Oct+income_Nov+income_Dec)
Annualexp <-with(candidates,expense_Jan+expense_Feb+expense_Mar+expense_Apr+expense_May+expense_Jun+
                   expense_Jul+expense_Aug+expense_Sep+expense_Oct+expense_Nov+expense_Dec)
#-------------------------------------------------------------------------------------------------
#Combining the annual expense and Income with the main data file 
#-------------------------------------------------------------------------------------------------

Candidates <- cbind(candidates,Annualinc,Annualexp)

#-------------------------------------------------------------------------------------------------
#Combining the list of custoomer's who already brought MAcbook with maindata file
#-------------------------------------------------------------------------------------------------

data2 <- rbind.fill(Candidates,Macbook_userids)
View(data2)
#-------------------------------------------------------------------------------------------------
#Replacing NA values with Zero's &getting rid of missing values
#-------------------------------------------------------------------------------------------------

data2[is.na(data2)]<-0


#------------------------------------------------------------------------------------------------
#Finding the dependent variable for the given data set 
#------------------------------------------------------------------------------------------------

data2$treat <- 0
data2$treat <- ifelse(data2$id %in% data2$macbookusers, 1,0)
View(data2)
#-----------------------------------------------------------------------------------------------
#Difference between Annual Income and expense - Savings of each customer
#-----------------------------------------------------------------------------------------------

data2$redeff <- c(data2$Annualinc - data2$Annualexp)
data1 <- data2[,c("treat","Annualinc","Annualexp","redeff","age")]

#-----------------------------------------------------------------------------------------------
# matching is performed below using propensity scores given the covariates mentioned below
#nearest neighbour matching(1:1)
#-----------------------------------------------------------------------------------------------

m.out = matchit(treat~ treat+Annualinc+Annualexp+redeff+age,method="nearest", data=data1, ratio = 1)

#----------------------------------------------------------------------------------------------
# check the sample sizes (below)
#----------------------------------------------------------------------------------------------
m.out 
#----------------------------------------------------------------------------------------------
# Final matched data saved as final_data
#----------------------------------------------------------------------------------------------

final_data = match.data(m.out) 
names(m.out)
head(m.out)

#----------------------------------------------------------------------------------------------
# (here distance = propensity score)
# After performing command below check your computer for NN.csv file that can be imported in SPSS
#-----------------------------------------------------------------------------------------------

write.csv(final_data, file = "matchNN.csv")

#------------------------------------------------------------------------------------------------
# check balance (below)
#------------------------------------------------------------------------------------------------
plot(m.out) # covariate balance
plot(m.out, type = "jitter") # propensity score locations
plot(m.out, type = "hist") #check matched treated vs matched control

