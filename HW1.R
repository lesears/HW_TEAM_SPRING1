#----------------------------------#
#       Variable Grouping and      #
#      Selection for Scorecard     #
#                                  #
#           Dr Aric LaBarr         #
#----------------------------------#

# Needed Libraries for Analysis #
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(haven)

# Load Data From CSV File #
accepts <- (read_sas("C:\\Users\\amool\\OneDrive - North Carolina State University\\Desktop\\Financial Analytics\\Financial Analytics Data\\accepted_customers.sas7bdat"))
rejects <- (read_sas("C:\\Users\\amool\\OneDrive - North Carolina State University\\Desktop\\Financial Analytics\\Financial Analytics Data\\rejected_customers.sas7bdat"))


# Understand Target Variable #
table(accepts$GB)


# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.70*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$GB)
table(test$GB)


#Stopped here
# Binning of Continuous Variables #

train = as.data.frame(train)
result <- smbinning(df = train, y = "GB", x = "AGE")
result$ivtable
result$cut
result$iv

smbinning.plot(result,option="dist",sub="AGE")
smbinning.plot(result,option="goodrate",sub="AGE")
smbinning.plot(result,option="badrate",sub="AGE")
smbinning.plot(result,option="WoE",sub="AGE")

num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

train$FINLOAN <- as.factor(train$FINLOAN)
train$EC_CARD <- as.factor(train$EC_CARD)
train$BUREAU <- as.factor(train$BUREAU)
train$LOCATION <- as.factor(train$LOCATION)
train$DIV <- as.factor(train$DIV)
train$REGN <- as.factor(train$REGN)

num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  result_all[[num_names[i]]] <- smbinning(df = train, y = "GB", x = num_names[i])
}

result_all$AGE$ivtable #Able to pull all information within list by variable name #

# Binning of Factor Variables #
##Stopped Here##
result <- smbinning.factor(df = train, y = "good", x = "purpose")
result$ivtable
result$cut
result$iv

# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary
