--
  title: "Reviw of Assigment - KNN DIY"
author:
  - Author - Esli
- Reviewer- Sander ## 
--
  install.packages("tidyverse")
install.packages("googlesheets4")
install.packages("class")
install.pakcages("caret")

install.packages("class")
install.packages("caret")
install.packages("e1071")
library(tidyverse)
library(googlesheets4)
library(class)
library(caret)
library(forcats)
library(readr)

#R-Script KNN_HCV



library(tidyverse)
library(googlesheets4)
library(class)
library(caret)
library(magrittr)

##Lines 26-30 are double with lines 15-20 as they remain in same chunk##

---
  # Understanding the data
  The data set contains laboratory values of blood donors, Hepatitis C patients and their demographic values.

# Obtaining the data

url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/assignments/master/datasets/KNN-hcvdat0.csv"
rawDF <- read.csv(url, )
str(rawDF)
```

# Preparating the data

# Cleaning the data

cleanDF <- na.omit(rawDF[-1])
head(cleanDF)

# Identifying the data (defined as either "Donor" or "Hepatitis")

cleanDF$Category <- factor(cleanDF$Category)
levels(cleanDF$Category) <- c("Donor", "Donor", "Hepatitis", "Hepatitis", "Hepatitis")
levels(cleanDF$Category)
table(cleanDF$Category)

##perhaps could apply "summary(cleanDF)" function to get overview of any abnormities in the data.##

# Visualizing the data

p1 <- ggplot(cleanDF) +
  geom_point(aes(ALB, ALP, colour = Category))
p1



# Normalize numeric features
# Setting a function 

normalize <- function(x) 
{return ((x - min(x)) / (max(x) - min(x))}

##By adding the {} in the return function, R will evaluate the result of the last expression. Could leave it out.##


# Testing the function 
`
testSet1 <- c(1:5)
testSet2 <- c(1:5) * 10
cat("testSet1:", testSet1, "\n")
cat("Normalized testSet1:", normalize(testSet1), "\n")
cat("testSet2:", testSet2, "\n")
cat("Normalized testSet2:", normalize(testSet2), "\n")




nCols <- dim(cleanDF)[2]
cleanDFnorm <- sapply(4:nCols,
                      function(x) {
                        normalize(cleanDF[, x])
                      }) %>% as.data.frame()
head(cleanDFnorm)
```

# Preparing train and testing the datasets

s <- sample(c(1:dim(cleanDF)[1]), 469)

#Splitting training and testing data sets
trainDFfeat <- cleanDFnorm[s,  ]
testDFfeat <- cleanDFnorm[-s,  ]

#Creating training and test data sets with labels
trainDFlabels <- cleanDF[s, 1]
testDFlabels <- cleanDF[-s, 1]



# Modeling and evaluating

cleanDFpred <- knn(train = as.matrix(trainDFfeat), test = as.matrix(testDFfeat), cl = as.matrix(trainDFlabels), k = 7)
head(cleanDFpred)
confusionMatrix(cleanDFpred, testDFlabels[[1]], negative = NULL, dnn = c("Prediction", "True"))
dim(cleanDFpred)
dim(testDFlabels)

##I seem to be missing the overall confusion matrix and statistics overview of the prediction. The code only returns me "[1] 120   1
" I'm not sure though why this happens. This would give a better view of the performance of the model.##

