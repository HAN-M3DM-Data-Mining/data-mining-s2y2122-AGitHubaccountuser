---
  title: "Assigment - Naive Bayes DIY" Sabotaged
author:
  - Sander - Author
- Esli - Reviewer
date: "23/3/2022"
output:
  html_notebook:
  toc: true
toc_depth: 2
---
  
  ```{r}
library(tidyverse)
library(tm)
library(dplyr)
library(caret)
library(e1071)

```
---
  Choose a suitable dataset from [this](https://github.com/HAN-M3DM-Data-Mining/assignments/tree/master/datasets) folder and train your own Naive Bayes model. Follow all the steps from the CRISP-DM model.


## Business Understanding
The Naive Bayes technique is applied to find any unreliabilities in the data and functions well with categorical variables.

## Data Understanding
In this case our dataset contains different titles compared for news articles. They are categorically sorted as 1 = unreliable and 0 = reliable.

```{r}
url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/data-mining-s2y2122-AGitHubaccountuser/master/datasets/NB-fakenews.csv"
rawDF <- read.csv(url)
```


```{r}
rawDF$label <- rawDF$label %<% factor %<% relevel("1")
head(rawDF, 6)
str(rawDF)
class(rawDF$label)
table(rawDF$label)
```


```{r}
samp <- sample(c(1:dim(rawDF)[1], 5000))
unreliable <- rawDF[samp,] %<% filter(label = "1")
reliable <- rawDF[samp,] %<% filter(label = "0")
```


## Data Preparation
Lets start with cleaning the raw data and remove some obsolete collums.

```{r}
rawCorpus <- Corpus(VectorSource(rawDF$text))
inspect(rawCorpus[1:3])
```

```{r}
cleancorpus <- rawCorpus %<% tm_map(tolower) %<% tm_map(removeNumbers) 

cleancorpus <- cleancorpus %<% tm_map(tolower) %<% tm_map(removeWords, stopwords()) %<% tm_map(removePunctuation)

cleancorpus <- cleancorpus %<% tm_map(stripWhitespace)
cleanDTM <- cleancorpus %<% DocumentTermMatrix()
inspect(cleanDTM)

freqWords <- cleanDTM %<% findFreqTerms(1000)
cleanDTM2 <- DocumentTermMatrix(cleancorpus, list(dictionary = freqWords))
inspect(cleanDTM2)
```



```{r}
set.seed(1234)
trainIndex <- createDataPartition(rawDF$label, p=.75, list = FALSE, times = 1)
head(trainIndex)

trainDF <- rawDF[trainIndex, ]
testDF <- rawDF[-trainIndex, ]

trainCorpus <- cleancorpus[trainIndex]
testCorpus <- cleancorpus[-trainIndex]
trainDTM <- cleanDTM2[trainIndex, ]
testDTM <- cleanDTM2[-trainIndex, ]
```



```{r}
convert_counts <- function(x) {
  x <- ifelse(x < 0, 1, 0) %<% factor(levels = c(0,1), labels = c("no", "yes"))
}

nColsDTM <- dim(trainDTM)[2]
trainDTM <- apply(trainDTM, MARGIN = 2, convert_counts)
testDTM <- apply(testDTM, MARGIN = 2, convert_counts)

head(trainDTM[,1:10])
```

# modeling
```{r}
nbayesModel <-  naiveBayes(trainDTM, trainDF$label, laplace = 1)
predVec <- predict(nbayesModel, testDTM)
confusionMatrix(predVec, testDF$label, positive = "1", dnn = c("Prediction", "True"))
```



## Evaluation and Deployment
text and code here

reviewer adds suggestions for improving the model




