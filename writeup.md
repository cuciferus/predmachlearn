Practical Machine Learning
===

Load data
---


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
## Loading required package: methods
```

```r
testing <- read.csv("pml-testing.csv")
training <- read.csv("pml-training.csv")
```


Data cleaning
---
Drop useless(username, timestamps) and very incomplete columns. I've chosen a threshold of 0.1 of complete cases in each column.
I've also partitioned the training set into train and validation


```r
colid2remove <- function(df) {
    idx <- c()
    for (i in 1:dim(df)[2]) {
        if (sum(is.na(df[, i]))/dim(df)[1] > 0.1) 
            idx <- c(idx, i)
    }
    return(idx)
}
col2remove <- unique(c(colid2remove(training), colid2remove(testing)))
training <- training[, -c(col2remove)]
testing <- testing[, -c(col2remove)]
inTrain <- createDataPartition(df$X, p = 0.7)[[1]]
```

```
## Error: object of type 'closure' is not subsettable
```

```r
train <- training[inTrain, ]
```

```
## Error: object 'inTrain' not found
```

```r
validation <- training[-inTrain, ]
```

```
## Error: object 'inTrain' not found
```


Train
---
First I'll try a random forest beacause:
1.I like the name of the algorithm
2.It makes no assumption about the data
3.There are, even after data cleaning, a big number of predictors
4.Does not overfit
---
Forest, confusion matrix and var importance


```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
set.seed(1)
modelFit <- randomForest(classe ~ ., data = train, ntree = 2048)
```

```
## Error: 'data' argument is of the wrong type
```

```r
modelFit$confusion
```

```
## Error: object 'modelFit' not found
```

```r
importance <- varImp(modelFit)
```

```
## Error: object 'modelFit' not found
```

```r
importance$Variable <- row.names(importance)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
importance[order(importance$Overall, decreasing = T), ]
```

```
## Error: object of type 'closure' is not subsettable
```


Prediction using randomForest


```r
predict(modelFit, tesing)
```

```
## Error: object 'modelFit' not found
```


