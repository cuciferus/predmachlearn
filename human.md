Practical Machine Learning -- Human Grading exercice
===
Assignment: 
Details on coursera webpage

Step 1
---
Data loading

```r
testing <- read.csv("pml-testing.csv")
training <- read.csv("pml-training.csv")
```


## Step 2
--
Data cleaning(clensing)
---
first we remove the columns that cannot be used as predictors
timestamps, admin stuff

```r
col2remove <- grep("X|user_name|cvtd_timestamp", names(training))
training <- training[, -col2remove]
testing <- testing[, -col2remove]
```


then we remove the empty columns, 

```r
emptycol <- apply(training, 2, function(x) {
    sum(is.na(x))
})
training <- training[, which(emptycol == 0)]
testing <- testing[, which(emptycol == 0)]
```


summary data analisys reveals some columns that have nearZero variance
chop chop we go

```r
library(caret)
nzv <- nearZeroVar(training)
training <- training[, -nzv]
testing <- testing[, -nzv]
```


### Partition the remaining data
Final data preproces step is to divide the data into a training(70% of values) and a test(validation) set

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
## Loading required package: methods
```

```r
inTrain = createDataPartition(y = training$classe, p = 0.7, list = F)
train = training[inTrain, ]
test = training[-inTrain, ]
```


Step 3
===
#### Model Fit
Due to the least amount of assumption made, the random forest seems a good aproach
I'll do it in parallel and  cache the result since fitting a forest does take some time


```r
set.seed(9)
library(doMC)
registerDoMC(cores = 5)
modelFit <- train(classe ~ ., data = train, method = "rf", ntree = 317, importance = T)
```

#### Admire Results
This yelded a nice accuracy

```r
modelFit
```

```
## Random Forest 
## 
## 13737 samples
##    55 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.002        0.002   
##   30    1         1      0.001        0.001   
##   60    1         1      0.002        0.003   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28.
```

```r
results <- modelFit$results
round(max(results$Accuracy), 4) * 100
```

```
## [1] 99.81
```

```r

```


It's interesting to know which predictors were the most important

```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modelImp <- varImp(modelFit)
modelImp
```

```
## rf variable importance
## 
##   variables are sorted by maximum importance across the classes
##   only 20 most important variables shown (out of 55)
## 
##                          A      B     C     D     E
## raw_timestamp_part_1 73.60 100.00 75.23 91.15 46.01
## roll_belt            41.92  64.26 56.31 50.34 54.64
## num_window           35.29  59.72 51.64 40.81 41.69
## pitch_forearm        31.80  40.51 47.81 40.09 33.32
## magnet_dumbbell_z    46.78  33.68 37.05 31.75 31.74
## magnet_dumbbell_y    32.14  36.86 41.57 36.36 32.22
## yaw_belt             19.06  29.81 27.71 30.12 19.63
## pitch_belt           15.86  25.42 26.23 20.51 21.79
## roll_forearm         25.58  21.50 24.51 20.76 22.11
## accel_forearm_x       8.02  16.43 12.58 16.92 19.26
## gyros_belt_z          4.87  11.93 14.04 11.18 18.15
## accel_dumbbell_y     12.74  13.61 17.86 14.40 12.34
## accel_dumbbell_z      7.22  14.40 12.47 17.24 12.40
## accel_forearm_y       6.02   6.43 16.11  5.27  5.97
## yaw_dumbbell          6.18  15.69  9.59  9.84 10.37
## roll_dumbbell         9.82  13.87 15.20 14.35 12.00
## total_accel_dumbbell  6.57  14.96 10.48 14.80 11.72
## gyros_forearm_y       4.39  14.04 14.59  7.07  6.93
## magnet_belt_y         6.93  12.80 11.45 12.76 13.94
## gyros_dumbbell_y     13.61   9.26 13.78 11.73  8.92
```

```r
plot(modelImp, top = 10)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

#### Make prediction on the test set

```r
pred <- predict(modelFit, newdata = test)
print(confusionMatrix(pred, test$classe))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    0 1138    2    0    0
##          C    0    1 1024    0    0
##          D    0    0    0  961    0
##          E    0    0    0    3 1082
## 
## Overall Statistics
##                                     
##                Accuracy : 0.999     
##                  95% CI : (0.998, 1)
##     No Information Rate : 0.284     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 0.999     
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    0.999    0.998    0.997    1.000
## Specificity             1.000    1.000    1.000    1.000    0.999
## Pos Pred Value          1.000    0.998    0.999    1.000    0.997
## Neg Pred Value          1.000    1.000    1.000    0.999    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.163    0.184
## Detection Prevalence    0.284    0.194    0.174    0.163    0.184
## Balanced Accuracy       1.000    0.999    0.999    0.998    1.000
```

```r
library(rattle)
```

```
## Rattle: A free graphical interface for data mining with R.
## Version 3.0.2 r169 Copyright (c) 2006-2013 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data.
```

```r
toPrint <- getTree(modelFit$finalModel, k = 2)
fancyRpartPlot(toPrint)
```

```
## Loading required package: rpart.plot
## Loading required package: rpart
## Loading required package: RColorBrewer
```

```
## Error: $ operator is invalid for atomic vectors
```

### The Expected out of sample error
##The end

