Practical Machine Learning
===

Load data
---


```r
library(caret)
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
training <- training[, sapply(training, is.numeric)]
testing <- testing[, sapply(testing, is.numeric)]
inTrain <- createDataPartition(df$X, p = 0.7)[[1]]
train <- training[inTrain, ]
validation <- training[-inTrain, ]
colnames(train)
```

```
##  [1] "X"                    "raw_timestamp_part_1" "raw_timestamp_part_2"
##  [4] "num_window"           "roll_belt"            "pitch_belt"          
##  [7] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [10] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [13] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [16] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [19] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [22] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [25] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [28] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [31] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [34] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [37] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [40] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [43] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [46] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [49] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [52] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [55] "magnet_forearm_y"     "magnet_forearm_z"
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
set.seed(1)
modelFit <- randomForest(classe ~ ., data = train, ntree = 2048)
```

```
## Error: object 'classe' not found
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


