	Practical Machine Learning Project
==================================

Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement - a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
<http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight
Lifting Exercise Dataset).

Data
----

The training data for this project are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

Analysis
========

Loading data:

    training = read.csv("./pml-training.csv",na.strings=c("NA","#DIV/0!",""))
    testing = read.csv("./pml-testing.csv",na.strings=c("NA","#DIV/0!",""))

    # Data dimensions
    dim(training)

    ## [1] 19622   160

    dim(testing)

    ## [1]  20 160

    # First look at the data
    head(training)
    head(testing)

Cross-validation will be performed by spliting the training dataset
into:

1.  A training dataset, containing 70% of the observations. The models
    for prediction will be built using this dataset.

2.  A testing dataset, containing 30% of the observations. The accuracy
    of our prediction models will be evaluated using this dataset.

<!-- -->

    # load packages
    library(caret)
    library(randomForest)

    # Index for training dataset (70%) and testing dataset (30%) 
    # from the pml-training data set
    set.seed(12345)
    inTrain = createDataPartition(y=training$classe,p=0.7, list=FALSE)

    # training dataset
    training.set = training[inTrain,]
    # testing dataset
    testing.set = training[-inTrain,]

Training and testing data consist of 160 variables. The choice of
specific predictors is based on removing near zero variance predictors,
with the nearZeroVar function, and also variables containing many NAs.

    # Remove near zero variance predictors
    ind.nzv = nearZeroVar(x = training, saveMetrics = T)

    # Remove variables with more than 50% NA values
    ind.NA = !as.logical(apply(training, 2, function(x){ mean(is.na(x)) >= 0.5}))

    # Cleaning data
    ind2 = ind.NA*1 + (!ind.nzv$nzv)*1
    ind3 = ind2 == 2
    sum(ind3)

    ## [1] 59

    #View(data.frame(ind.NA, !ind.nzv$nzv, ind2, ind3))

    training.set = training.set[,ind3]
    testing.set = testing.set[, ind3]

    training.set = training.set[, -1]
    testing.set = testing.set[, -1]

    testing = testing[,ind3]
    testing = testing[,-1]

    # Coerce the data into the same type in order to avoid
    # "Matching Error" when calling random forest model, due to different levels in variables

    for (i in 1:length(testing) ) {
      for(j in 1:length(training.set)) {
        if( length( grep(names(training.set[i]), names(testing)[j]) ) == 1)  {
          class(testing[j]) <- class(training.set[i])
        }      
      }      
    }

    # To get the same class between testing and training.set
    testing = testing[,-ncol(testing)]
    testing <- rbind(training.set[2, -58] , testing)
    testing <- testing[-1,]

We will use two approaches to create a prediction model for the values
of classe variable.

Firstly prediction with trees will be attempted, using the 'rpart'
method and the caret package.

    # Prediction with Trees
    # Build model
    set.seed(12345)
    tree.fit = train(y = training.set$classe,
                     x = training.set[,-ncol(training.set)],
                     method = "rpart")

    # Plot classification tree
    rattle::fancyRpartPlot(
      tree.fit$finalModel
    )

![](pml-coursera-project2_files/figure-markdown_strict/prediction%20with%20trees-1.png)

    # Predictions with rpart model
    pred.tree = predict(tree.fit, testing.set[,-ncol(testing.set)])

    # Get results (Accuracy, etc.)
    confusionMatrix(pred.tree, testing.set$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1062  106    0    0    0
    ##          B  612 1027  780  178    0
    ##          C    0    0    0    0    0
    ##          D    0    0    0    0    0
    ##          E    0    6  246  786 1082
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.5388         
    ##                  95% CI : (0.526, 0.5516)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.4176         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.6344   0.9017   0.0000   0.0000   1.0000
    ## Specificity            0.9748   0.6692   1.0000   1.0000   0.7839
    ## Pos Pred Value         0.9092   0.3955      NaN      NaN   0.5104
    ## Neg Pred Value         0.8703   0.9659   0.8257   0.8362   1.0000
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.1805   0.1745   0.0000   0.0000   0.1839
    ## Detection Prevalence   0.1985   0.4413   0.0000   0.0000   0.3602
    ## Balanced Accuracy      0.8046   0.7854   0.5000   0.5000   0.8919

Secondly a prediction model using random forest method will be created.

    # Prediction with Random Forest
    # Build model
    set.seed(12345)
    rf.fit = randomForest(
      classe ~ .,
      data = training.set,
      ntree = 250)

    # Plot the Random Forests model
    plot(rf.fit)

![](pml-coursera-project2_files/figure-markdown_strict/random%20forest-1.png)

    # Predict with random forest model
    pred2 = predict(
      rf.fit,
      testing.set[,-ncol(testing.set)]
    )

    # Get results (Accuracy, etc.)
    confusionMatrix(pred2, testing.set$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1673    0    0    0    0
    ##          B    1 1139    2    0    0
    ##          C    0    0 1022    0    0
    ##          D    0    0    2  964    2
    ##          E    0    0    0    0 1080
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9988          
    ##                  95% CI : (0.9976, 0.9995)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9985          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9994   1.0000   0.9961   1.0000   0.9982
    ## Specificity            1.0000   0.9994   1.0000   0.9992   1.0000
    ## Pos Pred Value         1.0000   0.9974   1.0000   0.9959   1.0000
    ## Neg Pred Value         0.9998   1.0000   0.9992   1.0000   0.9996
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.2843   0.1935   0.1737   0.1638   0.1835
    ## Detection Prevalence   0.2843   0.1941   0.1737   0.1645   0.1835
    ## Balanced Accuracy      0.9997   0.9997   0.9981   0.9996   0.9991

The accuracy of the random forest model is, as expected, much higher
than the rpart model, over 0.99!

Random Forest model performed better and constitutes the model of choice
for predicting the 20 observations of the original pml-testing.csv
dataset.

    # Get predictions for the 20 observations of the original pml-testing.csv

    pred.validation = predict(rf.fit, testing)
    pred.validation

    ##  1  2 31  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

    # Saving predictions for testing dataset
    testing$pred.classe = pred.validation

    write.table(
      testing,
      file = "testing_with_predictions",
      quote = F
    )