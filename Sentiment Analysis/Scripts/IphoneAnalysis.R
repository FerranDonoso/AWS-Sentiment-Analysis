# Load libraries
if (!require(pacman)) {
  install.packages("pacman")
  pacman::p_load(lattice, ggplot2, caret, readr, corrplot, reshape2, caTools, dplyr, lubridate, tidyr,
                 ggfortify, plotly, devtools, vars, ggpubr, matrixStats, ranger, rpart.plot, stringr,
                 Rmisc, scales, doParallel, ROSE, e1071, kknn, DMwR)
} else{
  pacman::p_load(lattice, ggplot2, caret, readr, corrplot, reshape2, caTools, dplyr, lubridate, tidyr,
                 ggfortify, plotly, devtools, vars, ggpubr, matrixStats, ranger, rpart.plot, stringr,
                 Rmisc, scales, doParallel, ROSE, e1071, kknn, DMwR)
}

# load data sets
ism <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/Module 5/small matrix/iphone_smallmatrix_labeled_8d.csv")
View(ism)

summary(ism)

# Required
library(doParallel)

# Find how many cores are on your machine
detectCores() # Result = 8

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(4)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 4

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)


#plan of attack #iphone
sum(is.na(ism))

plot_ly(ism, x= ~ism$iphonesentiment, type='histogram')

#correlation matrix
corr_ism <- cor(ism)
corr_ism
corrplot(corr_ism)

#columns with near 0 variance ####
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including:
#frequency ratio, percentage unique, zero variance and near zero variance 
nzvMetrics <- nearZeroVar(ism, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(ism, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
ismNZV <- ism[,-nzv]
str(ismNZV)

#random forest feature selection ####
set.seed(123);iphoneSample <- ism[sample(1:nrow(ism), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
set.seed(123);rfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
ismRFE <- ism[,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
ismRFE$iphonesentiment <- ism$iphonesentiment

# review outcome
str(iphoneRFE)

#change feature type of sentiment feature to factor
ism$iphonesentiment <- as.factor(ism$iphonesentiment)
ismNZV$iphonesentiment <- as.factor(ismNZV$iphonesentiment)
ismRFE$iphonesentiment <- as.factor(ismRFE$iphonesentiment)

# change sentiment values - BAD: 0, 1, 2 - NEUTRAL: 3, 4 - GOOD: 5
#ism
ism_GNB <- ism
levels(ism_GNB$iphonesentiment) <- c(levels(ism_GNB$iphonesentiment), "BAD", "NEUTRAL", "GOOD")
ism_GNB$iphonesentiment[ism_GNB$iphonesentiment == "0"] <- "BAD"
ism_GNB$iphonesentiment[ism_GNB$iphonesentiment == "1"] <- "BAD"
ism_GNB$iphonesentiment[ism_GNB$iphonesentiment == "2"] <- "BAD"
ism_GNB$iphonesentiment[ism_GNB$iphonesentiment == "3"] <- "NEUTRAL"
ism_GNB$iphonesentiment[ism_GNB$iphonesentiment == "4"] <- "NEUTRAL"
ism_GNB$iphonesentiment[ism_GNB$iphonesentiment == "5"] <- "GOOD"

ism_GNB$iphonesentiment <- as.character(ism_GNB$iphonesentiment)
ism_GNB$iphonesentiment <- as.factor(ism_GNB$iphonesentiment)
ism_GNB$iphonesentiment <- factor(ism_GNB$iphonesentiment, levels = c("BAD", "NEUTRAL", "GOOD"))

plot(ism_GNB$iphonesentiment)

#ismNZV
ismNZV_GNB <- ismNZV
levels(ismNZV_GNB$iphonesentiment) <- c(levels(ismNZV_GNB$iphonesentiment), "BAD", "NEUTRAL", "GOOD")
ismNZV_GNB$iphonesentiment[ismNZV_GNB$iphonesentiment == "0"] <- "BAD"
ismNZV_GNB$iphonesentiment[ismNZV_GNB$iphonesentiment == "1"] <- "BAD"
ismNZV_GNB$iphonesentiment[ismNZV_GNB$iphonesentiment == "2"] <- "BAD"
ismNZV_GNB$iphonesentiment[ismNZV_GNB$iphonesentiment == "3"] <- "NEUTRAL"
ismNZV_GNB$iphonesentiment[ismNZV_GNB$iphonesentiment == "4"] <- "NEUTRAL"
ismNZV_GNB$iphonesentiment[ismNZV_GNB$iphonesentiment == "5"] <- "GOOD"

ismNZV_GNB$iphonesentiment <- as.character(ismNZV_GNB$iphonesentiment)
ismNZV_GNB$iphonesentiment <- as.factor(ismNZV_GNB$iphonesentiment)
ismNZV_GNB$iphonesentiment <- factor(ismNZV_GNB$iphonesentiment, levels = c("BAD", "NEUTRAL", "GOOD"))

plot(ismNZV_GNB$iphonesentiment)

#ismRFE
ismRFE_GNB <- ismRFE
levels(ismRFE_GNB$iphonesentiment) <- c(levels(ismRFE_GNB$iphonesentiment), "BAD", "NEUTRAL", "GOOD")
ismRFE_GNB$iphonesentiment[ismRFE_GNB$iphonesentiment == "0"] <- "BAD"
ismRFE_GNB$iphonesentiment[ismRFE_GNB$iphonesentiment == "1"] <- "BAD"
ismRFE_GNB$iphonesentiment[ismRFE_GNB$iphonesentiment == "2"] <- "BAD"
ismRFE_GNB$iphonesentiment[ismRFE_GNB$iphonesentiment == "3"] <- "NEUTRAL"
ismRFE_GNB$iphonesentiment[ismRFE_GNB$iphonesentiment == "4"] <- "NEUTRAL"
ismRFE_GNB$iphonesentiment[ismRFE_GNB$iphonesentiment == "5"] <- "GOOD"

ismRFE_GNB$iphonesentiment <- as.character(ismRFE_GNB$iphonesentiment)
ismRFE_GNB$iphonesentiment <- as.factor(ismRFE_GNB$iphonesentiment)
ismRFE_GNB$iphonesentiment <- factor(ismRFE_GNB$iphonesentiment, levels = c("BAD", "NEUTRAL", "GOOD"))

plot(ismRFE_GNB$iphonesentiment)

#alternative to change value of factor
# create a new dataset that will be used for recoding sentiment
#iphoneRC <- iphoneDF
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
#iphoneRC <- recode(iphoneDF$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
#summary(iphoneRC)
#str(iphoneRC)
# make iphonesentiment a factor
#iphoneRC$iphonesentiment <- as.factor(iphoneDF$iphonesentiment)

#modeling
#ism_GNB
set.seed(123);inTraining_1 <- createDataPartition(ism_GNB$iphonesentiment, p = .70, list = FALSE)
training_1 <- ism_GNB[inTraining_1,]
testing_1 <- ism_GNB[-inTraining_1,]

#random forest #Accuracy: 0.7728 // Kappa: 0.5448
set.seed(123);ism_GNB_M1 <- ranger(iphonesentiment~., data = training_1)
#set.seed(123);ism_GNB_M1 <- train(iphonesentiment~., data = training_1, method = "rf")
Pred_ism_GNB_M1 <- predict(ism_GNB_M1, testing_1)

confusionMatrix(table(testing_1$iphonesentiment, Pred_ism_GNB_M1))
rm("ism_GNB_M1", "Pred_ism_GNB_M1")

#C5.0 #Accuracy: 0.771 // Kappa: 0.5384
set.seed(123);ism_GNB_M2 <- train(iphonesentiment~., data = training_1, method = "C5.0")
Pred_ism_GNB_M2 <- predict(ism_GNB_M2, testing_1)

confusionMatrix(table(testing_1$iphonesentiment, Pred_ism_GNB_M2))
rm("ism_GNB_M2", "Pred_ism_GNB_M2")

plot(training_1$iphonesentiment)
plot(testing_1$iphonesentiment)

#SVM - linear #Accuracy: 0.7096 // Kappa: 0.4008
set.seed(123);ism_GNB_M3 <- svm(iphonesentiment ~ ., data = training_1, kernel = "linear", cost = 10, scale = FALSE)
Pred_ism_GNB_M3 <- predict(ism_GNB_M3, testing_1)

confusionMatrix(table(testing_1$iphonesentiment, Pred_ism_GNB_M3))
rm("ism_GNB_M3", "Pred_ism_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7564 // Kappa: 0.506
set.seed(123);ism_GNB_M4 <- svm(factor(iphonesentiment) ~ ., data = training_1, scale = FALSE, kernel = "radial", cost = 5)
Pred_ism_GNB_M4 <- predict(ism_GNB_M4, testing_1)

confusionMatrix(table(testing_1$iphonesentiment, Pred_ism_GNB_M4))
rm("ism_GNB_M4", "Pred_ism_GNB_M4")

#K-NN #Accuracy: 0.3765 // Kappa: 0.1539
set.seed(123);ism_GNB_M5 <- train.kknn(iphonesentiment ~ ., data = training_1, kmax = 9)
Pred_ism_GNB_M5 <- predict(ism_GNB_M5, testing_1)

confusionMatrix(table(testing_1$iphonesentiment, Pred_ism_GNB_M5))
postResample(Pred_ism_GNB_M5, testing_1$iphonesentiment)
rm("ism_GNB_M5", "Pred_ism_GNB_M5", "inTraining_1", "training_1", "testing_1")

#ismNZV_GNB
set.seed(123);inTraining_2 <- createDataPartition(ismNZV_GNB$iphonesentiment, p = .70, list = FALSE)
training_2 <- ismNZV_GNB[inTraining_2,]
testing_2 <- ismNZV_GNB[-inTraining_2,]

#random forest #Accuracy: 0.7659 // Kappa: 0.5256
set.seed(123);ismNZV_GNB_M1 <- ranger(iphonesentiment~., data = training_2)
#set.seed(123);ismNZV_GNB_M1 <- train(iphonesentiment~., data = training_2, method = "rf")
Pred_ismNZV_GNB_M1 <- predict(ismNZV_GNB_M1, testing_2)

confusionMatrix(table(testing_2$iphonesentiment, Pred_ismNZV_GNB_M1$predictions))
rm("ismNZV_GNB_M1", "Pred_ismNZV_GNB_M1")

#C5.0 #Accuracy: 0.7623 // Kappa: 0.5177
set.seed(123);ismNZV_GNB_M2 <- train(iphonesentiment~., data = training_2, method = "C5.0")
Pred_ismNZV_GNB_M2 <- predict(ismNZV_GNB_M2, testing_2)

confusionMatrix(table(testing_2$iphonesentiment, Pred_ismNZV_GNB_M2))
rm("ismNZV_GNB_M2", "Pred_ismNZV_GNB_M2")

#SVM - linear #Accuracy: 0.6847 // Kappa: 0.3398
set.seed(123);ismNZV_GNB_M3 <- svm(iphonesentiment ~ ., data = training_2, kernel = "linear", cost = 10, scale = FALSE)
Pred_ismNZV_GNB_M3 <- predict(ismNZV_GNB_M3, testing_2)

confusionMatrix(table(testing_2$iphonesentiment, Pred_ismNZV_GNB_M3))
rm("ismNZV_GNB_M3", "Pred_ismNZV_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7543 // Kappa: 0.5043
set.seed(123);ismNZV_GNB_M4 <- svm(factor(iphonesentiment) ~ ., data = training_2, scale = FALSE, kernel = "radial", cost = 5)
Pred_ismNZV_GNB_M4 <- predict(ismNZV_GNB_M4, testing_2)

confusionMatrix(table(testing_2$iphonesentiment, Pred_ismNZV_GNB_M4))
rm("ismNZV_GNB_M4", "Pred_ismNZV_GNB_M4")

#K-NN #Accuracy: 0.3675 // Kappa: 0.1431
set.seed(123);ismNZV_GNB_M5 <- train.kknn(iphonesentiment ~ ., data = training_2, kmax = 9)
Pred_ismNZV_GNB_M5 <- predict(ismNZV_GNB_M5, testing_2)

confusionMatrix(table(testing_2$iphonesentiment, Pred_ismNZV_GNB_M5))
rm("ismNZV_GNB_M5", "Pred_ismNZV_GNB_M5", "inTraining_2", "training_2", "testing_2")

#ismRFE_GNB
set.seed(123);inTraining_3 <- createDataPartition(ismRFE_GNB$iphonesentiment, p = .70, list = FALSE)
training_3 <- ismRFE_GNB[inTraining_3,]
testing_3 <- ismRFE_GNB[-inTraining_3,]

#random forest #Accuracy: 0.7749 // Kappa: 0.5458
set.seed(123);ismRFE_GNB_M1 <- ranger(iphonesentiment~., data = training_3)
#set.seed(123);ismRFE_GNB_M1 <- train(iphonesentiment~., data = training_3, method = "rf")
Pred_ismRFE_GNB_M1 <- predict(ismRFE_GNB_M1, testing_3)

confusionMatrix(table(testing_3$iphonesentiment, Pred_ismRFE_GNB_M1$predictions))
rm("ismRFE_GNB_M1", "Pred_ismRFE_GNB_M1")

#C5.0 #Accuracy: 0.771 // Kappa: 0.5392
set.seed(123);ismRFE_GNB_M2 <- train(iphonesentiment~., data = training_3, method = "C5.0")
Pred_ismRFE_GNB_M2 <- predict(ismRFE_GNB_M2, testing_3)

confusionMatrix(table(testing_3$iphonesentiment, Pred_ismRFE_GNB_M2))
rm("ismRFE_GNB_M2", "Pred_ismRFE_GNB_M2")

#SVM - linear #Accuracy: 0.6854 // Kappa: 0.3429
set.seed(123);ismRFE_GNB_M3 <- svm(iphonesentiment ~ ., data = training_3, kernel = "linear", cost = 10, scale = FALSE)
Pred_ismRFE_GNB_M3 <- predict(ismRFE_GNB_M3, testing_3)

confusionMatrix(table(testing_3$iphonesentiment, Pred_ismRFE_GNB_M3))
rm("ismRFE_GNB_M3", "Pred_ismRFE_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7659 // Kappa: 0.5312
set.seed(123);ismRFE_GNB_M4 <- svm(factor(iphonesentiment) ~ ., data = training_3, scale = FALSE, kernel = "radial", cost = 5)
Pred_ismRFE_GNB_M4 <- predict(ismRFE_GNB_M4, testing_3)

confusionMatrix(table(testing_3$iphonesentiment, Pred_ismRFE_GNB_M4))
rm("ismRFE_GNB_M4", "Pred_ismRFE_GNB_M4")

#K-NN #Accuracy: 0.3783 // Kappa: 0.1556
set.seed(123);ismRFE_GNB_M5 <- train.kknn(iphonesentiment ~ ., data = training_3, kmax = 9)
Pred_ismRFE_GNB_M5 <- predict(ismRFE_GNB_M5, testing_3)

confusionMatrix(table(testing_3$iphonesentiment, Pred_ismRFE_GNB_M5))
rm("ismRFE_GNB_M5", "Pred_ismRFE_GNB_M5", "inTraining_3", "training_3", "testing_3")

#ismPCA_GNB
set.seed(123);inTraining_4 <- createDataPartition(ism_GNB$iphonesentiment, p = .70, list = FALSE)
training_4 <- ism_GNB[inTraining_4,]
testing_4 <- ism_GNB[-inTraining_4,]

#pca
# data = training and testing from iphoneDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(training_4[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, training_4[,-59])

# add the dependent to training
train.pca$iphonesentiment <- training_1$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing_4[,-59])

# add the dependent to training
test.pca$iphonesentiment <- testing_4$iphonesentiment

# inspect results
str(train.pca)
str(test.pca)

#random forest #Accuracy: 0.7682 // Kappa: 0.5363
set.seed(123);ismPCA_GNB_M1 <- ranger(iphonesentiment~., data = train.pca)
#set.seed(123);ismPCA_GNB_M1 <- train(iphonesentiment~., data = train.pca, method = "rf")
Pred_ismPCA_GNB_M1 <- predict(ismPCA_GNB_M1, test.pca)

confusionMatrix(table(test.pca$iphonesentiment, Pred_ismPCA_GNB_M1$predictions))
rm("ismPCA_GNB_M1", "Pred_ismPCA_GNB_M1")

#C5.0 #Accuracy: 0.7576 // Kappa: 0.515
set.seed(123);ismPCA_GNB_M2 <- train(iphonesentiment~., data = train.pca, method = "C5.0")
Pred_ismPCA_GNB_M2 <- predict(ismPCA_GNB_M2, test.pca)

confusionMatrix(table(test.pca$iphonesentiment, Pred_ismPCA_GNB_M2))
rm("ismPCA_GNB_M2", "Pred_ismPCA_GNB_M2")

#SVM - linear #Accuracy: 0.6867 // Kappa: 0.3442
set.seed(123);ismPCA_GNB_M3 <- svm(iphonesentiment ~ ., data = train.pca, kernel = "linear", cost = 10, scale = FALSE)
Pred_ismPCA_GNB_M3 <- predict(ismPCA_GNB_M3, test.pca)

confusionMatrix(table(test.pca$iphonesentiment, Pred_ismPCA_GNB_M3))
rm("ismPCA_GNB_M3", "Pred_ismPCA_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7145 // Kappa: 0.4012
set.seed(123);ismPCA_GNB_M4 <- svm(factor(iphonesentiment) ~ ., data = train.pca, scale = FALSE, kernel = "radial", cost = 5)
Pred_ismPCA_GNB_M4 <- predict(ismPCA_GNB_M4, test.pca)

confusionMatrix(table(test.pca$iphonesentiment, Pred_ismPCA_GNB_M4))
rm("ismPCA_GNB_M4", "Pred_ismPCA_GNB_M4")

#K-NN #Accuracy: 0.3809 // Kappa: 0.1572
set.seed(123);ismPCA_GNB_M5 <- train.kknn(iphonesentiment ~ ., data = train.pca, kmax = 9)
Pred_ismPCA_GNB_M5 <- predict(ismPCA_GNB_M5, test.pca)

confusionMatrix(table(test.pca$iphonesentiment, Pred_ismPCA_GNB_M5))
rm("ismPCA_GNB_M5", "Pred_ismPCA_GNB_M5", "train.pca", "test.pca", "inTraining_4", "training_4", "testing_4")
