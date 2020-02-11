#load libraries
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(lattice, ggplot2, caret, readr, corrplot, reshape2, caTools, dplyr, lubridate, tidyr,
               ggfortify, plotly, devtools, vars, ggpubr, matrixStats, ranger, rpart.plot, stringr,
               Rmisc, scales, doParallel, ROSE, e1071, kknn, DMwR, rstudioapi)

# load data sets
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

ssm <- read_csv("galaxy_smallmatrix_labeled_9d.csv")
View(ssm)

summary(ssm)

#plan of attack #samsung
sum(is.na(ssm))

plot_ly(ssm, x = ~ssm$galaxysentiment, type = "histogram")

#correlation matrix
corr_ssm <- cor(ssm)
corr_ssm
corrplot(corr_ssm)

#columns with near 0 variance ####
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including:
#frequency ratio, percentage unique, zero variance and near zero variance 
nzvMetrics <- nearZeroVar(ssm, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(ssm, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
ssmNZV <- ssm[, -nzv]
str(ssmNZV)

#random forest feature selection ####
set.seed(123);samsungSample <- ssm[sample(1:nrow(ssm), 1000, replace = FALSE), ]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
set.seed(123);rfeResults <- rfe(samsungSample[, 1:58], 
                                samsungSample$galaxysentiment, 
                                sizes = (1:58), 
                                rfeControl = ctrl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type = c("g", "o"))

# create new data set with rfe recommended features
ssmRFE <- ssm[, predictors(rfeResults)]

# add the dependent variable to iphoneRFE
ssmRFE$galaxysentiment <- ssm$galaxysentiment

# review outcome
str(iphoneRFE)

#change feature type of sentiment feature to factor
ssm$galaxysentiment <- as.factor(ssm$galaxysentiment)
ssmNZV$galaxysentiment <- as.factor(ssmNZV$galaxysentiment)
ssmRFE$galaxysentiment <- as.factor(ssmRFE$galaxysentiment)

# change sentiment values - BAD: 0, 1, 2 - NEUTRAL: 3, 4 - GOOD: 5
#ism
ssm_GNB <- ssm
levels(ssm_GNB$galaxysentiment) <- c(levels(ssm_GNB$galaxysentiment), "BAD", "NEUTRAL", "GOOD")
ssm_GNB$galaxysentiment[ssm_GNB$galaxysentiment == "0"] <- "BAD"
ssm_GNB$galaxysentiment[ssm_GNB$galaxysentiment == "1"] <- "BAD"
ssm_GNB$galaxysentiment[ssm_GNB$galaxysentiment == "2"] <- "BAD"
ssm_GNB$galaxysentiment[ssm_GNB$galaxysentiment == "3"] <- "NEUTRAL"
ssm_GNB$galaxysentiment[ssm_GNB$galaxysentiment == "4"] <- "NEUTRAL"
ssm_GNB$galaxysentiment[ssm_GNB$galaxysentiment == "5"] <- "GOOD"

ssm_GNB$galaxysentiment <- as.character(ssm_GNB$galaxysentiment)
ssm_GNB$galaxysentiment <- as.factor(ssm_GNB$galaxysentiment)
ssm_GNB$galaxysentiment <- factor(ssm_GNB$galaxysentiment, levels = c("BAD", "NEUTRAL", "GOOD"))

plot(ssm_GNB$galaxysentiment)

#ismNZV
ssmNZV_GNB <- ssmNZV
levels(ssmNZV_GNB$galaxysentiment) <- c(levels(ssmNZV_GNB$galaxysentiment), "BAD", "NEUTRAL", "GOOD")
ssmNZV_GNB$galaxysentiment[ssmNZV_GNB$galaxysentiment == "0"] <- "BAD"
ssmNZV_GNB$galaxysentiment[ssmNZV_GNB$galaxysentiment == "1"] <- "BAD"
ssmNZV_GNB$galaxysentiment[ssmNZV_GNB$galaxysentiment == "2"] <- "BAD"
ssmNZV_GNB$galaxysentiment[ssmNZV_GNB$galaxysentiment == "3"] <- "NEUTRAL"
ssmNZV_GNB$galaxysentiment[ssmNZV_GNB$galaxysentiment == "4"] <- "NEUTRAL"
ssmNZV_GNB$galaxysentiment[ssmNZV_GNB$galaxysentiment == "5"] <- "GOOD"

ssmNZV_GNB$galaxysentiment <- as.character(ssmNZV_GNB$galaxysentiment)
ssmNZV_GNB$galaxysentiment <- as.factor(ssmNZV_GNB$galaxysentiment)
ssmNZV_GNB$galaxysentiment <- factor(ssmNZV_GNB$galaxysentiment, levels = c("BAD", "NEUTRAL", "GOOD"))

plot(ssmNZV_GNB$galaxysentiment)

#ismRFE
ssmRFE_GNB <- ssmRFE
levels(ssmRFE_GNB$galaxysentiment) <- c(levels(ssmRFE_GNB$galaxysentiment), "BAD", "NEUTRAL", "GOOD")
ssmRFE_GNB$galaxysentiment[ssmRFE_GNB$galaxysentiment == "0"] <- "BAD"
ssmRFE_GNB$galaxysentiment[ssmRFE_GNB$galaxysentiment == "1"] <- "BAD"
ssmRFE_GNB$galaxysentiment[ssmRFE_GNB$galaxysentiment == "2"] <- "BAD"
ssmRFE_GNB$galaxysentiment[ssmRFE_GNB$galaxysentiment == "3"] <- "NEUTRAL"
ssmRFE_GNB$galaxysentiment[ssmRFE_GNB$galaxysentiment == "4"] <- "NEUTRAL"
ssmRFE_GNB$galaxysentiment[ssmRFE_GNB$galaxysentiment == "5"] <- "GOOD"

ssmRFE_GNB$galaxysentiment <- as.character(ssmRFE_GNB$galaxysentiment)
ssmRFE_GNB$galaxysentiment <- as.factor(ssmRFE_GNB$galaxysentiment)
ssmRFE_GNB$galaxysentiment <- factor(ssmRFE_GNB$galaxysentiment, levels = c("BAD", "NEUTRAL", "GOOD"))

plot(ssmRFE_GNB$galaxysentiment)

#modeling
#ssm_GNB
set.seed(123);inTraining_1 <- createDataPartition(ssm_GNB$galaxysentiment, p = .70, list = FALSE)
training_1 <- ssm_GNB[inTraining_1, ]
testing_1 <- ssm_GNB[-inTraining_1, ]

#random forest #Accuracy: 0.7712 // Kappa: 0.5191
set.seed(123);ssm_GNB_M1 <- ranger(galaxysentiment ~., data = training_1)
#set.seed(123);ism_GNB_M1 <- train(galaxysentiment~., data = training_1, method = "rf")
Pred_ssm_GNB_M1 <- predict(ssm_GNB_M1, testing_1)

confusionMatrix(table(testing_1$galaxysentiment, Pred_ssm_GNB_M1$predictions))
rm("ssm_GNB_M1", "Pred_ssm_GNB_M1")

#C5.0 #Accuracy: 0.7738 // Kappa: 0.5287
set.seed(123);ssm_GNB_M2 <- train(galaxysentiment ~., data = training_1, method = "C5.0")
Pred_ssm_GNB_M2 <- predict(ssm_GNB_M2, testing_1)

confusionMatrix(table(testing_1$galaxysentiment, Pred_ssm_GNB_M2))
rm("ssm_GNB_M2", "Pred_ssm_GNB_M2")

plot(training_1$galaxysentiment)
plot(testing_1$galaxysentiment)

#SVM - linear #Accuracy: 0.711 // Kappa: 0.3753
set.seed(123);ssm_GNB_M3 <- svm(galaxysentiment ~., data = training_1, kernel = "linear", cost = 10, scale = FALSE)
Pred_ssm_GNB_M3 <- predict(ssm_GNB_M3, testing_1)

confusionMatrix(table(testing_1$galaxysentiment, Pred_ssm_GNB_M3))
rm("ssm_GNB_M3", "Pred_ssm_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7603 // Kappa: 0.4952
set.seed(123);ssm_GNB_M4 <- svm(factor(galaxysentiment) ~., data = training_1, scale = FALSE, kernel = "radial", cost = 5)
Pred_ssm_GNB_M4 <- predict(ssm_GNB_M4, testing_1)

confusionMatrix(table(testing_1$galaxysentiment, Pred_ssm_GNB_M4))
rm("ssm_GNB_M4", "Pred_ssm_GNB_M4")

#K-NN #Accuracy: 0.7115 // Kappa: 0.4425
set.seed(123);ssm_GNB_M5 <- train.kknn(galaxysentiment ~., data = training_1, kmax = 9)
Pred_ssm_GNB_M5 <- predict(ssm_GNB_M5, testing_1)

confusionMatrix(table(testing_1$galaxysentiment, Pred_ssm_GNB_M5))
postResample(Pred_ssm_GNB_M5, testing_1$galaxysentiment)
rm("ssm_GNB_M5", "Pred_ssm_GNB_M5", "inTraining_1", "training_1", "testing_1")

#ismNZV_GNB
set.seed(123);inTraining_2 <- createDataPartition(ssmNZV_GNB$galaxysentiment, p = .70, list = FALSE)
training_2 <- ssmNZV_GNB[inTraining_2, ]
testing_2 <- ssmNZV_GNB[-inTraining_2, ]

#random forest #Accuracy: 0.7647 // Kappa: 0.5055
set.seed(123);ssmNZV_GNB_M1 <- ranger(galaxysentiment ~., data = training_2)
#set.seed(123);ismNZV_GNB_M1 <- train(galaxysentiment~., data = training_2, method = "rf")
Pred_ssmNZV_GNB_M1 <- predict(ssmNZV_GNB_M1, testing_2)

confusionMatrix(table(testing_2$galaxysentiment, Pred_ssmNZV_GNB_M1$predictions))
rm("ssmNZV_GNB_M1", "Pred_ssmNZV_GNB_M1")

#C5.0 #Accuracy: 0.7603 // Kappa: 0.4964
set.seed(123);ssmNZV_GNB_M2 <- train(galaxysentiment ~., data = training_2, method = "C5.0")
Pred_ssmNZV_GNB_M2 <- predict(ssmNZV_GNB_M2, testing_2)

confusionMatrix(table(testing_2$galaxysentiment, Pred_ssmNZV_GNB_M2))
rm("ssmNZV_GNB_M2", "Pred_ssmNZV_GNB_M2")

#SVM - linear #Accuracy: 0.7035 // Kappa: 0.3505
set.seed(123);ssmNZV_GNB_M3 <- svm(galaxysentiment ~., data = training_2, kernel = "linear", cost = 10, scale = FALSE)
Pred_ssmNZV_GNB_M3 <- predict(ssmNZV_GNB_M3, testing_2)

confusionMatrix(table(testing_2$galaxysentiment, Pred_ssmNZV_GNB_M3))
rm("ssmNZV_GNB_M3", "Pred_ssmNZV_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7523 // Kappa: 0.481
set.seed(123);ssmNZV_GNB_M4 <- svm(factor(galaxysentiment) ~., data = training_2, scale = FALSE, kernel = "radial", cost = 5)
Pred_ssmNZV_GNB_M4 <- predict(ssmNZV_GNB_M4, testing_2)

confusionMatrix(table(testing_2$galaxysentiment, Pred_ssmNZV_GNB_M4))
rm("ssmNZV_GNB_M4", "Pred_ssmNZV_GNB_M4")

#K-NN #Accuracy: 0.7017 // Kappa: 0.4195
set.seed(123);ssmNZV_GNB_M5 <- train.kknn(galaxysentiment ~., data = training_2, kmax = 9)
Pred_ssmNZV_GNB_M5 <- predict(ssmNZV_GNB_M5, testing_2)

confusionMatrix(table(testing_2$galaxysentiment, Pred_ssmNZV_GNB_M5))
rm("ssmNZV_GNB_M5", "Pred_ssmNZV_GNB_M5", "inTraining_2", "training_2", "testing_2")

#ismRFE_GNB
set.seed(123);inTraining_3 <- createDataPartition(ssmRFE_GNB$galaxysentiment, p = .70, list = FALSE)
training_3 <- ssmRFE_GNB[inTraining_3, ]
testing_3 <- ssmRFE_GNB[-inTraining_3, ]

#random forest #Accuracy: 0.7732 // Kappa: 0.5239
set.seed(123);ssmRFE_GNB_M1 <- ranger(galaxysentiment ~., data = training_3)
#set.seed(123);ismRFE_GNB_M1 <- train(galaxysentiment~., data = training_3, method = "rf")
Pred_ssmRFE_GNB_M1 <- predict(ssmRFE_GNB_M1, testing_3)

confusionMatrix(table(testing_3$galaxysentiment, Pred_ssmRFE_GNB_M1$predictions))
rm("ssmRFE_GNB_M1", "Pred_ssmRFE_GNB_M1")

#C5.0 #Accuracy: 0.7732 // Kappa: 0.5285
set.seed(123);ssmRFE_GNB_M2 <- train(galaxysentiment ~., data = training_3, method = "C5.0")
Pred_ssmRFE_GNB_M2 <- predict(ssmRFE_GNB_M2, testing_3)

confusionMatrix(table(testing_3$galaxysentiment, Pred_ssmRFE_GNB_M2))
rm("ssmRFE_GNB_M2", "Pred_ssmRFE_GNB_M2")

#SVM - linear #Accuracy: 0.718 // Kappa: 0.3913
set.seed(123);ssmRFE_GNB_M3 <- svm(galaxysentiment ~., data = training_3, kernel = "linear", cost = 10, scale = FALSE)
Pred_ssmRFE_GNB_M3 <- predict(ssmRFE_GNB_M3, testing_3)

confusionMatrix(table(testing_3$galaxysentiment, Pred_ssmRFE_GNB_M3))
rm("ssmRFE_GNB_M3", "Pred_ssmRFE_GNB_M3")

#SVM - nonlinear #Accuracy: 0.7668 // Kappa: 0.5135
set.seed(123);ssmRFE_GNB_M4 <- svm(factor(galaxysentiment) ~., data = training_3, scale = FALSE, kernel = "radial", cost = 5)
Pred_ssmRFE_GNB_M4 <- predict(ssmRFE_GNB_M4, testing_3)

confusionMatrix(table(testing_3$galaxysentiment, Pred_ssmRFE_GNB_M4))
rm("ssmRFE_GNB_M4", "Pred_ssmRFE_GNB_M4")

#K-NN #Accuracy: 0.7128 // Kappa: 0.4447
set.seed(123);ssmRFE_GNB_M5 <- train.kknn(galaxysentiment ~., data = training_3, kmax = 9)
Pred_ssmRFE_GNB_M5 <- predict(ssmRFE_GNB_M5, testing_3)

confusionMatrix(table(testing_3$galaxysentiment, Pred_ssmRFE_GNB_M5))
rm("ssmRFE_GNB_M5", "Pred_ssmRFE_GNB_M5", "inTraining_3", "training_3", "testing_3")

#ismPCA_GNB
set.seed(123);inTraining_4 <- createDataPartition(ssm_GNB$galaxysentiment, p = .70, list = FALSE)
training_4 <- ssm_GNB[inTraining_4, ]
testing_4 <- ssm_GNB[-inTraining_4, ]

#pca
# data = training and testing from iphoneDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(training_4[, -59], method = c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, training_4[, -59])

# add the dependent to training
train.pca$galaxysentiment <- training_4$galaxysentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing_4[, -59])

# add the dependent to training
test.pca$galaxysentiment <- testing_4$galaxysentiment

# inspect results
str(train.pca)
str(test.pca)

#random forest #Accuracy: 0.7645 // Kappa: 0.513
set.seed(123);ssmPCA_GNB_M1 <- ranger(galaxysentiment ~., data = train.pca)
#set.seed(123);ismPCA_GNB_M1 <- train(galaxysentiment~., data = train.pca, method = "rf")
Pred_ssmPCA_GNB_M1 <- predict(ssmPCA_GNB_M1, test.pca)

confusionMatrix(table(test.pca$galaxysentiment, Pred_ssmPCA_GNB_M1$predictions))
rm("ssmPCA_GNB_M1", "Pred_ssmPCA_GNB_M1")

#C5.0 #Accuracy: 0.759 // Kappa: 0.4988
set.seed(123);ssmPCA_GNB_M2 <- train(galaxysentiment ~., data = train.pca, method = "C5.0")
Pred_ssmPCA_GNB_M2 <- predict(ssmPCA_GNB_M2, test.pca)

confusionMatrix(table(test.pca$galaxysentiment, Pred_ssmPCA_GNB_M2))
rm("ssmPCA_GNB_M2", "Pred_ssmPCA_GNB_M2")

#SVM - linear #Accuracy: 0.6903 // Kappa: 0.3168
set.seed(123);ssmPCA_GNB_M3 <- svm(galaxysentiment ~., data = train.pca, kernel = "linear", cost = 10, scale = FALSE)
Pred_ssmPCA_GNB_M3 <- predict(ssmPCA_GNB_M3, test.pca)

confusionMatrix(table(test.pca$galaxysentiment, Pred_ssmPCA_GNB_M3))
rm("ssmPCA_GNB_M3", "Pred_ssmPCA_GNB_M3")

#SVM - nonlinear #Accuracy: 0.726 // Kappa: 0.4049
set.seed(123);ssmPCA_GNB_M4 <- svm(factor(galaxysentiment) ~., data = train.pca, scale = FALSE, kernel = "radial", cost = 5)
Pred_ssmPCA_GNB_M4 <- predict(ssmPCA_GNB_M4, test.pca)

confusionMatrix(table(test.pca$galaxysentiment, Pred_ssmPCA_GNB_M4))
rm("ssmPCA_GNB_M4", "Pred_ssmPCA_GNB_M4")

#K-NN #Accuracy: 0.7004 // Kappa: 0.4255
set.seed(123);ssmPCA_GNB_M5 <- train.kknn(galaxysentiment ~., data = train.pca, kmax = 9)
Pred_ssmPCA_GNB_M5 <- predict(ssmPCA_GNB_M5, test.pca)

confusionMatrix(table(test.pca$galaxysentiment, Pred_ssmPCA_GNB_M5))
rm("ssmPCA_GNB_M5", "Pred_ssmPCA_GNB_M5", "train.pca", "test.pca", "inTraining_4", "training_4", "testing_4")

