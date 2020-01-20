#load large matrix
LargeMatrixMerged <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/Module 5/combined large matrix/LargeMatrixMerged.csv")
View(LargeMatrixMerged)

LargeMatrixMerged$iphonesentiment <- 0

LargeMatrixMerged$galaxysentiment <- 0

#iphone
#ism_GNB
#random forest #Accuracy: 0.7728 // Kappa: 0.5448
set.seed(123);ism_GNB_M1 <- ranger(iphonesentiment~., data = ism_GNB)
Pred_ism_GNB_M1 <- predict(ism_GNB_M1, LargeMatrixMerged)

LargeMatrixMerged$iphonesentiment <- Pred_ism_GNB_M1$predictions

#samsung
#ssm_GNB
#random forest #Accuracy: 0.7712 // Kappa: 0.5191
set.seed(123);ssm_GNB_M1 <- ranger(galaxysentiment~., data = ssm_GNB)
Pred_ssm_GNB_M1 <- predict(ssm_GNB_M1, LargeMatrixMerged)

LargeMatrixMerged$galaxysentiment <- Pred_ssm_GNB_M1$predictions

plot(LargeMatrixMerged$galaxysentiment)
plot(LargeMatrixMerged$iphonesentiment)

# results table
table(LargeMatrixMerged$galaxysentiment)
table(LargeMatrixMerged$iphonesentiment)

#preference
# 1 = BAD // 2 = NEUTRAL // 3 = GOOD
levels(LargeMatrixMerged$galaxysentiment) <- c(levels(LargeMatrixMerged$galaxysentiment), 1, 2, 3)
LargeMatrixMerged$galaxysentiment[LargeMatrixMerged$galaxysentiment == "BAD"] <- 1
LargeMatrixMerged$galaxysentiment[LargeMatrixMerged$galaxysentiment == "NEUTRAL"] <- 2
LargeMatrixMerged$galaxysentiment[LargeMatrixMerged$galaxysentiment == "GOOD"] <- 3

levels(LargeMatrixMerged$iphonesentiment) <- c(levels(LargeMatrixMerged$iphonesentiment), 1, 2, 3)
LargeMatrixMerged$iphonesentiment[LargeMatrixMerged$iphonesentiment == "BAD"] <- 1
LargeMatrixMerged$iphonesentiment[LargeMatrixMerged$iphonesentiment == "NEUTRAL"] <- 2
LargeMatrixMerged$iphonesentiment[LargeMatrixMerged$iphonesentiment == "GOOD"] <- 3

LargeMatrixMerged$galaxysentiment <- as.numeric(LargeMatrixMerged$galaxysentiment)
LargeMatrixMerged$iphonesentiment <- as.numeric(LargeMatrixMerged$iphonesentiment)

LargeMatrixMerged$preference <- ifelse(LargeMatrixMerged$galaxysentiment > LargeMatrixMerged$iphonesentiment, "GALAXY", ifelse(LargeMatrixMerged$iphonesentiment > LargeMatrixMerged$galaxysentiment, "IPHONE", ifelse(LargeMatrixMerged$galaxysentiment == LargeMatrixMerged$iphonesentiment, "INDIFFERENT", "")))

pie(table(LargeMatrixMerged$preference))
