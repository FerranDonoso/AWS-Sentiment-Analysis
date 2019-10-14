### Load libraries
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
  pacman::p_load(lattice, ggplot2, caret, readr, corrplot, reshape2, caTools,
                 dplyr, RMySQL, lubridate, tidyr, ggfortify, forecast, plotly, fracdiff, tseries,
                 devtools, vars, xts, astsa, ggpubr, prophet, matrixStats)
} else{
  pacman::p_load(lattice, ggplot2, caret, readr, corrplot, reshape2, caTools,
                 dplyr, RMySQL, lubridate, tidyr, ggfortify, forecast, plotly, fracdiff, tseries,
                 devtools, vars, xts, astsa, ggpubr, prophet, matrixStats)
}

### Load data sets
dengue_features_test <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/hackaton1/dengue_features_test.csv")
dengue_features_train <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/hackaton1/dengue_features_train.csv")
dengue_labels_train <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/hackaton1/dengue_labels_train.csv")
submission_format <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/hackaton1/submission_format.csv")

features_test_1 <- dengue_features_test
features_train_1 <- dengue_features_train
labels_train_1 <- dengue_labels_train
submission_format_1 <- submission_format

View(features_test_1)
View(features_train_1)
View(labels_train_1)
View(submission_format_1)

### Exploratory analysis
str(features_test_1)
str(features_train_1)
str(labels_train_1)
str(submission_format_1)

features_test_1$city <- as.factor(features_test_1$city)
features_train_1$city <- as.factor(features_train_1$city)
labels_train_1$city <- as.factor(labels_train_1$city)
submission_format_1$city <- as.factor(submission_format_1$city)

summary(features_test_1)
summary(features_train_1)
summary(labels_train_1)
summary(submission_format_1)

### Check for outliers
boxplot(features_train_1$ndvi_ne)$out
boxplot(features_train_1$ndvi_nw)$out
boxplot(features_train_1$ndvi_se)$out
boxplot(features_train_1$ndvi_sw)$out
boxplot(features_train_1$precipitation_amt_mm)$out
boxplot(features_train_1$reanalysis_air_temp_k)$out
boxplot(features_train_1$reanalysis_avg_temp_k)$out
boxplot(features_train_1$reanalysis_dew_point_temp_k)$out
boxplot(features_train_1$reanalysis_max_air_temp_k)$out
boxplot(features_train_1$reanalysis_min_air_temp_k)$out
boxplot(features_train_1$reanalysis_precip_amt_kg_per_m2)$out
boxplot(features_train_1$reanalysis_relative_humidity_percent)$out
boxplot(features_train_1$reanalysis_sat_precip_amt_mm)$out
boxplot(features_train_1$reanalysis_specific_humidity_g_per_kg)$out
boxplot(features_train_1$reanalysis_tdtr_k)$out
boxplot(features_train_1$station_avg_temp_c)$out
boxplot(features_train_1$station_diur_temp_rng_c)$out
boxplot(features_train_1$station_max_temp_c)$out
boxplot(features_train_1$station_min_temp_c)$out
boxplot(features_train_1$station_precip_mm)$out

# what to do with year and weekofyear format
# what to do with NAs
# what to do with outliers

### Correlation matrix
features_train_2 <- features_train_1
features_train_2$total_cases <- labels_train_1$total_cases
View(features_train_2)

# remove city and week_start_date
features_train_3 <- features_train_2[, -c(1, 4)]

corr_train <- cor(features_train_3)
corr_train
corrplot(corr_train)

### Data split, 75% train, 25% test and cross validation
set.seed(123)
inTraining <- createDataPartition(features_train_2$total_cases, p = .75, list = FALSE)
training <- features_train_2[inTraining,]
testing <- features_train_2[-inTraining,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

### Random forest
rfFit1 <- train(total_cases~., data = training, method = "rf", trControl=fitControl, na.action = na.omit)
rfFit1

PredictionsrfFit1 <- predict(rfFit1,testing)
postResample(PredictionsrfFit1, testing$total_cases)





