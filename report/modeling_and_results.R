# Set Environment Variables For Input/Output Paths


## define input path
PATH_MAIN_DATA   <- Sys.getenv("STATS_412_PROJECT_DATA")

## define output path
PATH_IMAGE_DIR  <- Sys.getenv("STATS_412_PROJECT_IMAGE_DIR")


# Loading the Data & Libraries

library(caTools)
library(ROCR)
library(pROC)
library(dplyr)
library(AMR)
library(ggplot2)
library(car)
library(ggResidpanel)
library(ggfortify)
library(xgboost) 
library(caret)
library(glmnet)
library(alr4)
library(ROSE)
library(sjPlot)
 


# Load original data, add additional variables, make encode dummy variables (treatment), and then scale the data for non-PCA models

 
orig_df <- na.omit(read.csv(paste0(PATH_MAIN_DATA, "/framingham.csv")))
facts <- c('male', 'TenYearCHD', 'education', 'currentSmoker', 
           'BPMeds', 'prevalentStroke', 'prevalentHyp', 'diabetes')
orig_df[, facts] <- lapply(orig_df[, facts], factor)

levels(orig_df$education) <- c("no high school", "high school", "bachelors", "graduate school")

# squared terms
orig_df$diaBP.SQ <- (orig_df$diaBP) ^ 2
orig_df$glucose.SQ <- (orig_df$glucose) ^ 2
orig_df$sysBP.SQ <- (orig_df$sysBP) ^ 2

# interactions
orig_df$ageXcigsPerDay <- orig_df$age*orig_df$cigsPerDay
orig_df$glucoseXsysBP <- orig_df$glucose*orig_df$sysBP
orig_df$sysBPXcigsPerDay <- orig_df$sysBP*orig_df$cigsPerDay

# make dummy variables
dummy <- dummyVars(" ~ .", data = orig_df, fullRank = TRUE)
orig_df <- data.frame(predict(dummy, newdata = orig_df))

# set seed for consistency
set.seed(4)

# Split data, then apply ROSE to the training data to bootstrap the training set
train.index <- createDataPartition(orig_df$TenYearCHD.1, p = 0.8, list = F)
holdout_data <- orig_df[train.index[1:ceiling(length(train.index)*0.2)], ]
train_data <- orig_df[train.index[ceiling(length(train.index)*0.2):length(train.index)], ]
train_data <- ovun.sample(TenYearCHD.1 ~ ., data = train_data, seed = 4)$data
test_data <- orig_df[-train.index, ]

# scale the data for model 1 and model 3
train_data_scaled <- train_data
holdout_data_scaled <- holdout_data
test_data_scaled <- test_data


# scale data with no data leakage
normParam <- preProcess(train_data[, -18])
train_data_scaled[, -18] <- as.data.frame(predict(normParam, train_data[, -18]))
holdout_data_scaled[, -18] <- as.data.frame(predict(normParam, holdout_data[, -18]))
test_data_scaled[, -18] <- as.data.frame(predict(normParam, test_data[, -18]))

 


# Load original data, add additional variables, make encode dummy variables (treatment), and then scale the data for PCA 

 
orig_df_PCA <- na.omit(read.csv(paste0(PATH_MAIN_DATA, "/framingham.csv")))
facts <- c('male', 'TenYearCHD', 'education', 'currentSmoker', 
           'BPMeds', 'prevalentStroke', 'prevalentHyp', 'diabetes')
orig_df_PCA[, facts] <- lapply(orig_df_PCA[, facts], factor)

# squared terms
orig_df_PCA$diaBP.SQ <- (orig_df_PCA$diaBP) ^ 2
orig_df_PCA$glucose.SQ <- (orig_df_PCA$glucose) ^ 2
orig_df_PCA$sysBP.SQ <- (orig_df_PCA$sysBP) ^ 2

# interactions
orig_df_PCA$ageXcigsPerDay <- orig_df_PCA$age*orig_df_PCA$cigsPerDay
orig_df_PCA$glucoseXsysBP <- orig_df_PCA$glucose*orig_df_PCA$sysBP
orig_df_PCA$sysBPXcigsPerDay <- orig_df_PCA$sysBP*orig_df_PCA$cigsPerDay

# make dummy variables for PCA
dummy <- dummyVars(" ~ .", data = orig_df_PCA, fullRank = TRUE)
orig_df_PCA <- data.frame(predict(dummy, newdata = orig_df_PCA))

# split data
set.seed(4)
train.index_PCA <- createDataPartition(orig_df_PCA$TenYearCHD, p = 0.8, list = F)
holdout_data_PCA <- orig_df_PCA[train.index_PCA[1:ceiling(length(train.index_PCA)*0.2)], ]
train_data_PCA <- orig_df_PCA[train.index_PCA[ceiling(length(train.index_PCA)*0.2):length(train.index_PCA)], ]
test_data_PCA <- orig_df_PCA[-train.index_PCA, ]

# then apply ROSE to the training data to bootstrap the training set
train_data_PCA <- ovun.sample(TenYearCHD.1 ~ ., data = train_data_PCA, seed = 4)$data

# scale data with no data leakage
normParam <- preProcess(train_data_PCA[, -18])
train_data_PCA[, -18] <- predict(normParam, train_data_PCA[, -18])
holdout_data_PCA[, -18] <- predict(normParam, holdout_data_PCA[, -18])

# remove response, apply pca
PCs <- prcomp(train_data_PCA[, -18], scale = FALSE, center = FALSE)
train_data_PCA = cbind(as.data.frame(PCs$x), TenYearCHD = train_data_PCA[, 18])

# adjust new holdout and test data with training loadings (new data leakage)
x <- as.matrix(holdout_data_PCA[, -18])
x <- x %*% PCs$rotation
holdout_data_PCA <- data.frame(x, TenYearCHD = holdout_data_PCA[, 18])

 


# Logistic Regression Modeling (Using Feature Subset Selection)

## Backward Stepwise Feature Selection

 
glm_bs1 <- glm(TenYearCHD.1 ~ ., data = train_data_scaled, family = binomial)
summary(glm_bs1)

# use backward selection
glm_bs2 <- step(glm_bs1, direction = "backward", trace = 0) 
summary(glm_bs2)

# difference in deviance can be used to compare the backward selection model
# and the original
anova(glm_bs2, glm_bs1, test = "Chisq")
 


## Goodness of fit 

 
subset_LR <- glm(TenYearCHD.1 ~ male.1 + age + education.high.school + education.bachelors + 
                   education.graduate.school + cigsPerDay + BPMeds.1 + prevalentHyp.1 + 
                   totChol + sysBP + diaBP + diaBP.SQ + glucose.SQ, 
                 family = binomial, 
                 data = train_data_scaled)

summary(subset_LR)

# test deviance against null model
null_LR <- glm(TenYearCHD.1 ~ 1, data = train_data_scaled, family = binomial)

anova(null_LR, subset_LR, test = "Chisq")

# could equivalently be done
# pchisq(3245.3 - 2808.6, 2340-2327, lower=FALSE)

# chisq goodness of fit test
ts <- sum(residuals(subset_LR, type = "pearson") ^ 2)
cat("P-Value for the Pearson Goodness of Fit Test  = ", pchisq(ts, subset_LR$df.residual, lower=FALSE), "\n")
 


## Leverage, Influence, and Residual Analysis

 
par(mfrow = c(2, 2))
resid_panel(subset_LR, plots = c("resid", "qq"), theme = "minimal", type = "pearson")
ggsave(paste0(PATH_IMAGE_DIR,"/subset_LR_pearson_residuals.png"))

resid_panel(subset_LR, plots = c("resid", "qq"), theme = "minimal", type = "deviance")
ggsave(paste0(PATH_IMAGE_DIR,"/subset_LR_dev_residuals.png"))

par(mfrow = c(1, 2))
autoplot(subset_LR, which = c(4, 5)) + theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/subset_LR_diagnostics.png"))


 


## Holdout Set CV

 
# defines holdout set usage for no data leakage
cv_train_data_scaled <- rbind(holdout_data_scaled, train_data_scaled)

cv_train_data_scaled$TenYearCHD.1 <- factor(cv_train_data_scaled$TenYearCHD.1)
levels(cv_train_data_scaled$TenYearCHD.1) <- c("no", "yes")

fit_on <- list(ts = nrow(holdout_data_scaled):nrow(cv_train_data_scaled))
pred_on <- list(hs = 1:nrow(holdout_data_scaled))

train.control <- trainControl(method = "cv", classProbs = TRUE, 
                              index =  fit_on, indexOut = pred_on, 
                              summaryFunction = twoClassSummary)

cv_subset_glm <- train(TenYearCHD.1 ~ male.1 + age + education.high.school + education.bachelors + 
                         education.graduate.school + cigsPerDay + BPMeds.1 + prevalentHyp.1 + 
                         totChol + sysBP + diaBP + diaBP.SQ + glucose.SQ, 
                       data = cv_train_data_scaled, 
                       method = "glm", 
                       family = "binomial", 
                       metric = "ROC", 
                       trControl = train.control)

print(cv_subset_glm)

levels(cv_train_data_scaled$TenYearCHD.1) <- c(0, 1)
 


# Logistic Regression (Using Lasso + Ridge = Elastic)

## Finding Optimal Regularization Parameter

 
# defines holdout set usage for no data leakage
cv_train_data_scaled <- rbind(holdout_data_scaled, train_data_scaled)

cv_train_data_scaled$TenYearCHD.1 <- factor(cv_train_data_scaled$TenYearCHD.1)
levels(cv_train_data_scaled$TenYearCHD.1) <- c("no", "yes")

# predictor variables
cv_train_x <- model.matrix(TenYearCHD.1 ~ ., cv_train_data_scaled)[, -1]

# Convert the outcome (class) to a numerical variable
cv_train_y <- cv_train_data_scaled$TenYearCHD.1

fit_on <- list(ts = nrow(holdout_data_scaled):nrow(cv_train_data_scaled))
pred_on <- list(hs = 1:nrow(holdout_data_scaled))

train.control <- trainControl(method = "cv", classProbs = TRUE, 
                              index =  fit_on, indexOut = pred_on, 
                              summaryFunction = twoClassSummary)

# large grid
regGrid <- expand.grid(alpha = seq(0, 1, by = 0.2), lambda = seq(0, 0.115, by = 0.001))

cv_elastic_glm <- train(cv_train_x, cv_train_y, 
                        method = "glmnet", 
                        family = "binomial", 
                        metric = "ROC", 
                        tuneGrid = regGrid, 
                        trControl = train.control)

ggplot(cv_elastic_glm) + theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/elastic_LR_tuning.png"))

levels(cv_train_data_scaled$TenYearCHD.1) <- c(0, 1)

# fit "optimal" model on smaller training set 
train_x <- model.matrix(TenYearCHD.1 ~ ., train_data_scaled)[, -1]
train_y <- train_data_scaled$TenYearCHD.1

elastic.lr <- glmnet(train_data_scaled[, -18], train_data_scaled[, 18], alpha = 0.6, family = "binomial", lambda = 0.098)

# print coefs
coef(elastic.lr)
 


## Manual Residual Analysis

 
# formulas from lecture 
fittedvals <- predict(elastic.lr, newx = train_x, type = "response")

pearsonResiduals <- (train_y - fittedvals) / sqrt(fittedvals*(1-fittedvals))

devianceResiduals <- ifelse(as.logical(train_y), 1, -1) * 
  sqrt(-2*(train_y *log(fittedvals) + (1-train_y)*log(1-fittedvals)))

par(mfrow = c(1, 2))

ggplot(data.frame(fittedvals, devianceResiduals), aes(x = fittedvals, y = devianceResiduals)) + 
  geom_point() +  
  geom_hline(yintercept = 0, color = "blue") + 
  ggtitle("Residuals Plot") +
  xlab("Fitted Values") + 
  ylab("Pearson Residuals") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/elastic_LR_peason_residuals.png"))

ggplot(data.frame(fittedvals, devianceResiduals), aes(x = fittedvals, y = devianceResiduals)) + 
  geom_point() +  
  geom_hline(yintercept = 0, color = "blue") + 
  ggtitle("Residuals Plot") +
  xlab("Fitted Values") + 
  ylab("Deviance Residuals") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/elastic_LR_dev_residuals.png"))

 


## Deviance 

 
# can compare with feature selection model
deviance(elastic.lr)
 


# Logistic Regression Modeling (Using PCA)

## Choosing the number of Principal Components

 
# sumamry of the sample principal components
summary(PCs)

# plots
var_explained = PCs$sdev ^ 2 / sum(PCs$sdev ^ 2)

ggplot(data.frame(x = 1:length(PCs$sdev), y = var_explained)) + 
  geom_line(aes(x = x, y = y, size=1), color = "#EEA236", show.legend = F) + 
  geom_point(aes(x = x, y = y, size=2), color = "#EEA236", show.legend = F) +  
  
  ggtitle("Variance Explained") +
  xlab("Sample Principal Component Number") + 
  ylab("Variance Explained") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/pca_LR_scree.png"))

tot_var_explained = cumsum(PCs$sdev ^ 2) / sum(PCs$sdev ^ 2)

ggplot(data.frame(x = 1:length(PCs$sdev), y = tot_var_explained), aes(x = x, y = y)) + 
  geom_line() +  
  ggtitle("Cumulative Variance Explained") +
  xlab("Sample Principal Component Number") + 
  ylab("Cumulative Variance Explained") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/pca_LR_cumul_scree.png"))

# create biplot to visualize results of PCA
ggplot_pca(PCs, arrows_colour = "red", arrows_size = 1) + theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/pca_LR_biplot.png"))

ggplot(train_data_PCA, aes(x = PC1, y = PC2, col = factor(TenYearCHD))) + 
  geom_point() +  
  ggtitle("PC Separability") +
  xlab("PC1") + 
  ylab("PC2") +
  theme_minimal() +
  guides(color = guide_legend(title = "Has Heart Disease?")) + 
  scale_color_discrete(labels = c("No", "Yes"))
ggsave(paste0(PATH_IMAGE_DIR,"/pca_seperability.png"))

# not really seeing much separability

# Holdout Set CV

# defines holdout set usage for no data leakage
cv_train_data_PCA <- rbind(holdout_data_PCA, train_data_PCA)

cv_train_data_PCA$TenYearCHD <- factor(cv_train_data_PCA$TenYearCHD)
levels(cv_train_data_scaled$TenYearCHD) <- c("no", "yes")

fit_on <- list(ts = nrow(holdout_data_PCA):nrow(cv_train_data_PCA))
pred_on <- list(hs = 1:nrow(holdout_data_PCA))

train.control <- trainControl(method = "cv", classProbs = TRUE, 
                              index =  fit_on, indexOut = pred_on, 
                              summaryFunction = twoClassSummary)

cv_train_data_PCA$TenYearCHD <- factor(cv_train_data_PCA$TenYearCHD)
levels(cv_train_data_PCA$TenYearCHD) <- c("no", "yes")

# examine the AUC performance for different number of PCs
vAUC <- c()
for (k in 1:23){
  print(paste("*********************** Number of PCs  = ", k, "***********************"))
  cv_pca_glm <- train(TenYearCHD ~ ., 
                      data = cv_train_data_PCA[, c(1:k, 24)], 
                      method = "glm", 
                      family = "binomial", 
                      metric = "ROC", 
                      trControl = train.control)
  print(cv_pca_glm)
  
  vAUC <- c(vAUC, cv_pca_glm$results$ROC)
  
}

levels(cv_train_data_PCA$TenYearCHD) <- c(0, 1)

ggplot(data.frame(x=1:23, vAUC)) + 
  geom_line(aes(x = 1:23, y = vAUC, size=1), color = "#46B8DA", show.legend = F) + 
  geom_point(aes(x = 1:23, y = vAUC, size=2), color = "#46B8DA", show.legend = F) +  
  ggtitle("Validation Performance") +
  xlab("Number of Components Used") + 
  ylab("ROC-AUC") +
  theme_minimal() 
ggsave(paste0(PATH_IMAGE_DIR,"/pca_validation.png"))

 


## Goodness of fit

 
pca_LR <- glm(TenYearCHD ~ ., data = train_data_PCA[, c(1:8, 24)], family = binomial)
summary(pca_LR)

# test deviance against null model
null_LR <- glm(TenYearCHD ~ 1, data = train_data_PCA[, c(1:8, 24)], family = binomial)
anova(null_LR, pca_LR, test = "Chisq")

# could equivalently be done
# pchisq(3245.3 - 2841.1, 2340-2329, lower=FALSE)

# chisq goodness of fit test
ts <- sum(residuals(pca_LR, type = "pearson") ^ 2)
cat("P-Value for the Pearson Goodness of Fit Test  = ",  pchisq(ts, pca_LR$df.residual, lower=FALSE), "\n")
 


## Leverage, Influence, and Residual Analysis

 
par(mfrow = c(2, 2))
resid_panel(pca_LR, plots = c("resid", "qq"), theme = "minimal", type = "pearson")
ggsave(paste0(PATH_IMAGE_DIR,"/pca_LR_pearson_residuals.png"))

resid_panel(pca_LR, plots = c("resid", "qq"), theme = "minimal", type = "deviance")
ggsave(paste0(PATH_IMAGE_DIR,"/pca_LR_dev_residuals.png"))

par(mfrow = c(1, 2))
autoplot(pca_LR, which = c(4, 5)) + theme_minimal()

ggsave(paste0(PATH_IMAGE_DIR,"/pca_LR_diagnostics.png"))
 


# Extreme Gradient Boosted Classifier

 
train_x = data.matrix(train_data_scaled[, -18])
train_y = train_data_scaled[, 18]

# define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = as.numeric(train_y))

xgb.model <- xgboost(data = xgb_train, 
                     max.depth = 2, 
                     nrounds = 5, 
                     objective = 'binary:logistic',
                     lambda = 2, 
                     gamma = 0, 
                     colsample_bytree = 1, 
                     verbose = F)     
 


## Holdout Set CV

 
# defines holdout set usage for no data leakage
cv_train_data_scaled <- rbind(holdout_data_scaled, train_data_scaled)

train_y <- factor(cv_train_data_scaled[, 18])
levels(train_y) <- c("no", "yes")

fit_on <- list(ts = nrow(holdout_data_scaled):nrow(cv_train_data_scaled))
pred_on <- list(hs = 1:nrow(holdout_data_scaled))

xgbGrid <- expand.grid(nrounds = seq(1, 100, by = 4), 
                       max_depth = c(2, 3, 4, 5), 
                       eta = 0.1, # default
                       gamma = 0, #default
                       colsample_bytree = 1, # default
                       min_child_weight = 1, # default
                       subsample = 1 # default
)

xgb_trcontrol <- trainControl(method = "cv", classProbs = TRUE, 
                              index =  fit_on, indexOut = pred_on, 
                              summaryFunction = twoClassSummary)

xgb_model = train(as.matrix(cv_train_data_scaled[, -18]), train_y,
                  trControl = xgb_trcontrol, 
                  tuneGrid = xgbGrid, 
                  method = "xgbTree", 
                  metric = "ROC", 
                  verbosity = 0
)

#print(xgb_model)
ggplot(xgb_model) + theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/xgb_tuning.png"))

levels(cv_train_data_scaled$TenYearCHD.1) <- c(0, 1)
 


# Retrain Best Models Using Entire Training Set

## Update new training and test sets (no more holdout set)

 
# use original split, then apply over sampling again to the training data to 
# bootstrap the larger training set
train_data <- orig_df[train.index, ]
train_data <- ovun.sample(TenYearCHD.1 ~ ., data = train_data, seed = 4)$data
test_data <- orig_df[-train.index, ]

# scale the data for model 1 and model 3
train_data_scaled <- train_data
test_data_scaled <- test_data

normParam <- preProcess(train_data[, -18])
train_data_scaled[, -18] <- as.data.frame(predict(normParam, train_data[, -18]))
test_data_scaled[, -18] <- as.data.frame(predict(normParam, test_data[, -18]))

# for PCA datasets 
# use original split, then apply over sampling again to the training data to 
# bootstrap the larger training set

train_data_PCA <- orig_df_PCA[train.index_PCA, ]
train_data_PCA <- ovun.sample(TenYearCHD.1 ~ ., data = train_data_PCA, seed = 4)$data
test_data_PCA <- orig_df_PCA[-train.index_PCA, ]


# scale data with no data leakage
normParam <- preProcess(train_data_PCA[, -18])
train_data_PCA[, -18] <- predict(normParam, train_data_PCA[, -18])
test_data_PCA[, -18] <- predict(normParam, test_data_PCA[, -18])

# remove response, apply pca
PCs <- prcomp(train_data_PCA[, -18], scale = FALSE, center = FALSE)
train_data_PCA = cbind(as.data.frame(PCs$x), TenYearCHD = train_data_PCA[, 18])

# adjust new holdout and test data with training loadings (new data leakage)
x <- as.matrix(test_data_PCA[, -18])
x <- x %*% PCs$rotation
test_data_PCA <- data.frame(x, TenYearCHD = test_data_PCA[, 18])

 


## Logistic Regression with Feature Selection

 
best.subset.lr <- glm(TenYearCHD.1 ~ male.1 + age + education.high.school + education.bachelors +
                        education.graduate.school + cigsPerDay + BPMeds.1 + prevalentHyp.1 + 
                        totChol + sysBP + diaBP + diaBP.SQ + glucose.SQ, 
                      family = binomial, 
                      data = train_data)

summary(best.subset.lr)
plot_model(best.subset.lr) + 
  theme_minimal() +
  ggtitle("Ten Year CHD") +
  scale_x_discrete(labels = c(expression("(Glucose)"^"2"), 
                              expression("(Diabolic Blood Pressure)"^"2"), 
                              "Diabolic Blood Pressure", 
                              "Systolic Blood Pressure",
                              "Total Cholesterol", 
                              "Prevalent Hypertension (Yes)", 
                              "Blood Pressure Medication (Yes)", 
                              "Cigarettes Per Day", 
                              "Education (Graduate School)", 
                              "Education (Bachelor's)", 
                              "Education (High School)", 
                              "Age", 
                              "Gender (Male)"))

ggsave(paste0(PATH_IMAGE_DIR,"/subset_LR_coefs.png"))

 


## Logistic Regression with Elastic Net

 
train_x <- model.matrix(TenYearCHD.1 ~ ., train_data)[, -1]
train_y <- train_data$TenYearCHD.1

test_x <- model.matrix(TenYearCHD.1 ~ ., test_data)[, -1]
test_y <- train_data$TenYearCHD.1

best.elastic.lr <- glmnet(train_x, train_y, alpha = 0.6, family = "binomial", lambda = 0.098)
coef(best.elastic.lr)
 


## Logistic Regression with PCA

 
best.pca.lr <- glm(TenYearCHD ~ ., data = train_data_PCA[, c(1:8, 24)], family = binomial)
summary(best.pca.lr)
 


## XGBoost

 
# setup data for xgboost again
trainx = data.matrix(train_data_scaled[, -18])
trainy = train_data_scaled[, 18]

# define predictor and response variables in testing set
testx = data.matrix(test_data_scaled[, -18])
testy = test_data_scaled[, 18]

# define final training and testing sets
xgb_train = xgb.DMatrix(data = trainx, label = as.numeric(trainy))
xgb_test = xgb.DMatrix(data = testx, label = as.numeric(testy))

# create best model from CV above, using entire training set
best.xgb.model <- xgboost(data = xgb_train,
                          max.depth = 2,  
                          nrounds = 13, 
                          objective = 'binary:logistic', 
                          lambda = 2, 
                          gamma = 0, 
                          colsample_bytree = 1, 
                          verbose = F)     

summary(best.xgb.model)
importance_matrix = xgb.importance(colnames(xgb_train), model = best.xgb.model)
importance_matrix

ggplot(importance_matrix, aes(x = Gain, y = forcats::fct_reorder(Feature, Gain))) +
  geom_col() +
  theme_minimal() +
  ylab("") +
  ggtitle("XGBoost Feature Importance") +
  scale_y_discrete(labels = c("Age * Cigarettes Per Day",
                              "Blood Pressure Medication (Yes)",
                              "Total Cholesterol",
                              "Heart Rate",
                              "Glucose",
                              "Diabolic Blood Pressure",
                              "Glucose * Systolic Blood Pressure",
                              "Gender (Male)",
                              "Systolic Blood Pressure",
                              "Systolic Blood Pressure * Cigarettes Per Day",
                              "Age"))
ggsave(paste0(PATH_IMAGE_DIR,"/xgb_importance.png"))
 


# Finding Optimal Thresholds 

 
train_result_subset_lr <- predict(best.subset.lr, newdata = train_data, type = "response")
xtrain <- model.matrix(TenYearCHD.1 ~ ., train_data)[, -1]
train_result_elastic_lr <- predict(best.elastic.lr, newx = train_x, type = "response")
train_result_pca_lr <- predict(best.pca.lr, newdata = train_data_PCA, type = "response")
train_result_xgb <- predict(best.xgb.model, newdata = xgb_train)

train_metrics_subset <- data.frame()
for (threshold in seq(0.41, 0.6, 0.01)){
  train_subset_lr_threshold <- as.factor(as.numeric(train_result_subset_lr > threshold))
  train_subset_lr_table <- table(train_subset_lr_threshold, factor(train_data$TenYearCHD))
  temp <- confusionMatrix(train_subset_lr_table, positive = "1")
  model <- "subset"
  temp <- data.frame(cbind(model, cbind(threshold, t(temp$byClass))))
  train_metrics_subset <- rbind(train_metrics_subset, temp)
}

train_metrics_elastic <- data.frame()
for (threshold in seq(0.41, 0.6, 0.01)){
  train_elastic_lr_threshold <- as.factor(as.numeric(train_result_elastic_lr > threshold))
  train_elastic_lr_table <- table(train_elastic_lr_threshold, factor(train_y))
  temp <- confusionMatrix(train_elastic_lr_table, positive = "1")
  model <- "elastic"
  temp <- data.frame(cbind(model, cbind(threshold, t(temp$byClass))))
  train_metrics_elastic <- rbind(train_metrics_elastic, temp)
}

train_metrics_pca <- data.frame()
for (threshold in seq(0.41, 0.6, 0.01)){
  train_pca_lr_threshold <- as.factor(as.numeric(train_result_pca_lr > threshold))
  train_pca_lr_table <- table(train_pca_lr_threshold, factor(train_data$TenYearCHD))
  temp <- confusionMatrix(train_pca_lr_table, positive = "1")
  model <- "pca"
  temp <- data.frame(cbind(model, cbind(threshold, t(temp$byClass))))
  train_metrics_pca <- rbind(train_metrics_pca, temp)
}

train_metrics_xgb <- data.frame()
for (threshold in seq(0.41, 0.6, 0.01)){
  train_xgb_threshold <- as.factor(as.numeric(train_result_xgb > threshold))
  train_xgb_table <- table(train_xgb_threshold, factor(train_data$TenYearCHD))
  temp <- confusionMatrix(train_xgb_table, positive = "1")
  model <- "xgb"
  temp <- data.frame(cbind(model, cbind(threshold, t(temp$byClass))))
  train_metrics_xgb <- rbind(train_metrics_xgb, temp)
}

train_metrics_subset %>%
  select(model, threshold, Sensitivity, Specificity, Balanced.Accuracy) %>%
  mutate_at(c("Sensitivity", "Specificity", "Balanced.Accuracy"), as.numeric) %>%
  mutate(Gmean = sqrt(Sensitivity * Specificity)) %>%
  arrange(desc(Gmean))

train_metrics_elastic %>%
  select(model, threshold, Sensitivity, Specificity, Balanced.Accuracy) %>%
  mutate_at(c("Sensitivity", "Specificity", "Balanced.Accuracy"), as.numeric) %>%
  mutate(Gmean = sqrt(Sensitivity * Specificity)) %>%
  arrange(desc(Gmean))

train_metrics_pca %>%
  select(model, threshold, Sensitivity, Specificity, Balanced.Accuracy) %>%
  mutate_at(c("Sensitivity", "Specificity", "Balanced.Accuracy"), as.numeric) %>%
  mutate(Gmean = sqrt(Sensitivity * Specificity)) %>%
  arrange(desc(Gmean))

train_metrics_xgb %>%
  select(model, threshold, Sensitivity, Specificity, Balanced.Accuracy) %>%
  mutate_at(c("Sensitivity", "Specificity", "Balanced.Accuracy"), as.numeric) %>%
  mutate(Gmean = sqrt(Sensitivity * Specificity)) %>%
  arrange(desc(Gmean))
 


# Testing Results

 
# store predicted probabilities for all four models
result_subset_lr <- predict(best.subset.lr, newdata = test_data, type = "response")
result_elastic_lr <- predict(best.elastic.lr, newx = test_x, type = "response")
result_pca_lr <- predict(best.pca.lr, newdata = test_data_PCA, type = "response")
result_xgb <- predict(best.xgb.model, newdata = xgb_test)

# confusion matrices
cat("Log Reg with Feature Selection", "\n\n")
subset_lr_threshold <- as.factor(as.numeric(result_subset_lr>0.45))
subset_lr_table <- table(subset_lr_threshold, factor(test_data$TenYearCHD))
confusionMatrix(subset_lr_table, positive = "1")

cat("Log Reg with Elastic", "\n\n")
elastic_lr_threshold <- as.factor(as.numeric(result_elastic_lr>0.49))
elastic_lr_table <- table(elastic_lr_threshold, factor(test_data$TenYearCHD))
confusionMatrix(elastic_lr_table, positive = "1")

cat("Log Reg with PCA", "\n\n")
pca_lr_threshold <- as.factor(as.numeric(result_pca_lr>0.51))
pca_lr_table <- table(pca_lr_threshold, factor(test_data_PCA$TenYearCHD))
confusionMatrix(pca_lr_table, positive = "1")

cat("XGBOOST", "\n\n")
xgb_threshold <- as.factor(as.numeric(result_xgb > 0.5))
xgb_table <- table(xgb_threshold, factor(test_data_scaled$TenYearCHD.1))
confusionMatrix(xgb_table, positive = "1")
 


 
# store prediction results
pred_subset_lr <- prediction(result_subset_lr, test_data$TenYearCHD)
pred_elastic_lr <- prediction(result_elastic_lr, test_data$TenYearCHD)
pred_pca_lr <- prediction(result_pca_lr, test_data$TenYearCHD)
pred_xgb <- prediction(result_xgb, test_data$TenYearCHD)

# plot different useful performance metrics
measures <- c("acc", "sens", "spec", "fpr")
meas_names <- c("Accuracy", "Sensitivity (TPR)", "Specificity", "FPR")
cols <- c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA")

measure <- "acc"
meas_name <- "Accuracy"
subset_df <- data.frame(x = performance(pred_subset_lr, measure)@x.values[[1]], 
                        y = performance(pred_subset_lr, measure)@y.values[[1]])
elastic_df <- data.frame(x = performance(pred_elastic_lr, measure)@x.values[[1]], 
                         y = performance(pred_elastic_lr, measure)@y.values[[1]])
pca_df <- data.frame(x = performance(pred_pca_lr, measure)@x.values[[1]], 
                     y = performance(pred_pca_lr, measure)@y.values[[1]])
xgb_df <- data.frame(x = performance(pred_xgb, measure)@x.values[[1]], 
                     y = performance(pred_xgb, measure)@y.values[[1]])

ggplot() + 
  geom_line(data = subset_df, aes(x = x, y = y, color = cols[1]), lwd=1.5) + 
  geom_line(data = elastic_df, aes(x = x, y = y, color = cols[2]), lwd=1.5) + 
  geom_line(data = pca_df, aes(x = x, y = y, color = cols[3]), lwd=1.5) +
  geom_line(data = xgb_df, aes(x = x, y = y, color = cols[4]), lwd=1.5) +
  labs(color = "Model") + 
  xlab("Threshold") + 
  ylab(meas_name) +
  scale_color_manual(values = cols, 
                     labels  = c("Log Reg (Feat. Sel.)", 
                                 "Log Reg (Elastic)", 
                                 "Log Reg (PCA)", 
                                 "XGBoost")) +
  geom_hline(yintercept = 0.85, linetype = "dotted", color = "black", size = 0.8) +
  annotate("text", x=0.15, y=0.83, label= "Natural Class Balance", size = 2.5) + 
  theme(legend.position = "right") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/results_acc.png"))

measure <- "sens"
meas_name <- "Sensitivity (TPR)"
subset_df <- data.frame(x = performance(pred_subset_lr, measure)@x.values[[1]], 
                        y = performance(pred_subset_lr, measure)@y.values[[1]])
elastic_df <- data.frame(x = performance(pred_elastic_lr, measure)@x.values[[1]], 
                         y = performance(pred_elastic_lr, measure)@y.values[[1]])
pca_df <- data.frame(x = performance(pred_pca_lr, measure)@x.values[[1]], 
                     y = performance(pred_pca_lr, measure)@y.values[[1]])
xgb_df <- data.frame(x = performance(pred_xgb, measure)@x.values[[1]], 
                     y = performance(pred_xgb, measure)@y.values[[1]])

ggplot() + 
  geom_line(data = subset_df, aes(x = x, y = y, color = cols[1]), lwd=1.5) + 
  geom_line(data = elastic_df, aes(x = x, y = y, color = cols[2]), lwd=1.5) + 
  geom_line(data = pca_df, aes(x = x, y = y, color = cols[3]), lwd=1.5) +
  geom_line(data = xgb_df, aes(x = x, y = y, color = cols[4]), lwd=1.5) +
  labs(color = "Model") + 
  xlab("Threshold") + 
  ylab(meas_name) +
  scale_color_manual(values = cols, 
                     labels  = c("Log Reg (Feat. Sel.)", 
                                 "Log Reg (Elastic)", 
                                 "Log Reg (PCA)", 
                                 "XGBoost")) +
  theme(legend.position = "right") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/results_sens.png"))

measure <- "spec"
meas_name <- "Specificity"
subset_df <- data.frame(x = performance(pred_subset_lr, measure)@x.values[[1]], 
                        y = performance(pred_subset_lr, measure)@y.values[[1]])
elastic_df <- data.frame(x = performance(pred_elastic_lr, measure)@x.values[[1]], 
                         y = performance(pred_elastic_lr, measure)@y.values[[1]])
pca_df <- data.frame(x = performance(pred_pca_lr, measure)@x.values[[1]], 
                     y = performance(pred_pca_lr, measure)@y.values[[1]])
xgb_df <- data.frame(x = performance(pred_xgb, measure)@x.values[[1]], 
                     y = performance(pred_xgb, measure)@y.values[[1]])

ggplot() + 
  geom_line(data = subset_df, aes(x = x, y = y, color = cols[1]), lwd=1.5) + 
  geom_line(data = elastic_df, aes(x = x, y = y, color = cols[2]), lwd=1.5) + 
  geom_line(data = pca_df, aes(x = x, y = y, color = cols[3]), lwd=1.5) +
  geom_line(data = xgb_df, aes(x = x, y = y, color = cols[4]), lwd=1.5) +
  labs(color = "Model") + 
  xlab("Threshold") + 
  ylab(meas_name) +
  scale_color_manual(values = cols, 
                     labels  = c("Log Reg (Feat. Sel.)", 
                                 "Log Reg (Elastic)", 
                                 "Log Reg (PCA)", 
                                 "XGBoost")) +
  theme(legend.position = "right") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/results_spec.png"))

measure <- "fpr"
meas_name <- "FPR"
subset_df <- data.frame(x = performance(pred_subset_lr, measure)@x.values[[1]], 
                        y = performance(pred_subset_lr, measure)@y.values[[1]])
elastic_df <- data.frame(x = performance(pred_elastic_lr, measure)@x.values[[1]], 
                         y = performance(pred_elastic_lr, measure)@y.values[[1]])
pca_df <- data.frame(x = performance(pred_pca_lr, measure)@x.values[[1]], 
                     y = performance(pred_pca_lr, measure)@y.values[[1]])
xgb_df <- data.frame(x = performance(pred_xgb, measure)@x.values[[1]], 
                     y = performance(pred_xgb, measure)@y.values[[1]])

ggplot() + 
  geom_line(data = subset_df, aes(x = x, y = y, color = cols[1]), lwd=1.5) + 
  geom_line(data = elastic_df, aes(x = x, y = y, color = cols[2]), lwd=1.5) + 
  geom_line(data = pca_df, aes(x = x, y = y, color = cols[3]), lwd=1.5) +
  geom_line(data = xgb_df, aes(x = x, y = y, color = cols[4]), lwd=1.5) +
  labs(color = "Model") + 
  xlab("Threshold") + 
  ylab(meas_name) +
  scale_color_manual(values = cols, 
                     labels  = c("Log Reg (Feat. Sel.)", 
                                 "Log Reg (Elastic)", 
                                 "Log Reg (PCA)", 
                                 "XGBoost")) +
  theme(legend.position = "right") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/results_fpr.png"))

subset_df <- data.frame(x = performance(pred_subset_lr, "sens", "fpr")@x.values[[1]], 
                        y = performance(pred_subset_lr, "sens", "fpr")@y.values[[1]])
elastic_df <- data.frame(x = performance(pred_elastic_lr, "sens", "fpr")@x.values[[1]], 
                         y = performance(pred_elastic_lr, "sens", "fpr")@y.values[[1]])
pca_df <- data.frame(x = performance(pred_pca_lr, "sens", "fpr")@x.values[[1]], 
                     y = performance(pred_pca_lr, "sens", "fpr")@y.values[[1]])
xgb_df <- data.frame(x = performance(pred_xgb, "sens", "fpr")@x.values[[1]], 
                     y = performance(pred_xgb, "sens", "fpr")@y.values[[1]])

ggplot() + 
  geom_line(data = subset_df, aes(x = x, y = y, color = cols[1]), lwd=1.5) + 
  geom_line(data = elastic_df, aes(x = x, y = y, color = cols[2]), lwd=1.5) + 
  geom_line(data = pca_df, aes(x = x, y = y, color = cols[3]), lwd=1.5) +
  geom_line(data = xgb_df, aes(x = x, y = y, color = cols[4]), lwd=1.5) +
  labs(color = "Model") + 
  xlab("1 - Specifiicity (FPR)") + 
  ylab("Sensitivity (TPR)") +
  scale_color_manual(values = cols, 
                     labels  = c("Log Reg (Feat. Sel.)", 
                                 "Log Reg (Elastic)", 
                                 "Log Reg (PCA)", 
                                 "XGBoost")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black", size = 0.5) +
  theme(legend.position = "right") +
  theme_minimal()
ggsave(paste0(PATH_IMAGE_DIR,"/results_roc.png"))

 

roc_subset_lr <- roc(test_data$TenYearCHD, result_subset_lr)
auc_subset_lr <- roc_subset_lr$auc
cat("ROC-AUC from Log Reg w/ Feature Selection: ", auc_subset_lr, "\n")

roc_elastic_lr <- roc(test_data$TenYearCHD, result_elastic_lr)
auc_elastic_lr <- roc_elastic_lr$auc
cat("ROC-AUC from Log Reg w/ ElasticNet: ", auc_elastic_lr, "\n")

roc_pca_lr <- roc(test_data$TenYearCHD, result_pca_lr)
auc_pca_lr <- roc_pca_lr$auc
cat("ROC-AUC from Log Reg w/ PCA: ", auc_pca_lr, "\n")

roc_xgb <- roc(as.numeric(testy), result_xgb)
auc_xgb <- roc_xgb$auc
cat("ROC-AUC from XGBoost: ", auc_xgb, "\n")


