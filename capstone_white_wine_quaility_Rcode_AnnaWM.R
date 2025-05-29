# Required packages 

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(rpart)) install.packages('rpart')
library(rpart)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(knitr)) install.packages('knitr')
library(knitr)


#####################################
# Wine quality dataset preparation #
####################################

## Import and tidy the Wine Quality datasets

## sourced from https://archive.ics.uci.edu/dataset/186/wine+quality

# dataset citation: P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
# Modeling wine preferences by data mining from physicochemical properties. 
# In Decision Support Systems, Elsevier, 47(4):547-553, 2009.

# import the white wine csv file
winequal_temp_w <- read_csv("winequality-white.csv")

# use tidyr::separate to separate the one character column into 12 columns of data
# remove spaces from the column names by adding an underscore, so that there no 
# problems running the machine learning algorithms
# (rf had an error when there were spaces in the column names)

winequal_white <- winequal_temp_w %>% 
  separate(col = `fixed acidity;volatile acidity;citric acid;residual sugar;
           chlorides;free sulfur dioxide;total sulfur dioxide;density;pH;
           sulphates;alcohol;quality`, 
           into = c("fixed_acidity", "volatile_acidity", "citric_acid", 
                    "residual_sugar", "chlorides", "free_sulfur_dioxide", 
                    "total_sulfur_dioxide", "density", "pH", "sulphates", 
                    "alcohol", "quality"), 
           sep = ";")

# convert from character to integer
winequal_white <- winequal_white %>% mutate_if(is.character, as.numeric)


##############################
# Exploratory data analysis #
#############################

# number of rows and columns
dim(winequal_white)

# summary of the 12 variables in the dataset
summary(winequal_white)


########################
## Data distributions ##
########################

# histogram of quailty variable in winequal_white
winequal_white %>%
  ggplot(aes(quality)) + # x-axis is quality
  geom_histogram(colour = "black") + # plot histogram
  scale_x_continuous(n.breaks = 7) + # 7 x-axis marks
  scale_y_log10(n.breaks = 15) + # make y-axis log base 10, 15 axis marks
  ggtitle("Distribution of white wine quality") + # plot title
  ylab("count (log10)") + # y-axis label
  theme_minimal() + # set minimal theme
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks


# histogram of alcohol variable in winequal_white
winequal_white %>%
  ggplot(aes(alcohol)) + # x-axis is alcohol
  geom_histogram(colour = "black", bins = 50) + # plot histogram
  scale_x_continuous(n.breaks = 10) + # 10 x-axis marks
  scale_y_continuous(n.breaks = 10) + # 10 y-axis marks
  ggtitle("Distribution of alcohol") + # plot title
  xlab("alcohol (% by volume)") + # x-axis label
  theme_minimal() + # set minimal theme
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks


# histogram of density variable in winequal_white  
winequal_white %>%
  ggplot(aes(density)) + # x-axis is density
  geom_histogram(colour = "black", bins = 50) + # plot histogram
  scale_x_continuous(n.breaks = 10) + # 10 x-axis marks
  scale_y_continuous(n.breaks = 10) + # 10 y-axis marks
  ggtitle("Distribution of density") + # plot title
  xlab("density (g/L)") + # x-axis label
  theme_minimal() + # set minimal theme
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks

###############################
# Look at quality vs features #
##############################

# Determine correlation coefficients of the 11 physicochemical 
# properties of winequla_white with the quality score

fa_cor <-  cor(winequal_white$quality, winequal_white$fixed_acidity)
va_cor <-  cor(winequal_white$quality, winequal_white$volatile_acidity)
ca_cor <-  cor(winequal_white$quality, winequal_white$citric_acid)
rs_cor <-  cor(winequal_white$quality, winequal_white$residual_sugar)
c_cor <-  cor(winequal_white$quality, winequal_white$chlorides)
fsd_cor <-  cor(winequal_white$quality, winequal_white$free_sulfur_dioxide)
tsd_cor <-  cor(winequal_white$quality, winequal_white$total_sulfur_dioxide)
d_cor <-  cor(winequal_white$quality, winequal_white$density)
p_cor <-  cor(winequal_white$quality, winequal_white$pH)
s_cor <-  cor(winequal_white$quality, winequal_white$sulphates)
a_cor <-  cor(winequal_white$quality, winequal_white$alcohol)


# create table of 11 variables with the correlation coefficients
tibble(
  ID = seq(1:11),
  variable =  c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", 
                "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide",
                                   "density", "pH", "sulphates", "alcohol"),
  correlation_coefficient = c(fa_cor, va_cor, ca_cor, rs_cor, c_cor, fsd_cor,
                             tsd_cor, d_cor, p_cor, s_cor, a_cor)) %>%
  kable(caption = "Correlation coefficients of 11 variables with quality score")

#---------------------
## Plot the scatter graphs of quality vs variable for top 3 correlation coefficients  
  
#  plot quality vs alcohol with a regression line

# define parameters for the regression line, mean, standard deviation 
# and correlation coefficient
mu_x <- mean(winequal_white$alcohol)
mu_y <- mean(winequal_white$quality)
s_x <- sd(winequal_white$alcohol)
s_y <- sd(winequal_white$quality)
r <- cor(winequal_white$quality, winequal_white$alcohol)

# use ggplot to plot at quality vs alcohol
winequal_white %>% 
  ggplot(aes(alcohol, quality)) + 
  geom_point(alpha = 0.5) + # scatter graph with 0.5 alpha
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) + # calculate regression line
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  ggtitle("Quality vs alcohol") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks

#-----------
#  plot quality vs density with a regression line

# define parameters for the regression line, mean, standard deviation 
# and correlation coefficient
mu_x <- mean(winequal_white$density)
mu_y <- mean(winequal_white$quality)
s_x <- sd(winequal_white$density)
s_y <- sd(winequal_white$quality)
r <- d_cor

# use ggplot to plot at quality vs density
winequal_white %>%
  ggplot(aes(density, quality)) +
  geom_point(alpha = 0.5) + # scatter graph with 0.5 alpha
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) + # calculate regression line
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  ggtitle("Quality vs density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks

#------------
#  plot quality vs chlorides with a regression line

# define parameters for the regression line, mean, standard deviation 
# and correlation coefficient
mu_x <- mean(winequal_white$chlorides)
mu_y <- mean(winequal_white$quality)
s_x <- sd(winequal_white$chlorides)
s_y <- sd(winequal_white$quality)
r <- cor(winequal_white$quality, winequal_white$chlorides)

# use ggplot to plot at quality vs chlorides
winequal_white %>%
  ggplot(aes(chlorides, quality)) +
  geom_point(alpha = 0.5) +  # scatter graph with 0.5 alpha
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) + # calculate regression line
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  ggtitle("Quality vs chlorides") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks

###############################
# Machine learning algorithms #
###############################

# table of characteristics to build into parameters of the machine learning model
tibble(iteration = c(1,2,3),
       machine_learning_algorithms = c("Generalised linear regression",
                                       "Regression trees",
                                       "Random forests")) %>%  
  kable(caption = "Machine learning algorithms to develop wine quality predictive model")

#####################
# Model evaluation #
###################

# RMSE written out as an R function.
# RMSE is the sqrt of the mean of (true_ratings - predicted_ratings) squared 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#####################
# Model validation #
####################

# Create training and test datasets from winequal_white 
# for use in developing machine learning algorithm

# set the seed with rounding
set.seed(1, sample.kind="Rounding")

# create the partition index using caret::createDataPartion function
winequal_test_index <- createDataPartition(y = winequal_white$quality, times = 1,
                                       p = 0.2, list = FALSE)

# use the partition index to create the train_set and test_set
winequal_train_set <- winequal_white[-winequal_test_index,] # 80% of winequal_white data
winequal_test_set <- winequal_white[winequal_test_index,] # 20% of winequal_white data


#################################
# Generalised linear regression #
#################################

# Baseline model with generalised linear regression (glm) with 5 predictors

# using caret::train to calculate the algorithm
# predict quality using 5 predictors
train_5pred_glm <- train(quality ~  alcohol + chlorides + residual_sugar + 
                           volatile_acidity + 
                           total_sulfur_dioxide, 
                         method = "glm",
                         data = winequal_train_set)

# use predict function to make quality score predictions using trained algorithm
predict_train_5pred_glm <- predict(train_5pred_glm, winequal_test_set, type = "raw")

# calculate RMSE to evaluate the algorithm 
rmse_5pred_glm <- RMSE(predict_train_5pred_glm, winequal_test_set$quality)
# print RMSE
rmse_5pred_glm

#-------------------------
# Baseline model with generalised linear regression (glm) with 8 predictors

# using caret::train to calculate the algorithm
# predict quality using 8 predictors
train_8pred_glm <- train(quality ~  alcohol + chlorides + residual_sugar + 
                           citric_acid + volatile_acidity + fixed_acidity + 
                           total_sulfur_dioxide + sulphates, 
                         method = "glm", # use glm algorithm
                         data = winequal_train_set) # on winequal train set

# use predict function to make quality score predictions using trained algorithm
predict_train_8pred_glm <- predict(train_8pred_glm, winequal_test_set, type = "raw")

# calculate RMSE to evaluate the algorithm 
rmse_8pred_glm <- RMSE(predict_train_8pred_glm, winequal_test_set$quality)
# print RMSE
rmse_8pred_glm


#------------------------

# Baseline model with generalised linear regression (glm) with all predictors

# using caret::train to calculate the algorithm
train_allpred_glm <- train(quality ~  ., # predict quality using all other predictors
                           method = "glm", # use glm algorithm
                           data = winequal_train_set) # on winequal train set

# use predict function to make quality score predictions using trained algorithm
predict_train_allpred_glm <- predict(train_allpred_glm, winequal_test_set, type = "raw")

# calculate RMSE to evaluate the algorithm 
rmse_allpred_glm <- RMSE(predict_train_allpred_glm, winequal_test_set$quality)
# print RMSE
rmse_allpred_glm


#------------------------------
## Summary of glm algorithms 

#make table of RMSES for glm 
tibble(iteration = c(1:3),
       model_type = c("GLM with 11 predictors",
                      "GLM with 8 predictors",
                      "GLM with 5 predictors"),
       RMSE = c(rmse_allpred_glm, rmse_8pred_glm, rmse_5pred_glm)) %>%
  kable(caption = "Generalised linear model iterations in order of best performance measured by RMSE")

###################
# Regression tree #
###################

## No tuning of parameters

# fit regression tree model using all predictors using rpart function
fit_regression_tree <- rpart(quality ~ ., data = winequal_train_set)

# plot the regression tree model
plot(fit_regression_tree, margin = 0.1)
text(fit_regression_tree, cex = 0.75)

# use predict function to make predictions usig fitted model  
predict_regression_tree <- predict(fit_regression_tree, winequal_test_set)

# calculate RMSE using caret::RMSE
rmse_rt_allpreds <- RMSE(predict_regression_tree, winequal_test_set$quality)
# print RMSE
rmse_rt_allpreds


#-----------------------

# Fit the regression tree model on the training set
# With tuning  the cp parameter and cross validation

# https://scientistcafe.com/ids/regression-and-decision-tree-basic - reference for how to use the caret package to tune regression tree

#set seed for reproducibility
set.seed(1, sample.kind = "Rounding")

# use caret:train to tune cp 
train_rpart_cv10 <- train(
  quality ~., # all 11 predictors
  data = winequal_train_set, # train set
  method = "rpart", # using rpart
  trControl = trainControl("cv", number = 10), # cross validation of 10 samples
  tuneGrid = data.frame(cp = seq(0, 0.05, len = 25))) # sequence of 25 cp to test

# plot the tuning of cp
ggplot(train_rpart_cv10) +
  theme_minimal()

# print the value of best cp with lowest RMSE
train_rpart_cv10$bestTune

# use tuned model to predict quality against test set
predict_rpart_cv10 <- predict(train_rpart_cv10, winequal_test_set)

# calculate RMSE
rmse_rpart_cv10 <- RMSE(predict_rpart_cv10, winequal_test_set$quality)
# print RMSE
rmse_rpart_cv10
#-----------------------------

# Fit the regression tree model on the training set
# With tuning the maxdepth parameter and cross validation

#set seed for reproducibility
set.seed(1, sample.kind = "Rounding")

# use caret:train to tune maxdepth
train_rpart2_cv10 <- train(
  quality ~., # all 11 predictors
  data = winequal_train_set, # train set
  method = "rpart2", # rpart2 for tuning maxdepth
  tuneLength = 12, # 12 values
  trControl = trainControl("cv", number = 10)) # cross validation of 10 samples

# plot the tuning of RMSE vs maxdepth
ggplot(train_rpart2_cv10) +
  theme_minimal()

# print the best value of maxdepth with lowest RMSE 
train_rpart2_cv10$bestTune

# use tuned model to predict quality against test set
predict_rpart2_cv10 <- predict(train_rpart2_cv10, winequal_test_set)

# calculate RMSE
rmse_rpart2_cv10 <- RMSE(predict_rpart2_cv10, winequal_test_set$quality)
# print RMSE
rmse_rpart2_cv10

#---------- putting it together

# fit the regression tree model against the train set using 
# the tuned cp and maxdepth parameters from above

fit_rpart_tuned <- rpart(quality ~ ., 
                         data = winequal_train_set, 
                         maxdepth = train_rpart2_cv10$bestTune$maxdepth, 
                         cp = train_rpart_cv10$bestTune$cp)

# plot the tuned regression tree model
plot(fit_rpart_tuned, margin = 0.1)
text(fit_rpart_tuned, cex = 0.75)

# use tuned fitted model to predict quality against test set
predict_rpart_tuned <- predict(fit_rpart_tuned, winequal_test_set)

# calculate RMSE
rmse_rpart_tuned <- RMSE(predict_rpart_tuned, winequal_test_set$quality)
# print RMSE
rmse_rpart_tuned

#################
# Random forest #
################


# initial random forest model with no tuning

# set the seed for reproducibility
set.seed(1, sample.kind = "Rounding")

# using randomForest function to fit the model against train data
fit_random_forest <- randomForest(quality ~., data = winequal_train_set)

# plot the fitted model
plot(fit_random_forest)

# use fitted model to predict quality against test set
predict_random_forest <- predict(fit_random_forest, winequal_test_set)

# calculate RMSE
rmse_random_forest <- RMSE(predict_random_forest, winequal_test_set$quality)
# print RMSE
rmse_random_forest


#---------------4.5  mins to run the tuning, 1 min to fit the model 

# random forest algorithm tuning mtry

# set the seed for reproducibility
set.seed(1, sample.kind = "Rounding")

# use 5 fold cross validation
control <- trainControl(method="cv", number = 5)
# assign the values of mtry to tune for
grid <- data.frame(mtry = seq(1, 6, 1))
# caret::train
train_rf_4 <-  train(quality ~., # predict quality 
                     method = "rf", # using random forest algorithm
                     data = winequal_train_set, # on the train set
                     ntree = 400, # number of trees
                     trControl = control, # cross validation
                     tuneGrid = grid) # tune for mtry

# print the tuning of the model, RMSE, Rsquared and MAE
train_rf_4

# print best tuned mtry that minimises RMSE
train_rf_4$bestTune$mtry

# fir the randomForest model with best tuned mtry
fit_rf_4 <- randomForest(quality ~., data = winequal_train_set,
                         mtry = train_rf_4$bestTune$mtry)

# plot the best tuned fitted model
plot(fit_rf_4)

# use tuned fitted model to predict quality against test set
predict_fit_rf_4 <- predict(fit_rf_4, winequal_test_set)

# calculate RMSE
rmse_rf_4 <- RMSE(predict_fit_rf_4, winequal_test_set$quality)
# print RMSE
rmse_rf_4 

#-------------------------------------
############
# Results #
###########

# table of best RMSEs for each machine learning method

tibble(iteration = c(1,2,3),
       machine_learning_algorithm = c("Generalised linear regression",
                                      "Regression trees",
                                      "Random forests"),
       parameters = c("all predictors", "cp = 0.002, maxdepth = 5", "mtry = 3, ntree = 400"), 
      RMSE = c(rmse_allpred_glm,  rmse_rpart_tuned, rmse_rf_4)) %>%  
kable(caption = "Performance of machine learning algorithms for wine quality predictive model as measured by RMSE")

#---------------------------------

#Final model is the random forest as it has the lowest RMSE

#Therefore applying the model to the dataset:
fit_final_model_rf <- randomForest(quality ~., data = winequal_white,
                                   mtry = train_rf_4$bestTune$mtry)
  
  
# adding predicted quality score to the whole white wine dataset
final_model_rf <- winequal_white %>% 
  mutate(predict_quality = predict(fit_final_model_rf, winequal_white))


# use ggplot to plot the actual quality scores vs the predicted quality scores
final_model_rf %>% ggplot(aes(predict_quality, quality)) + # x-axis is predicted quality
  geom_point(alpha = 0.5) + # plot scattergraph
  scale_x_continuous(n.breaks = 15) + # 7 x-axis breaks
  scale_y_continuous(n.breaks = 7) + # 15 y-axis breaks
  ggtitle("Actual quality vs predicted quality") + # plot title
  ylab("actual quality score") + # y label
  xlab("predicted quality score") + # x label
  theme_minimal() + # set theme
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks
