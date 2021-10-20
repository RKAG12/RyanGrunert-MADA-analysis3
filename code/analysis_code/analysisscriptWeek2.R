###############################
# Analysis script 2
#The following is the analysis script for week 2 of this project.
#This is for Module 9: Evaluating your model fits
#This also shows the cross-valuation workflow for analysis

#load needed packages. make sure they are installed.
library(tidyverse)
library(tidymodels)
library(rsample)

#Setting the seed for reproducible analysis
set.seed(100)

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
df <- readRDS(data_location)

###########Data Splitting################
#Putting 3/4 of the data into a training set
data_split <- initial_split(df, prop = 3/4)

#Creating data frames for the two sets
train_data <- training(data_split)
test_data <- testing(data_split)

##############Workflow Creation and Model Fitting###################
#Creating new recipe to fit the categorical outcome of interest (Nausea) to all predictors
Nausea_rec <- recipe(Nausea ~ ., data = train_data)

summary(Nausea_rec)#Shows all the types, roles, and sources for the variables in the training dataset

#Building a logistic model
lr_mod <- logistic_reg() %>%
          set_engine("glm")

#Building the logistic model workflow
Nausea_wflow <- workflow() %>%
                add_model(lr_mod) %>%
                add_recipe(Nausea_rec)
Nausea_wflow
# ══ Workflow ═══════════════════════════════════════════
# Preprocessor: Recipe
# Model: logistic_reg()
# 
# ── Preprocessor ───────────────────────────────────────
# 0 Recipe Steps
# 
# ── Model ──────────────────────────────────────────────
# Logistic Regression Model Specification (classification)
# 
# Computational engine: glm 

#Preparing the recipe and train the model
Nausea_fit <- Nausea_wflow %>%
              fit(data = train_data)
#Object finalized recipe and fitted model objects

#Extracting the model and recipe
Nausea_fit %>%
    extract_fit_parsnip() %>%
    tidy()



####################Model 1 Evaluation#####################
#lr_mod = logistic model
#Nausea_rec = preprocessing recipe
#Nausea_wflow = bundling the model and recipe together
#Nausea_fit = trained our workflow using fit()

#predict applies recipe to new data, and passes them to fitted model

####On the training data
predict(Nausea_fit, train_data)


Nausea_aug1 <- augment(Nausea_fit, train_data)

Nausea_aug1 %>% #Selecting the probability that of no
  select(Nausea, .pred_No)


Nausea_aug1 %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot()

Nausea_aug1 %>%
  roc_auc(truth = Nausea, .pred_No)

#This model had a ROC-AUC of 0.810, so it might be useful.


####On the testing data
predict(Nausea_fit, test_data)


Nausea_aug2 <- augment(Nausea_fit, test_data)

Nausea_aug2 %>% #Selecting the probability that of no
  select(Nausea, .pred_No)

####ROC curve for the Nausea test dataset
#Probability of No for Nausea based on the model.
Nausea_aug2 %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot()

Nausea_aug2 %>%
  roc_auc(truth = Nausea, .pred_No)


#We have a ROC-AUC of 0.643, so this model might not be useful. Has
#a lower number than the training dataset.




##########################Alternative Model Evaluation############################
#This model only fits RunnyNose to the Nausea variable

#Creating new recipe to fit the categorical outcome of interest (Nausea) to RunnyNose predictor

Nausea_rec2 <- recipe(Nausea ~ RunnyNose, data = train_data)

#Creating another logistic model to use
lr_mod2 <- logistic_reg() %>%
  set_engine("glm")

##Building the second logistic model workflow
Nausea_wflow2 <- workflow() %>%
  add_model(lr_mod2) %>%
  add_recipe(Nausea_rec2)

#Preparing the recipe and train the model
Nausea_fit2 <- Nausea_wflow2 %>%
  fit(data = train_data)

#Looking at the new fit
Nausea_fit2 %>%
  extract_fit_parsnip() %>%
  tidy()

#####Creating the ROC curve for the second model fit
predict(Nausea_fit2, test_data)


Nausea_aug3 <- augment(Nausea_fit2, test_data)

Nausea_aug3 %>% #Selecting the probability that of no
  select(Nausea, .pred_No)

####ROC curve for the Nausea test dataset
#Probability of No for Nausea based on the model.
Nausea_aug3 %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot()

Nausea_aug3 %>%
  roc_auc(truth = Nausea, .pred_No)

#For this model we get a ROC-AUC of 0.518, this model is not useful.

