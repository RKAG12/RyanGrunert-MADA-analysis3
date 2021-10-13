###############################
# Analysis script

#load needed packages. make sure they are installed.
library(tidyverse)
library(tidymodels)

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
df <- readRDS(data_location)

#####Fitting the linear model with the continuous outcome and important predictor
#Continuous outcome: BodyTemp
#Predictor: RunnyNose

linmod <- linear_reg() %>% #Setting up the linear model engine and function
  set_engine("lm") %>%
  set_mode("regression")

mod1 <- linmod %>% #Running the linear model 
  fit(BodyTemp ~ RunnyNose, data = df)


#####Fitting the linear model with the continuous outcome and all predictors

mod2 <- linmod %>% #Running the linear model
  fit(BodyTemp ~ ., data = df)

#####Comparing model results
CompMod1 <- anova(mod1$fit, mod2$fit, test = "Chisq")

glance(mod1)
glance(mod2)

#The test above tells us that we should use the more complex model, and 
#glancing at the two linear models, model 2 (the more complex one) has
#a lower AIC value. Both models are statistically significant with a p-value < 0.05.


#####Fitting logistic model with categorical outcome and important predictor
#Categorical outcome: Nausea
#Predictor: RunnyNose

logmod <- logistic_mod <- logistic_reg() %>% #Setting up the logistic model
  set_engine("glm") %>%
  set_mode("classification")

mod3 <- logmod %>% #Running the model
  fit(Nausea ~ RunnyNose, data = df)

#####Fitting logistic model with Nausea and all predictors

mod4 <- logmod %>%
  fit(Nausea ~ ., data = df)

#####Comparing model results with a likelihood ratio test
CompMod2 <- anova(mod3$fit, mod4$fit, test = "LRT")

glance(mod3)
glance(mod4)

#The test above tells us that we should use the more complex model
#with all the parameters. Glancing at both logistic models, mod4 (the complex one)
#has a higher log likelihood and lower AIC value, making it the better fit
#for the dataset.

#Saving the comparison results
saveRDS(CompMod1, file = here("results", "analysis", "LinearModelTable.rds"))
saveRDS(CompMod2, file = here("results", "analysis", "LogisticModelTable.rds"))


  