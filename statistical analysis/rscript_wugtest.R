library(lme4)  
library(ggplot2)  

data = wug_test 
summary(data)  


#scale and center all variables
data$c.lnh <- scale(data$LNH, center = TRUE, scale=FALSE)  
data$c.h <- scale(data$Hscaled, center = TRUE, scale=FALSE)  
data$c.trainingdata <- scale(data$`GPT-3 training data`, center = TRUE, scale=FALSE)  
data$modelN <- as.numeric(data$ModelN)  
data$question <-data$`Question Number`  
data$questiontype <- as.numeric(data$questiontype)  
data$evaluator <- as.numeric(data$evaluator)  


# numerize and factorize variables
data$c.lnh <- as.numeric(data$c.lnh)  
data$c.trainingdata <- as.numeric(data$c.trainingdata)  
data$question <- as.numeric(data$question)  
data$questiontype <- as.factor(data$`Question type`)  
data$language <- as.factor(data$`Language`)  
data$model <- as.factor(data$`Model`)  
  
# build models
# Hscaled, with question as random effect  
model1 <- glmer(`Binary rating`~ c.h*c.trainingdata  + (1|question), data = data, family = binomial)  
summary(model1)  

model2 <- glmer(`Binary rating`~ c.lnh*c.trainingdata  + (1|question), data = data, family = binomial)  
summary(model2) 



