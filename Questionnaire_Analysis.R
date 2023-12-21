# Preamble ====================================================================

install.packages("readxl")
install.packages("psych")
install.packages("robustlmm")
library("readxl")
library("psych")
library("robustlmm")
library("caret")
library("caTools") 
library("dplyr")


# Setup R to read current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999)

#===============================================================================
# Statistics for Engagement & Reflection  (see Section 3.6.1)  =================
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")


## Test Confounding Variables ==================================================

# Considers themselves as a Musician vs Non-Musician
# Mann Whitney U... although named wilcox by r studio
wilcox.test(my_data$UEQ_Engagement~my_data$Musician_Groups, correct = FALSE, exact=FALSE)
wilcox.test(my_data$RiCE~my_data$Musician_Groups, correct = FALSE, exact=FALSE)

# Year of study 
# Mann Whitney U... although named wilcox by r studio
wilcox.test(my_data$UEQ_Engagement~my_data$Year_of_Study, correct = FALSE, exact=FALSE)
wilcox.test(my_data$RiCE~my_data$Year_of_Study, correct = FALSE, exact=FALSE)

# Degree Programme
kruskal.test(my_data$UEQ_Engagement~my_data$Course_of_Study)
kruskal.test(my_data$RiCE~my_data$Course_of_Study)


## Linear Regression Models ====================================================

# Split the datasets
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced")

set.seed(2342) #<-- Setup a random seed for reproducability.
split_data = sample.split(my_data$Age,SplitRatio = 0.6) 
train_data = subset(my_data,split_data == TRUE) 
test_data = subset(my_data,split_data == FALSE) 
nrow(train_data) # Print sizes
nrow(test_data)

### RiCE : Model 1 ====
big_model <- lm(`RiCE` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = RiCE ~ UEQ_Focused_Attention + UEQ_Reward, data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE`)

### RiCE-Ex : Model 2 ====
big_model <- lm(`RiCE-Ex` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = `RiCE-Ex` ~ UEQ_Aesthetic_Appeal, data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE-Ex`)

### RiCE-Cp : Model 3 ====
big_model <- lm(`RiCE-Cp` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = `RiCE-Cp` ~ UEQ_Focused_Attention + UEQ_Reward + UEQ_Aesthetic_Appeal, data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE-Cp`)


### RiCE-Se : Model 4 ====
big_model <- lm(`RiCE-Se` ~ UEQ_Focused_Attention
                + UEQ_Aesthetic_Appeal
                + UEQ_Perceived_Usability
                + UEQ_Reward,
                data=train_data)
summary(big_model)

step(big_model, direction = "backward")

found_model <- lm(formula = `RiCE-Se` ~ UEQ_Reward, 
                  data = train_data)
summary(found_model)

pred <- predict(found_model, test_data) 
postResample(pred, test_data$`RiCE-Se`)
