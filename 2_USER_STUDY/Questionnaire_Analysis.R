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



# Statistics for RiCEv2 (see Section 4.3.1) ====================================

set.seed(8273) #<-- Randomly generated seed for reproducability.

## Make data set splits 
scale_dev = read_excel("Questionnaire_Data.xlsx", sheet = "RiCEV2")
split_data = sample.split(scale_dev$Q13,SplitRatio = 0.6) 
train_data = subset(scale_dev,split_data == TRUE) 
test_data = subset(scale_dev,split_data == FALSE) 
nrow(train_data) # Print sizes
nrow(test_data)

## All data
selectedItemsTr = train_data[, c("Q13", "Q29", "Q7",
                               "Q19", "Q14", "Q21", 
                               "Q11", "Q23", "ExR1")]
selectedItemsTe = test_data[, c("Q13", "Q29", "Q7",
                                "Q19", "Q14", "Q21", 
                                "Q11", "Q23", "ExR1")]
resulttrain = psych::alpha(selectedItemsTr)$total$raw_alpha
resulttest = psych::alpha(selectedItemsTe)$total$raw_alpha
signif(resulttrain,2) 
signif(resulttest,2)

## RiCE-Pa
selectedItemsTr = train_data[, c("Q22", "Q25", "PaR1")]
selectedItemsTe = test_data[, c("Q22", "Q25", "PaR1")]
resulttrain = psych::alpha(selectedItemsTr)$total$raw_alpha
resulttest = psych::alpha(selectedItemsTe)$total$raw_alpha
signif(resulttrain,2)
signif(resulttest,2)

## RiCEv2-Se
selectedItemsTr = train_data[, c("Q19", "Q14", "Q21")]
selectedItemsTe = test_data[, c("Q19", "Q14", "Q21")]
resulttrain = psych::alpha(selectedItemsTr)$total$raw_alpha
resulttest = psych::alpha(selectedItemsTe)$total$raw_alpha
signif(resulttrain,2)
signif(resulttest,2)

## RiCEv2-Ex
selectedItemsTr= train_data[, c("Q11", "Q23", "ExR1")]
selectedItemsTe = test_data[, c("Q11", "Q23", "ExR1")]
resulttrain = psych::alpha(selectedItemsTr)$total$raw_alpha
resulttest = psych::alpha(selectedItemsTe)$total$raw_alpha
signif(resulttrain,2)
signif(resulttest,2)

## RiCEv2-Cp
selectedItemsTr = train_data[, c("Q13", "Q29", "Q7")]
selectedItemsTe = test_data[, c("Q13", "Q29", "Q7")]
resulttrain = psych::alpha(selectedItemsTr)$total$raw_alpha
resulttest = psych::alpha(selectedItemsTe)$total$raw_alpha
signif(resulttrain,2)
signif(resulttest,2)


#===============================================================================
# Statistics for Engagement & Reflection  (see Section 4.5.1)  =================


my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced - post RiCE")


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
my_data = read_excel("Questionnaire_Data.xlsx", sheet = "data reduced - post RiCE")

set.seed(2342) #<-- Setup a random seed for reproducability.
split_data = sample.split(my_data$Age,SplitRatio = 0.6) 
train_data = subset(my_data,split_data == TRUE) 
test_data = subset(my_data,split_data == FALSE) 
nrow(train_data) # Print sizes
nrow(test_data)

### RiCE ====
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

### RiCE-Ex ====
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

### RiCE-Cp ====
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


### RiCE-Se ====
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
