#Linear Discriminant Analysis  
rm(list = ls()) 
pacman::p_load(dplyr, data.table, dplyr, 
               lmtest, sandwich, multiwayvcov,
               gmodels, stargazer)
library(MASS)
library(tidyverse)
library(caret)

#dataset 
setwd("D:/LMU/Wise2e1/seminar/paper")
orig_dat <- read.csv("DataSet.csv")

#subset for treated mothers 
df_treated <- orig_dat[orig_dat$threegroup == 2,]
df_treated$recover <- as.factor(df_treated$recover_perm)
head(df_treated) 
df_treated$recover



#define independent variables 
df_treated_reg <- df_treated %>%
  dplyr::select(recover,hamd_baseline,bdq_baseline,mo_bmi,mo_ht,depindex_0,
         edu_mo_baseline,age_baseline,age_baseline_sq,ses,employed_mo_baseline,mo_emp,ln_income_fa,income_mo,wealth_baseline,
         relationshipindex,edu_fa_baseline,fatherfinancial,
         childmort,first_childXtreat,c_age_int,mspss_baseline) 

attach(df_treated_reg)
df_treated_reg$genhealth[genhealth == "Very good"] <- 4
df_treated_reg$genhealth[genhealth == "Good"] <- 3
df_treated_reg$genhealth[genhealth == "Moderate"] <- 2
df_treated_reg$genhealth[genhealth == "Bad"] <- 1
df_treated_reg$genhealth 
detach(df_treated_reg)
df_treated_reg <- na.omit(df_treated_reg)
head(df_treated_reg)
dim(df_treated_reg)

#spilt to train and test set 
##set.seed(123) 
##training <- df_treated_reg$recover %>%
##  createDataPartition(p=0.7,list=FALSE) 
##train <- df_treated_reg[training, ]
##test <- df_treated_reg[-training, ] 


#construct LDA function 
##regress all selected 
library(MASS) 
ld_all <- lda(recover ~.,data=df_treated_reg)
ld_all 
#predict using LDA 
predict_all <- predict(ld_all)
newGroup <- predict_all$class 
cbind(df_treated_reg$recover,predict_all$x,newGroup)

#evaluation 
tab <- table(df_treated_reg$recover,newGroup) 
tab 
erro <- 1-sum(diag(prop.table(tab)))
erro
plot(tab) 

#input new data for prediction 
##predict(ld_all,newdata = data.frame()) 



