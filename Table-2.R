rm(list = ls())
#Install and call packages
install.packages("remotes")
remotes::install_github("ritest")
install.packages("ritest")
#load various packages
pacman::p_load(dplyr, data.table, dtplyr, 
               lmtest, sandwich, multiwayvcov,
               gmodels, stargazer,ggthemes,gridExtra)
###Prepare 
#Read in data set
setwd("D:/LMU/Wise21/seminar/paper")
orig_dat <- read.csv("DataSet.csv")
head(orig_dat) 


#------------------------------------------------------------------------------------------------
###Table2 
#regress depression on treatment and control vars 
##define variables 
Depression_severity <- c("depindex_6m","depindex_1y","depindex_7y") 
Perceived_social_support <- c("mspss_6m","mspss_1y","mspss_tot")
df_intcontrols <- c("monthint", "monthintsq", "doi0", "doi0Xtreat", "intervr1", "intervr2", "intervr3", "intervr4", "intervr5", "intervr6", "intervr7", "intervr8", "intervr9")
df_adjust <- c("age_baseline","age_baseline_sq","employed_mo_baseline","mo_emp",
               "grandmother_baseline", "MIL","wealth_baseline","edu_lvl_mo_1",
               "edu_lvl_mo_2","edu_lvl_mo_3", "edu_mo_baseline", "edu_fa_baseline",
               "kids_no","first_child","hamd_baseline","mspss_baseline","doi0")
outcome_dep <- c("depressed_6m","depindex_6m","depressed_1y","depindex_1y","depressed","depindex_7y","recover_perm","recover_never")

table2_full <- orig_dat 
table2_control <- orig_dat[orig_dat$threegroup == 3,] #control group:depressed without intervention 
print(table2_control) 


#col1:control group mean and sd
#try sapply 
df_describe <- orig_dat[orig_dat$threegroup  == 3,] %>%
  select(depressed_6m,depindex_6m,depressed_1y,depindex_1y,depressed,depindex_7y,recover_perm,recover_never) 
df_describe
mean_table2 <- sapply(df_describe,mean,na.rm=TRUE)  
mean_table2[2]
sd_table2 <- sapply(df_describe,sd,na.rm=TRUE) 
sd_table2 
sd_table2[1]

#store values 
reg <- list()
vcov_uc <- list()
cluster_se <- list()
table2_full <- as.data.frame(table2_full) 
dim(table2_full) 
head(table2_full) 
Depression <- c("depressed_6m","depressed_1y","depressed") #depression at 6m,1y,7y  
Depression_severity <- c("depindex_6m","depindex_1y","depindex_7y") 
outcome_dep <- c("depressed_6m","depindex_6m","depressed_1y","depindex_1y","depressed","depindex_7y","recover_perm","recover_never")  

#reg2: no control 
for (i in 1:length(outcome_dep)){
  # save regression "text"
  f <- paste(outcome_dep[i], " ~ treat")
  #Run regression and save output in list
  reg[[i]] <- lm(formula = f, data = table2_full)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i]] <- cluster.vcov(reg[[i]],table2_full$uc)
  coeftest(reg[[i]],vcov_uc[[i]])
  cluster_se[[i]] <- sqrt(diag(vcov_uc[[i]])) 
} 


#reg3: all controls 
#store values 
list_control <- paste(df_intcontrols,collapse="+")
list_adjust <- paste(df_adjust,collapse = "+")
print(list_control)
print(list_adjust)
list_covar <- paste(list_control,"+",list_adjust)
show(list_covar) 
for (i in 1:length(outcome_dep)){
  # save regression "text"
  f <- paste(outcome_dep[i]," ~ treat+",list_covar) 
  #Run regression and save output in list
  reg[[i+8]] <- lm(formula = f, data = table2_full)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+8]] <- cluster.vcov(reg[[i+8]],table2_full$uc)
  coeftest(reg[[i+8]],vcov_uc[[i+8]])
  cluster_se[[i+8]] <- sqrt(diag(vcov_uc[[i+8]]))
}


#RI-p-value------------------------------------------------------------
##randomization inference p-value 
##stata: randcmd 
##R:ritest from github
install.packages("devtools")
library(devtools)
install_github("grantmcdermott/ritest")
library(ritest)
ri_regress <- list() 
remotes::install_github("grantmcdermott/ritest")
ri_regress <- lm(depressed_6m ~ treat, data=table2_full) 
ritest(ri_regress,'treat',cluster='uc', reps=1000, seed=1234)
for (i in 1:length(outcome_dep)) {
  f <- paste(outcome_dep[i]," ~ treat") 
  ri_regress[[i]] <- lm(f,data=table2_full) 
  ri_p[[i]] <- ritest(ri_regress[[i]],"treat",cluster="uc",reps=1000,seed=1234)
}
coeftest(ri_p[[1]]) 

#-----------------------------------------------------------------------------------------------------

#Table 3 
##define subsets 
table3_girl <- orig_dat[orig_dat$girl == 1,] 
table3_girl <- table3_girl[table3_girl$threegroup == 2,]
dim(table3_girl)
table3_boy <- orig_dat[orig_dat$girl == 0,]
table3_boy <- table3_boy[table3_boy$threegroup == 2,]

#eliminate NAs 
#mydata<-mydata[complete.cases(mydata),] 
table3_boy <- table3_boy[complete.cases(table3_boy),]
table3_girl <- table3_girl[complete.cases(table3_girl),] 
#subset for control
table3_ctrl_girl <- table3_girl[table3_girl$threegroup == 3,] 
table3_ctrl_boy <- table3_boy[table3_boy$threegroup == 3,]
#subset of mean by gender 
df_describe <- orig_dat[orig_dat$threegroup  == 3,] %>%
  select(depressed_6m,depindex_6m,depressed_1y,depindex_1y,depressed,depindex_7y,recover_perm,recover_never,girl) 
df_describe_girl <- df_describe[df_describe$girl == 1,]
df_describe_girl <- df_describe_girl[complete.cases(df_describe_girl),]
df_describe_boy <- df_describe[df_describe$girl == 0,]
df_describe_boy <- df_describe_boy[complete.cases(df_describe_boy),]
#dependent variable 
outcome_dep <- c("depressed_6m","depindex_6m","depressed_1y","depindex_1y","depressed","depindex_7y","recover_perm","recover_never")  


#control mean girl 
mean_table3g <- sapply(df_describe_girl,mean,na.rm=TRUE)  
mean_table3g[1] 
#control mean boy 
mean_table3b <- sapply(df_describe_boy,mean,na.rm=TRUE)  
mean_table3b[1]



#girl regression 
for (i in 1:length(outcome_dep)){
  # save regression "text"
  f <- paste(outcome_dep[i]," ~ treat +",list_covar) 
  #Run regression and save output in list
  reg[[i+16]] <- lm(formula = f, data = table3_girl)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+16]] <- cluster.vcov(reg[[i+16]],table3_girl$uc)
  coeftest(reg[[16]],vcov_uc[[16]])
  cluster_se[[i+16]] <- sqrt(diag(vcov_uc[[i+16]])) 
}

#boys regression 
for (i in 1:length(outcome_dep)){
  # save regression "text"
  f <- paste(outcome_dep[i]," ~ treat +",list_covar) 
  #Run regression and save output in list
  reg[[i+24]] <- lm(formula = f, data = table3_boy)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+24]] <- cluster.vcov(reg[[i+24]],table3_boy$uc)
  coeftest(reg[[i+24]],vcov_uc[[i+24]])
  cluster_se[[i+24]] <- sqrt(diag(vcov_uc[[i+24]])) 
}


#p-values 
##generate interaction 
#create interaction term
orig_dat$table3 <- (is.na(orig_dat$threegroup) == FALSE & (orig_dat$threegroup == 2 | orig_dat$threegroup == 3))
table(orig_dat$table3)
#Create new data set with relevant observations
dat2 <- orig_dat[orig_dat$table3 == TRUE,]
head(dat2)
print(dat2$depressed_6m)
dat2$treatment <- (dat2$threegroup == 2)  
dat2$treat_girl <- dat2$treatment*dat2$girl
summary(dat2)
summary(dat2$treat_girl)
outcome_dep <- c("depressed_6m","depindex_6m","depressed_1y","depindex_1y","depressed","depindex_7y","recover_perm","recover_never")  
attach(dat2) 
reg_p1 <- lm(depressed_6m ~ treatment + girl + treat_girl,data=dat2) 
reg_p2 <- lm(depindex_6m ~ treatment + girl + treat_girl)
reg_p3 <- lm(depressed_1y ~ treatment + girl + treat_girl)
reg_p4 <- lm(depindex_1y ~ treatment + girl + treat_girl)
reg_p5 <- lm(depressed ~ treatment + girl + treat_girl) 
reg_p6 <- lm(depindex_7y ~ treatment + girl + treat_girl)
reg_p7 <- lm(recover_perm ~ treatment + girl + treat_girl)
reg_p8 <- lm(recover_never ~ treatment + girl + treat_girl) 
summary(reg_p1) 
print(coeftest(reg_p1)[4,4]) #extract p value 


#----------------------------------------------------------------------------------
#generate tables
##Table2
#outcome variable list 
col1<-rep(c("Depressed(6m)","","Depression severity(6m)","",
            "Depressed(1y)","","Depression severity(1y)","",
            "Depressed(7y)","","Depression severity(7y)","",
            "Recovered permanently","","Never recovered",""))

#col2:control group mean 
col2 <- c(mean_table2[1],sd_table2[1],
          mean_table2[2],sd_table2[2],
          mean_table2[3],sd_table2[3],
          mean_table2[4],sd_table2[4],
          mean_table2[5],sd_table2[5],
          mean_table2[6],sd_table2[6],
          mean_table2[7],sd_table2[7],
          mean_table2[8],sd_table2[8])
col2 <- round(col2,digits=2)
col2 

#col3:pooled regression 
col3 <- c(coeftest(reg[[1]])[2,1],coeftest(reg[[1]])[2,2],
          coeftest(reg[[2]])[2,1],coeftest(reg[[2]])[2,2],
          coeftest(reg[[3]])[2,1],coeftest(reg[[3]])[2,2],
          coeftest(reg[[4]])[2,1],coeftest(reg[[4]])[2,2],
          coeftest(reg[[5]])[2,1],coeftest(reg[[5]])[2,2],
          coeftest(reg[[6]])[2,1],coeftest(reg[[6]])[2,2],
          coeftest(reg[[7]])[2,1],coeftest(reg[[7]])[2,2],
          coeftest(reg[[8]])[2,1],coeftest(reg[[8]])[2,2])
col3 <- round(col3,digits=2)
col3


#col4:pooled with adjustment 
col4 <- c(coeftest(reg[[9]])[2,1],coeftest(reg[[9]])[2,2],
          coeftest(reg[[10]])[2,1],coeftest(reg[[10]])[2,2],
          coeftest(reg[[11]])[2,1],coeftest(reg[[11]])[2,2],
          coeftest(reg[[12]])[2,1],coeftest(reg[[12]])[2,2],
          coeftest(reg[[13]])[2,1],coeftest(reg[[13]])[2,2],
          coeftest(reg[[14]])[2,1],coeftest(reg[[14]])[2,2],
          coeftest(reg[[15]])[2,1],coeftest(reg[[15]])[2,2],
          coeftest(reg[[16]])[2,1],coeftest(reg[[16]])[2,2])
col4 <- round(col4,digits=2)
col4

#col5:p-value 
col5 <- c(round(coeftest(reg[[1]])[2,4],digits = 3),"",
          round(coeftest(reg[[2]])[2,4],digits = 3),"",
          round(coeftest(reg[[3]])[2,4],digits = 3),"",
          round(coeftest(reg[[4]])[2,4],digits = 3),"",
          round(coeftest(reg[[5]])[2,4],digits = 3),"",
          round(coeftest(reg[[6]])[2,4],digits = 3),"",
          round(coeftest(reg[[7]])[2,4],digits = 3),"",
          round(coeftest(reg[[8]])[2,4],digits = 3),"") 
col5 


#generate table2 
table2 <-cbind("Dependent Variables"=col1,
               "Control Group Mean"=col2,
               "Pooled"=col3,
               "Pooled(adjust)"=col4,
               "P-value(RI)"=col5)
table2
#Save with Stargazer
stargazer(table2,
          title="Table 2 - Trajectory of Maternal  Mental Health",
          align=TRUE,rownames = FALSE,
          type="html",out="Table2.html") 

#---------------------------------------------------------------------------------------------
##Table3 
#outcome variable list 
col6 <- rep(c("Depressed(6m)","","Depression severity(6m)","",
              "Depressed(1y)","","Depression severity(1y)","",
              "Depressed(7y)","","Depression severity(7y)","",
              "Recovered permanently","","Never recovered",""))

#col7:girl control mean 
col7 <- c(mean_table3g[1],"",
          mean_table3g[2],"",
          mean_table3g[3],"",
          mean_table3g[4],"",
          mean_table3g[5],"",
          mean_table3g[6],"",
          mean_table3g[7],"",
          mean_table3g[8],"")
col7 <- round(col7,digits=2)
col7

#col8:boy control mean 
col8 <- c(mean_table3b[1],"",
          mean_table3b[2],"",
          mean_table3b[3],"",
          mean_table3b[4],"",
          mean_table3b[5],"",
          mean_table3b[6],"",
          mean_table3b[7],"",
          mean_table3b[8],"")
col8 <- round(col8,digits=2)
col8

#col9:girl regression 
col9 <- c(coeftest(reg[[17]])[2,1],coeftest(reg[[17]])[2,2],
          coeftest(reg[[18]])[2,1],coeftest(reg[[18]])[2,2],
          coeftest(reg[[19]])[2,1],coeftest(reg[[19]])[2,2],
          coeftest(reg[[20]])[2,1],coeftest(reg[[20]])[2,2],
          coeftest(reg[[21]])[2,1],coeftest(reg[[21]])[2,2],
          coeftest(reg[[22]])[2,1],coeftest(reg[[22]])[2,2],
          coeftest(reg[[23]])[2,1],coeftest(reg[[23]])[2,2],
          coeftest(reg[[24]])[2,1],coeftest(reg[[24]])[2,2])
col9 <- round(col9,digits=2)
col9

#col10:boy regression 
col10 <- c(coeftest(reg[[25]])[2,1],coeftest(reg[[25]])[2,2],
           coeftest(reg[[26]])[2,1],coeftest(reg[[26]])[2,2],
           coeftest(reg[[27]])[2,1],coeftest(reg[[27]])[2,2],
           coeftest(reg[[28]])[2,1],coeftest(reg[[28]])[2,2],
           coeftest(reg[[29]])[2,1],coeftest(reg[[29]])[2,2],
           coeftest(reg[[30]])[2,1],coeftest(reg[[30]])[2,2],
           coeftest(reg[[31]])[2,1],coeftest(reg[[31]])[2,2],
           coeftest(reg[[32]])[2,1],coeftest(reg[[32]])[2,2])
col10 <- round(col10,digits=2)
col10


#col11 equality test p-value 
col11 <- c(round(coeftest(reg_p1)[4,4],digits=2), "",
           round(coeftest(reg_p2)[4,4],digits=2), "",
           round(coeftest(reg_p3)[4,4],digits=2), "",
           round(coeftest(reg_p4)[4,4],digits=2), "",
           round(coeftest(reg_p5)[4,4],digits=2), "",
           round(coeftest(reg_p6)[4,4],digits=2), "",
           round(coeftest(reg_p7)[4,4],digits=2), "",
           round(coeftest(reg_p8)[4,4],digits=2), "")
col11

#generate Table3 
table3 <-cbind("Dependent Variables"=col6,
               "Girl Control Mean"=col7,
               "Boy control mean"=col8,
               "Girl regression"=col9,
               "Boy regression"=col10,
               "Equality p-value"=col11) 
table3

#Save with Stargazer
stargazer(table3,
          title="Table 3 - Intervention effects:by child gender",
          align=TRUE,rownames = FALSE,
          type="html",out="Table3.html")  






