##Table3 
rm(list = ls())
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
### Table3    
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

reg <- list()
vcov_uc <- list()
cluster_se <- list()

#-------------------------------------------------------------------------------------------------
#regression 
#Table 3 
##define subsets 
table3_girl <- orig_dat[orig_dat$girl == 1,] 
dim(table3_girl) 
table3_boy <- orig_dat[orig_dat$girl == 0,]
dim(table3_boy) 

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
#covariables 
list_control <- paste(df_intcontrols,collapse="+")
list_adjust <- paste(df_adjust,collapse = "+")
print(list_control)
print(list_adjust)
list_covar <- paste(list_control,"+",list_adjust)
show(list_covar) 


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
  reg[[i]] <- lm(formula = f, data = table3_girl)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i]] <- cluster.vcov(reg[[i]],table3_girl$uc)
  coeftest(reg[[i]],vcov_uc[[i]])
  cluster_se[[i]] <- sqrt(diag(vcov_uc[[i]])) 
}
summary(reg[[1]]) 
coeftest(reg[[1]])[2,1]

#boys regression 
for (i in 1:length(outcome_dep)){
  # save regression "text"
  f <- paste(outcome_dep[i]," ~ treat +",list_covar) 
  #Run regression and save output in list
  reg[[i+8]] <- lm(formula = f, data = table3_boy)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+8]] <- cluster.vcov(reg[[i+8]],table3_boy$uc)
  coeftest(reg[[i+8]],vcov_uc[[i+8]])
  cluster_se[[i+8]] <- sqrt(diag(vcov_uc[[i+8]])) 
}
coeftest(reg[[9]])[2,1]


#p-values equality test 
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
reg_p <- list()
for (i in 1:length(outcome_dep)) {
  f <- paste(outcome_dep[i],"~ treatment + girl + treat_girl")
  reg_p[[i]] <- lm(f,data=dat2)
}
coeftest(reg_p[[1]])[3,4] 

#reg_p1 <- lm(depressed_6m ~ treatment + girl + treat_girl,data=dat2) 
#reg_p2 <- lm(depindex_6m ~ treatment + girl + treat_girl)
#reg_p3 <- lm(depressed_1y ~ treatment + girl + treat_girl)
#reg_p4 <- lm(depindex_1y ~ treatment + girl + treat_girl)
#reg_p5 <- lm(depressed ~ treatment + girl + treat_girl) 
#reg_p6 <- lm(depindex_7y ~ treatment + girl + treat_girl)
#reg_p7 <- lm(recover_perm ~ treatment + girl + treat_girl)
#reg_p8 <- lm(recover_never ~ treatment + girl + treat_girl) 

#-----------------------------------------------------------------------------------
##generate table with stargazer 
#outcome variable list 
col1 <- c("Depressed(6m)","","Depression severity(6m)","",
          "Depressed(1y)","","Depression severity(1y)","",
          "Depressed(7y)","","Depression severity(7y)","",
          "Recovered permanently","","Never recovered","")

#col2:girl control mean 
col2 <- c(round(mean_table3g[1],digits = 2),"",
          round(mean_table3g[2],digits = 2),"",
          round(mean_table3g[3],digits = 2),"",
          round(mean_table3g[4],digits = 2),"",
          round(mean_table3g[5],digits = 2),"",
          round(mean_table3g[6],digits = 2),"",
          round(mean_table3g[7],digits = 2),"",
          round(mean_table3g[8],digits = 2),"")
col2 <- round(col2,digits=2)
col2

#col3:boy control mean 
col3 <- c(round(mean_table3b[1],digits = 2),"",
          round(mean_table3b[2],digits = 2),"",
          round(mean_table3b[3],digits = 2),"",
          round(mean_table3b[4],digits = 2),"",
          round(mean_table3b[5],digits = 2),"",
          round(mean_table3b[6],digits = 2),"",
          round(mean_table3b[7],digits = 2),"",
          round(mean_table3b[8],digits = 2),"")
col3 <- round(col3,digits=2)
col3

#col4:girl regression 
col4 <- c(coeftest(reg[[1]])[2,1],coeftest(reg[[1]])[2,2],
          coeftest(reg[[2]])[2,1],coeftest(reg[[2]])[2,2],
          coeftest(reg[[3]])[2,1],coeftest(reg[[3]])[2,2],
          coeftest(reg[[4]])[2,1],coeftest(reg[[4]])[2,2],
          coeftest(reg[[5]])[2,1],coeftest(reg[[5]])[2,2],
          coeftest(reg[[6]])[2,1],coeftest(reg[[6]])[2,2],
          coeftest(reg[[7]])[2,1],coeftest(reg[[7]])[2,2],
          coeftest(reg[[8]])[2,1],coeftest(reg[[8]])[2,2])
col4 <- round(col4,digits=2)
col4

#col5:boy regression 
col5 <- c(coeftest(reg[[9]])[2,1],coeftest(reg[[9]])[2,2],
          coeftest(reg[[10]])[2,1],coeftest(reg[[10]])[2,2],
          coeftest(reg[[11]])[2,1],coeftest(reg[[11]])[2,2],
          coeftest(reg[[12]])[2,1],coeftest(reg[[12]])[2,2],
          coeftest(reg[[13]])[2,1],coeftest(reg[[13]])[2,2],
          coeftest(reg[[14]])[2,1],coeftest(reg[[14]])[2,2],
          coeftest(reg[[15]])[2,1],coeftest(reg[[15]])[2,2],
          coeftest(reg[[16]])[2,1],coeftest(reg[[16]])[2,2])
col5 <- round(col5,digits=2)
col5

#col6 equality test p-value 
col6 <- c(round(coeftest(reg[[1]])[4,4],digits=2), "",
           round(coeftest(reg[[2]])[4,4],digits=2), "",
           round(coeftest(reg[[3]])[4,4],digits=2), "",
           round(coeftest(reg[[4]])[4,4],digits=2), "",
           round(coeftest(reg[[5]])[4,4],digits=2), "",
           round(coeftest(reg[[6]])[4,4],digits=2), "",
           round(coeftest(reg[[7]])[4,4],digits=2), "",
           round(coeftest(reg[[8]])[4,4],digits=2), "")
col6

#generate Table3 
table3 <-cbind("Dependent Variables"=col1,
               "Girl Control Mean"=col2,
               "Boy control mean"=col3,
               "Girl regression"=col4,
               "Boy regression"=col5,
               "Equality p-value"=col6) 
table3

#Save with Stargazer
stargazer(table3,
          title="Table 3 - Intervention effects:by child gender",
          align=TRUE,rownames = FALSE,
          type="html",out="Table3.html")  

