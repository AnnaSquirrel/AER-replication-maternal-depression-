#prepare for table6 
rm(list = ls())
pacman::p_load(dplyr, data.table, dtplyr, 
               lmtest, sandwich, multiwayvcov,
               gmodels, stargazer)
setwd("D:/LMU/Wise21/seminar/paper")
orig_dat <- read.csv("DataSet.csv")

#criterion:not NA,treatment + control group 
orig_dat$table6 <- (is.na(orig_dat$threegroup) == FALSE) & (orig_dat$threegroup==2 | orig_dat$threegroup==3)
table(orig_dat$table6)
#Create new data set with relevant observations
dat1 <- orig_dat[orig_dat$table6 == TRUE,]

#list variables 
##control variables
df_intcontrols <- c("monthint", "monthintsq", "doi0", "doi0Xtreat", "intervr1", "intervr2", "intervr3", "intervr4", "intervr5", "intervr6", "intervr7", "intervr8", "intervr9")
df_adjust <- c("age_baseline","age_baseline_sq","employed_mo_baseline","mo_emp",
               "grandmother_baseline", "MIL","wealth_baseline","edu_lvl_mo_1",
               "edu_lvl_mo_2","edu_lvl_mo_3", "edu_mo_baseline", "edu_fa_baseline",
               "kids_no","first_child","hamd_baseline","mspss_baseline","doi0")
list_control <- paste(df_intcontrols,collapse="+")
print(list_control)
list_adjust <- paste(df_adjust,collapse = "+")
print(list_adjust)
list_covar <- paste(list_control,"+",list_adjust)
print(list_covar)  

dat1_girl <- dat1[dat1$girl == 1,]
dat1_boy <- dat1[dat1$girl == 0,]


#-------------------------------------------------------------------------------------------
#Table 6 
y_list1 <- c("healthindex","cognindex","emoindex","childmort")
#Loop
reg <- list()
vcov_uc <- list()
cluster_se <- list()
##Loop1:pooled 
for (i in 1:length(y_list1) ){
  # save regression "text"
  f <- paste(y_list1[i],"~ Group +",list_control) 
  #Run regression and save output in list
  reg[[i]] <- lm(formula = f, data = dat1)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i]] <- cluster.vcov(reg[[i]],dat1$uc)
  coeftest(reg[[i]],vcov_uc[[i]])
  cluster_se[[i]] <- sqrt(diag(vcov_uc[[i]]))
}


##Loop2:pooled+adjust 
for (i in 1:length(y_list1) ){
  f <- paste(y_list1[i],"~ Group +",list_covar) 
  #Run regression and save output in list
  reg[[i+4]] <- lm(formula = f, data = dat1)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+4]] <- cluster.vcov(reg[[i+4]],dat1$uc)
  coeftest(reg[[i+4]],vcov_uc[[i+4]])
  cluster_se[[i+4]] <- sqrt(diag(vcov_uc[[i+4]])) 
}


##Loop3:mother of girls 
#define subset 
for (i in 1:length(y_list1) ){
  f <- paste(y_list1[i],"~Group+",list_covar) 
  #Run regression and save output in list
  reg[[i+8]] <- lm(formula = f, data = dat1_girl)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+8]] <- cluster.vcov(reg[[i+8]],dat1_girl$uc)
  coeftest(reg[[i+8]],vcov_uc[[i+8]])
  cluster_se[[i+8]] <- sqrt(diag(vcov_uc[[i+8]])) 
}


##Loop4:mother of boys 
for (i in 1:length(y_list1) ){
  f <- paste(y_list1[i],"~Group+",list_covar) 
  #Run regression and save output in list
  reg[[i+12]] <- lm(formula = f, data = dat1_boy)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+12]] <- cluster.vcov(reg[[i+12]],dat1_boy$uc)
  coeftest(reg[[i+12]],vcov_uc[[i+12]])
  cluster_se[[i+12]] <- sqrt(diag(vcov_uc[[i+12]])) 
}

#outcome variable list 
col1<-rep(c("Physical development","","Cognitive development","",
            "Socio-emotional development","","Sibling survival index",""))

#col2:pooled regressions
col2 <- c(coeftest(reg[[1]])[2,1],coeftest(reg[[1]])[2,2],
          coeftest(reg[[2]])[2,1],coeftest(reg[[2]])[2,2],
          coeftest(reg[[3]])[2,1],coeftest(reg[[3]])[2,2],
          coeftest(reg[[4]])[2,1],coeftest(reg[[4]])[2,2])
col2 <- round(col2,digits=2)
col2

#col3:pooled + adjust 
col3 <- c(coeftest(reg[[5]])[2,1],coeftest(reg[[5]])[2,2],
          coeftest(reg[[6]])[2,1],coeftest(reg[[6]])[2,2],
          coeftest(reg[[7]])[2,1],coeftest(reg[[7]])[2,2],
          coeftest(reg[[8]])[2,1],coeftest(reg[[8]])[2,2])
col3 <- round(col3,digits=2)
col3


#col5:mothers of girls 
col5 <- c(coeftest(reg[[9]])[2,1],coeftest(reg[[9]])[2,2],
          coeftest(reg[[10]])[2,1],coeftest(reg[[10]])[2,2],
          coeftest(reg[[11]])[2,1],coeftest(reg[[11]])[2,2],
          coeftest(reg[[12]])[2,1],coeftest(reg[[12]])[2,2])
col5 <- round(col5,digits=2)
col5

#col6:mothers of boys
col6 <- c(coeftest(reg[[13]])[2,1],coeftest(reg[[13]])[2,2],
          coeftest(reg[[14]])[2,1],coeftest(reg[[14]])[2,2],
          coeftest(reg[[15]])[2,1],coeftest(reg[[15]])[2,2],
          coeftest(reg[[16]])[2,1],coeftest(reg[[16]])[2,2])
col6 <- round(col6,digits=2)
col6

#col4: P-values
# FWER 
col4_orig <- c(round(p.adjust(coeftest(reg[[1]])[3,4],method = "BH"),digits=3), "",
          round(p.adjust(coeftest(reg[[2]])[3,4],method = "BH"),digits=3), "",
          round(p.adjust(coeftest(reg[[3]])[3,4],method = "BH"),digits=3), "",
          round(p.adjust(coeftest(reg[[4]])[3,4],method = "BH"),digits=3), "")
col4_orig 


install.packages("ritest")
library(ritest)
#ritest(estimation, 'treat', strata=, cluster=, reps=1000, seed=1234)



#col7:test of equality 
#create interaction term
dat1$treat_girl <- dat1$Group*dat1$girl
summary(dat1$treat_girl)
# Mothers of boys
for (i in 1:length(y_list1) ){
  # Create text for regression
  f <- paste(y_list1[i], " ~ treat + treat_girl + girl",sep="")
  #Run regression and save output in list, start with slot 19
  reg[[i+16]] <- lm(formula = f, data = dat1)
}
col7 <- c(round(coeftest(reg[[17]])[3,4],digits=3), "",
          round(coeftest(reg[[18]])[3,4],digits=3), "",
          round(coeftest(reg[[19]])[3,4],digits=3), "",
          round(coeftest(reg[[20]])[3,4],digits=3), "")
col7 <- round(col7,digits=2)
col7

#Create Table6 
table6 <-cbind("Dependent Variables"=col1,
               "Pooled"=col2,
               "Pooled(adjusted)"=col3,
               "FWER p-value"=col4,
               "Girls"=col5,
               "Boys"=col6,
               "Equality p-value"=col7)
table6


#Save with Stargazer
stargazer(table6,
          title="Table6 - Intervention Effects on Child Outcomes at Age Seven",
          align=TRUE,
          type="html",out="Table6.html")  


