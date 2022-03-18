#prepare for table4 
rm(list = ls())
pacman::p_load(dplyr, data.table, dtplyr, 
               lmtest, sandwich, multiwayvcov,
               gmodels, stargazer)
setwd("D:/LMU/Wise21/seminar/paper")
orig_dat <- read.csv("DataSet.csv")
names(orig_dat) 
head(orig_dat)
str(orig_dat, list.len=ncol(orig_dat)) 
stargazer(orig_dat,type="text") 

#----------------------------------------------------------------------------------------------------------------------
#Table 4
##Defining the sample (using threegroup variable)
summary(orig_dat$threegroup) 
table(orig_dat$threegroup)
#criterion:not NA,treatment + control group 
orig_dat$table4 <- (is.na(orig_dat$threegroup) == FALSE) & (orig_dat$threegroup==2 | orig_dat$threegroup==3)
table(orig_dat$table4)
#Create new data set with relevant observations
dat1 <- orig_dat[orig_dat$table4 == TRUE,]

#list variables 
##treatment group
dat1$treatment <- dat1$threegroup == 2
table(dat1$treatment)
##control variables
df_intcontrols <- c("monthint", "monthintsq", "doi0", "doi0Xtreat", "intervr1", "intervr2", "intervr3", "intervr4", "intervr5", "intervr6", "intervr7", "intervr8", "intervr9")
df_adjust <- c("age_baseline","age_baseline_sq","employed_mo_baseline","mo_emp",  
               "grandmother_baseline","MIL","wealth_baseline","edu_lvl_mo_1","edu_lvl_mo_2",
               "edu_lvl_mo_3","edu_mo_baseline","edu_fa_baseline","kids_no","first_child","hamd_baseline","mspss_baseline","doi0")
list_control <- paste(df_intcontrols,collapse="+")
print(list_control)
list_adjust <- paste(df_adjust,collapse = "+")
print(list_adjust)
list_covar <- paste(list_control,"+",list_adjust)
print(list_covar) 

#dependent variables
y_list <- c("motherfinancial", "parentmoney", "parenttime", "parentstyle", "fertility_vars")

#Create empty matrices & lists 
reg <- list()
vcov_uc <- list()
cluster_se <- list()

#Loop
##Loop1:pooled 
for (i in 1:length(y_list) ){
  # save regression "text"
  f <- paste(y_list[i], " ~ Group + monthint + monthintsq+ doi0 + doi0Xtreat + intervr1 + intervr2 +  intervr3 +  intervr4 +  intervr5 +  intervr6 + intervr7 + intervr8 + intervr9",sep="")
  #Run regression and save output in list
  reg[[i]] <- lm(formula = f, data = dat1)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i]] <- cluster.vcov(reg[[i]],dat1$uc)
  coeftest(reg[[i]],vcov_uc[[i]])
  cluster_se[[i]] <- sqrt(diag(vcov_uc[[i]]))
}


##Loop2:pooled+adjust 
for (i in 1:length(y_list) ){
  f <- paste(y_list[i]," ~Group+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0",sep="") 
  #Run regression and save output in list
  reg[[i+5]] <- lm(formula = f, data = dat1)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+5]] <- cluster.vcov(reg[[i+5]],dat1$uc)
  coeftest(reg[[i+5]],vcov_uc[[i+5]])
  cluster_se[[i+5]] <- sqrt(diag(vcov_uc[[i+5]])) 
}


##Loop3:mother of girls 
#define subset 
dat1_girl <- dat1[dat1$girl == 1,]
dat1_boy <- dat1[dat1$girl == 0,]
for (i in 1:length(y_list) ){
  f <- paste(y_list[i]," ~ Group+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0",sep="") 
  #Run regression and save output in list
  reg[[i+10]] <- lm(formula = f, data = dat1_girl)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+10]] <- cluster.vcov(reg[[i+10]],dat1_girl$uc)
  coeftest(reg[[i+10]],vcov_uc[[i+10]])
  cluster_se[[i+10]] <- sqrt(diag(vcov_uc[[i+10]])) 
}


##Loop4:mother of boys 
for (i in 1:length(y_list) ){
  f <- paste(y_list[i]," ~ Group+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0",sep="") 
  #Run regression and save output in list
  reg[[i+15]] <- lm(formula = f, data = dat1_boy)
  #Clustered standard errors + saving them for the table
  vcov_uc[[i+15]] <- cluster.vcov(reg[[i+15]],dat1_boy$uc)
  coeftest(reg[[i+15]],vcov_uc[[i+15]])
  cluster_se[[i+15]] <- sqrt(diag(vcov_uc[[i+15]])) 
}


#outcome variable list 
col1 <- c("Mother's financial empowerment","","Parental investment (monetary)","",
            "Parental investment(time-intensive)","","Parenting Style","","Fertility trajectory","")

#col2:pooled regressions
col2 <- c(coeftest(reg[[1]])[2,1],coeftest(reg[[1]])[2,2],
          coeftest(reg[[2]])[2,1],coeftest(reg[[2]])[2,2],
          coeftest(reg[[3]])[2,1],coeftest(reg[[3]])[2,2],
          coeftest(reg[[4]])[2,1],coeftest(reg[[4]])[2,2],
          coeftest(reg[[5]])[2,1],coeftest(reg[[5]])[2,2])
col2 <- round(col2,digits=2)
col2

#col3:pooled + adjust 
col3 <- c(coeftest(reg[[6]])[2,1],coeftest(reg[[6]])[2,2],
          coeftest(reg[[7]])[2,1],coeftest(reg[[7]])[2,2],
          coeftest(reg[[8]])[2,1],coeftest(reg[[8]])[2,2],
          coeftest(reg[[9]])[2,1],coeftest(reg[[9]])[2,2],
          coeftest(reg[[10]])[2,1],coeftest(reg[[10]])[2,2])
col3 <- round(col3,digits=2)
col3

#col4: P-values
#-> row 3, column 4, add empty elements
#FWER p value 
##p.adjust(p, method = p.adjust.methods, n = length(p))
##c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")
col4 <- c(round(p.adjust(coeftest(reg[[6]])[2,4],method = "bonferroni"),digits=3), "",
          round(p.adjust(coeftest(reg[[7]])[2,4],method = "bonferroni"),digits=3), "",
          round(p.adjust(coeftest(reg[[8]])[2,4],method = "bonferroni"),digits=3), "",
          round(p.adjust(coeftest(reg[[9]])[2,4],method = "bonferroni"),digits=3), "",
          round(p.adjust(coeftest(reg[[10]])[2,4],method = "bonferroni"),digits=3), "")
col4
#for (i in length(1:10)){
#  p[[i]] <- p.adjust(as.numeric(coeftest(reg[[i]])[2,4]),method="bonferroni") 
#}
#p <- p.adjust(coeftest(reg[[6]])[2,4],method = "bonferroni")
#print(p)



#col5:mothers of girls 
col5 <- c(coeftest(reg[[11]])[2,1],coeftest(reg[[11]])[2,2],
          coeftest(reg[[12]])[2,1],coeftest(reg[[12]])[2,2],
          coeftest(reg[[13]])[2,1],coeftest(reg[[13]])[2,2],
          coeftest(reg[[14]])[2,1],coeftest(reg[[14]])[2,2],
          coeftest(reg[[15]])[2,1],coeftest(reg[[15]])[2,2])
col5 <- round(col5,digits=2)
col5

#col6:mothers of boys
col6 <- c(coeftest(reg[[16]])[2,1],coeftest(reg[[16]])[2,2],
          coeftest(reg[[17]])[2,1],coeftest(reg[[17]])[2,2],
          coeftest(reg[[18]])[2,1],coeftest(reg[[18]])[2,2],
          coeftest(reg[[19]])[2,1],coeftest(reg[[19]])[2,2],
          coeftest(reg[[20]])[2,1],coeftest(reg[[20]])[2,2])
col6 <- round(col6,digits=2)
col6


#col7:chow test: test of equality 
##chow.test(y1,x1,y2,x2,x=NULL) 
dat <- orig_dat %>%
  select(threegroup,monthint,monthintsq,doi0,doi0Xtreat,intervr1,intervr2,
         intervr3,intervr4,intervr5,intervr6,intervr7,intervr8,intervr9,
         motherfinancial,parentmoney,parenttime,parentstyle,fertility_vars,uc,Group,
         depindex_7y,motherfinancial_7y,parentmoney,
         parenttime,parentstyle,no_kids_postt,girl,monthint,monthintsq,doi0,doi0Xtreat,intervr1,intervr2,intervr3,intervr4,intervr5,intervr6,intervr7,intervr8,intervr9,age_baseline,age_baseline_sq,employed_mo_baseline,mo_emp,grandmother_baseline,MIL,wealth_baseline,edu_lvl_mo_1,edu_lvl_mo_2,edu_lvl_mo_3,edu_mo_baseline,edu_fa_baseline,kids_no,first_child,hamd_baseline,mspss_baseline,doi0)
dat$sample_table6 <- (is.na(dat$threegroup) == FALSE & (dat$threegroup == 2 | dat$threegroup == 3))
table(dat$sample_table6)
#Create new data set with relevant observations
dat2 <- dat[dat$sample_table6 == TRUE,]
head(dat2) 
dat2$treatment <- (dat2$threegroup == 2)  
dat2$treat_girl <- dat2$treatment*dat2$girl
summary(dat2)
summary(dat2$treat_girl)
y_list <- c("motherfinancial", "parentmoney", "parenttime", "parentstyle", "fertility_vars")
attach(dat2) 
reg_p1 <- lm(motherfinancial ~ treatment+girl+treat_girl+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0) 
reg_p2 <- lm(parentmoney ~ treatment+girl+treat_girl+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0)
reg_p3 <- lm(parenttime ~ treatment+girl+treat_girl+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0)
reg_p4 <- lm(parentstyle ~ treatment+girl+treat_girl+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0)
reg_p5 <- lm(fertility_vars ~ treatment+girl+treat_girl+monthint+monthintsq+doi0+doi0Xtreat+intervr1+intervr2+intervr3+intervr4+intervr5+intervr6+intervr7+intervr8+intervr9+age_baseline+age_baseline_sq+employed_mo_baseline+mo_emp+grandmother_baseline+MIL+wealth_baseline+edu_lvl_mo_1+edu_lvl_mo_2+edu_lvl_mo_3+edu_mo_baseline+edu_fa_baseline+kids_no+first_child+hamd_baseline+mspss_baseline+doi0) 
summary(reg_p1) 
print(coeftest(reg_p1)) 

col7 <- c(round(coeftest(reg_p1)[4,4],digits=2), "",
          round(coeftest(reg_p2)[4,4],digits=2), "",
          round(coeftest(reg_p3)[4,4],digits=2), "",
          round(coeftest(reg_p4)[4,4],digits=2), "",
          round(coeftest(reg_p5)[4,4],digits=2), "")
col7


#Create Table4 
table4 <-cbind("Dependent variables"=col1,
               "Pooled"=col2,
               "Pooled(adjust)"=col3,
               "P-value"=col4,
               "Girls"=col5,
               "Boys"=col6,
               "Equality p-value"=col7)
table4


#Save with Stargazer
stargazer(table4,
          title="Table4 - Intervention effects on mother's decision-making",
          align=TRUE,
          type="html",out="Table4.html") 








