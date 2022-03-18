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
dat <- orig_dat %>%
  select(threegroup,monthint,monthintsq,doi0,doi0Xtreat,intervr1,intervr2,
         intervr3,intervr4,intervr5,intervr6,intervr7,intervr8,intervr9,
         motherfinancial,parentmoney,parenttime,parentstyle,fertility_vars,uc,Group,
         depindex_7y,motherfinancial_7y,parentmoney,
         parenttime,parentstyle,no_kids_postt,girl) 
#----------------------------------------------------------------------------------------------------------------------
#Table 5 
##classify sample
summary(dat$threegroup) 
table(dat$threegroup) 
#Define dummy variable for the sample criterion: Non-Treated Depressed vs. non-depressed
dat$sample_table5 <- (is.na(dat$threegroup) == FALSE & (dat$threegroup==1 |  dat$threegroup==3))
table(dat$sample_table5)
#Create new data set with relevant observations
dat2 <- dat[dat$sample_table5 == TRUE,] 
#Dummy for depressed at baseline
dat2$depression <- (dat2$threegroup==3)  
df_outcomes2 <- c("depindex_7y","motherfinancial_7y","parentmoney","parenttime","parentstyle", "no_kids_postt")

##Regressions
#List of dependent variables
y_list2 <- c("depindex_7y", "motherfinancial_7y", "parentmoney", "parenttime", "parentstyle", "no_kids_postt")
#Create list for loops
reg2 <- list()

#Loop 1: All genders pooled
for (i in 1:length(y_list2) ){
  # Create text for regression
  f <- paste(y_list2[i], " ~ depression",sep="")
  #Run regression and save output in list
  reg2[[i]] <- lm(formula = f, data = dat2)
}

#Create datasets by gender
dat2_girl <- dat2[dat2$girl == 1,]
dat2_boy <- dat2[dat2$girl == 0,]

# Loop 2: Mothers of girls 
for (i in 1:length(y_list2) ){
  # Create text for regression
  f <- paste(y_list2[i], " ~ depression",sep="")
  #Run regression and save output in list, start with slot 7
  reg2[[i+6]] <- lm(formula = f, data = dat2_girl)
}

# Loop 3: Mothers of boys
for (i in 1:length(y_list2) ){
  # Create text for regression
  f <- paste(y_list2[i], " ~ depression",sep="")
  #Run regression and save output in list, start with slot 13
  reg2[[i+12]] <- lm(formula = f, data = dat2_boy)
}


# Loop 4: Interaction framework
#create interaction term
dat2$depr_girl <- dat2$depression*dat2$girl
summary(dat2$depr_girl)

# Mothers of boys
for (i in 1:length(y_list2) ){
  # Create text for regression
  f <- paste(y_list2[i], " ~ depression + depr_girl + girl",sep="")
  #Run regression and save output in list, start with slot 19
  reg2[[i+18]] <- lm(formula = f, data = dat2)
}

#check coeftest result 
coeftest(reg2[[1]]) #extract [2,1](estimate),[2,3](std error)


#outcome variable list 
col1<-rep(c("Depression Index (7y)","","Mother's financial empowerment (7y)","","Parental investment (monetary)",
            "","Parental investment(time-intensive)","","Parenting Style","","Number of kids born past 7 years",""))

#Extract values from pooled regressions
col2 <- c(coeftest(reg2[[1]])[2,1],coeftest(reg2[[1]])[2,2],
          coeftest(reg2[[2]])[2,1],coeftest(reg2[[2]])[2,2],
          coeftest(reg2[[3]])[2,1],coeftest(reg2[[3]])[2,2],
          coeftest(reg2[[4]])[2,1],coeftest(reg2[[4]])[2,2],
          coeftest(reg2[[5]])[2,1],coeftest(reg2[[5]])[2,2],
          coeftest(reg2[[6]])[2,1],coeftest(reg2[[6]])[2,2])
col2 <- round(col2,digits=2)
col2

col3 <- c(coeftest(reg2[[7]])[2,1],coeftest(reg2[[7]])[2,2],
          coeftest(reg2[[8]])[2,1],coeftest(reg2[[8]])[2,2],
          coeftest(reg2[[9]])[2,1],coeftest(reg2[[9]])[2,2],
          coeftest(reg2[[10]])[2,1],coeftest(reg2[[10]])[2,2],
          coeftest(reg2[[11]])[2,1],coeftest(reg2[[11]])[2,2],
          coeftest(reg2[[12]])[2,1],coeftest(reg2[[12]])[2,2])
col3 <- round(col3,digits=2)
col3

col4 <- c(coeftest(reg2[[13]])[2,1],coeftest(reg2[[13]])[2,2],
          coeftest(reg2[[14]])[2,1],coeftest(reg2[[14]])[2,2],
          coeftest(reg2[[15]])[2,1],coeftest(reg2[[15]])[2,2],
          coeftest(reg2[[16]])[2,1],coeftest(reg2[[16]])[2,2],
          coeftest(reg2[[17]])[2,1],coeftest(reg2[[17]])[2,2],
          coeftest(reg2[[18]])[2,1],coeftest(reg2[[18]])[2,2])
col4 <- round(col4,digits=2)
col4

#P-values
coeftest(reg2[[19]]) #-> For the p-Value, we need row 3, column 4. We add empty elements
col5 <- c(round(coeftest(reg2[[19]])[3,4],digits=2), "",
          round(coeftest(reg2[[20]])[3,4],digits=2), "",
          round(coeftest(reg2[[21]])[3,4],digits=2), "",
          round(coeftest(reg2[[22]])[3,4],digits=2), "",
          round(coeftest(reg2[[23]])[3,4],digits=2), "",
          round(coeftest(reg2[[24]])[3,4],digits=2), "")

#Create Table similar to table 5
table5 <-cbind(Variable=col1,
               Pooled=col2,
               Girls=col3,
               Boys=col4,
               p_value=col5)
table5

#Save with Stargazer
stargazer(table5,
          title="Table 5 - Depression Gap in Mother's Outcomes",
          align=TRUE,
          type="html",out="Table5.html")