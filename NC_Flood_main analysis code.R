
######################################################################################
# Association of flooding exposure with cause-specific mortality in North Carolina, US
# R code for the main model
# Jie Ban, Caroline Sutton, Yiqun Ma, Chengyi Lin, Kai Chen
# Yale University, New Haven, CT
# October 26, 2023
######################################################################################

library(dplyr)
library(splines)
library(metafor)

###########First stage analysis############################################

mydata<-select(dataset,date,countycode,death,dow,time,flood,temperature,precipitation)

model<-glm(death~as.factor(flood)+ns(temperature,df_temp)+ns(precipitation,df_prcp)+ns(time,5*df_time)+dow,
           data=mydata,family = quasipoisson)
  

###########Second stage analysis############################################
  
county_est<- data.frame(mcode=countycode,B_death=county_coef,SE_death=cpunty_se)  

yi<-county_est$B_death  
er<-county_est$SE_death
vi<-er*er
meta<-rma(yi,vi,data=county_est,method="REML")
  
rr<-exp(meta$beta*1)
lrr<-exp((meta$beta-1.96*meta$se)*1)
urr<-exp((meta$beta+1.96*meta$se)*1)
