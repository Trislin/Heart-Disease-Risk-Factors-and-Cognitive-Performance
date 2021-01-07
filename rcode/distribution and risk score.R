library('sas7bdat')
library('haven')
library('dplyr')
library('tidyr')
np<-read.sas7bdat('data/np_summary_through2018_16900_s.sas7bdat')
wkthru<-read.sas7bdat('data/vr_wkthru_ex09_1_1001.sas7bdat')
stroke<-read.sas7bdat('data/vr_soe_2018_a_1311.sas7bdat')
ultra<-read_sas('data/carotid1_6s_SHARE.sas7bdat')
stroke<-read_sas('data/vr_soe_2018_a_1311.sas7bdat')
dem<-read_sas('data/dem_cum_through2018_02242020.sas7bdat')
names(np)[1:2]<-c('IDTYPE','ID')
names(dem)[1:2]<-c('IDTYPE','ID')


table(wkthru$ATT6)
###sample
gen2_t<-pivot_longer(wkthru, cols = -c(IDTYPE, ID,SEX), 
                     names_to = c('.value', 'EXAMCYCLE'), 
                     names_pattern = '(.*)(\\d+)')

gen2ex6<-gen2_t%>%filter(EXAMCYCLE==6)%>%select(IDTYPE,ID,SEX,EXAMCYCLE,DATE,AGE,CALC_LDL,HDL,DBP,SBP,DMRX,CURRSMK)


###any gen2NAs removed 
gen2ex6.final.any<-gen2ex6[rowSums(is.na(gen2ex6))==0,]
5124-3402
#gen2ex6.na.any<-gen2ex6[rowSums(is.na(gen2ex6))>0,]
#gen2ex6.na.any%>%filter(!is.na(DATE))

###merge gen2 and np
np.gen2<-np%>%filter(IDTYPE==1)
np.gen2<-np.gen2[,c(1:6,12,15:17,21:52)]
gen2ex6.np<-merge(gen2ex6.final.any,np.gen2,by=c('IDTYPE','ID'))

#remove duplicates based on closest np exam
gen2ex6.np<-gen2ex6.np%>%group_by(ID)%>%arrange(ID,abs(DATE-examdate))%>%slice(1)%>%ungroup()

3402-2684
#flowchart
#test<-gen2ex6.np
#test<-test[rowSums(is.na(test[,c(18:29,33:36)]))==0,]
#2684-2537

###merging with ultrasound dataset
names(ultra)
ultra<-ultra[,c(1:6,35,37,7,8,15,17,19,20,25)]
gen2ex6.np.ultra<-merge(gen2ex6.np,ultra,by=c('IDTYPE','ID'))
2684-2603
#flowchart
#test<-merge(test,ultra,by=c('IDTYPE','ID'))
#2537-2461
#names(test)
#test<-test[rowSums(is.na(test[,c(53,55,57,58)]))==0,]
#2461-2223
#238+76


###stroke
stroke.gen2<-stroke%>%filter(IDTYPE==1&EVENT%in%c(10,11,13:17,19))
final.data<-filter(gen2ex6.np.ultra, !gen2ex6.np.ultra$ID%in%stroke.gen2$ID)
2603-2388
#flowchart
#test<-filter(test, !test$ID%in%stroke.gen2$ID)
2223-2058
###dementia
dem.gen2<-dem%>%filter(IDTYPE==1&demrv046==1)
final.data<-filter(final.data, !final.data$ID%in%dem.gen2$ID)
2200
#flow chart
#test<-filter(test, !test$ID%in%dem.gen2$ID)
#2058-1924
###Distribution
library('ggplot2')
pdf("distribution of np scores.pdf",width=10)
par(mfrow=c(3, 3))
colnames <- dimnames(np)[[2]]
for (i in c(15:17,21:26)){
  d <- density(np[,i],na.rm = TRUE)
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}
for (i in c(27:35)){
  d <- density(np[,i],na.rm = TRUE)
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}
for (i in c(36:42,46:47)){
  d <- density(np[,i],na.rm = TRUE)
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}
for (i in c(48:52)){
  d <- density(np[,i],na.rm = TRUE)
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}
dev.off()


library(tidyr)
library(dplyr)

###Calculating risk score

###step1 age
final.data$age_rfs<-NA
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(30:34)]<--1
final.data$age_rfs[final.data$SEX==2&final.data$AGE%in%c(29:34)]<--9 #the youngest female is 29 years old
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(35:39)]<-0
final.data$age_rfs[final.data$SEX==2&final.data$AGE%in%c(35:39)]<--4
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(40:44)]<-1
final.data$age_rfs[final.data$SEX==2&final.data$AGE%in%c(40:44)]<-0
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(45:49)]<-2
final.data$age_rfs[final.data$SEX==2&final.data$AGE%in%c(45:49)]<-3
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(50:54)]<-3
final.data$age_rfs[final.data$SEX==2&final.data$AGE%in%c(50:54)]<-6
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(55:59)]<-4
final.data$age_rfs[final.data$SEX==2&final.data$AGE%in%c(55:59)]<-7
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(60:64)]<-5
final.data$age_rfs[final.data$SEX==2&final.data$AGE>59]<-8
final.data$age_rfs[final.data$SEX==1&final.data$AGE%in%c(65:69)]<-6
final.data$age_rfs[final.data$SEX==1&final.data$AGE>69]<-7


###step2 Cholesterol
final.data$ldl_rfs<-NA
final.data$ldl_rfs[final.data$SEX==1&final.data$CALC_LDL<160]<--3
final.data$ldl_rfs[final.data$SEX==2&final.data$CALC_LDL<160]<--2
final.data$ldl_rfs[final.data$CALC_LDL%in%c(160:199)]<-0
final.data$ldl_rfs[final.data$CALC_LDL%in%c(200:239)]<-1
final.data$ldl_rfs[final.data$SEX==1&final.data$CALC_LDL%in%c(240:279)]<-2
final.data$ldl_rfs[final.data$SEX==2&final.data$CALC_LDL%in%c(240:279)]<-1
final.data$ldl_rfs[final.data$CALC_LDL>279]<-3

###step3 HDL-C
final.data$hdl_rfs<-NA
final.data$hdl_rfs[final.data$SEX==1&final.data$HDL<35]<-2
final.data$hdl_rfs[final.data$SEX==2&final.data$HDL<35]<-5
final.data$hdl_rfs[final.data$SEX==1&final.data$HDL%in%c(35:44)]<-1
final.data$hdl_rfs[final.data$SEX==2&final.data$HDL%in%c(35:44)]<-2
final.data$hdl_rfs[final.data$SEX==1&final.data$HDL%in%c(45:59)]<-0
final.data$hdl_rfs[final.data$SEX==2&final.data$HDL%in%c(45:49)]<-1
final.data$hdl_rfs[final.data$SEX==2&final.data$HDL%in%c(50:59)]<-0
final.data$hdl_rfs[final.data$SEX==1&final.data$HDL>59]<--2
final.data$hdl_rfs[final.data$SEX==2&final.data$HDL>59]<--3

###step4 Blood pressure
final.data$bp_rfs<-NA
final.data$bp_rfs[final.data$SBP<130&final.data$DBP<85]<-0
final.data$bp_rfs[final.data$SBP<140&final.data$DBP%in%c(85:89)]<-1
final.data$bp_rfs[final.data$SBP%in%c(130:139)&final.data$DBP<90]<-1
final.data$bp_rfs[final.data$SBP<160&final.data$DBP%in%c(90:99)]<-2
final.data$bp_rfs[final.data$SBP%in%c(140:159)&final.data$DBP<100]<-2
final.data$bp_rfs[final.data$SBP>159|final.data$DBP>99]<-3

###step5 diabetes
final.data$db_rfs<-NA
final.data$db_rfs[final.data$DMRX==0]<-0
final.data$db_rfs[final.data$SEX==1&final.data$DMRX==1]<-2
final.data$db_rfs[final.data$SEX==2&final.data$DMRX==1]<-4

###step6 smoker
final.data$smk_rfs<-NA
final.data$smk_rfs[final.data$CURRSMK==0]<-0
final.data$smk_rfs[final.data$CURRSMK==1]<-2

###step7 add up points

final.data$total_rfs<-rowSums(final.data[,c(66:71)])
names(final.data)

###remove NAs in variables that will be used in the model
names(final.data)
final.data<-final.data[rowSums(is.na(final.data[,c(18:29,33:36,53,55,57,58)]))==0,]
1924
#final.data_s<-final.data[rowSums(is.na(final.data[,c(18:29,31:36,49,50,53,55,57,58)]))==0,]
152

#q1<-gen2ex6.final.any%>%filter(is.na(age_rfs))
#what to code for people who's age is over 75? *there is also one woman's age is 29
#sensitivity analysis (treat everyone over 70 the same) (give them highest score )


###all sample
###restrict to sample after middle-age 45/50

#summary stats for total score
mean(final.data$AGE)
sd(final.data$AGE)
