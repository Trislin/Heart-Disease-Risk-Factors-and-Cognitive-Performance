
install.packages('multilevel')
library(multilevel)
install.packages('mediation')
library(mediation)

final.data$ICD_MEMX_t<-log(final.data$ICD_MEMX)
final.data$ICB_MEMX_t<-log(final.data$ICB_MEMX)
final.data$memory<-rowSums(final.data[,c(18:29)])
final.data$trailsAB<-final.data$trailsB-final.data$trailsA
final.data$executive_function<-rowSums(final.data[,c(35,76)]) #sim & transformed trailsA,B
final.data$GCP<-rowSums(final.data[,c(75,77,36)]) #memory+executive function+hvot
names(final.data)

#check distribution of each variable used in the model
par(mfrow=c(2, 3))
colnames <- dimnames(final.data)[[2]]
for (i in c(72,73,53,55,57,58)){
  d <- density(final.data[,i],na.rm = TRUE)
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}



###NP composite score- memory


#1[total effect]chd risk factor->memory 
TEM<-lm(memory~total_rfs+as.factor(Educg),data=final.data)
summary(TEM)
#2[path A]chd risk factor->atherosclerosis
PAM<-lm(ICD_MEMX_t~total_rfs+as.factor(Educg),data = final.data)
summary(PAM)
#3[path B]atherosclerosis->memory
PBM<-lm(memory~ICB_MEMX_t+total_rfs+as.factor(Educg),data = final.data)
summary(PBM)

model.II<-mediate(PAM,PBM,treat = "total_rfs", mediator = "ICD_MEMX_t", boot = TRUE, sims=1000)
summary(model.II)

orr.memory<-(-0.7126)
orrci.memory<-c((orr.memory+1.96*0.1078), (orr.memory-1.96*0.1078))

icd.chd.beta<-0.030201
icd.chd.betaci<-c(icd.chd.beta-1.96*0.002749, icd.chd.beta+1.96*0.002749)

icb.chd.beta<-0.038303
icb.chd.betaci<-c(icb.chd.beta-1.96*0.002509, icb.chd.beta+1.96*0.002509)

memory.icd.beta<-(-5.6451)
memory.icd.betaci<-c(memory.icd.beta-1.96*0.8856, memory.icd.beta+1.96*0.8856)

memory.icb.beta<-(-6.4797)
memory.icb.betaci<-c(memory.icb.beta-1.96*0.9695,memory.icb.beta+1.96*0.9695)


###NP composite score- executive function
#[total effect]chd risk factor->executive function
TEE<-lm(executive_function~total_rfs+as.factor(Educg),data=final.data)
summary(TEE)
#[path A]chd risk factor->atherosclerosis
PAE<-lm(ICB_MEMX_t~total_rfs+as.factor(Educg),data=final.data)
summary(PAE)
#[path B]atherosclerosis->executive function
PBE<-lm(executive_function~ICD_MEMX_t+total_rfs+as.factor(Educg),data=final.data)
summary(PBE)

model.IV<-mediate(PAE,PBE,treat = "total_rfs", mediator = "ICD_MEMX_t", boot = TRUE, sims=1000)
summary(model.IV)

orr.ef<-(-0.06414)
orrci.ef<-c((orr.ef+1.96*0.01990), (orr.ef-1.96*0.01990))

executive.icb.beta<-(-0.67819)
executive.icb.betaci<-c(executive.icb.beta-1.96*0.18048,executive.icb.beta+1.96*0.18048)

executive.icd.beta<-(-0.48217)
executive.icd.betaci<-c(executive.icb.beta-1.96*0.16493,executive.icb.beta+1.96*0.16493)


###NP composite score- GCP
#[total effect]chd risk factor->executive function
TEG<-lm(GCP~total_rfs+as.factor(Educg),data=final.data)
summary(TEG)
#[path A]chd risk factor->atherosclerosis
PAG<-lm(ICB_MEMX_t~total_rfs+as.factor(Educg),data=final.data)
summary(PAG)

#[path B]atherosclerosis->executive function
PBG<-lm(GCP~ICD_MEMX_t+total_rfs+as.factor(Educg),data=final.data)
summary(PBG)

model.V<-mediate(PAG,PBG,treat = "total_rfs", mediator = "ICB_MEMX_t", boot = TRUE, sims=1000)
summary(model.V)

orr.gcp<-(-0.9251)
orrci.gcp<-c((orr.gcp+1.96*0.1234), (orr.gcp-1.96*0.1234))

gcp.icb.beta<-(-8.4014)
gcp.icb.betaci<-c(gcp.icb.beta-1.96*1.1063, gcp.icb.beta+1.96*1.1063)

gcp.icd.beta<-(-7.0110)
gcp.icd.betaci<-c(gcp.icd.beta-1.96*1.0120, gcp.icd.beta+1.96*1.0120)
