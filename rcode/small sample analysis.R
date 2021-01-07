
final.data_s$ICD_MEMX_t<-log(final.data_s$ICD_MEMX)
final.data_s$ICB_MEMX_t<-log(final.data_s$ICB_MEMX)
final.data_s$memory_s<-rowSums(final.data_s[,c(18:29)])
final.data_s$trailsAB<-final.data_s$trailsB-final.data_s$trailsA
final.data_s$executive_function_s<-rowSums(final.data_s[,c(35,76,32,49,50)]) #sim & transformed trailsA,B
final.data_s$GCP_s<-rowSums(final.data_s[,c(75,77,36,31)]) #memory+executive function+dsf+hvot
names(final.data_s)

#1[total effect]chd risk factor->memory 
TEM_s<-lm(memory_s~total_rfs+Educg,data=final.data_s)
summary(TEM_s)
#2[path A]chd risk factor->atherosclerosis
PAM<-lm(ICD_MEMX_t~total_rfs+Educg,data = final.data_s)
summary(PAM)
#3[path B]atherosclerosis->memory
PBM<-lm(memory_s~ICD_MEMX_t+total_rfs+Educg,data = final.data_s)
summary(PBM)

model.II<-mediate(PAM,PBM,treat = "total_rfs", mediator = "ICD_MEMX_t", boot = TRUE, sims=1000)
summary(model.II)


###NP composite score- executive function
#[total effect]chd risk factor->executive function
TEE_s<-lm(executive_function_s~total_rfs+Educg,data=final.data_s)
summary(TEE_s)
#[path A]chd risk factor->atherosclerosis
PAE_s<-lm(ICB_MEMX_t~total_rfs+Educg,data=final.data_s)
summary(PAE_s)
#[path B]atherosclerosis->executive function
PBE_s<-lm(executive_function_s~ICB_MEMX_t+total_rfs+Educg,data=final.data_s)
summary(PBE_s)

model.IV<-mediate(PAE_s,PBE_s,treat = "total_rfs", mediator = "ICB_MEMX_t", boot = TRUE, sims=100)
summary(model.IV)


###NP composite score- GCP
#[total effect]chd risk factor->executive function
TEG_s<-lm(GCP_s~total_rfs+Educg,data=final.data_s)
summary(TEG_s)
#[path A]chd risk factor->atherosclerosis
PAG_s<-lm(ICD_MEMX_t~total_rfs+Educg,data=final.data_s)
summary(PAG_s)
#[path B]atherosclerosis->executive function
PBG_s<-lm(GCP_s~ICD_MEMX_t+total_rfs+Educg,data=final.data_s)
summary(PBG_s)

model.V<-mediate(PAG_s,PBG_s,treat = "total_rfs", mediator = "ICD_MEMX_t", boot = TRUE, sims=1000)
summary(model.V)


