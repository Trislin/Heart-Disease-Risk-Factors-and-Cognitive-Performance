
library('mice')


final.data.np<-final.data[,c(18:52)]
names(final.data.np)


np_imputed<-mice(final.data.np, m=5, maxit = 50, method = 'pmm',seed=500,print=FALSE)
summary(np_imputed)
np_completed<-complete(np_imputed,2)

names(final.data)
final.data_c<-final.data
final.data_c[,c(18:52)]<-np_completed[,c(1:35)]

###analysis 
names(final.data_c)
final.data_c$executive_function_c<-rowSums(final.data_c[,c(76,35,32,49,50)]) #trailsAB+sim+dsb+fas+fas_animal
final.data_c$GCP_c<-rowSums(final.data_c[,c(75,79,31,36)])#memory+executivefunction_c+dsf+hvot

###NP composite score- executive function
#[total effect]chd risk factor->executive function
TEE_c<-lm(executive_function_c~total_rfs+Educg,data=final.data_c)
summary(TEE_c)
#[path A]chd risk factor->atherosclerosis
PAE_c<-lm(ICD_MEMX_t~total_rfs+Educg,data=final.data_c)
summary(PAE_c)
#[path B]atherosclerosis->executive function
PBE_c<-lm(executive_function_c~ICD_MEMX_t+total_rfs+Educg,data=final.data_c)
summary(PBE_c)

model.IV<-mediate(PAE_c,PBE_c,treat = "total_rfs", mediator = "ICD_MEMX_t", boot = TRUE, sims=1000)
summary(model.IV)


###NP composite score- GCP
#[total effect]chd risk factor->executive function
TEG_c<-lm(GCP_c~total_rfs+Educg,data=final.data_c)
summary(TEG_c)
#[path A]chd risk factor->atherosclerosis
PAG_c<-lm(ICD_MEMX_t~total_rfs+Educg,data=final.data_c)
summary(PAG_c)
#[path B]atherosclerosis->executive function
PBG_c<-lm(GCP_c~total_rfs+ICD_MEMX_t+Educg,data=final.data_c)
summary(PBG_c)

model.IV<-mediate(PAG_c,PBG_c,treat = "total_rfs", mediator = "ICD_MEMX_t", boot = TRUE, sims=1000)
summary(model.IV)
