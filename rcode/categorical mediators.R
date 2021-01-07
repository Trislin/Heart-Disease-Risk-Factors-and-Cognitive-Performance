library(data.table)
count_L<-table(final.data[,55])
count_R<-table(final.data[,53])

pdf("distribution of Percent Stenosis.pdf",width=10)
bp_L<-barplot(count_L,main='L_Steno',xlab='Percent Stenosis on the Left',names.arg = c('None','1-24%','25-49%','50-74%','75-99%','100%'),cex.names = 0.7)
text(bp_L,0,lbls_L,cex=0.7,pos=3)
bp_R<-barplot(count_R,main='R_Steno',xlab='Percent Stenosis on the Right',names.arg = c('None','1-24%','25-49%','50-74%','75-99%','100%'),cex.names = 0.7)
text(bp_R,0,lbls_R,cex=0.7,pos=3)
dev.off()
?barplot

lbls_L<-paste(count_L,"\n",round(prop.table(count_L)*100),'%')
lbls_R<-paste(count_R,"\n",round(prop.table(count_R)*100),'%')


library(MASS)
install.packages('rms')
library(rms)
library(car)
install.packages('brant')
library(brant)

names(final.data)
final.data$L_STENO<-as.factor(final.data$L_STENO)
final.data$R_STENO<-as.factor(final.data$R_STENO)
final.data$Educg_f<-as.factor(final.data$Educg)
###memory
TEM<-lm(memory~total_rfs+Educg_f,data=final.data)
summary(TEM)
PALM<-lm(L_STENO~total_rfs+Educg_f,data = final.data)
summary(PALM)

lsteno.chd.beta<-0.056844
lsteno.chd.betaci<-c(lsteno.chd.beta-1.96*0.004146, lsteno.chd.beta+1.96*0.004146)


#round(exp(PALM$coefficients),3)#odds ratio

#PALM_s<-polr(L_STENO~total_rfs+Educg_f,data = final.data,Hess=TRUE)
#PALM_s
PBLM<-lm(memory~total_rfs+L_STENO+Educg_f,data=final.data)
summary(PBLM)
Anova(PBLM)
model.LM<-mediate(PALM,PBLM,treat = "total_rfs", mediator = "L_STENO", boot = TRUE, sims=1000)
summary(model.LM)

memory.lsteno.beta<-(-3.1908)
memory.lsteno.betaci<-c(memory.lsteno.beta-1.96*0.5889,memory.lsteno.beta+1.96*0.5889)

PARM<-lm(R_STENO~total_rfs+Educg_f,data = final.data)
summary(PARM)

rsteno.chd.beta<-0.055404
rsteno.chd.betaci<-c(rsteno.chd.beta-1.96*0.004275, rsteno.chd.beta+1.96*0.004275)

round(exp(PARM$coefficients),3)
PBRM<-lm(memory~total_rfs+R_STENO+Educg_f,data=final.data)
summary(PBRM)

memory.rsteno.beta<-(-2.7310)
memory.rsteno.betaci<-c(memory.rsteno.beta-1.96*0.5722, memory.rsteno.beta+1.96*0.5722)

Anova(PBRM)
model.RM<-mediate(PARM,PBRM,treat = "total_rfs", mediator = "R_STENO", boot = TRUE, sims=1000)
summary(model.RM)

summary(final.data$L_STENO)
final.data$L_STENO[final.data$L_STENO==5]<-6
final.data$L_STENO[final.data$L_STENO==4]<-5
final.data$L_STENO[final.data$L_STENO==3]<-4
final.data$L_STENO[final.data$L_STENO==2]<-3
final.data$L_STENO[final.data$L_STENO==1]<-2
final.data$L_STENO[final.data$L_STENO==0]<-1
final.data$L_STENO<-as.numeric(final.data$L_STENO)
final.data$L_STENO[final.data$L_STENO==5]<-4
final.data$L_STENO[final.data$L_STENO==6]<-4
final.data$L_STENO[final.data$L_STENO==4]<-3
table(final.data$L_STENO)

dispersion_test <- function(x) 
{
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  
  cat("Dispersion test of count data:\n",
      length(x), " data points.\n",
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")
  
  invisible(res)
}



dispersion_test(final.data$L_STENO)

install.packages('lavaan')
library(lavaan)

set.seed(1234)
mod1<-"
L_STENO~a*total_rfs
memory~b*L_STENO
memory~cp*total_rfs
ab:=a*b
total:=cp+ab"
fsem1 <- sem(mod1, data = final.data, se = "bootstrap", bootstrap = 1000)
summary(fsem1,standardized=TRUE)

###executive function
TEE<-lm(executive_function~total_rfs+Educg_f,data=final.data)
summary(TEE)
PALE<-lm(L_STENO~total_rfs+Educg_f,data = final.data)
summary(PALE)

#round(exp(PALE$coefficients),3)#odds ratio

PBLE<-lm(executive_function~total_rfs+L_STENO+Educg_f,data=final.data)
summary(PBLE)

executive.lsteno.beta<-(-0.07784)
executive.lsteno.betaci<-c(executive.lsteno.beta-1.96*0.10959, executive.lsteno.beta+1.96*0.10959)

Anova(PBLE)
model.LE<-mediate(PALE,PBLE,treat = "total_rfs", mediator = "L_STENO", boot = TRUE, sims=1000)
summary(model.LE)

PARE<-lm(R_STENO~total_rfs+Educg_f,data = final.data)
summary(PARE)
round(exp(PARM$coefficients),3)
PBRE<-lm(executive_function~total_rfs+R_STENO+Educg_f,data=final.data)
summary(PBRE)

executive.rsteno.beta<-(-0.18604)
executive.rsteno.betaci<-c(executive.rsteno.beta-1.96*0.10622, executive.rsteno.beta+1.96*0.10622)

Anova(PBRE)
model.RE<-mediate(PARE,PBRE,treat = "total_rfs", mediator = "R_STENO", boot = TRUE, sims=1000)
summary(model.RE)

###GCP
TEG<-glm(GCP~total_rfs+Educg_f,data=final.data)
summary(TEG)
PALG<-glm(L_STENO~total_rfs+Educg_f,data = final.data)
summary(PALG)

round(exp(PALM$coefficients),3)#odds ratio

PBLG<-lm(GCP~total_rfs+L_STENO+Educg_f,data=final.data)
summary(PBLG)

gcp.lsteno.beta<-(-3.8054)
gcp.lsteno.betaci<-c(gcp.lsteno.beta-1.96*0.6738, gcp.lsteno.beta+1.96*0.6738)

Anova(PBLG)
model.LG<-mediate(PALG,PBLG,treat = "total_rfs", mediator = "L_STENO", boot = TRUE, sims=1000)
summary(model.LG)

PARG<-glm(R_STENO~total_rfs+Educg_f,data = final.data)
summary(PARM)
round(exp(PARM$coefficients),3)
PBRG<-lm(GCP~total_rfs+R_STENO+Educg_f,data=final.data)
summary(PBRG)

gcp.rsteno.beta<-(-3.4597)
gcp.rsteno.betaci<-c(gcp.rsteno.beta-1.96*0.6542, gcp.rsteno.beta+1.96*0.6542)

Anova(PBRG)

model.RG<-mediate(PARG,PBRG,treat = "total_rfs", mediator = "R_STENO", boot = TRUE, sims=1000)
summary(model.RG)

