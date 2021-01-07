#odds ratio visualization 

library(ggplot2)
library(RColorBrewer)
display.brewer.all()
# Create labels
boxLabels = c("Memory", "Executive Function", "Global Cognition")

# Enter summary data. boxOdds are the odds ratios (calculated elsewhere), boxCILow is the lower bound of the CI, boxCIHigh is the upper bound.

df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(orr.memory,orr.ef,orr.gcp),
  boxCILow = c(orrci.memory[2],orrci.ef[2],orrci.gcp[2]),
  boxCIHigh = c(orrci.memory[1],orrci.ef[1],orrci.gcp[1])
)

# Plot
df%>% ggplot(aes(x = boxOdds, y = yAxis, color=as.factor(boxLabels))) + 
  geom_vline(aes(xintercept = 0.3), size = .0001, linetype = "dashed") +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 4) +
  theme_classic() +
  scale_color_brewer(palette = "Set2") + 
  scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
  ylab("Cognitive Function Domain") +
  xlab("Beta Coefficient (95%CI)") +
  labs(color = "Cognitive Function Domain") +
  ggtitle("Association Between CHD Risk Score and Cognitive Function") +
  theme(plot.title = element_text(face='bold',size = 12,hjust=0.5))
  




boxLabels = c("Mean Intimal Thickness of Common Carotid Bulb", "Mean Intimal Thickness of Distal ICA", "Left Stenosis","Right Stenosis")

df <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(icb.chd.beta,icd.chd.beta,lsteno.chd.beta, rsteno.chd.beta),
  boxCILow = c(icb.chd.betaci[1],icd.chd.betaci[1],lsteno.chd.betaci[1],rsteno.chd.betaci[1]),
  boxCIHigh = c(icb.chd.betaci[2],icd.chd.betaci[2],lsteno.chd.betaci[2],rsteno.chd.betaci[2])
)


# Plot2
df%>% ggplot(aes(x = boxOdds, y = yAxis, color=factor(boxLabels,levels = c("Mean Intimal Thickness of Common Carotid Bulb", "Mean Intimal Thickness of Distal ICA", "Left Stenosis","Right Stenosis")))) + 
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 4) +
  theme_classic() +
  scale_color_brewer(palette = "Set2") + 
  scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
  scale_x_continuous(breaks=seq(0.01,0.08,0.01))+
  ylab("Measure of Atherosclerosis") +
  xlab("Beta Coefficient (95%CI)") +
  labs(color = "Measure of Atherosclerosis") +
  ggtitle("Association Between CHD Risk Score and Measure of Atherosclerosis") +
  theme(plot.title = element_text(face='bold',size = 12,hjust=0.5))

boxLabels = c(rep(c("Mean Intimal Thickness of Common Carotid Bulb", "Mean Intimal Thickness of Distal ICA", "Left Stenosis","Right Stenosis"),3))
boxgroup=c(rep("Memory",4),rep("Executive Function",4),rep("Global Cognition",4))
df <- data.frame(
  yAxis = rep(4:1,1),
  boxOdds = c(memory.icb.beta,memory.icd.beta,memory.lsteno.beta,memory.rsteno.beta,executive.icb.beta,executive.icd.beta,executive.lsteno.beta,executive.rsteno.beta,gcp.icb.beta,gcp.icd.beta,gcp.lsteno.beta,gcp.rsteno.beta),
  boxCILow = c(memory.icb.betaci[1],memory.icd.betaci[1],memory.lsteno.betaci[1],memory.rsteno.betaci[1],executive.icb.betaci[1],executive.icd.betaci[1],executive.lsteno.betaci[1],executive.rsteno.betaci[1],gcp.icb.betaci[1],gcp.icd.betaci[1],gcp.lsteno.betaci[1],gcp.rsteno.betaci[1]),
  boxCIHigh = c(memory.icb.betaci[2],memory.icd.betaci[2],memory.lsteno.betaci[2],memory.rsteno.betaci[2],executive.icb.betaci[2],executive.icd.betaci[2],executive.lsteno.betaci[2],executive.rsteno.betaci[2],gcp.icb.betaci[2],gcp.icd.betaci[2],gcp.lsteno.betaci[2],gcp.rsteno.betaci[2]),
  boxgroup = factor(boxgroup, levels = c("Memory","Executive Function","Global Cognition")),
  boxLabels = factor(boxLabels, levels = c("Mean Intimal Thickness of Common Carotid Bulb", "Mean Intimal Thickness of Distal ICA", "Left Stenosis","Right Stenosis"))
  )

df%>% ggplot(aes(x = boxOdds, y=yAxis, color=as.factor(boxLabels))) + 
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 4) +
  #facet_grid(rows = vars(boxgroup))+
  facet_wrap(~boxgroup,dir = 'v')+
  theme_classic() +
  theme(axis.text.y=element_blank(),
        strip.text = element_text(face='bold',size = 10),
        plot.title = element_text(face='bold',size = 12,hjust=0.5)) +
  scale_color_brewer(palette = "Set2") + 
 # scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
 # scale_x_continuous(breaks=seq(0.01,0.08,0.01))+
  ylab("") +
  xlab("Beta Coefficient (95%CI)") +
  labs(color = "Measure of Atherosclerosis") +
  ggtitle("Association Between ICA Atherosclerosis and Cognitive Function")
