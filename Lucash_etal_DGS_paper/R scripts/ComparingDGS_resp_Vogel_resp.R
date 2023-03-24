#This is the R script to compare the observed resp data and the resp simulated by LANDIS-II

library(raster)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(Metrics)

date<-Sys.Date()

model_dir <-"D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/SingleCell_Vogel/"
obs_dir <-"C:/Users/mlucash/Dropbox (University of Oregon)/Lucash_Lab/Lucash/Alaska_Reburns_Project/DAMM/"
setwd(model_dir)

output_dir <-paste0("D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/Output_Sims_DGS_methods_paper/")

#AGB csv files

year1 <- 1990

LANDIS_csv<-read.csv (paste0 (model_dir, "DGS-succession-monthly-log.csv"))
LANDIS_subset<-LANDIS_csv[c(1,2,3,4,11,12),c("Month","avgResp")]
colnames(LANDIS_subset)<-c("Month","Respiration")
LANDIS_subset_ordered<-LANDIS_subset[order(LANDIS_subset$Month),]

Vogel_csv<-read.csv (paste0 (obs_dir, "Vogel_soil_resp_meanMonthly.csv"))
colnames(Vogel_csv)<-c("Month","Respiration")

combined_csv<-rbind(LANDIS_subset_ordered, Vogel_csv)

Scenario<-c(rep("DGS", 6), rep("Observed", 6))

combined_files<-cbind(Scenario, combined_csv)

# Plot to make 
plt.cols.short<-c("steelblue","gray50")

png_name<-paste(output_dir, "Soil respiration_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

ggplot(combined_files, aes(x=(Month), y=Respiration, group=Scenario))+
    geom_line(aes(x=(Month), y=Respiration, colour=Scenario, linetype=Scenario), linewidth=1.1)+
  scale_color_manual(values = plt.cols.short) +
  theme_classic()+xlab("Month")+ylab(expression(Heterotrophic~respiration~(g~C/m^2))) + theme(legend.position = c(0.15,0.9))+
  geom_point(aes(colour = factor(Scenario)), size=3)+
  scale_y_continuous(breaks=seq(0, 70, 10), limits=c(0,70))+
  theme(legend.key.size = unit(1.1, 'cm'), legend.text = element_text(size=12), legend.title=element_blank())
  
dev.off()

obs<-combined_files%>%
  filter(combined_files$Scenario=="Observed")

landis<-combined_files%>%
  filter(combined_files$Scenario=="DGS")

rmse(obs$Respiration, landis$Respiration)

June<-combined_files%>%
  filter(combined_files$Month=="6")

(51-43)/51

July<-combined_files%>%
  filter(combined_files$Month=="7")

(57.7936-34.96)/57.79

Aug<-combined_files%>%
  filter(combined_files$Month=="8")

(57-22)/57

