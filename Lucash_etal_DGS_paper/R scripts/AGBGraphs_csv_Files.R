#This is the R script that I used to create graphs of AGB and SOC from the csv files.

library(raster)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)

date<-Sys.Date()

model_dir <-"D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"
#model_dir <-"E:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_FinalTHU/Analysis_by_THU/"
setwd(model_dir)

output_dir <-paste0(model_dir, "/Output_Sims_DGS_methods_paper/")

#AGB csv files

year1 <- 1990

baseline_csv1<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119A/DGS-succession-log-short.csv"))
baseline_csv2<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119B/DGS-succession-log-short.csv"))
baseline_csv3<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119C/DGS-succession-log-short.csv"))
baseline_csv4<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119D/DGS-succession-log-short.csv"))
baseline_csv5<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119E/DGS-succession-log-short.csv"))
CC_csv1<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_A/DGS-succession-log-short.csv"))
CC_csv2<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_B/DGS-succession-log-short.csv"))
CC_csv3<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_C/DGS-succession-log-short.csv"))
CC_csv4<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_D/DGS-succession-log-short.csv"))
CC_csv5<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_E/DGS-succession-log-short.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, CC_csv1, CC_csv2, CC_csv3, CC_csv4, CC_csv5)

Scenario<-c(rep("Historical", 5*nrow(baseline_csv1)), (rep("ClimateChange", 5*nrow(CC_csv1))))
Scenario_rep<-c((rep("Hist_rep1",51)), (rep("Hist_rep2",51)), (rep("Hist_rep3", 51)), (rep("Hist_rep4", 51)), (rep("Hist_rep5", 51)), (rep("CC_rep1", 51)),(rep("CC_rep2", 51)),(rep("CC_rep3", 51)),(rep("CC_rep4", 51)),(rep("CC_rep5", 51)))
ActualYear<-rep(baseline_csv1$Time + year1, times=5)

combined_files<-cbind(Scenario, combined_csv, ActualYear, Scenario_rep)

# Plot to make 
plt.cols.long<-c("red","orange","darkorange","gold", "orangered1", "blue","steelblue","cyan", "darkslategray2", "darkslategray4")

ggplot(combined_files, aes(x=(ActualYear), y=mean_AGB, group=Scenario_rep))+
  geom_line(aes(x=(ActualYear), y=AGB, colour=Scenario_rep))+
  scale_color_manual(values = plt.cols.long) +
  theme_classic()+xlab(NULL)+ylab("AGB") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

all_data_ribbon<-ddply(combined_files, .(ActualYear, Scenario), summarize,
                       mean_AGB = mean(AGB),
                       SD_AGB = sd(AGB),
                       SE_AGB = sd(AGB)/sqrt(length(AGB)),
                       mean_SOC = mean(SOMTC),
                       SD_SOC = sd(SOMTC),
                       SE_SOC = sd(SOMTC)/sqrt(length(SOMTC)))

all_data_ribbon$AGB_Carbon<-all_data_ribbon$mean_AGB*0.47
all_data_ribbon$AGB_SD_Carbon<-all_data_ribbon$SD_AGB*0.47

all_data_ribbon$Scenario <- factor(all_data_ribbon$Scenario, levels = c("Historical", "ClimateChange"))

write.csv(all_data_ribbon, "AGB_SoilC.csv")

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_AGB, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(ActualYear), y=mean_AGB, colour=Scenario))+
  #geom_line(aes(x=ActualYear, y=mean_VWC, group=Scenario), color="grey") +
  geom_ribbon(aes(ymin=mean_AGB-SD_AGB, ymax=mean_AGB+SD_AGB, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  ylim(0,10000) +
  theme_classic()+xlab(NULL)+ylab("Aboveground biomass (g/m2)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

png_name<-paste(output_dir, "AG_Carbon_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

C_plot <- ggplot(all_data_ribbon, aes(x=(ActualYear), y=AGB_Carbon, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=AGB_Carbon, colour=Scenario))+
  geom_ribbon(aes(ymin=AGB_Carbon-AGB_SD_Carbon, ymax=AGB_Carbon+AGB_SD_Carbon, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Historical climate", "Climate Change"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Aboveground~carbon~(gC/m^2))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(breaks=seq(0, 9000, 1000), limits=c(0,4000))
C_plot + theme(legend.position = c(0.2, 0.35)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 2.5)) #this adjusts the axis title location
dev.off()

png_name<-paste(output_dir, "AGB_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

bioplot <- ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_AGB, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_AGB, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_AGB-SD_AGB, ymax=mean_AGB+SD_AGB, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Baseline", "Climate Change"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Aboveground~biomass~(g/m^2))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(breaks=seq(0, 8000, 1000), limits=c(0,8000))
bioplot + theme(legend.position = c(0.2, 0.35)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 2.5)) #this adjusts the axis title location

png_name<-paste(output_dir, "SoilC_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

SoilCplot <- ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_SOC, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_SOC, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_SOC-SD_SOC, ymax=mean_SOC+SD_SOC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Baseline", "Climate Change"), values = plt.cols.short) +
  theme_classic()+xlab("Years")+ylab(expression(Soil~C~(g/m^2))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(breaks=seq(0.0, 12000, 2000), limits=c(0,12000))
SoilCplot + theme(legend.position = c(0.8, 0.55)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 2.5)) #this adjusts the axis title location
dev.off()


#Directory where soil temp PNG files are stored
model_dir <- "C:/Users/mlucash/Dropbox (University of Oregon)/Lucash_Lab/Lucash/Alaska_Reburns_Project/Manuscripts/DGS_model_description_paper/Figures/AGBGraphs/"  
setwd(model_dir)

#all_maps <- list.files(paste0(model_dir), pattern = ".png", recursive = T, full = T)
plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})

ggsave("AGB.TIFF", marrangeGrob(grobs=plots, nrow=1, ncol=2, dpi=400))
dev.off() 
