#This is the R script that I used after to graph the csv file from SCRPPLE

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(plyr)
library(dplyr)
library(scales)
library(patchwork)
library(car)
library(test.)

dir<-("G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/")
setwd(dir)

year1 <- 1990

baseline_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119A/scrapple-summary-log.csv"))
baseline_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119B/scrapple-summary-log.csv"))
baseline_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119C/scrapple-summary-log.csv"))
baseline_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119D/scrapple-summary-log.csv"))
baseline_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119E/scrapple-summary-log.csv"))
CC_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_A/scrapple-summary-log.csv"))
CC_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_B/scrapple-summary-log.csv"))
CC_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_C/scrapple-summary-log.csv"))
CC_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_D/scrapple-summary-log.csv"))
CC_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_E/scrapple-summary-log.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, CC_csv1, CC_csv2, CC_csv3, CC_csv4, CC_csv5)
Scenario<-c(rep("Historical", 5*nrow(baseline_csv1)), (rep("Climate change", 5*nrow(CC_csv1))))
combined_files<-cbind(Scenario, combined_csv)
combined_files$ActualYear<-rep(combined_files$SimulationYear + year1)
combined_files$TotalIgnitions<-combined_files$NumberFiresAccidental+ combined_files$NumberFiresLightning

# Plot, 
all_data_ribbon<-ddply(combined_files, .(ActualYear, Scenario), summarize,
                       mean_ha = mean(TotalBurnedSitesLightning*2.25),
                       SD_ha = sd(TotalBurnedSitesLightning*2.25),
                       SE_ha = sd(TotalBurnedSitesLightning)*2.25/sqrt(length(TotalBurnedSitesLightning))*2.25,
                       mean_ignit = mean(TotalIgnitions),
                       SD_ignit = sd(TotalIgnitions),
                       SE_ignit = sd(TotalIgnitions)/sqrt(length(TotalIgnitions)))

all_data_ribbon$Scenario <- factor(all_data_ribbon$Scenario, levels = c("Historical", "Climate change"))

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

#hectares_burned
#png_name<-paste(output_dir, "Hectares_burned.png", sep="")
HectaresBurned<-ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_ha, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(ActualYear), y=mean_ha, colour=Scenario))+
  #geom_line(aes(x=ActualYear, y=mean_VWC, group=Scenario), color="grey") +
  geom_ribbon(aes(ymin=mean_ha-SD_ha, ymax=mean_ha+SD_ha, fill=Scenario), alpha=0.25)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab("Years")+ylab("Annual hectares burned")+theme(legend.position='none')+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  scale_y_continuous(labels=scales::comma, limits=c(-3000,40000))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15), size=12))
HectaresBurned
#ggsave(filename=png_name, plot = HectaresBurned, width = 5,  height = 4, units = "in",  dpi = 300)

# number of ignitions
#png_name<-paste(output_dir, "Ignitions.png", sep="")
Ignitions<-ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_ignit, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(ActualYear), y=mean_ignit, colour=Scenario))+
  #geom_line(aes(x=ActualYear, y=mean_VWC, group=Scenario), color="grey") +
  geom_ribbon(aes(ymin=mean_ignit-SD_ignit, ymax=mean_ignit+SD_ignit, fill=Scenario), alpha=0.25)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  ylim(-0.5,4) +
  theme_classic()+xlab(NULL)+ylab("Mean number of fires per year")+ 
  theme(legend.position = c(0.15,0.9), legend.text=element_text(size=12), legend.title=element_text(size=14))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12), axis.text.x = element_blank(), axis.title.x = element_blank())+
  scale_x_continuous(breaks=seq(1990, 2040, 10))
#ggsave(filename=png_name, plot = Ignitions, width = 5,  height = 4, units = "in",  dpi = 300)
Ignitions 

 
png_name<-paste(output_dir, "Stacked_Fire_Graphs.png", sep="")
StackedPlot<-Ignitions/HectaresBurned
ggsave(filename=png_name, plot = StackedPlot, dpi = 300)


# how did ignitions vary with CC?
oneway.test(TotalIgnitions ~ Scenario, data = combined_files, var.equal = FALSE)

# how did variability ignitions vary with CC?
leveneTest(combined_files$TotalIgnitions,combined_files$Scenario)

#  The number of annual ignitions varied from X to X, with X% higher variation under climate change than historical climate

min<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical")%>%
  summarize(min=min(mean_ignit, na.rm=T), max=max(mean_ignit, na.rm=T))
min

min<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change")%>%
  summarize(min=min(mean_ignit, na.rm=T), max=max(mean_ignit, na.rm=T))
min

CV_historical<-combined_files %>% 
  dplyr::filter(Scenario == "Historical")%>%
  summarize(var(TotalIgnitions))
CV_historical

CV_CC<-combined_files %>% 
  dplyr::filter(Scenario == "Climate change")%>%
  summarize(var(TotalIgnitions))
CV_CC

100*(CV_CC-CV_historical)/CV_CC

oneway.test(TotalBurnedSitesLightning ~ Scenario, data = combined_files, var.equal = FALSE)

# Annual hectares burned were also significantly higher under climate change (X ha) than historical climate (X ha). 
mean<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical")%>%
  summarize(mean=mean(mean_ha, na.rm=T))
mean

mean_CC<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change")%>%
  summarize(mean=mean(mean_ha, na.rm=T))
mean_CC

two.way <- aov(TotalIgnitions ~ Scenario, data = combined_files)
two.way

# Hectares burned increased with time with a larger increase under climate change (%) than historical climate
mean_time1<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical" & ActualYear == 1991)%>%
  summarize(mean=mean(mean_ha, na.rm=T))

mean_time50<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical" & ActualYear == 2040)%>%
  summarize(mean=mean(mean_ha, na.rm=T))
mean_time50/mean_time1

100*(mean_time50-mean_time1)/mean_time1


mean_time1<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change" & ActualYear == 1991)%>%
  summarize(mean=mean(mean_ha, na.rm=T))

mean_time50<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change" & ActualYear == 2040)%>%
  summarize(mean=mean(mean_ha, na.rm=T))

mean_time50/mean_time1
100*(mean_time50-mean_time1)/mean_time1

mean_H<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical")%>%
  summarize(mean=mean(mean_ha, na.rm=T))

mean_CC<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change")%>%
  summarize(mean=mean(mean_ha, na.rm=T))

100*abs(mean_H- mean_CC)/((mean_H+ mean_CC)/2)

min_H<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical")%>%
  summarize(min=min(mean_ha, na.rm=T))

max_H<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Historical")%>%
  summarize(max=max(mean_ha, na.rm=T))

min_CC<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change")%>%
  summarize(min=min(mean_ha, na.rm=T))

max_CC<-all_data_ribbon %>% 
  dplyr::filter(Scenario == "Climate change")%>%
  summarize(max=max(mean_ha, na.rm=T))

max_H/min_H
min_CC/max_CC
