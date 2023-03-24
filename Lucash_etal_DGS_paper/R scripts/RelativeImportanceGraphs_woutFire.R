
# This script reads in all the csv files from all the scenarios and graphs them
# so I can report the relative importance of the different factors controlling growth and regen.

library(raster)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(png)

date<-Sys.Date()

dir<-("D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/")
setwd(dir)

output_dir <-paste0(dir, "Output_Sims_DGS_methods_paper/")

year1 <- 1990
replicates<-5  # of replicates of each scenario

baseline_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_A/DGS-succession-log-short.csv"))
baseline_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_B/DGS-succession-log-short.csv"))
baseline_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_C/DGS-succession-log-short.csv"))
baseline_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_D/DGS-succession-log-short.csv"))
baseline_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_E/DGS-succession-log-short.csv"))

temp_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_A/DGS-succession-log-short.csv"))
temp_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_B/DGS-succession-log-short.csv"))
temp_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_C/DGS-succession-log-short.csv"))
temp_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_D/DGS-succession-log-short.csv"))
temp_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_E/DGS-succession-log-short.csv"))

SM_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_A/DGS-succession-log-short.csv"))
SM_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_B/DGS-succession-log-short.csv"))
SM_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_C/DGS-succession-log-short.csv"))
SM_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_D/DGS-succession-log-short.csv"))
SM_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_E/DGS-succession-log-short.csv"))

NoFire_dir<-("D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Mar_2023_Sims/")

N_csv1<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NLimit_A/DGS-succession-log-short.csv"))
N_csv2<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NLimit_B/DGS-succession-log-short.csv"))
N_csv3<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NLimit_C/DGS-succession-log-short.csv"))
N_csv4<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NLimit_D/DGS-succession-log-short.csv"))
N_csv5<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NLimit_E/DGS-succession-log-short.csv"))

NoFire_csv1<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_A/DGS-succession-log-short.csv"))
NoFire_csv2<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_B/DGS-succession-log-short.csv"))
NoFire_csv3<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_C/DGS-succession-log-short.csv"))
NoFire_csv4<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_D/DGS-succession-log-short.csv"))
NoFire_csv5<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_E/DGS-succession-log-short.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, temp_csv1, temp_csv2, temp_csv3, temp_csv4, temp_csv5, SM_csv1, SM_csv2, SM_csv3, SM_csv4, SM_csv5, N_csv1, N_csv2, N_csv3, N_csv4, N_csv5, NoFire_csv1, NoFire_csv2, NoFire_csv3, NoFire_csv4, NoFire_csv5)

scenario_name<-c(rep("Baseline", replicates*nrow(baseline_csv1)), (rep("Soil_temp", replicates*nrow(temp_csv1))), (rep("Soil_moisture", replicates*nrow(SM_csv1))),(rep("Soil_nitrogen", replicates*nrow(N_csv1))), (rep("Fire", replicates*nrow(NoFire_csv1))))
Year<-rep(baseline_csv1$Time + year1, n=5)

combined_files<-cbind(scenario_name, combined_csv, Year)

all_data_ribbon<-ddply(combined_files, .(Year, scenario_name), summarize,
                       mean_AGB = mean(AGB),
                       SD_AGB = sd(AGB),
                       SE_AGB = sd(AGB)/sqrt(length(AGB)))

all_data_ribbon$scenario_name <- factor(all_data_ribbon$scenario_name, levels = c("Baseline", "Soil_temp", "Soil_moisture", "Soil_nitrogen", "Fire"))

#This is the ribbon graph of scenarios
#plt.cols.short <- c("grey30", "myblue", "mygreen") #Number corresponds to scenarios
plt.cols.short <- c("orange", "forestgreen", "steelblue", "brown", "red") #Number corresponds to scenarios
legend_title<-'Scenarios'

png_name<-paste(output_dir, "AG_Biomass_GrowthLimits_", date, ".png", sep="")
png(png_name, width = 5, height = 4, units = 'in', res = 300)

ggplot(all_data_ribbon, aes(x=(Year), y=mean_AGB, group=scenario_name)) +
  geom_line(aes(x=(Year), y=mean_AGB, colour=scenario_name), linewidth=1.1)+
  geom_ribbon(aes(ymin=mean_AGB-SD_AGB, ymax=mean_AGB+SD_AGB, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Climate Change", "CC, No soil temp limitation", "CC, No soil moisture limitation", "CC no soil nitrogen limitation", "CC, No Fire"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Aboveground~biomass~(g/m^2))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(breaks=seq(0, 10000, 1000), limits=c(0,10000))+
 theme(legend.position = c(0.75, 0.82)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 2.5)) #this adjusts the axis title location

dev.off()
#Difference values
baseline_AGB<-subset (combined_files, combined_files$scenario_name=="Baseline")
nitrogen_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_nitrogen")
temp_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_temp")
SM_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_moisture")
Fire_AGB<-subset (combined_files, combined_files$scenario_name=="Fire")

#N_diff<-baseline_AGB$mean_AGB-nitrogen_AGB$mean_AGB
temp_diff<-temp_AGB$AGB-baseline_AGB$AGB
SM_diff<-SM_AGB$AGB-baseline_AGB$AGB 
N_diff<-nitrogen_AGB$AGB-baseline_AGB$AGB 
Fire_diff<-Fire_AGB$AGB-baseline_AGB$AGB 
Diffs_all<-(c(temp_diff, SM_diff, N_diff, Fire_diff))
#should I just use abs or all diffs??? 
#Diffs<-abs(c(temp_diff, SM_diff))

Time<-rep(baseline_AGB$Year, 12)
scenario_name<-c((rep("Soil_temp", length(temp_diff))),(rep("Soil_moisture", length(SM_diff))),(rep("Soil_nitrogen", length(N_diff))), (rep("Fire", length(Fire_diff))))

Difference_matrix<-cbind.data.frame(scenario_name, Time, Diffs_all)
Difference_matrix$Diffs<- ifelse(Difference_matrix$Diffs_all<0,0,Difference_matrix$Diffs_all)

myblue <- rgb(70, 130, 180, max = 255, alpha = 160) #125 is midway
mygreen <- rgb(34, 139, 34, max = 255, alpha = 160) #125 is midway
plt.cols.shorter <- c(myblue, mygreen, "sienna", "firebrick2")

#Difference_matrix_SM_ST<-subset (Difference_matrix, !Difference_matrix$scenario_name=="Nitrogen")
Difference_matrix$scenario_name <- factor(Difference_matrix$scenario_name, levels = c("Soil_moisture", "Soil_temp", "Soil_nitrogen", "Fire"))
#Difference_matrix_SM_ST$scenario_name <- factor(Difference_matrix_SM_ST$scenario_name, levels = c("Baseline", "Soil_temp", "Soil_moisture"))


plot<-ggplot(data = Difference_matrix , aes(x = Time, y = Diffs, fill = scenario_name)) +
  geom_bar(position="fill", stat="identity")+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+    #labs(title="Relative importance of different factors on AGB", x ="Time (years)", y = "Difference from baseline", fill="Scenario Name")+
  scale_x_continuous( breaks=seq(1990, 2040, 10),labels=c("2000","2010","2020","2030","2040","2050"))

png_name<-paste(output_dir, "RelativeImportance_", date, ".png", sep="")
png(png_name, width = 6, height = 5, units = 'in', res = 300)

RI_plot <- ggplot(Difference_matrix, aes(x=(Time), y=Diffs, fill= scenario_name)) +
  geom_bar(position="fill", stat="identity")+
  #geom_ribbon(aes(ymin=AGB_Carbon-AGB_SD_Carbon, ymax=AGB_Carbon+AGB_SD_Carbon, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_fill_manual(name = NULL, labels = c("Soil moisture", "Soil temperature", "Soil nitrogen", "Fire"), values = plt.cols.shorter) +
  theme_classic()+
  ylab(expression(Relative~importance~on~aboveground~biomass)) + theme(legend.position = "right")+
  theme(axis.text.x=element_blank())+xlab(NULL)+
  theme(axis.title.y=element_text(vjust=3, siz=12))+
  scale_y_continuous(breaks=seq(0,1,0.25))
RI_plot + theme(legend.position = c(0.75, 0.3)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 3)) #this adjusts the axis title location
dev.off()

########## Switching to boxplots which I have decided not to use  ###########################
Difference_matrix_ST<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_temp")
Difference_matrix_SM<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_moisture")
RelativeDiffs<-cbind.data.frame(Difference_matrix_ST, Difference_matrix_SM)
colnames(RelativeDiffs)<-c("scenario_SM","Time","SM_diffs", "scenario_ST","Time_ST","ST_diffs")
RelativeDiffs$RelativeDiff_SM<-RelativeDiffs$SM_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)
RelativeDiffs$RelativeDiff_ST<-RelativeDiffs$ST_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)

graphing_relativeDiff_Time<-c(RelativeDiffs$Time, RelativeDiffs$Time)
graphing_relativeDiff_Scenario<-c(as.character(RelativeDiffs$scenario_SM), as.character(RelativeDiffs$scenario_ST))
graphing_relativeDiff_Values<-c(RelativeDiffs$RelativeDiff_SM, RelativeDiffs$RelativeDiff_ST)
graphing<-cbind.data.frame(graphing_relativeDiff_Scenario, graphing_relativeDiff_Time, graphing_relativeDiff_Values)
colnames(graphing)<-c("scenario_name","Time","Diffs")
head(graphing)

plot<-ggplot(data = graphing , aes(x = as.factor(Time), y = Diffs, fill = scenario_name)) +
  geom_boxplot()+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)+
  scale_y_continuous(limits = c(0, 1))

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+
  scale_x_discrete( breaks=seq(1990, 2040, 10),labels=c("2000","2010","2020","2030","2040","2050"))

decades<-c(1991, 2000, 2010,2020,2030,2040)
graphing_decades<-subset(graphing, graphing$Time==decades)
plot<-ggplot(data = graphing_decades , aes(x = as.factor(Time), y = Diffs, fill = scenario_name)) +
  geom_boxplot()+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)+
  scale_y_continuous(limits = c(0, 1))

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+
  scale_x_discrete( breaks=seq(1990, 2040, 10),labels=c("1990","2000","2010","2020","2030","2040"))


#Maps of select years
baseline_map<-raster (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_A/biomass/TotalBiomass-50.img"))
#Nitrogen_map<-raster (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_NoNLimit_B/biomass/TotalBiomass-50.img"))
SoilTemp_map<-raster (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_NoTempLimit_A/biomass/TotalBiomass-50.img"))
SoilMoisture_map<-raster (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_NoSMLimit_A/biomass/TotalBiomass-50.img"))

N_imp_map<-baseline_map-Nitrogen_map
STemp_imp_map<-baseline_map-SoilTemp_map
SM_imp_map<-baseline_map-SoilMoisture_map

#Specify a pallette, I like the RColorBrewer Spectral option because I'm plotting from red to yellow to blue.
#The number 7 is the number of categories I want on my map and legend.
pal <- brewer.pal(7,"Spectral")

#Need to plot the raster first and remove many of the defaults.
plot(STemp_imp_map,  col=pal, legend=FALSE, axes=FALSE,  box=FALSE)
title("Soil Temperature", line = -2)
#Then add in just the legend, 5000 is the interval between values on the legend.
plot(STemp_imp_map, legend.only=TRUE, col=pal,
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(N_imp_map.range[1], N_imp_map.range[2], 5000),
                    labels=seq(N_imp_map.range[1], N_imp_map.range[2], 5000), 
                    cex.axis=0.6),
     legend.args=list(text='Difference in AGB', side=4, font=2.5, line=2.5, cex=0.8))

plot(STemp_imp_map,  col=pal, legend=FALSE, axes=FALSE,  box=FALSE)
title("Soil temperature", line = -2)

plot(SM_imp_map,  col=pal, legend=FALSE, axes=FALSE,  box=FALSE)
title("Soil moisture", line = -5)

######################################################
#Regeneration AGB files

baseline_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_A/DGS-succession-log-short.csv"))
baseline_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_B/DGS-succession-log-short.csv"))
baseline_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_C/DGS-succession-log-short.csv"))
baseline_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_D/DGS-succession-log-short.csv"))
baseline_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_E/DGS-succession-log-short.csv"))
ST_limit_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_A/DGS-succession-log-short.csv"))
ST_limit_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_B/DGS-succession-log-short.csv"))
ST_limit_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_C/DGS-succession-log-short.csv"))
ST_limit_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_D/DGS-succession-log-short.csv"))
ST_limit_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_E/DGS-succession-log-short.csv"))
SM_limit_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_A/DGS-succession-log-short.csv"))
SM_limit_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_B/DGS-succession-log-short.csv"))
SM_limit_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_C/DGS-succession-log-short.csv"))
SM_limit_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_D/DGS-succession-log-short.csv"))
SM_limit_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_E/DGS-succession-log-short.csv"))
NoFire_csv1<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_A/DGS-succession-log-short.csv"))
NoFire_csv2<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_B/DGS-succession-log-short.csv"))
NoFire_csv3<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_C/DGS-succession-log-short.csv"))
NoFire_csv4<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_D/DGS-succession-log-short.csv"))
NoFire_csv5<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_E/DGS-succession-log-short.csv"))

#combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, ST_limit_csv1, ST_limit_csv2, ST_limit_csv3, ST_limit_csv4, ST_limit_csv5, SM_limit_csv1, SM_limit_csv2, SM_limit_csv3, SM_limit_csv4, SM_limit_csv5, NoFire_csv1, NoFire_csv2, NoFire_csv3, NoFire_csv4, NoFire_csv5)
combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, ST_limit_csv1, ST_limit_csv2, ST_limit_csv3, ST_limit_csv4, ST_limit_csv5, SM_limit_csv1, SM_limit_csv2, SM_limit_csv3, SM_limit_csv4, SM_limit_csv5)

#scenario_name<-c(rep("Baseline", replicates*nrow(baseline_csv1)), (rep("Soil_temp", replicates*nrow(ST_limit_csv1))), (rep("Soil_moisture", replicates*nrow(ST_limit_csv5))),(rep("Fire", replicates*nrow(NoFire_csv1))))
scenario_name<-c(rep("Baseline", replicates*nrow(baseline_csv1)), (rep("Soil_temp", replicates*nrow(ST_limit_csv1))), (rep("Soil_moisture", replicates*nrow(ST_limit_csv5))))
Year<-rep(baseline_csv1$Time + year1, n=3) # N IS NUMBER OF SCENARIOS

combined_files<-cbind(scenario_name, combined_csv, Year)

all_data_ribbon<-ddply(combined_files, .(Year, scenario_name), summarize,
                       mean_AGB = mean(AGB),
                       SD_AGB = sd(AGB),
                       SE_AGB = sd(AGB)/sqrt(length(AGB)))

all_data_ribbon$scenario_name <- factor(all_data_ribbon$scenario_name, levels = c("Baseline", "Soil_temp", "Soil_moisture"))

#This is the ribbon graph of scenarios

plt.cols.short <- c("orange", "forestgreen", "steelblue") #Number corresponds to scenarios
legend_title<-'Scenarios'

# Plot, 

png_name<-paste(output_dir, "AGB_GrowthLimits_Est_", date, ".png", sep="")
png(png_name, width = 5, height = 4, units = 'in', res = 300)

ggplot(all_data_ribbon, aes(x=(Year), y=mean_AGB, group=scenario_name)) +
  geom_line(aes(x=(Year), y=mean_AGB, colour=scenario_name), lwd=1.1)+
  geom_ribbon(aes(ymin=mean_AGB-SD_AGB, ymax=mean_AGB+SD_AGB, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Climate Change", "CC, No soil temp limitation", "CC, No soil moisture limitation", "No Fire limitation"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Aboveground~biomass~of~regeneration~(g/m^2))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(limits=c(0,10000), breaks=seq(0, 10000, 2000))+
  theme(legend.position = c(0.3, 0.2)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 2.5)) #this adjusts the axis title location
dev.off()


#Difference values
baseline_AGB<-subset (combined_files, combined_files$scenario_name=="Baseline")
temp_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_temp")
SM_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_moisture")
#Fire_AGB<-subset (combined_files, combined_files$scenario_name=="Fire")

#temp_diff<-(temp_AGB$AGB-baseline_AGB$AGB)
#SM_diff<-(SM_AGB$AGB-baseline_AGB$AGB)

difference_function <- function(x,y) {
  ifelse((y-x) > 0, y-x, 0)}

temp_diff<-difference_function(y=temp_AGB$AGB, x=baseline_AGB$AGB)
SM_diff<-difference_function(y=SM_AGB$AGB, x=baseline_AGB$AGB)
#Fire_diff<-difference_function(y=Fire_AGB$AGB, x=baseline_AGB$AGB)

Diffs<-(c(temp_diff, SM_diff))

Time<-rep(baseline_AGB$Year, 2) # 2= SM+ST+Fire
scenario_name<-c(rep("Soil temp", length(temp_diff)),rep("Soil moisture", length(SM_diff)))

Difference_matrix<-cbind.data.frame(scenario_name, Time, Diffs)
Difference_matrix$scenario_name <- factor(Difference_matrix$scenario_name, levels = c("Soil moisture", "Soil temp"))

myblue <- rgb(70, 130, 180, max = 255, alpha = 130) #125 is midway
mygreen <- rgb(34, 139, 34, max = 255, alpha = 150) #125 is midway
plt.cols.shorter <- c(myblue, mygreen) #Number corresponds to scenarios

png_name<-paste(output_dir, "RelativeImportance_Est_", date, ".png", sep="")
png(png_name, width = 6, height = 5, units = 'in', res = 300)

ggplot(Difference_matrix, aes(x=(Time), y=Diffs, fill= scenario_name)) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(name = NULL, labels = c("Soil moisture", "Soil temperature", "Fire"), values = plt.cols.shorter) +
  theme_classic()+
  xlab(NULL)+ylab(expression(Relative~importance~on~regeneration)) + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(breaks=seq(0,1,0.25))+
theme(legend.position = c(0.75, 0.3)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 3, size=12)) #this adjusts the axis title location
dev.off()

#Directory where PNG files are stored
model_dir <- NoFire_dir<-("D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Mar_2023_Sims/Output_Sims_DGS_methods_paper/")
setwd(model_dir)

plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate=T)
})

ggsave("RelativeImport_Regen.TIFF", marrangeGrob(grobs=plots, nrow=2, ncol=1, dpi=300))
dev.off()


#Combining importance graphs

#Directory where soil temp PNG files are stored
model_dir <- "C:/Users/mlucash/Dropbox (University of Oregon)/Lucash_Lab/Lucash/Alaska_Reburns_Project/Manuscripts/DGS_model_description_paper/Figures/AGBGraphs/"  
setwd(model_dir)

#all_maps <- list.files(paste0(model_dir), pattern = ".png", recursive = T, full = T)
plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = T)
})

ggsave("AGB.TIFF", marrangeGrob(grobs=plots, nrow=1, ncol=2, dpi=400))
dev.off() 

######################################################
#Regeneration p-est files

baseline_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_A/DGS-prob-establish-log.csv"))
baseline_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_B/DGS-prob-establish-log.csv"))
baseline_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_C/DGS-prob-establish-log.csv"))
baseline_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_D/DGS-prob-establish-log.csv"))
baseline_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_E/DGS-prob-establish-log.csv"))
ST_limit_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_A/DGS-prob-establish-log.csv"))
ST_limit_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_B/DGS-prob-establish-log.csv"))
ST_limit_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_C/DGS-prob-establish-log.csv"))
ST_limit_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_D/DGS-prob-establish-log.csv"))
ST_limit_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_EST_E/DGS-prob-establish-log.csv"))
SM_limit_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_A/DGS-prob-establish-log.csv"))
SM_limit_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_B/DGS-prob-establish-log.csv"))
SM_limit_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_C/DGS-prob-establish-log.csv"))
SM_limit_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_D/DGS-prob-establish-log.csv"))
SM_limit_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_E/DGS-prob-establish-log.csv"))
NoFire_csv1<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_A/DGS-prob-establish-log.csv"))
NoFire_csv2<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_B/DGS-prob-establish-log.csv"))
NoFire_csv3<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_C/DGS-prob-establish-log.csv"))
NoFire_csv4<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_D/DGS-prob-establish-log.csv"))
NoFire_csv5<-read.csv (paste0 (NoFire_dir, "Calib_Landscape_Scrapple_230302_CC_NoFire_E/DGS-prob-establish-log.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, temp_csv1, temp_csv2, temp_csv3, temp_csv4, temp_csv5, SM_csv1, SM_csv2, SM_csv3, SM_csv4, SM_csv5, NoFire_csv1, NoFire_csv2, NoFire_csv3, NoFire_csv4, NoFire_csv5)

scenario_name<-c(rep("Baseline", replicates*nrow(baseline_csv1)), (rep("Soil_temp", replicates*nrow(ST_limit_csv1))), (rep("Soil_moisture", replicates*nrow(ST_limit_csv5))),(rep("Fire", replicates*nrow(NoFire_csv1))))
Year<-rep(baseline_csv1$Time + year1, n=4)

combined_files<-cbind(scenario_name, combined_csv, Year)

all_data_ribbon<-ddply(combined_files, .(Year, scenario_name), summarize,
                       mean_AGB = mean(AGB),
                       SD_AGB = sd(AGB),
                       SE_AGB = sd(AGB)/sqrt(length(AGB)))

all_data_ribbon$scenario_name <- factor(all_data_ribbon$scenario_name, levels = c("Baseline", "Soil_temp", "Soil_moisture", "Soil_nitrogen", "Fire"))

#This is the ribbon graph of scenarios

plt.cols.short <- c("orange", "forestgreen", "steelblue", "firebrick2") #Number corresponds to scenarios
legend_title<-'Scenarios'

# Plot, 

png_name<-paste(output_dir, "AG_Biomass_GrowthLimits_Est_", date, ".png", sep="")
png(png_name, width = 5, height = 4, units = 'in', res = 300)

ggplot(all_data_ribbon, aes(x=(Year), y=mean_AGB, group=scenario_name)) +
  geom_line(aes(x=(Year), y=mean_AGB, colour=scenario_name), lwd=1.1)+
  geom_ribbon(aes(ymin=mean_AGB-SD_AGB, ymax=mean_AGB+SD_AGB, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Climate Change", "CC, No soil temp limitation", "CC, No soil moisture limitation", "No Fire limitation"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Aboveground~biomass~of~regeneration~(g/m^2))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(limits=c(0,10000), breaks=seq(0, 10000, 2000))+
  theme(legend.position = c(0.3, 0.2)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 2.5)) #this adjusts the axis title location
dev.off()


#Difference values
baseline_AGB<-subset (combined_files, combined_files$scenario_name=="Baseline")
temp_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_temp")
SM_AGB<-subset (combined_files, combined_files$scenario_name=="Soil_moisture")
Fire_AGB<-subset (combined_files, combined_files$scenario_name=="Fire")

#temp_diff<-(temp_AGB$AGB-baseline_AGB$AGB)
#SM_diff<-(SM_AGB$AGB-baseline_AGB$AGB)

difference_function <- function(x,y) {
  ifelse((y-x) > 0, y-x, 0)}

temp_diff<-difference_function(y=temp_AGB$AGB, x=baseline_AGB$AGB)
SM_diff<-difference_function(y=SM_AGB$AGB, x=baseline_AGB$AGB)
Fire_diff<-difference_function(y=Fire_AGB$AGB, x=baseline_AGB$AGB)

Diffs<-(c(temp_diff, SM_diff, Fire_diff))

Time<-rep(baseline_AGB$Year, 3) # 2= SM+ST+Fire
scenario_name<-c(rep("Soil temp", length(temp_diff)),rep("Soil moisture", length(SM_diff)), rep("Fire", length(Fire_diff)))

Difference_matrix<-cbind.data.frame(scenario_name, Time, Diffs)
Difference_matrix$scenario_name <- factor(Difference_matrix$scenario_name, levels = c("Soil moisture", "Soil temp", "Fire"))

myblue <- rgb(70, 130, 180, max = 255, alpha = 130) #125 is midway
mygreen <- rgb(34, 139, 34, max = 255, alpha = 150) #125 is midway
plt.cols.shorter <- c(myblue, mygreen, "firebrick2") #Number corresponds to scenarios

png_name<-paste(output_dir, "RelativeImportance_Est_", date, ".png", sep="")
png(png_name, width = 5, height = 4, units = 'in', res = 300)

ggplot(Difference_matrix, aes(x=(Time), y=Diffs, fill= scenario_name)) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(name = NULL, labels = c("Soil moisture", "Soil temperature", "Fire"), values = plt.cols.shorter) +
  theme_classic()+
  xlab(NULL)+ylab(expression(Relative~importance~on~regeneration)) + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + scale_y_continuous(breaks=seq(0,1,0.25))+
  theme(legend.position = c(0.75, 0.3)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 3)) #this adjusts the axis title location
dev.off()

#Directory where PNG files are stored
model_dir <- "C:/Users/mlucash/Dropbox (University of Oregon)/Lucash_Lab/Lucash/Alaska_Reburns_Project/Manuscripts/DGS_model_description_paper/Figures/Rel_ImportRegenGraphs/"  
setwd(model_dir)

plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})

ggsave("RelativeImport_Regen.TIFF", marrangeGrob(grobs=plots, nrow=1, ncol=2, dpi=400))
dev.off()


#This was the switch to boxplots that I didn't end up using.
Difference_matrix_ST<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_temp")
Difference_matrix_SM<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_moisture")
RelativeDiffs<-cbind.data.frame(Difference_matrix_ST, Difference_matrix_SM)
colnames(RelativeDiffs)<-c("scenario_ST","Time","SM_diffs", "scenario_SM","Time_ST","ST_diffs")
RelativeDiffs$RelativeDiff_SM<-RelativeDiffs$SM_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)
RelativeDiffs$RelativeDiff_ST<-RelativeDiffs$ST_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)

graphing_relativeDiff_Time<-c(RelativeDiffs$Time, RelativeDiffs$Time)
graphing_relativeDiff_Scenario<-c(as.character(RelativeDiffs$scenario_SM), as.character(RelativeDiffs$scenario_ST))
graphing_relativeDiff_Values<-c(RelativeDiffs$RelativeDiff_SM, RelativeDiffs$RelativeDiff_ST)
graphing<-cbind.data.frame(graphing_relativeDiff_Scenario, graphing_relativeDiff_Time, graphing_relativeDiff_Values)
colnames(graphing)<-c("scenario_name","Time","Diffs")
head(graphing)

plot<-ggplot(data = graphing , aes(x = as.factor(Time), y = Diffs, fill = scenario_name)) +
  geom_boxplot()+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)+
  scale_y_continuous(limits = c(0, 1))

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+
  scale_x_discrete( breaks=seq(1990, 2040, 10),labels=c("2000","2010","2020","2030","2040","2050"))

decades<-c(1991, 2000, 2010,2020,2030,2040)
graphing_decades<-subset(graphing, graphing$Time==decades)
plot<-ggplot(data = graphing_decades , aes(x = as.factor(Time), y = Diffs, fill = scenario_name)) +
  geom_boxplot()+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)+
  scale_y_continuous(limits = c(0, 1))

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+
  scale_x_discrete( breaks=seq(1990, 2040, 10),labels=c("1990","2000","2010","2020","2030","2040"))



#Try to look at seasonal differences. Again, I didn't end up using this either.
baseline_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_A/DGS-succession-monthly-log.csv"))
baseline_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_B/DGS-succession-monthly-log.csv"))
baseline_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_C/DGS-succession-monthly-log.csv"))
#nitrogen_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_NoNLimit_A/DGS-succession-monthly-log.csv"))
#nitrogen_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_NoNLimit_B/DGS-succession-monthly-log.csv"))
#nitrogen_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_210327_CC_NoNLimit_C/DGS-succession-monthly-log.csv"))
temp_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_A/DGS-succession-monthly-log.csv"))
temp_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_B/DGS-succession-monthly-log.csv"))
temp_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_STLimit_C/DGS-succession-monthly-log.csv"))
SM_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_A/DGS-succession-monthly-log.csv"))
SM_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_B/DGS-succession-monthly-log.csv"))
SM_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119_CC_SMLimit_EST_C/DGS-succession-monthly-log.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, temp_csv1, temp_csv2, temp_csv3, SM_csv1, SM_csv2, SM_csv3)

scenario_name<-c(rep("Baseline", 3*nrow(baseline_csv1)), (rep("Soil_temp", 3*nrow(temp_csv1))), (rep("Soil_moisture", 3*nrow(SM_csv1))))
Year<-rep(baseline_csv1$Time + year1, n=9)

combined_files<-cbind(scenario_name, combined_csv, Year)

mycyan <- rgb(64, 224, 208, max = 255, alpha = 145) #125 is midway
myorange <- rgb(255, 140, 0, max = 255, alpha = 145) #125 is midway
plt.cols.shorter <- c(mycyan, myorange)

all_data_ribbon_time1<-subset (combined_files, combined_files$Time==1)
all_data_ribbon_time1<-ddply(all_data_ribbon_time1, .(Month, scenario_name), summarize,
                       mean_ANPP = mean(avgNPPtc),
                       SD_ANPP = sd(avgNPPtc),
                       SE_ANPP = sd(avgNPPtc)/sqrt(length(avgNPPtc)))

#This is the ribbon graph of scenarios at time 0
plt.cols.short <- c("grey30", "cyan", "violet") #Number corresponds to scenarios
legend_title<-'Scenarios'

plot<-ggplot(all_data_ribbon_time1, aes(x=(Month), y=mean_ANPP, fill=scenario_name)) +
  geom_line(aes(x=(Month), y=mean_ANPP, colour=scenario_name))+
  ylim(0, 300)+
  geom_ribbon(aes(ymin=mean_ANPP-SD_ANPP, ymax=mean_ANPP+SD_ANPP, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_fill_manual(values = plt.cols.short)+
  labs(x =NULL, y = "ANPP (g/m2/y)", colour="Limiting Factors")+
  theme_classic()


plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)),
        legend.position = "right")+
  scale_x_continuous( breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","June", "July","Aug","Sept","Oct","Nov","Dec"))

ggplot(all_data_ribbon_time1, aes(x=(Month), y=mean_ANPP, fill=scenario_name)) +
  geom_line(aes(x=(Month), y=mean_ANPP, colour=scenario_name))+
  ylim(0, 300)+
  geom_ribbon(aes(ymin=mean_ANPP-SD_ANPP, ymax=mean_ANPP+SD_ANPP, fill=scenario_name), alpha=0.25, show.legend = FALSE)+
  labs(x =NULL, y = "ANPP (g/m2/y)", colour="Limiting Factors")+
  theme_classic()

#Difference values
combined_files_region10<-subset (combined_files, combined_files$ClimateRegionName==10)
combined_files_time1<-subset (combined_files_region10, combined_files_region10$Time==1)

baseline_ANPP<-subset(combined_files_time1, combined_files_time1$scenario_name=="Baseline")
temp_ANPP<-subset (combined_files_time1, combined_files_time1$scenario_name=="Soil_temp")
SM_ANPP<-subset (combined_files_time1, combined_files_time1$scenario_name=="Soil_moisture")

temp_diff<-temp_ANPP$avgNPPtc-baseline_ANPP$avgNPPtc
SM_diff<-SM_ANPP$avgNPPtc-baseline_ANPP$avgNPPtc 
Diffs<-(c(temp_diff, SM_diff))

Month<-rep(1:12, times=6)
scenario_name<-c(rep("Soil_temp", length(temp_diff)),(rep("Soil_moisture", length(SM_diff))))

Difference_matrix<-cbind.data.frame(scenario_name, Month, Diffs)
Difference_matrix$scenario_name <- factor(Difference_matrix$scenario_name, levels = c("Soil_temp", "Soil_moisture"))

Difference_matrix_nonzero<-subset(Difference_matrix, Difference_matrix$Diffs>0)
plot<-ggplot(data = Difference_matrix_nonzero, aes(x = Month, y = Diffs, fill = scenario_name)) +
  geom_bar(position="fill", stat="identity")+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+
  scale_x_continuous( breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","June", "July","Aug","Sept","Oct","Nov","Dec"))

Difference_matrix_ST<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_temp")
Difference_matrix_SM<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_moisture")
RelativeDiffs<-cbind.data.frame(Difference_matrix_ST, Difference_matrix_SM)
colnames(RelativeDiffs)<-c("scenario_SM","Month","SM_diffs", "scenario_ST","Month_ST","ST_diffs")
RelativeDiffs$RelativeDiff_SM<-RelativeDiffs$SM_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)
RelativeDiffs$RelativeDiff_ST<-RelativeDiffs$ST_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)

graphing_relativeDiff_Time<-c(RelativeDiffs$Month, RelativeDiffs$Month)
graphing_relativeDiff_Scenario<-c(as.character(RelativeDiffs$scenario_SM), as.character(RelativeDiffs$scenario_ST))
graphing_relativeDiff_Values<-c(RelativeDiffs$RelativeDiff_ST, RelativeDiffs$RelativeDiff_SM)
graphing<-cbind.data.frame(graphing_relativeDiff_Scenario, graphing_relativeDiff_Time, graphing_relativeDiff_Values)
colnames(graphing)<-c("scenario_name","Month","Diffs")
head(graphing)
tail(graphing)


plt.cols.shorter <- c("cyan", "orange")
all_data_ribbon_diffs<-ddply(graphing, .(Month, scenario_name), summarize,
                       mean_diffs = mean(Diffs),
                       SD_diffs = sd(Diffs),
                       SE_diffs = sd(Diffs)/sqrt(length(Diffs)))
plot<-ggplot(all_data_ribbon_diffs, aes(x=(Month), y=mean_diffs, group=scenario_name)) +
  geom_line(aes(x=(Month), y=mean_diffs, colour=scenario_name))+
  geom_ribbon(aes(ymin=mean_diffs-SD_diffs, ymax=mean_diffs+SD_diffs, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.shorter, name="Limiting Factors") +
  theme_classic()+xlab(NULL)+ylab("Relative importance of different factors on monthly growth") + theme(legend.position = "right")

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 5))+
  scale_x_continuous( breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","June", "July","Aug","Sept","Oct","Nov","Dec"))

#This is the ribbon graph of scenarios at time 50

#Time = 50
all_data_ribbon_time50<-subset (combined_files, combined_files$Time==50)
all_data_ribbon_time50<-ddply(all_data_ribbon_time50, .(Month, scenario_name), summarize,
                              mean_ANPP = mean(avgNPPtc),
                              SD_ANPP = sd(avgNPPtc),
                              SE_ANPP = sd(avgNPPtc)/sqrt(length(avgNPPtc)))

plot<-ggplot(all_data_ribbon_time50, aes(x=(Month), y=mean_ANPP, fill=scenario_name)) +
  geom_line(aes(x=(Month), y=mean_ANPP, colour=scenario_name))+
  ylim(0, 300)+
  geom_ribbon(aes(ymin=mean_ANPP-SD_ANPP, ymax=mean_ANPP+SD_ANPP, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_fill_manual(values = plt.cols.short)+
  labs(x =NULL, y = "ANPP (g/m2/y)", colour="Limiting Factors")+
  theme_classic()

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)),
           legend.position = "right")+
  scale_x_continuous( breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","June", "July","Aug","Sept","Oct","Nov","Dec"))


#Difference values
combined_files_time50<-subset (combined_files_region10, combined_files_region10$Time==50)

baseline_ANPP<-subset(combined_files_time50, combined_files_time50$scenario_name=="Baseline")
temp_ANPP<-subset (combined_files_time50, combined_files_time50$scenario_name=="Soil_temp")
SM_ANPP<-subset (combined_files_time50, combined_files_time50$scenario_name=="Soil_moisture")

temp_diff<-temp_ANPP$avgNPPtc-baseline_ANPP$avgNPPtc
SM_diff<-SM_ANPP$avgNPPtc-baseline_ANPP$avgNPPtc 
Diffs<-(c(temp_diff, SM_diff))

Month<-rep(1:12, times=6)
scenario_name<-c(rep("Soil_temp", length(temp_diff)),(rep("Soil_moisture", length(SM_diff))))

Difference_matrix<-cbind.data.frame(scenario_name, Month, Diffs)
Difference_matrix$scenario_name <- factor(Difference_matrix$scenario_name, levels = c("Soil_temp", "Soil_moisture"))

Difference_matrix_nonzero<-subset(Difference_matrix, Difference_matrix$Diffs>0)
plot<-ggplot(data = Difference_matrix_nonzero, aes(x = Month, y = Diffs, fill = scenario_name)) +
  geom_bar(position="fill", stat="identity")+
  theme_classic()+
  labs(x =NULL, y = "Relative importance of different factors on aboveground biomass", fill="Scenario Name")+
  scale_fill_manual(name="Limiting factor", values = plt.cols.shorter)

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 8))+
  scale_x_continuous( breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","June", "July","Aug","Sept","Oct","Nov","Dec"))

Difference_matrix_ST<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_temp")
Difference_matrix_SM<-subset (Difference_matrix, Difference_matrix$scenario_name=="Soil_moisture")
RelativeDiffs<-cbind.data.frame(Difference_matrix_ST, Difference_matrix_SM)
colnames(RelativeDiffs)<-c("scenario_SM","Month","SM_diffs", "scenario_ST","Month_ST","ST_diffs")
RelativeDiffs$RelativeDiff_SM<-RelativeDiffs$SM_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)
RelativeDiffs$RelativeDiff_ST<-RelativeDiffs$ST_diffs/(RelativeDiffs$SM_diffs+RelativeDiffs$ST_diffs)

graphing_relativeDiff_Time<-c(RelativeDiffs$Month, RelativeDiffs$Month)
graphing_relativeDiff_Scenario<-c(as.character(RelativeDiffs$scenario_SM), as.character(RelativeDiffs$scenario_ST))
graphing_relativeDiff_Values<-c(RelativeDiffs$RelativeDiff_SM, RelativeDiffs$RelativeDiff_ST)
graphing<-cbind.data.frame(graphing_relativeDiff_Scenario, graphing_relativeDiff_Time, graphing_relativeDiff_Values)
colnames(graphing)<-c("scenario_name","Month","Diffs")
head(graphing)
tail(graphing)


plt.cols.shorter <- c("cyan", "orange")
all_data_ribbon_diffs<-ddply(graphing, .(Month, scenario_name), summarize,
                             mean_diffs = mean(Diffs),
                             SD_diffs = sd(Diffs),
                             SE_diffs = sd(Diffs)/sqrt(length(Diffs)))
plot<-ggplot(all_data_ribbon_diffs, aes(x=(Month), y=mean_diffs, group=scenario_name)) +
  geom_line(aes(x=(Month), y=mean_diffs, colour=scenario_name))+
  geom_ribbon(aes(ymin=mean_diffs-SD_diffs, ymax=mean_diffs+SD_diffs, fill=scenario_name), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.shorter, name="Limiting Factors") +
  theme_classic()+xlab(NULL)+ylab("Relative importance of different factors on monthly growth") + theme(legend.position = "right")

plot+theme(axis.title.y = element_text(margin=margin(0,0,0,20)))+
  theme(axis.title.y = element_text(vjust = 5))+
  scale_x_continuous( breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","June", "July","Aug","Sept","Oct","Nov","Dec"))
