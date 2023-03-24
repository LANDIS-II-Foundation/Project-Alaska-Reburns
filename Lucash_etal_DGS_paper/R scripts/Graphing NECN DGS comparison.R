#This is the R script that I used to compare the output between DGS and NECN

#library(raster)
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(patchwork)
library(plotrix)
library(stringr)
#library(hrbrthemes)
library(tidyr)
library(lubridate)
#library(conflicted)

date<-Sys.Date()

calibration_dir<-"D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Final Round_SingleCell/Observed/"
SmithLake2_obs<-read.csv (paste0 (calibration_dir, "SmithLake2/SmithLake2_observed-soil moi temp Comparison.csv"))
Birch_obs_hourly<-read.csv (paste0 (calibration_dir, "UP1A/UP1A_observed-soil moi temp Comparison.csv"))

Birch_obs<- Birch_obs_hourly%>%
  group_by(ActualYear, Month, Day) %>% 
  dplyr::summarise(VWC = mean(VWC, na.rm = T), soilTemp = mean(SoilTemp, na.rm = T)) 

Burned_obs<- read.csv(paste0 (calibration_dir, "US_Rpf/US-Rpf_observed-soil moi temp Comparison.csv"))
combined_csv_obs<-rbind(SmithLake2_obs[,c("ActualYear","Month","Day","VWC","soilTemp")], Birch_obs, Burned_obs[,c("ActualYear","Month","Day","VWC","soilTemp")])
Scenario<-c(rep("Observed", nrow(SmithLake2_obs)), rep("Observed", nrow(Birch_obs)), rep("Observed", nrow(Burned_obs)))
Scenario_rep<-c(rep("Observed-SmithLake2", nrow(SmithLake2_obs)), rep("Observed-UP1A", nrow(Birch_obs)), rep("Observed-US-Rpf", nrow(Burned_obs)))
Sites<-c(rep("SmithLake2", nrow(SmithLake2_obs)), rep("UP1A", nrow(Birch_obs)), rep("US-Rpf", nrow(Burned_obs)))
combined_csv_obs_scenarios<-cbind(combined_csv_obs, Scenario, Scenario_rep, Sites)

combined_csv_obs_scenarios_select <- combined_csv_obs_scenarios %>% dplyr::select(Scenario, Scenario_rep, Sites, ActualYear, Month, VWC, soilTemp)
colnames(combined_csv_obs_scenarios_select)<-c("Scenario", "Scenario_rep","Sites","ActualYear", "Month", "VWC","SoilTemperature")

model_dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Final Round_SingleCell/"
#model_dir <-"E:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_FinalTHU/Analysis_by_THU/"
setwd(model_dir)

output_dir <-paste0(model_dir, "/Output_Sims_DGS_methods_paper/")
active_cells<-99465

SoilDepth<-77 #max soil depth in cm for SmithLake2 and UP1A
SoilDepth_burned<-201 #max soil depth in cm, burned site

#calibrate csv files

DGS_SmithLake2_csv<-read.csv (paste0 (model_dir, "DGS/SmithLake2/DGS-calibrate-log.csv"))
DGS_SmithLake2_csv$VWC<-DGS_SmithLake2_csv$availableWater
DGS_SmithLake2_csv2 <- DGS_SmithLake2_csv %>% dplyr::select(Year, Month, VWC, soilTemp)
DGS_SmithLake2_csv2$ActualYear<-(DGS_SmithLake2_csv2$Year + 2013)
DGS_Birch_csv<-read.csv (paste0 (model_dir, "DGS/UP1A_Birch/DGS-calibrate-log.csv"))
DGS_Birch_csv$VWC<-DGS_Birch_csv$availableWater
DGS_Birch_csv2 <- DGS_Birch_csv %>% dplyr::select(Year, Month, VWC, soilTemp)
DGS_Birch_csv2$ActualYear<-(DGS_Birch_csv2$Year + 2008)
DGS_Burn_csv<-read.csv (paste0 (model_dir, "DGS/US_Rpf_BurnedSpruce/DGS-calibrate-log.csv"))
DGS_Burn_csv$VWC<-DGS_Burn_csv$availableWater
DGS_Burn_csv2 <- DGS_Burn_csv %>% dplyr::select(Year, Month, VWC, soilTemp)
DGS_Burn_csv2$ActualYear<-(DGS_Burn_csv2$Year + 2011)
NECN_SmithLake2_csv<-read.csv (paste0 (model_dir, "NECN/SmithLake2_NECN/NECN-calibrate-log.csv"))
NECN_SmithLake2_csv$VWC<-NECN_SmithLake2_csv$AvailableWater/SoilDepth
NECN_SmithLake2_csv2 <- NECN_SmithLake2_csv %>% dplyr::select(Year, Month, VWC, SoilTemperature)
NECN_SmithLake2_csv2$ActualYear<-(NECN_SmithLake2_csv2$Year + 2013)
NECN_Birch_csv<-read.csv (paste0 (model_dir, "NECN/UP1A_Birch_NECN/NECN-calibrate-log.csv"))
NECN_Birch_csv$VWC<-NECN_Birch_csv$AvailableWater/SoilDepth
NECN_Birch_csv2 <- NECN_Birch_csv %>% dplyr::select(Year, Month, VWC, SoilTemperature)
NECN_Birch_csv2$ActualYear<-(NECN_Birch_csv2$Year + 2008)
NECN_Burn_csv<-read.csv (paste0 (model_dir, "NECN/US_Rpf_BurnedSpruce_NECN/NECN-calibrate-log.csv"))
NECN_Burn_csv$VWC<-NECN_Burn_csv$AvailableWater/SoilDepth_burned
NECN_Burn_csv2 <- NECN_Burn_csv %>% dplyr::select(Year, Month, VWC, SoilTemperature)
NECN_Burn_csv2$ActualYear<-(NECN_Burn_csv2$Year + 2011)

combined_csv1<-rbind(NECN_SmithLake2_csv2, NECN_Birch_csv2, NECN_Burn_csv2)
combined_csv2<-rbind(DGS_SmithLake2_csv2, DGS_Birch_csv2, DGS_Burn_csv2)
colnames(combined_csv2)<-c("Year", "Month","VWC","SoilTemperature", "ActualYear")
combined_csv<-rbind(combined_csv1, combined_csv2)

Scenario<-c(rep("NECN", nrow(NECN_SmithLake2_csv)), rep("NECN", nrow(NECN_Birch_csv)), rep("NECN", nrow(NECN_Burn_csv)), rep("DGS", nrow(DGS_SmithLake2_csv)), rep("DGS", nrow(DGS_Birch_csv)), rep("DGS", nrow(DGS_Burn_csv)))
Scenario_rep<-c(rep("SmithLake2-NECN",nrow(NECN_SmithLake2_csv)), rep("UP1A-NECN",nrow(NECN_Birch_csv)), rep("US-Rpf-NECN", nrow(NECN_Burn_csv)), rep("SmithLake2-DGS",nrow(DGS_SmithLake2_csv)), rep("UP1A-DGS",nrow(DGS_Birch_csv)), rep("US-Rpf-DGS", nrow(DGS_Burn_csv)))
Sites<-c(rep("SmithLake2",nrow(NECN_SmithLake2_csv)), rep("UP1A",nrow(NECN_Birch_csv)), rep("US-Rpf", nrow(NECN_Burn_csv)), rep("SmithLake2",nrow(DGS_SmithLake2_csv)), rep("UP1A",nrow(DGS_Birch_csv)), rep("US-Rpf", nrow(DGS_Burn_csv)))

combined_files_final<-cbind(Scenario, Scenario_rep, Sites, combined_csv[,c("ActualYear", "Month","VWC", "SoilTemperature")])
combined_obs_simulated<-rbind(combined_files_final, combined_csv_obs_scenarios_select)

write.csv(combined_obs_simulated, "DGS_vs_NECN_obs_comparison_092722.csv")
# Plot to make 
plt.cols.long<-c("red","orange","darkorange","gold", "orangered1", "blue","steelblue","cyan", "darkslategray2", "darkslategray4")

all_data_ribbon<-ddply(combined_obs_simulated, .(ActualYear, Month, Sites, Scenario), dplyr::summarize,
                       mean_SM = mean(VWC),
                       SD_SM= sd(VWC),
                       SE_SM = sd(VWC)/sqrt(length(VWC)),
                       mean_ST = mean(SoilTemperature),
                       SD_ST = sd(SoilTemperature),
                       SE_ST = sd(SoilTemperature)/sqrt(length(SoilTemperature)))

all_data_ribbon_date_adj<-all_data_ribbon %>% dplyr::mutate('Date' = make_date(year = ActualYear, month = Month))
all_data_ribbon_date<-all_data_ribbon_date_adj %>% dplyr::filter(Sites == "US-Rpf" & ActualYear<2017| Sites == "UP1A" & ActualYear<2012| Sites == "SmithLake2" & ActualYear<2019)

all_data_ribbon_date$Sites <- factor(all_data_ribbon_date$Sites, levels = c("US-Rpf", "UP1A","SmithLake2"))

plt.cols.short <- c("dodgerblue", "salmon", "black") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

SM<-ggplot(all_data_ribbon_date, aes(x=(Date), y=mean_SM, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(Date), y=mean_SM, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_SM-SD_SM, ymax=mean_SM+SD_SM, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  ylim(0,0.5) +
  theme_classic()+xlab(NULL)+ylab(expression(theta[VWC]))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  scale_x_continuous(breaks=seq(1990, 2020, 2))+
  theme(legend.position = "right", legend.text=element_text(size=10), legend.title=element_text(size=11))+
  guides(colour = guide_legend(override.aes = list(size=1.5), title="Scenario"))+
  facet_wrap (~Sites, nrow=3)
SM

ST<-ggplot(all_data_ribbon_date, aes(x=(Date), y=mean_ST, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(Date), y=mean_ST, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_ST-SD_ST, ymax=mean_ST+SD_ST, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  #ylim(0,0.5) +
  theme_classic()+xlab(NULL)+ylab(Soil~temperature)+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  #scale_x_continuous(breaks=seq(1990, 2040, 1))+
  theme(legend.position="NULL")+
  #guides(colour = guide_legend(override.aes = list(size=1.5), title="Scenario"))+
  facet_wrap (~Sites, nrow=3)

ST
#Switch to reclass extent

png_name<-paste(output_dir, "Stacked_DGS_NECN_Graphs.png", sep="")
StackedPlot_DGSNECN<-ST+SM
ggsave(filename=png_name, plot = StackedPlot_Veg, dpi = 400)

write.csv(all_data_ribbon_date, "DGS_vs_NECN_obs_summary_092722.csv")

######################################################################################
#Calculating values in the results section of the paper.

#First describing soil temperature

#Line 703. NECN simulated a 49.5 ?C difference in minimum and maximum monthly soil temperature influencing tree growth
min_NECN<-all_data_ribbon_date %>% 
              dplyr::filter(Scenario == "NECN")%>%
              summarize(min=min(mean_ST))

max_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "NECN")%>%
  summarize(max=max(mean_ST))

max_NECN-min_NECN  #value is 49.5

#L704. observed variation across the field sites was only X 

min<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed")%>%
  summarize(min=min(mean_ST, na.rm=T))

max<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed")%>%
  summarize(max=max(mean_ST, na.rm=T))

obs_range<-max-min  #value is 19.8


#L705. DGS variation across the field sites was only 21.7

min_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS")%>%
  summarize(min=min(mean_ST, na.rm=T))

max_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS")%>%
  summarize(max=max(mean_ST, na.rm=T))

DGS_range<-max_DGS-min_DGS  #value is 21.7

#L706. DGS was only 10 % higher than observed range
(DGS_range-obs_range)/obs_range

#L707. Across all sites, NECN overestimated soil temperature in the summer months, suggesting it was X ?C, 

summer_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "NECN" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
summer_NECN  #15.5

#L708. compared to the simulated temperatures of 5.3 ?C by DGS 

summer_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
summer_DGS  #15.4

# L709. and the observed value of 5.4 ?C. 

summer<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
summer  #5.4

#NECN underestimated soil temperature in the winter months, suggesting it was X ?C  colder than the 
winter_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "NECN" & (Month == c(11,12,1,2)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
winter_NECN  #-13.2

#simulated temperatures of X ?C by DGS and 
winter_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS" & (Month == c(11,12,1,2)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
winter_DGS  #-1.2

#the observed value of -X  ?C.

winter<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed" & (Month == c(11,12,1,2)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
winter  # -2.2

# L710. 11 degrees colder

winter_NECN-winter

# L711. 1 degrees diff

winter_DGS-winter

# This was particularly notable in the spruce site where simulated soil temperature 
# in NECN ranged from -16.2 in the winter to 15.2 in the summer (L714)

winter_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "NECN" & (Month == c(11, 12, 1, 2)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
winter_NECN

summer_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "NECN" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
summer_NECN

#observed values ranged only from -0.4 to 2.6 (L714)

winter<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "Observed" & (Month == c(11, 12, 1, 2)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
winter

summer<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "Observed" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
summer

# L715. DGS performed much better -2.6 in the summer, 4.1 in the winter
winter_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "DGS" & (Month == c(11, 12, 1, 2)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
winter_DGS

summer_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "DGS" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_ST, na.rm=T))
summer_DGS

#Then describing soil moisture ########################

# L719. NECN simulated soil moisture that was 89 (SmithLake2), 60 (UP1A) and 89% (burned) lower than observed values. 
mean_SM<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed" & Sites == "SmithLake2")%>%
  summarize(mean=mean(mean_SM, na.rm=T))
mean_SM

mean_SM_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "NECN" & Sites == "SmithLake2")%>%
  summarize(mean=mean(mean_SM, na.rm=T))

mean_SM_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS" & Sites == "SmithLake2")%>%
  summarize(mean=mean(mean_SM, na.rm=T))

#100*abs(mean_SM_NECN- mean_SM)/((mean_SM_NECN+ mean_SM)/2)
100*(mean_SM_NECN- mean_SM)/mean_SM
100*(mean_SM_DGS- mean_SM)/mean_SM

mean_SM<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed" & Sites == "UP1A")%>%
  summarize(mean=mean(mean_SM, na.rm=T))
mean_SM

mean_SM_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "NECN" & Sites == "UP1A")%>%
  summarize(mean=mean(mean_SM, na.rm=T))

mean_SM_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS" & Sites == "UP1A")%>%
  summarize(mean=mean(mean_SM, na.rm=T))

#100*abs(mean_SM_NECN- mean_SM)/((mean_SM_NECN+ mean_SM)/2)
100*(mean_SM_NECN- mean_SM)/mean_SM
100*(mean_SM_DGS- mean_SM)/mean_SM

mean_SM<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed" & Sites == "US-Rpf")%>%
  summarize(mean=mean(mean_SM, na.rm=T))
mean_SM

mean_SM_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS" & Sites == "US-Rpf")%>%
  summarize(mean=mean(mean_SM, na.rm=T))

#100*abs(mean_SM_NECN- mean_SM)/((mean_SM_NECN+ mean_SM)/2)
100*(mean_SM_NECN- mean_SM)/mean_SM
100*(mean_SM_DGS- mean_SM)/mean_SM

# Annual mean soil moisture was 0.16 for DGS
mean_SM_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "DGS")%>%
  summarize(mean=mean(mean_SM, na.rm=T))
mean_SM_DGS

# 0.03 for NECN, and (L720)

mean_SM_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "NECN")%>%
  summarize(mean=mean(mean_SM, na.rm=T))
mean_SM_NECN

# 0.20 across all field sites. (L722)

mean_SM<-all_data_ribbon_date %>% 
  dplyr::filter(Scenario == "Observed")%>%
  summarize(mean=mean(mean_SM, na.rm=T))
mean_SM

(mean_SM_DGS-mean_SM)/mean_SM
(mean_SM_NECN-mean_SM)/mean_SM

#At the burned site, soil moisture in NECN ranged from 0.008 in the winter (L723)
winter_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "US-Rpf"& Scenario == "NECN" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter_NECN

#to 0.03 in the summer, while (L723)
summer_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "US-Rpf"& Scenario == "NECN" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer_NECN

#observed values ranged only from 0.05 (winter) to (L724)
winter<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "US-Rpf"& Scenario == "Observed" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter

#to 0.28 (summer). L724

summer<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "US-Rpf"& Scenario == "Observed" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer

#Soil moisture in DGS ranged from 0.1 in the winter to (L724)
winter_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "US-Rpf"& Scenario == "DGS" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter_DGS

#0.2 in the summer in the burned site. (L725)

summer_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "US-Rpf"& Scenario == "DGS" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer_DGS

#summarize burned site

#NECN simulated soil moisture that was 87% lower than observations in the winter 
100*(winter_NECN-winter)/winter

#NECN simulated soil moisture that was 88% lower than observations in the summer 
100*(summer_NECN-summer)/summer

#DGS simulated soil moisture that was 145% lower than observations in the winter
100*(winter_DGS-winter)/winter

#DGS simulated soil moisture that was 26% lower than observations in the summer 
100*(summer_DGS-summer)/summer


#At the spruce site, soil moisture in NECN ranged from 0.017 in the winter (L727)
winter_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "NECN" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter_NECN

#to 0.1 in the summer, while (0.07)
summer_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "NECN" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer_NECN

#observed values ranged only from 0.05 (winter) to 
winter<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "Observed" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter

#to X (summer). 

summer<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "Observed" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer

#Soil moisture in DGS ranged from 0.07 in the winter to (L728)
winter_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "DGS" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter_DGS

#0.2 in the summer  (L728)

summer_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "SmithLake2"& Scenario == "DGS" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer_DGS

#summarize spruce site

#NECN simulated soil moisture that was 90% lower than observations in the winter 
100*(winter_NECN-winter)/winter

#NECN simulated soil moisture that was 82% lower than observations in the summer 
100*(summer_NECN-summer)/summer

#DGS simulated soil moisture that was 64% lower than observations in the winter
100*(winter_DGS-winter)/winter

#DGS simulated soil moisture that was 42% lower than observations in the summer 
100*(summer_DGS-summer)/summer

#At the UP1A site, soil moisture in NECN ranged from 0.02 in the winter ()
winter_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "UP1A"& Scenario == "NECN" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter_NECN

#to 0.09 in the summer, while (L726)
summer_NECN<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "UP1A"& Scenario == "NECN" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer_NECN

#observed values ranged only from 0.08 (winter) to (L727)
winter<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "UP1A"& Scenario == "Observed" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter

#to 0.10 (summer). L724

summer<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "UP1A"& Scenario == "Observed" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer

#Soil moisture in DGS ranged from 0.1 in the winter to (L727)
winter_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "UP1A"& Scenario == "DGS" & (Month == c(1,2, 12, 11)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
winter_DGS

#0.165 in the summer  (L725)

summer_DGS<-all_data_ribbon_date %>% 
  dplyr::filter(Sites == "UP1A"& Scenario == "DGS" & (Month == c(6,7,8)))%>%
  summarize(mean=mean(mean_SM, na.rm=T))
summer_DGS

#NECN  simulated soil moisture that was 70% lower than observations in the summer 
100*(winter_NECN-winter)/winter

#NECN simulated soil moisture that was 12% lower than observations in the summer (L727)
100*(summer_NECN-summer)/summer

#DGS simulated soil moisture that was 23% higher than observations in the summer (L727)
100*(winter_DGS-winter)/winter

#DGS simulated soil moisture that was 65% higher than observations in the summer (L727)
100*(summer_DGS-summer)/summer





