#This is the R script that I used to graph ABG from the csv file and the Reclass results from an intermediate csv file.

library(raster)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(plyr)
library(dplyr)
library(patchwork)
library(plotrix)
library(stringr)
library(hrbrthemes)
library(tidyr)
library(scales)

date<-Sys.Date()

model_dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"
#model_dir <-"E:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_FinalTHU/Analysis_by_THU/"
setwd(model_dir)

output_dir <-paste0(model_dir, "Output_Sims_DGS_methods_paper/")
active_cells<-99465


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

Scenario<-c(rep("Historical", 5*nrow(baseline_csv1)), (rep("Climate Change", 5*nrow(CC_csv1))))
Scenario_rep<-c((rep("Hist_rep1",51)), (rep("Hist_rep2",51)), (rep("Hist_rep3", 51)), (rep("Hist_rep4", 51)), (rep("Hist_rep5", 51)), (rep("CC_rep1", 51)),(rep("CC_rep2", 51)),(rep("CC_rep3", 51)),(rep("CC_rep4", 51)),(rep("CC_rep5", 51)))
ActualYear<-rep(baseline_csv1$Time + year1, times=5)

combined_files<-cbind(Scenario, combined_csv, ActualYear, Scenario_rep)

# Plot to make 
plt.cols.long<-c("red","orange","darkorange","gold", "orangered1", "blue","steelblue","cyan", "darkslategray2", "darkslategray4")

all_data_ribbon<-ddply(combined_files, .(ActualYear, Scenario), summarize,
                       mean_AGB = mean(AGB),
                       SD_AGB = sd(AGB),
                       SE_AGB = sd(AGB)/sqrt(length(AGB)),
                       mean_SOC = mean(SOMTC),
                       SD_SOC = sd(SOMTC),
                       SE_SOC = sd(SOMTC)/sqrt(length(SOMTC)))

all_data_ribbon$AGB_Carbon<-all_data_ribbon$mean_AGB*0.47
all_data_ribbon$AGB_SD_Carbon<-all_data_ribbon$SD_AGB*0.47

all_data_ribbon$Scenario <- factor(all_data_ribbon$Scenario, levels = c("Historical", "Climate Change"))

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

AGB<-ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_AGB, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(ActualYear), y=mean_AGB, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_AGB-SD_AGB, ymax=mean_AGB+SD_AGB, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  ylim(0,12000) +
  theme_classic()+xlab(NULL)+ylab(expression(Aboveground~biomass~(g~m^"-2")))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12), axis.text.x = element_blank())+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  theme(legend.position = c(0.77,0.9), legend.text=element_text(size=9), legend.title=element_text(size=11))+
  guides(colour = guide_legend(override.aes = list(size=1.5), title="Scenario"))

#Switch to reclass extent

final_reclass_matrix_H<-read.csv(paste0(output_dir,"Reclass/Historical_Reclass_over_time.csv"))
all_data_ribbon_reclass_historical<-ddply(final_reclass_matrix_H, .(Time, Scenario), summarize,
                                  mean_Conifer = mean(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                                  SD_Conifer = sd(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                                  mean_Hard = mean(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)),
                                  SD_Hard = sd(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)))

final_reclass_matrix_CC<-read.csv(paste0(output_dir,"Reclass/ClimateChange_Reclass_over_time.csv"))
all_data_ribbon_reclass_CC<-ddply(final_reclass_matrix_CC, .(Time, Scenario), summarize,
                          mean_Conifer = mean(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                          SD_Conifer = sd(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                          mean_Hard = mean(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)),
                          SD_Hard = sd(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)))

all_data_ribbon_reclass<-rbind(all_data_ribbon_reclass_historical, all_data_ribbon_reclass_CC)

year1<-1989
all_data_ribbon_reclass$ActualYear<-rep(all_data_ribbon_reclass$Time + year1)

all_data_ribbon_reclass_conifer<-all_data_ribbon_reclass[,c(1:4,7)]
all_data_ribbon_reclass_conifer$ForestType<-rep("Conifer", times=nrow(all_data_ribbon_reclass_conifer))
colnames(all_data_ribbon_reclass_conifer)<-c("Time", "Scenario", "mean_Percent", "SD_Percent", "ActualYear", "ForestType")
all_data_ribbon_reclass_hard_columns<-all_data_ribbon_reclass[,c(5:6)]
all_data_ribbon_reclass_ID<-all_data_ribbon_reclass[,c(1:2)]
all_data_hard<-cbind(all_data_ribbon_reclass_ID, all_data_ribbon_reclass_hard_columns, all_data_ribbon_reclass[,c(7)])
all_data_hard$ForestType<-rep("Hardwood", times=nrow(all_data_hard))
colnames(all_data_hard)<-c("Time", "Scenario", "mean_Percent", "SD_Percent", "ActualYear", "ForestType")

all_data_ribbon_reclass_plotting<-rbind(all_data_ribbon_reclass_conifer, all_data_hard)
all_data_ribbon_reclass_plotting$CombinedScenario<-str_c(all_data_ribbon_reclass_plotting$Scenario, '- ', all_data_ribbon_reclass_plotting$ForestType)

plt.cols.short <- c("forestgreen", "darkorange", "seagreen2","red") #Number corresponds to scenarios
all_data_ribbon_reclass_plotting$CombinedScenario <- factor(all_data_ribbon_reclass_plotting$CombinedScenario, levels = c("Historical- Conifer", "Climate Change- Conifer", "Historical- Hardwood", "Climate Change- Hardwood"))
legend_title<-"Scenario- Forest Type"

Extent<-ggplot(all_data_ribbon_reclass_plotting, aes(x=(ActualYear))) +
  geom_line(aes(x=(ActualYear), y=mean_Percent, colour=CombinedScenario))+
  geom_ribbon(aes(ymin=mean_Percent-SD_Percent, ymax=mean_Percent+SD_Percent, fill=CombinedScenario), alpha=0.4, show.legend = FALSE)+
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab("Years")+ylab("Percent of Landscape (%)") +
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  scale_y_continuous(limits=c(0,1))+
  theme(legend.position = c(0.8,0.85), legend.text=element_text(size=9), legend.title=element_text(size=11))+
  guides(colour = guide_legend(override.aes = list(size=1.5), title="Scenario- Forest type"))
Extent


png_name<-paste(output_dir, "Stacked_BiomassExtent_Graphs.png", sep="")
StackedPlot_Veg<-AGB/Extent
ggsave(filename=png_name, plot = StackedPlot_Veg, dpi = 400)

##################################Output Species Biomass Loop#############################
#Now I read in all the LANDIS maps.  For species looping, I used the truncated species name (8 letters) because it matches the filenames in LANDIS.
Scenario_LUT <- read.csv ("Scenarios_DGS_Paper_NoLimits.csv")
scen_list<-(Scenario_LUT[1:10,1])

Time_unique<-c(0:50)

spp_file<-read.csv(paste0(model_dir, "Species_DGS_Paper.csv"))
spplist<-spp_file[,1]

#Set up a null matrix
LANDIS_spp_output_matrix<-NULL
for (scenario in 1:length(scen_list)) {
  each_scenario<-(scen_list[scenario])
  
  for (m in 1:length(Time_unique)){   # for each scenario and replicate, compiles data into a single data frame  
    each_time<-(Time_unique[m])
    
    for (s in 1:length(spplist)){#for each species...
      each_spp<-(spplist[s])
      spp_LANDIS_all<-as.data.frame(raster(paste(model_dir,each_scenario,"/biomass_rp/", each_spp,"-", each_time,".img",sep="")))#LANDIS unique spp biomass.
      colnames(spp_LANDIS_all)<-c("LANDIS_Biomass")
      spp_LANDIS_all[is.na(spp_LANDIS_all)]<-0 
      spp_LANDIS<-subset(spp_LANDIS_all, spp_LANDIS_all$LANDIS_Biomass>0)
      avg_biomass<-mean(as.numeric(spp_LANDIS$LANDIS))   #Units of g/m2
      extent <-length(as.numeric(spp_LANDIS$LANDIS))*2.25  #number of cells, corrected for resolution = Total hectares of each species
      Total_biomass <-avg_biomass*extent #Total hectares of each species
      avg_landscape_biomass<-Total_biomass/active_cells
      SE_avg_biomass <-std.error(as.numeric(spp_LANDIS$LANDIS))
      LANDIS_spp_output_row<-cbind.data.frame(each_scenario, each_time, each_spp, avg_landscape_biomass, extent, Total_biomass, avg_biomass)
      LANDIS_spp_output_matrix<-rbind(LANDIS_spp_output_matrix, LANDIS_spp_output_row)
    } #closes species loop
  } #closes time loop
} #closes scenario loop
colnames(LANDIS_spp_output_matrix)<-c("Scenario_Folder", "Time", "Species", "Avg_Biomass_gm2", "Extent_ha", "Total_Biomass_g", "SE_AvgBiomass")

#useful if you want to have a LUT with scenarios
output_summary_final <- left_join(LANDIS_spp_output_matrix, Scenario_LUT, by=c("Scenario_Folder"="Folder"))

head(output_summary_final)

graphing_matrix<-left_join(output_summary_final, spp_file, by=c("Species"="MapName"))
head(graphing_matrix)

# Location for putting graphs
#output_dir<-paste(sim_dir, "Output_nc_landscape/", sep="")

write.csv(graphing_matrix, file=paste0(model_dir,"SppBiomass_summary_09272022.csv"))

####  Graphs  ##############

graphing_matrix<-read.csv(paste0(model_dir,"SppBiomass_summary_09272022.csv"))

graphing_matrix$Scenario <- factor(graphing_matrix$Scenario, levels = c("Historical", "Climate change"))
graphing_matrix$Name <- factor(graphing_matrix$Name, levels = c("Black spruce", "White spruce", "Tamarack","Paper birch", "Alder", "Balsam poplar","Quaking aspen","Shrub birch", "Willow", "Sphagnum", "Feathermoss","Turfmoss"))
colnames(graphing_matrix)[4] <- "SpeciesCombined"
colnames(graphing_matrix)[10] <- "Species"
graphing_matrix$ActualYear<-graphing_matrix$Time+1990

species_biomass<-ggplot(graphing_matrix, aes(fill=Species, y=Avg_Biomass_gm2, x=ActualYear)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("darkgreen","forestgreen","yellow", "orange1", "firebrick4", "firebrick1", "lightpink1", "dodgerblue", "dodgerblue4", "greenyellow","darkolivegreen4", "yellowgreen")) +
  scale_y_continuous(labels=comma)+
  theme_classic() +
  xlab(NULL)+
  ylab(expression(Aboveground~biomass~(g~m^"-2")))+
  facet_wrap(~Scenario, ncol=2)+
  theme(axis.title.y=element_text(hjust=0.5))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  #scale_x_continuous(breaks=seq(1990, 2040, 10))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  #theme(axis.title.x = element_text(margin = margin(t = 15, b=-0),size=12))+
  theme(legend.position = "right", legend.text=element_text(size=10), legend.title=element_text(size=11))
species_biomass 

species_extent<-ggplot(graphing_matrix, aes(fill=Species, y=Extent_ha, x=ActualYear)) + 
  geom_bar(position="stack", stat="identity", show.legend = FALSE) +
  scale_fill_manual(values=c("darkgreen","forestgreen","yellow", "orange1", "firebrick4", "firebrick1", "lightpink1", "dodgerblue", "dodgerblue4", "greenyellow","darkolivegreen4", "yellowgreen")) +
  theme_classic() +
  xlab("Years")+
  scale_y_continuous(labels=comma)+
  ylab(expression(Extent~(ha)))+
  facet_wrap(~Scenario, ncol=2)+
  theme(axis.title.y=element_text(hjust=0.5))+
  theme(axis.title.x=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  theme(axis.title.x = element_text(margin = margin(t = 15, b=-0),size=12))
#theme(legend.position = "right", legend.text=element_text(size=10), legend.title=element_text(size=11))
species_extent 

#plt.cols.short <- gg_color_hue(number_species) #Number corresponds to species/scenarios

graphing_matrix_noMoss<-graphing_matrix[which(graphing_matrix$Forest.Type=="Conifer"| graphing_matrix$Forest.Type=="Hardwood"),]
  
graphing_matrix_noMoss$Scenario <- factor(graphing_matrix_noMoss$Scenario, levels = c("Historical", "Climate change"))
graphing_matrix_noMoss$Species <- factor(graphing_matrix_noMoss$Species, levels = c("Black spruce", "White spruce", "Tamarack","Paper birch", "Alder", "Balsam poplar","Quaking aspen","Shrub birch", "Willow"))

graphing_matrix_noMoss$ActualYear<-graphing_matrix_noMoss$Time+1990


species_biomass<-ggplot(graphing_matrix_noMoss, aes(fill=Species, y=Avg_Biomass_gm2, x=ActualYear)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("forestgreen","green","yellow", "orange1", "firebrick4", "firebrick1", "lightpink1", "dodgerblue", "dodgerblue4")) +
  scale_y_continuous(labels=comma)+
  theme_classic() +
  xlab(NULL)+
  ylab(expression(Aboveground~biomass~(g~m^"-2")))+
  facet_wrap(~Scenario, ncol=2)+
  theme(axis.title.y=element_text(hjust=0.5))+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())+
  #scale_x_continuous(breaks=seq(1990, 2040, 10))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  #theme(axis.title.x = element_text(margin = margin(t = 15, b=-0),size=12))+
  theme(legend.position = "right", legend.text=element_text(size=10), legend.title=element_text(size=11))
species_biomass 

species_extent<-ggplot(graphing_matrix_noMoss, aes(fill=Species, y=Extent_ha, x=ActualYear)) + 
  geom_bar(position="stack", stat="identity", show.legend = FALSE) +
  scale_fill_manual(values=c("forestgreen","green","yellow", "orange1", "firebrick4", "firebrick1", "lightpink1", "dodgerblue", "dodgerblue4")) +
  theme_classic() +
  xlab("Years")+
  scale_y_continuous(labels=comma)+
  ylab(expression(Extent~(ha)))+
  facet_wrap(~Scenario, ncol=2)+
  theme(axis.title.y=element_text(hjust=0.5))+
  theme(axis.title.x=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  theme(axis.title.x = element_text(margin = margin(t = 15, b=-0),size=12))
  #theme(legend.position = "right", legend.text=element_text(size=10), legend.title=element_text(size=11))
species_extent 

png_name<-paste(output_dir, "Stacked_SpeciesBiomassExtent_Graphs.png", sep="")
StackedPlot_Spp<-species_biomass/species_extent
ggsave(filename=png_name, plot = StackedPlot_Spp, dpi = 400)

#Calcs for paper, #Welches ANOVA

boxplot(combined_files$AGB ~ combined_files$Scenario)
oneway.test(AGB ~ Scenario, data = combined_files, var.equal = FALSE)

Historical_1990<-combined_files %>% 
  dplyr::filter(ActualYear == 1990 & Scenario == "Historical")%>%
  summarize(mean=mean(AGB, na.rm=T))
Historical_1990

Historical_2010<-combined_files %>% 
  dplyr::filter(ActualYear == 1990+20 & Scenario == "Historical")%>%
  summarize(mean=mean(AGB, na.rm=T))
Historical_2010

Historical_2050<-combined_files %>% 
  dplyr::filter(ActualYear == 1990+50 & Scenario == "Historical")%>%
  summarize(mean=mean(AGB, na.rm=T))
Historical_2050

100*abs(Historical_2010- Historical_1990)/((Historical_2010+ Historical_1990)/2)

CC_1990<-combined_files %>% 
  dplyr::filter(ActualYear == 1990 & Scenario == "Climate Change")%>%
  summarize(mean=mean(AGB, na.rm=T))
CC_1990

CC_2010<-combined_files %>% 
  dplyr::filter(ActualYear == 1990+20 & Scenario == "Climate Change")%>%
  summarize(mean=mean(AGB, na.rm=T))
CC_2010

100*abs(CC_2010- CC_1990)/((CC_2010+ CC_1990)/2)

CC_2050<-combined_files %>% 
  dplyr::filter(ActualYear == 1990+50 & Scenario == "Climate Change")%>%
  summarize(mean=mean(AGB, na.rm=T))
CC_2050

100*abs(CC_2050- CC_2010)/((CC_2010+ CC_2050)/2)
100*abs(CC_2050- CC_1990)/((CC_2010+ CC_1990)/2)
(Historical_2010- CC_2010)/100
(Historical_2050- CC_2050)/100


#Reclass calcs

all_data_reclass<-rbind(final_reclass_matrix_H, final_reclass_matrix_CC)

H_1990_reclass<-all_data_reclass %>% 
  dplyr::filter(Time == 1 & Scenario == "Historical")%>%
  summarize(meanH=mean(Hardwoods_ha, na.rm=T), meanC=mean(Conifer_ha, na.rm=T))
H_1990_reclass/active_cells

H_2050_reclass<-all_data_reclass %>% 
  dplyr::filter(Time == 50 & Scenario == "Historical")%>%
  summarize(meanH=mean(Hardwoods_ha, na.rm=T), meanC=mean(Conifer_ha, na.rm=T))
H_2050_reclass/active_cells

CC_1990_reclass<-all_data_reclass %>% 
  dplyr::filter(Time == 1 & Scenario == "Climate Change")%>%
  summarize(meanH=mean(Hardwoods_ha, na.rm=T), meanC=mean(Conifer_ha, na.rm=T))
CC_1990_reclass/active_cells

CC_2050_reclass<-all_data_reclass %>% 
  dplyr::filter(Time == 50 & Scenario == "Climate Change")%>%
  summarize(meanH=mean(Hardwoods_ha, na.rm=T), meanC=mean(Conifer_ha, na.rm=T))
CC_2050_reclass/active_cells

100*abs(CC_2050- CC_2010)/((CC_2010+ CC_2050)/2)


#species comparisons

#methods, BS dominance
BS<-all_data_reclass %>% 
  dplyr::filter(Time == 1& Species=="Black spruce")%>%
  summarize(meanH=mean(Hardwoods_ha, na.rm=T))
BS/active_cells

#. Under climate change, however, white and black spruce decreased dramatically in biomass by X and X%, 


#white spruce

CC_1990<-graphing_matrix_noMoss %>% 
  dplyr::filter(ActualYear == 1990 & Scenario == "Climate change" & Species == "White spruce")%>%
  summarize(mean=mean(Avg_Biomass_gm2, na.rm=T))
CC_1990

CC_2050<-graphing_matrix_noMoss %>% 
  dplyr::filter(ActualYear == 1990+50 & Scenario == "Climate change" & Species == "White spruce")%>%
  summarize(mean=mean(Avg_Biomass_gm2, na.rm=T))
CC_2050

100*(CC_2050- CC_1990)/CC_1990

#black spruce
CC_1990<-graphing_matrix_noMoss %>% 
  dplyr::filter(ActualYear == 1990 & Scenario == "Climate change" & Species == "Black spruce")%>%
  summarize(mean=mean(Avg_Biomass_gm2, na.rm=T))
CC_1990

CC_2050<-graphing_matrix_noMoss %>% 
  dplyr::filter(ActualYear == 1990+50 & Scenario == "Climate change" & Species == "Black spruce")%>%
  summarize(mean=mean(Avg_Biomass_gm2, na.rm=T))
CC_2050

100*(CC_2050- CC_1990)/CC_1990

