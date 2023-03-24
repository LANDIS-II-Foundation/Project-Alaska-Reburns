##This is the R script to graph all the Reclass maps over time

library(raster)
library(plyr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(viridis)
library(stringr)

sim_dir<-("G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/")  #New computer external HD
setwd(sim_dir)

output_dir<-("G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/Output_Sims_DGS_methods_paper/")  #New computer external HD

active_cells<-99465

Scenario_LUT <- read.csv ("Scenarios_DGS_Paper_NoLimits.csv")
#historical
scen_list<-(Scenario_LUT[6:10,1])
#climate change
scen_list<-(Scenario_LUT[1:5,1])

##### Making raster maps of all the THU maps.
par(mfrow = c(2, 4))  # Set up a 2 x 2 plotting space

Time_unique<-seq(1,50, by=1)

#Set up a null matrix
LANDIS_output_matrix<-NULL
for (t in 1:length(scen_list)){
  scenario<-scen_list[t]
  reclass_dir <- paste(sim_dir, scenario, "/Reclass_rp/", sep="") #directory of biomass raster
  #all_reclass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.

  for (m in 1:length(Time_unique)){   # for each scenario and replicate, compiles data into a single data frame  
    each_time<-(Time_unique[m])
    reclass_map<-(raster(paste0(reclass_dir, "biomass-reclass-reclass1-", each_time,".img")))#LANDIS unique spp biomass.
    reclass_df<-as.data.frame(reclass_map)
    colnames(reclass_df)<-c("Reclass")
    reclass_df[is.na(reclass_df)]<-0 
    reclass_df_nonzero<-subset(reclass_df, reclass_df$Reclass>0)
    reclass_hardwoods<-subset(reclass_df_nonzero, reclass_df_nonzero$Reclass>1)
    reclass_conifers<-subset(reclass_df_nonzero, reclass_df_nonzero$Reclass>0 & reclass_df_nonzero$Reclass<2)
    extent_Conifer <-length(as.numeric(reclass_conifers$Reclass))  #number of cells, corrected for resolution = Total hectares of each species
    extent_Hardwoods <-length(as.numeric(reclass_hardwoods$Reclass))  #number of cells, corrected for resolution = Total hectares of each species
    LANDIS_output_row<-cbind.data.frame(scenario, each_time, extent_Conifer, extent_Hardwoods)
    LANDIS_output_matrix<-rbind(LANDIS_output_matrix, LANDIS_output_row)
  } 
}
colnames(LANDIS_output_matrix)<-c("Folder", "Time",  "Conifer_ha", "Hardwoods_ha")
head(LANDIS_output_matrix)

final_reclass_matrix<-left_join(LANDIS_output_matrix, Scenario_LUT, by=c("Folder"=  "Folder"))

write.csv(final_reclass_matrix, paste0(output_dir,"Historical_Reclass_over_time.csv"))
write.csv(final_reclass_matrix, paste0(output_dir,"ClimateChange_Reclass_over_time.csv"))

final_reclass_matrix<-read.csv(paste0(output_dir,"Historical_Reclass_over_time.csv"))
all_data_ribbon_historical<-ddply(final_reclass_matrix, .(Time, Scenario), summarize,
                       mean_Conifer = mean(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                       SD_Conifer = sd(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                       mean_Hard = mean(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)),
                       SD_Hard = sd(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)))
                       
final_reclass_matrix<-read.csv(paste0(output_dir,"ClimateChange_Reclass_over_time.csv"))
all_data_ribbon_CC<-ddply(final_reclass_matrix, .(Time, Scenario), summarize,
                          mean_Conifer = mean(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                          SD_Conifer = sd(Conifer_ha/(Hardwoods_ha + Conifer_ha)),
                          mean_Hard = mean(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)),
                          SD_Hard = sd(Hardwoods_ha/(Hardwoods_ha + Conifer_ha)))

all_data_ribbon<-rbind(all_data_ribbon_historical, all_data_ribbon_CC)

year1<-1989
all_data_ribbon$ActualYear<-rep(all_data_ribbon$Time + year1)

all_data_ribbon_conifer<-all_data_ribbon[,c(1:4,7)]
all_data_ribbon_conifer$ForestType<-rep("Conifer", times=nrow(all_data_ribbon_conifer))
colnames(all_data_ribbon_conifer)<-c("Time", "Scenario", "mean_Percent", "SD_Percent", "ActualYear", "ForestType")
all_data_ribbon_hard_columns<-all_data_ribbon[,c(5:6)]
all_data_ribbon_ID<-all_data_ribbon[,c(1:2)]
all_data_hard<-cbind(all_data_ribbon_ID, all_data_ribbon_hard_columns, all_data_ribbon[,c(7)])
all_data_hard$ForestType<-rep("Hardwood", times=nrow(all_data_hard))
colnames(all_data_hard)<-c("Time", "Scenario", "mean_Percent", "SD_Percent", "ActualYear", "ForestType")

all_data_ribbon_plotting<-rbind(all_data_ribbon_conifer, all_data_hard)
all_data_ribbon_plotting$CombinedScenario<-str_c(all_data_ribbon_plotting$Scenario, '- ', all_data_ribbon_plotting$ForestType)

plt.cols.short <- c("forestgreen", "darkorange", "seagreen2","red") #Number corresponds to scenarios
all_data_ribbon_plotting$CombinedScenario <- factor(all_data_ribbon_plotting$CombinedScenario, levels = c("Historical- Conifer", "ClimateChange- Conifer", "Historical- Hardwood", "ClimateChange- Hardwood"))
legend_title<-NULL

ggplot(all_data_ribbon_plotting, aes(x=(ActualYear))) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(ActualYear), y=mean_Percent, colour=CombinedScenario))+
  geom_ribbon(aes(ymin=mean_Percent-SD_Percent, ymax=mean_Percent+SD_Percent, fill=CombinedScenario), alpha=0.4, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  ylim(0,1) +
  theme_classic()+xlab(NULL)+ylab("Percent of Landscape (%)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))


 
