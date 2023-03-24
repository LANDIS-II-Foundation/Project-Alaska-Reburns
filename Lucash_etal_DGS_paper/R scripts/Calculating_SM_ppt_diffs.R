
# Script to calculate some relative diffs in the climate data.

library(ggplot2)
library(png)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)

date<-Sys.Date()

#model_dir <-"C:/Users/mlucash/Documents/Alaska_Reburns_Project_Sims/Output_Sims_DGS_methods_paper/"
model_dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/Output_Sims_DGS_methods_paper/"
setwd(model_dir)

output_dir<-model_dir

Data_frame_input  <- 
  list.files(pattern = "*.csv")%>% 
  map_df(~fread(.))

Data_frame_removeNAs<-subset(Data_frame_input, Data_frame_input$ActualYear>1)


#Ribbon Graphs where scenario replicates get combined into Hist or CC scenarios
all_data_ST_SM_summary<-ddply(Data_frame_removeNAs, .(ActualYear, Scenario), summarize,
                       mean_VWC = mean(mean_VWC_top_50cm_landscape),
                       SD_VWC = sd(mean_VWC_top_50cm_landscape),
                       SE_VWC = sd(mean_VWC_top_50cm_landscape)/sqrt(length(mean_VWC_top_50cm_landscape)),
                       mean_ST = mean(ST_mean_3m),
                       SD_ST = sd(ST_mean_3m),
                       SE_ST = sd(ST_mean_3m)/sqrt(length(ST_mean_3m)),
                       mean_ALT = mean(-1*seasonallyfreezingthickness_landscape),
                       SD_ALT = sd(-1*seasonallyfreezingthickness_landscape),
                       SE_SLT = sd(-1*seasonallyfreezingthickness_landscape)/sqrt(length(seasonallyfreezingthickness_landscape)))


climate_dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/Calib_Landscape_Scrapple_220119_CC_A/"
setwd(climate_dir)
Data_frame_CC_monthly  <- 
  list.files(pattern = "*monthly-log.csv")%>% 
  map_df(~fread(.))

Scenario<-c(rep("ClimateChange", nrow(Data_frame_CC_monthly)))
combined_CC_files<-cbind(Scenario, Data_frame_CC_monthly)

CC_summary<-ddply(combined_CC_files, .(Time, Scenario), summarize,
                    sum_ppt = sum(ppt),
                    SD_ppt = sd(ppt),
                    SE_ppt = sd(ppt)/sqrt(length(ppt)),
                    mean_temp = mean(airtemp),
                    SD_temp = sd(airtemp),
                    SE_temp = sd(airtemp)/sqrt(length(airtemp)))


dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"

baseline_csv1<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119A/DGS-succession-monthly-log.csv"))
baseline_csv2<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119B/DGS-succession-monthly-log.csv"))
baseline_csv3<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119C/DGS-succession-monthly-log.csv"))
baseline_csv4<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119D/DGS-succession-monthly-log.csv"))
baseline_csv5<-read.csv (paste0 (dir, "Calib_Landscape_Scrapple_220119E/DGS-succession-monthly-log.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5)

Scenario<-c(rep("Historical", 5*nrow(baseline_csv1)))

combined_hist_files<-cbind(Scenario, combined_csv)

#Ribbon Graphs where scenario replicates get combined into Hist or CC scenarios
hist_summary<-ddply(combined_hist_files, .(Time, Scenario), summarize,
               sum_ppt = sum(ppt)/5,
               SD_ppt = sd(ppt)/5,
               SE_ppt = (sd(ppt)/sqrt(length(ppt))/5),
               mean_temp = mean(airtemp),
               SD_temp = sd(airtemp),
               SE_temp = sd(airtemp)/sqrt(length(airtemp)))


combined_files<-rbind(hist_summary, CC_summary)
combined_files$ActualYear<-combined_files$Time + 1990

all_data<-left_join(all_data_ST_SM_summary,combined_files)

Data_frame_CC_start<-subset(all_data, all_data$Scenario=="ClimateChange" & all_data$ActualYear=="1991")
Data_frame_Hist_start<-subset(all_data, all_data$Scenario=="Historical"& all_data$ActualYear=="1991")
Data_frame_CC_end<-subset(all_data, all_data$Scenario=="ClimateChange"& all_data$ActualYear=="2039")
Data_frame_Hist_end<-subset(all_data, all_data$Scenario=="Historical"& all_data$ActualYear=="2039")

oneway.test(mean_VWC ~ Scenario, data = all_data, var.equal = FALSE)
oneway.test(mean_ST ~ Scenario, data = all_data, var.equal = FALSE)

# how did variability in ST vary with CC? L774
leveneTest(all_data$mean_ST,all_data$Scenario)

leveneTest(all_data$mean_VWC,all_data$Scenario)

# % declines in soil moisture under historical climate and then climate change
(Data_frame_Hist_end$mean_VWC-Data_frame_Hist_start$mean_VWC)/Data_frame_Hist_start$mean_VWC
(Data_frame_CC_end$mean_VWC-Data_frame_CC_start$mean_VWC)/Data_frame_CC_start$mean_VWC

# % declines in ST under historical climate and then climate change, unreported
(Data_frame_Hist_end$mean_ST-Data_frame_Hist_start$mean_ST)/Data_frame_Hist_start$mean_ST
(Data_frame_CC_end$mean_ST-Data_frame_CC_start$mean_ST)/Data_frame_CC_start$mean_ST

# oC change in ST under historical climate and then climate change
# Soil temperature increased slightly under historical climate (by 0.16 ?C) but increased dramatically under climate change by 1.1 ?C
(Data_frame_Hist_end$mean_ST-Data_frame_Hist_start$mean_ST)
(Data_frame_CC_end$mean_ST-Data_frame_CC_start$mean_ST)
(Data_frame_CC_end$mean_ST-Data_frame_CC_start$mean_ST)/50 #rate of ST warming per year, 0.019 oC


plt.cols<-c("orange", "darkslategray4")

#Graphs where each scenario replicate gets a line.
ggplot(all_data, aes(x=(ActualYear), y=mean_VWC, group=Scenario)) +
  scale_color_manual(values=plt.cols)+
  geom_line(aes(x=(ActualYear), y=mean_VWC, colour=Scenario))+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

ggplot(all_data, aes(x=(ActualYear), y=sum_ppt, group=Scenario)) +
  scale_color_manual(values=plt.cols)+
  geom_line(aes(x=(ActualYear), y=sum_ppt, colour=Scenario))+
  theme_classic()+xlab(NULL)+ylab("precipitation (cm)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

#zero in on these years: 2015-2022

subset_years<-subset(all_data, all_data$ActualYear>2014 & all_data$ActualYear < 2023)
Data_frame_CConly<-subset(subset_years, subset_years$Scenario=="ClimateChange")
Data_frame_Histonly<-subset(subset_years, subset_years$Scenario=="Historical")

mean(Data_frame_CConly$mean_VWC)
mean(Data_frame_CConly$sum_ppt)

mean(Data_frame_Histonly$mean_VWC)
mean(Data_frame_Histonly$sum_ppt)

(mean(Data_frame_CConly$mean_VWC)-mean(Data_frame_Histonly$mean_VWC))/mean(Data_frame_Histonly$mean_VWC)
(mean(Data_frame_CConly$sum_ppt)-mean(Data_frame_Histonly$sum_ppt))/mean(Data_frame_Histonly$sum_ppt)


#Switching to THU comparisons

model_dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"
output_dir<-paste0(model_dir, "Output_Sims_DGS_methods_paper/ByTHU/")

CC_csv1<-read.csv (paste0 (output_dir, "ClimateChange_220119_repA_byTHU.csv"))
CC_csv2<-read.csv (paste0 (output_dir, "ClimateChange_220119_repB_byTHU.csv"))
CC_csv3<-read.csv (paste0 (output_dir, "ClimateChange_220119_repC_byTHU.csv"))
CC_csv4<-read.csv (paste0 (output_dir, "ClimateChange_220119_repD_byTHU.csv"))
CC_csv5<-read.csv (paste0 (output_dir, "ClimateChange_220119_repE_byTHU.csv"))


all_data_scenarios<-rbind.data.frame(CC_csv1, CC_csv2, CC_csv3, CC_csv4, CC_csv5)

Data_frame_CC_OldConifer_start<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="OldConifer" & all_data_scenarios$actual_year=="1992")
Data_frame_CC_OldConifer_end<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="OldConifer" & all_data_scenarios$actual_year=="2040")
Data_frame_CC_YoungConifer_start<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="YoungConifer" & all_data_scenarios$actual_year=="1992")
Data_frame_CC_YoungConifer_end<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="YoungConifer" & all_data_scenarios$actual_year=="2040")

Data_frame_CC_OldHard_start<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="OldHardwood" & all_data_scenarios$actual_year=="1992")
Data_frame_CC_OldHard_end<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="OldHardwood" & all_data_scenarios$actual_year=="2040")
Data_frame_CC_YoungHard_start<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="YoungHardwood" & all_data_scenarios$actual_year=="1992")
Data_frame_CC_YoungHard_end<-subset(all_data_scenarios, all_data_scenarios$Scenario=="ClimateChange" & all_data_scenarios$VegetationType1=="YoungHardwood" & all_data_scenarios$actual_year=="2040")


100*(mean(Data_frame_CC_OldConifer_end$ST_mean_3m)-mean(Data_frame_CC_OldConifer_start$ST_mean_3m))/mean(Data_frame_CC_OldConifer_start$ST_mean_3m)
(mean(Data_frame_CC_OldConifer_end$ST_mean_3m))/mean(Data_frame_CC_OldConifer_start$ST_mean_3m)
(mean(Data_frame_CC_YoungConifer_end$ST_mean_3m))/mean(Data_frame_CC_YoungConifer_start$ST_mean_3m)
100*(mean(Data_frame_CC_OldHard_end$ST_mean_3m)-mean(Data_frame_CC_OldHard_start$ST_mean_3m))/mean(Data_frame_CC_OldHard_start$ST_mean_3m)

#increase in soil temp between start and end in old conifers, 0.4
(mean(Data_frame_CC_OldConifer_end$ST_mean_3m))- mean(Data_frame_CC_OldConifer_start$ST_mean_3m)
abs((mean(Data_frame_CC_OldConifer_end$ST_mean_3m))- mean(Data_frame_CC_OldConifer_start$ST_mean_3m))/50
#increase in soil temp between start and end in old hardwoods, 1.5
(mean(Data_frame_CC_OldHard_end$ST_mean_3m))- mean(Data_frame_CC_OldHard_start$ST_mean_3m)
abs((mean(Data_frame_CC_OldHard_end$ST_mean_3m))- mean(Data_frame_CC_OldHard_start$ST_mean_3m))/50
#increase in soil temp between start and end in young hardwoods, 1.9
mean(Data_frame_CC_YoungHard_end$ST_mean_3m)-mean(Data_frame_CC_YoungHard_start$ST_mean_3m)
abs(mean(Data_frame_CC_YoungHard_end$ST_mean_3m)-mean(Data_frame_CC_YoungHard_start$ST_mean_3m))/50

H_csv1<-read.csv (paste0 (output_dir, "Historical_220119_repA_byTHU.csv"))
H_csv2<-read.csv (paste0 (output_dir, "Historical_220119_repB_byTHU.csv"))
H_csv3<-read.csv (paste0 (output_dir, "Historical_220119_repC_byTHU.csv"))
H_csv4<-read.csv (paste0 (output_dir, "Historical_220119_repD_byTHU.csv"))
H_csv5<-read.csv (paste0 (output_dir, "Historical_220119_repE_byTHU.csv"))

#Climate change effect on ST

all_data_scenarios_byTHU<-rbind.data.frame(H_csv1, H_csv2, H_csv3, H_csv4, H_csv5, CC_csv1, CC_csv2, CC_csv3, CC_csv4, CC_csv5)
Data_frame_OldConifer<-subset(all_data_scenarios_byTHU, all_data_scenarios_byTHU$VegetationType1=="OldConifer")
oneway.test(ST_mean_3m ~ Scenario, data = Data_frame_OldConifer, var.equal = FALSE)

Data_frame_YoungConifer<-subset(all_data_scenarios_byTHU, all_data_scenarios_byTHU$VegetationType1=="YoungConifer")
oneway.test(ST_mean_3m ~ Scenario, data = Data_frame_YoungConifer, var.equal = FALSE)

Data_frame_OldHard<-subset(all_data_scenarios_byTHU, all_data_scenarios_byTHU$VegetationType1=="OldHardwood")
oneway.test(ST_mean_3m ~ Scenario, data = Data_frame_OldHard, var.equal = FALSE)

Data_frame_YoungHard<-subset(all_data_scenarios_byTHU, all_data_scenarios_byTHU$VegetationType1=="YoungHardwood")
oneway.test(ST_mean_3m ~ Scenario, data = Data_frame_YoungHard, var.equal = FALSE)

#CC effect on Soil moisture

oneway.test(VWC ~ Scenario, data = Data_frame_OldConifer, var.equal = FALSE)
oneway.test(VWC ~ Scenario, data = Data_frame_YoungConifer, var.equal = FALSE)
oneway.test(VWC ~ Scenario, data = Data_frame_OldHard, var.equal = FALSE)
oneway.test(VWC ~ Scenario, data = Data_frame_YoungHard, var.equal = FALSE)



