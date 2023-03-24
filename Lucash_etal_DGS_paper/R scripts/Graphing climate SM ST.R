
################## R Script to graph the monthly climate data from DGS.

# Load libraries
library(sqldf)
library(plyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(patchwork)

date<-Sys.Date()

# Set working directory
model_dir <- "G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"  
setwd(model_dir)
year1 <- 1990

output_dir<-paste0(model_dir, "Output_Sims_DGS_methods_paper/")

baseline_csv1<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119A/DGS-succession-monthly-log.csv"))
baseline_csv2<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119B/DGS-succession-monthly-log.csv"))
baseline_csv3<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119C/DGS-succession-monthly-log.csv"))
baseline_csv4<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119D/DGS-succession-monthly-log.csv"))
baseline_csv5<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119E/DGS-succession-monthly-log.csv"))
CC_csv1<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_A/DGS-succession-monthly-log.csv"))
CC_csv2<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_B/DGS-succession-monthly-log.csv"))
CC_csv3<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_C/DGS-succession-monthly-log.csv"))
CC_csv4<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_D/DGS-succession-monthly-log.csv"))
CC_csv5<-read.csv (paste0 (model_dir, "Calib_Landscape_Scrapple_220119_CC_E/DGS-succession-monthly-log.csv"))

combined_csv<-rbind(baseline_csv1, baseline_csv2, baseline_csv3, baseline_csv4, baseline_csv5, CC_csv1, CC_csv2, CC_csv3, CC_csv4, CC_csv5)

Scenario<-c(rep("Historical", 5*nrow(baseline_csv1)), (rep("Climate change", 5*nrow(CC_csv1))))
Scenario_rep<-c((rep("Hist_rep1",1200)), (rep("Hist_rep2",1200)), (rep("Hist_rep3", 1200)), (rep("Hist_rep4", 1200)), (rep("Hist_rep5", 1200)), (rep("CC_rep1", 1200)),(rep("CC_rep2", 1200)),(rep("CC_rep3", 1200)),(rep("CC_rep4", 1200)),(rep("CC_rep5", 1200)))
ActualYear<-rep(baseline_csv1$Time + year1, times=10)

combined_files<-cbind(Scenario, combined_csv, ActualYear, Scenario_rep)

# Load lookup tables for joining to data.  
Year_LUT_all <- read.csv ("Scenarios_DGS_Paper.csv")
Year_LUT<-Year_LUT_all[-1,]

#################################################### CENTURY LOG CARBON OUTPUT #########################################################
# Plot to make 
plt.cols.long<-c("red","orange","darkorange","gold", "orangered1", "blue","steelblue","cyan", "darkslategray2", "darkslategray4")


all_data_ribbon_climate<-ddply(combined_files, .(Scenario, ActualYear), summarize,
                       mean_ppt = sum(ppt),
                       SD_ppt = sd(ppt),
                       SE_ppt = sd(ppt)/sqrt(length(ppt)),
                       mean_temp = mean(airtemp),
                       SD_temp = sd(airtemp),
                       SE_temp = sd(airtemp)/sqrt(length(airtemp)),
                       mean_NEE = mean(avgNEE),
                       SD_NEE = sd(avgNEE),
                       SE_NEE = sd(avgNEE)/sqrt(length(avgNEE)))

all_data_ribbon_climate$Scenario <- factor(all_data_ribbon_climate$Scenario, levels = c("Historical", "Climate change"))

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

#precip graph
#png_name<-paste(output_dir, "Precip_scenarios", date, ".png", sep="")

ppt_graph<-ggplot(all_data_ribbon_climate, aes(x=(ActualYear), y=mean_ppt, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_ppt, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_ppt-SD_ppt, ymax=mean_ppt+SD_ppt, fill=Scenario), alpha=0.35)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Precipitaton (cm)") + theme(legend.position = "right")+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12), axis.text.x = element_blank(), axis.title.x = element_blank())+
  theme(legend.position = c(0.15,0.9), legend.text=element_text(size=11), legend.title=element_text(size=12))+
  scale_y_continuous(expand = c(0, 1), limits=c(0,1200))+
  scale_x_continuous(breaks=seq(1990, 2040, 10))
#ggsave(filename=png_name, plot = ppt_graph, width = 5,  height = 4, units = "in",  dpi = 300)
dev.off()

#temp graph
png_name<-paste(output_dir, "Temp_scenarios", date, ".png", sep="")

temp<-ggplot(all_data_ribbon_climate, aes(x=(ActualYear), y=mean_temp, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_temp, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_temp-SD_temp, ymax=mean_temp+SD_temp, fill=Scenario), alpha=0.35)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab(expression(Air~temperature~~(degree~C)))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12), axis.text.x = element_blank(), axis.title.x = element_blank())+
  theme(legend.position = c(0.15,0.9), legend.text=element_text(size=12), legend.title=element_text(size=13))+
  scale_y_continuous(expand = c(0, 0), limits=c(-30,30))+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))
temp

#ggsave(filename=png_name, plot = temp, width = 5,  height = 4, units = "in",  dpi = 300)

#NEE graph
png_name<-paste(output_dir, "NEE_scenarios", date, ".png", sep="")

NEE_graph<-ggplot(all_data_ribbon_climate, aes(x=(ActualYear+30), y=mean_NEE, group=Scenario)) +
  geom_line(aes(x=(ActualYear+30), y=mean_NEE, colour=Scenario))+
  #geom_ribbon(aes(ymin=mean_NEE-SD_NEE, ymax=mean_ppt+SD_NEE, fill=Scenario), alpha=0.35)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab(expression(Net~ecosystem~exchange~(g~m^2~y^-1))) + theme(legend.position = "right")+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12), axis.title.x = element_blank())+
  theme(legend.position = c(0.15,0.9), legend.text=element_text(size=11), legend.title=element_text(size=12))+
  #scale_y_continuous(expand = c(0, 1), limits=c(0,1200))+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_x_continuous(breaks=seq(1990+30, 2040+30, 10))
ggsave(filename=png_name, plot = NEE_graph, width = 5,  height = 4, units = "in",  dpi = 300)
dev.off()

model_dir <-"G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/Output_Sims_DGS_methods_paper/"
setwd(model_dir)

output_dir<-model_dir

Data_frame_input  <- 
  list.files(pattern = "*.csv")%>% 
  map_df(~fread(.))

plt.cols.long<-c("red","orange","darkorange","gold", "orangered1", "blue","steelblue","cyan", "darkslategray2", "darkslategray4")

Data_frame_removeNAs<-subset(Data_frame_input, Data_frame_input$ActualYear>1)

#Ribbon Graphs where scenario replicates get combined into Hist or CC scenarios
all_data_ribbon_soil<-ddply(Data_frame_removeNAs, .(ActualYear, Scenario), summarize,
                       mean_VWC = mean(mean_VWC_top_50cm_landscape),
                       SD_VWC = sd(mean_VWC_top_50cm_landscape),
                       SE_VWC = sd(mean_VWC_top_50cm_landscape)/sqrt(length(mean_VWC_top_50cm_landscape)),
                       mean_ST = mean(ST_mean_3m),
                       SD_ST = sd(ST_mean_3m),
                       SE_ST = sd(ST_mean_3m)/sqrt(length(ST_mean_3m)),
                       mean_ALT = mean(-1*seasonallyfreezingthickness_landscape),
                       SD_ALT = sd(-1*seasonallyfreezingthickness_landscape),
                       SE_SLT = sd(-1*seasonallyfreezingthickness_landscape)/sqrt(length(seasonallyfreezingthickness_landscape)))

all_data_ribbon_soil$Scenario <- factor(all_data_ribbon_soil$Scenario, levels = c("Historical", "ClimateChange"))
levels(all_data_ribbon_soil$Scenario) <- list(Historical = "Historical", ClimateChange = "Climate change")

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

#png_name<-paste(output_dir, "SoilMoist_", date, ".png", sep="")
#png(png_name, width = 6, height = 4, units = 'in', res = 300)

VWC_plot <- ggplot(all_data_ribbon_soil, aes(x=(ActualYear), y=mean_VWC, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_VWC, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_VWC-SD_VWC, ymax=mean_VWC+SD_VWC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Historical climate", "Climate Change"), values = plt.cols.short) +
  theme_classic()+xlab("Years")+ylab(expression(theta["VWC"])) + theme(legend.position = "none")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + 
  scale_y_continuous(breaks=seq(0, 0.6, 0.2), expand = c(0, 0.001), limits = c(0,0.6))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=13))
VWC_plot

Seasonally_frozen<-ggplot(all_data_ribbon_soil, aes(x=(ActualYear), y=mean_ALT, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_ALT, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_ALT-SD_ALT, ymax=mean_ALT+SD_ALT, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Seasonally frozen thickness (m)") + theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  scale_y_continuous(breaks=seq(-2,0.5, 0.5), expand = c(0, 0.001), limits = c(-2,0.5))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))

#png_name<-paste(output_dir, "SoilTemp_", date, ".png", sep="")
#png(png_name, width = 6, height = 4, units = 'in', res = 300)

ST_plot <- ggplot(all_data_ribbon_soil, aes(x=(ActualYear), y=mean_ST, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_ST, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_ST-SD_ST, ymax=mean_ST+SD_ST, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Historical climate", "Climate change"), values = plt.cols.short) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_classic()+xlab("Years")+ylab(expression(Soil~temperature~~(degree~C))) + theme(legend.position = "none")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + 
  scale_y_continuous(breaks=seq(-1, 2, 1), expand=c(0,0), limits = c(-1,2))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))


#Analysis_by_THU, THU graphs

THU_model_dir <-paste0(model_dir, "/ByTHU/")
setwd(THU_model_dir)

Data_frame_input_THU  <- 
  list.files(pattern = "*.csv")%>% 
  map_df(~fread(.))

Data_frame_input_THU_NoNAs<-subset(Data_frame_input_THU, Data_frame_input_THU$common_year<2041)

#png_name<-paste(output_dir, "SeasonalFreez_byTHU_", date, ".png", sep="")
#png(png_name, width = 6, height = 4, units = 'in', res = 300)

all_data_ribbon_THU<-ddply(Data_frame_input_THU_NoNAs, .(common_year, Scenario, VegetationType1), summarize,
                           mean_VWC = mean(VWC),
                           SD_VWC = sd(VWC),
                           SE_VWC = sd(VWC)/sqrt(length(VWC)),
                           mean_ST = mean(ST_mean_3m),
                           SD_ST = sd(ST_mean_3m),
                           SE_ST = sd(ST_mean_3m)/sqrt(length(ST_mean_3m)),
                           mean_ALT = mean(-1*sft),
                           SD_ALT = sd(-1*sft),
                           SE_SLT = sd(-1*sft)/sqrt(length(sft)))

all_data_summary_THU<-ddply(Data_frame_input_THU_NoNAs, .(Scenario, VegetationType1), summarize,
                            mean_VWC = mean(VWC),
                            SD_VWC = sd(VWC),
                            SE_VWC = sd(VWC)/sqrt(length(VWC)),
                            mean_ALT = mean(-1*sft),
                            SD_ALT = sd(-1*sft),
                            SE_SLT = sd(-1*sft)/sqrt(length(sft)))

all_data_ribbon_THU$Scenario <- recode_factor(all_data_ribbon_THU$Scenario, Historical = "Historical", ClimateChange = "Climate change")
all_data_ribbon_THU$Scenario <- factor(all_data_ribbon_THU$Scenario, levels = c("Historical", "Climate change"))
unique(all_data_ribbon_THU$Scenario)

all_data_ribbon_THU$VegetationType1 <- recode_factor(all_data_ribbon_THU$VegetationType1, YoungConifer = "Young conifers", OldConifer = "Old conifers", YoungHardwood = "Young hardwoods", OldHardwood = "Old hardwoods")
unique(all_data_ribbon_THU$VegetationType1)
all_data_ribbon_THU$VegetationType1 <- factor(all_data_ribbon_THU$VegetationType1, levels = c("Young conifers", "Young hardwoods","Old conifers","Old hardwoods"))

legend_title<-'Climate Scenarios'

#png_name<-paste(output_dir, "SoilMoist_byTHU_", date, ".png", sep="")
#png(png_name, width = 6, height = 4, units = 'in', res = 300)

SM_THU<-ggplot(all_data_ribbon_THU, aes(x=(common_year), y=mean_VWC, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_VWC, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_VWC-SD_VWC, ymax=mean_VWC+SD_VWC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  scale_y_continuous(breaks=seq(0.0,0.6, 0.2), expand=c(0,0), limits = c(0,0.6))+  
  theme_classic()+xlab("Years")+ylab(expression(theta["VWC"])) + theme(legend.position = "NULL")+
  scale_x_continuous(breaks=seq(1990, 2040, 10), limits=c(1990,2042))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=13))+
  facet_wrap(~ VegetationType1)
  

#png_name<-paste(output_dir, "SoilTemp_byTHU_", date, ".png", sep="")
#png(png_name, width = 6, height = 4, units = 'in', res = 300)

ST_THU<-ggplot(all_data_ribbon_THU, aes(x=(common_year), y=mean_ST, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_ST, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_ST-SD_ST, ymax=mean_ST+SD_ST, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  geom_hline(yintercept=0, linetype="dashed") +
  #theme_classic()+xlab(NULL)+ylab(expression(Soil~temperature~~(degree~C))) + theme(legend.position = "right")+
  theme_classic()+xlab("Years")+ylab(expression(Soil~temperature~~(degree~C))) + theme(legend.position = "NULL")+
  scale_x_continuous(breaks=seq(1990, 2040, 10), limits=c(1990,2042))+
  scale_y_continuous(breaks=seq(-2, 4, 2.0), expand=c(0,0),limits = c(-2,4))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  facet_wrap(~ VegetationType1)
dev.off()

ggplot(all_data_ribbon_THU, aes(x=(common_year), y=mean_ALT, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_ALT, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_ALT-SD_ALT, ymax=mean_ALT+SD_ALT, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab("Years")+ylab("Seasonally Freezing Thickness") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  scale_y_continuous(breaks=seq(-10, 1, 0.5), expand=c(0,0), limits = c(-10,1))+
  theme(axis.title.y = element_text(margin = margin(l = 3, r=15),size=12))+
  facet_wrap(~ VegetationType1)

# Plot the seasonally thawed thickness of those with permafrost. 

png_name<-paste(output_dir, "Stacked_Temp_Graphs.png", sep="")
StackedPlot_Temp<-temp/ST_plot/ST_THU 
ggsave(filename=png_name, plot = StackedPlot_Temp, dpi = 300)

png_name<-paste(output_dir, "Stacked_PPT_VWC_Graphs.png", sep="")
StackedPlot_VWC<-ppt_graph/VWC_plot/SM_THU
ggsave(filename=png_name, plot = StackedPlot_VWC, dpi = 300)
dev.off()
