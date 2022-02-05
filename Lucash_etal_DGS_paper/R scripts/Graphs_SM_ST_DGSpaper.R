library(ggplot2)
library(png)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)

date<-Sys.Date()

#model_dir <-"C:/Users/mlucash/Documents/Alaska_Reburns_Project_Sims/Output_Sims_DGS_methods_paper/"
model_dir <-"D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/Output_Sims_DGS_methods_paper/"
setwd(model_dir)

output_dir<-model_dir

Data_frame_input  <- 
  list.files(pattern = "*.csv")%>% 
  map_df(~fread(.))

plt.cols.long<-c("red","orange","darkorange","gold", "orangered1", "blue","steelblue","cyan", "darkslategray2", "darkslategray4")

Data_frame_removeNAs<-subset(Data_frame_input, Data_frame_input$ActualYear>1)

#Graphs where each scenario replicate gets a line.
ggplot(Data_frame_removeNAs, aes(x=(ActualYear), y=mean_VWC_top_50cm_landscape, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.long)+
  geom_line(aes(x=(ActualYear), y=mean_VWC_top_50cm_landscape, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))


ggplot(Data_frame_removeNAs, aes(x=(ActualYear), y=seasonallyfreezingthickness_landscape*-1, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.long)+
  geom_line(aes(x=(ActualYear), y=seasonallyfreezingthickness_landscape*-1, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Seasonally Freezing Thickness") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

ggplot(Data_frame_removeNAs, aes(x=(ActualYear), y=ST_mean_3m, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.long)+
  geom_line(aes(x=(ActualYear), y=ST_mean_3m, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Soil temperature (oC)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

#Ribbon Graphs where scenario replicates get combined into Hist or CC scenarios
all_data_ribbon<-ddply(Data_frame_removeNAs, .(ActualYear, Scenario), summarize,
                       mean_VWC = mean(mean_VWC_top_50cm_landscape),
                       SD_VWC = sd(mean_VWC_top_50cm_landscape),
                       SE_VWC = sd(mean_VWC_top_50cm_landscape)/sqrt(length(mean_VWC_top_50cm_landscape)),
                       mean_ST = mean(ST_mean_3m),
                       SD_ST = sd(ST_mean_3m),
                       SE_ST = sd(ST_mean_3m)/sqrt(length(ST_mean_3m)),
                       mean_ALT = mean(-1*seasonallyfreezingthickness_landscape),
                       SD_ALT = sd(-1*seasonallyfreezingthickness_landscape),
                       SE_SLT = sd(-1*seasonallyfreezingthickness_landscape)/sqrt(length(seasonallyfreezingthickness_landscape)))

all_data_ribbon$Scenario <- factor(all_data_ribbon$Scenario, levels = c("Historical", "ClimateChange"))

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

png_name<-paste(output_dir, "SoilMoist_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

VWC_plot <- ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_VWC, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_VWC, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_VWC-SD_VWC, ymax=mean_VWC+SD_VWC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Historical climate", "Climate Change"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Volumetric~soil~moisture)) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + 
  scale_y_continuous(breaks=seq(0, 0.5, 0.1), limits = c(0,0.5))
VWC_plot + theme(legend.position = c(0.17, 0.85)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 3.0)) #this adjusts the axis title location
dev.off()


ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_ALT, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_ALT, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_ALT-SD_ALT, ymax=mean_ALT+SD_ALT, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  ylim(-2,0) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Seasonally frozen thickness (m)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

png_name<-paste(output_dir, "SoilTemp_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

ST_plot <- ggplot(all_data_ribbon, aes(x=(ActualYear), y=mean_ST, group=Scenario)) +
  geom_line(aes(x=(ActualYear), y=mean_ST, colour=Scenario))+
  geom_ribbon(aes(ymin=mean_ST-SD_ST, ymax=mean_ST+SD_ST, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(name = "Scenario", labels = c("Historical climate", "Climate Change"), values = plt.cols.short) +
  theme_classic()+xlab(NULL)+ylab(expression(Soil~temperature~~(degree~C))) + theme(legend.position = "right")+
  scale_fill_manual(values = plt.cols.short)+
  scale_x_continuous(breaks=seq(1990, 2040, 10)) + 
  scale_y_continuous(breaks=seq(-1, 4, 1.0), limits = c(-1,4))
ST_plot + theme(legend.position = c(0.17, 0.85)) + #this adjusts the legend location.
  theme(axis.title.y = element_text(vjust = 3.0)) #this adjusts the axis title location
dev.off()

all_data_summary<-ddply(Data_frame_removeNAs, .(Scenario), summarize,
                        mean_VWC = mean(mean_VWC_top_50cm_landscape, na.rm = TRUE),
                        SD_VWC = sd(mean_VWC_top_50cm_landscape, na.rm = TRUE),
                        SE_VWC = sd(mean_VWC_top_50cm_landscape, na.rm = TRUE)/sqrt(length(mean_VWC_top_50cm_landscape)),
                        mean_ALT = mean(-1*seasonallyfreezingthickness_landscape),
                        SD_ALT = sd(-1*seasonallyfreezingthickness_landscape),
                        SE_SLT = sd(-1*seasonallyfreezingthickness_landscape)/sqrt(length(seasonallyfreezingthickness_landscape)))

all_data_summary


#Analysis_by_THU, THU graphs

THU_model_dir <-paste0(model_dir, "/ByTHU/")
setwd(THU_model_dir)

Data_frame_input_THU  <- 
  list.files(pattern = "*.csv")%>% 
  map_df(~fread(.))

Data_frame_input_THU_NoNAs<-subset(Data_frame_input_THU, Data_frame_input_THU$common_year<2041)

ggplot(Data_frame_input_THU_NoNAs, aes(x=(common_year), y=VWC, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.long)+
  geom_line(aes(x=(common_year), y=VWC, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10), limits=c(1990,2042))+
  facet_wrap(~ VegetationType1)

ggplot(Data_frame_input_THU_NoNAs, aes(x=(common_year), y=ST_mean_3m, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.long)+
  geom_line(aes(x=(common_year), y=ST_mean_3m, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Soil temperature") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10), limits=c(1990,2042))+
  facet_wrap(~ VegetationType1)

dev.off()

png_name<-paste(output_dir, "SeasonalFreez_byTHU_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

ggplot(Data_frame_input_THU_NoNAs, aes(x=(common_year), y=sft*-1, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.long)+
  geom_line(aes(x=(common_year), y=sft*-1, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Seasonally freezing thickness") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  facet_wrap(~ VegetationType1)

# Plot the seasonally freezing thickness of those with permafrost. 
Data_frame_input_THU_NoNAs %>% 
  filter(pf_exists == T) %>% 
  ggplot(aes(x = common_year, y = sft*-1)) + 
  facet_wrap(~ VegetationType1)+
  geom_line(show.legend = F)+
  geom_line(aes(x=(common_year), y=sft*-1, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Seasonally freezing thickness of those with permafrost") 

# Plot the seasonally freezing thickness of those without permafrost
# The -6 values are from the years of thawing
Data_frame_input_THU_NoNAs %>% 
  filter(pf_exists == F) %>% 
  ggplot(aes(x = common_year, y = sft*-1)) + 
  facet_wrap(~ VegetationType1)+
  geom_line(show.legend = F)+
  geom_line(aes(x=(common_year), y=sft*-1, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Seasonally freezing thickness of those without permafrost") 

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

all_data_ribbon_THU$Scenario <- factor(all_data_ribbon_THU$Scenario, levels = c("Historical", "ClimateChange"))

legend_title<-'Climate Scenarios'

png_name<-paste(output_dir, "SoilMoist_byTHU_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

ggplot(all_data_ribbon_THU, aes(x=(common_year), y=mean_VWC, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_VWC, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_VWC-SD_VWC, ymax=mean_VWC+SD_VWC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  #theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "NULL")+
  scale_x_continuous(breaks=seq(1990, 2040, 10), limits=c(1990,2042))+
  facet_wrap(~ VegetationType1)

dev.off()

png_name<-paste(output_dir, "SoilTemp_byTHU_", date, ".png", sep="")
png(png_name, width = 6, height = 4, units = 'in', res = 300)

ggplot(all_data_ribbon_THU, aes(x=(common_year), y=mean_ST, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_ST, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_ST-SD_ST, ymax=mean_ST+SD_ST, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  #theme_classic()+xlab(NULL)+ylab(expression(Soil~temperature~~(degree~C))) + theme(legend.position = "right")+
  theme_classic()+xlab(NULL)+ylab(expression(Soil~temperature~~(degree~C))) + theme(legend.position = "NULL")+
  scale_x_continuous(breaks=seq(1990, 2040, 10), limits=c(1990,2042))+
  scale_y_continuous(breaks=seq(-2, 4, 1.0), limits = c(-2,4))+
  facet_wrap(~ VegetationType1)
dev.off()

ggplot(all_data_ribbon_THU, aes(x=(common_year), y=mean_ALT, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_ALT, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_ALT-SD_ALT, ymax=mean_ALT+SD_ALT, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Seasonally Freezing Thickness") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  scale_y_continuous(breaks=seq(-2, 5, 1.0), limits = c(-2,5))+
  facet_wrap(~ VegetationType1)




all_data_ribbon_allTHUs<-ddply(Data_frame_input_THU_NoNAs, .(common_year, Scenario, THUName), summarize,
                       mean_VWC = mean(VWC),
                       SD_VWC = sd(VWC),
                       SE_VWC = sd(VWC)/sqrt(length(VWC)),
                       mean_ALT = mean(-1*sft),
                       SD_ALT = sd(-1*sft),
                       SE_SLT = sd(-1*sft)/sqrt(length(sft)))
all_data_ribbon_allTHUs$Scenario <- factor(all_data_ribbon_allTHUs$Scenario, levels = c("Historical", "ClimateChange"))

ggplot(all_data_ribbon_allTHUs, aes(x=(common_year), y=mean_VWC, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_VWC, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_VWC-SD_VWC, ymax=mean_VWC+SD_VWC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  facet_wrap(~ THUName)

ggplot(all_data_ribbon_allTHUs, aes(x=(common_year), y=mean_ALT, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_ALT, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_ALT-SD_ALT, ymax=mean_ALT+SD_ALT, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Seasonally Freezing Thickness (m)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))+
  facet_wrap(~ THUName)

# Plot the seasonally thawed thickness of those with permafrost. 
# There are none of these because they have taliks. 
Data_frame_input_THU_NoNAs %>% 
  filter(pf_exists == T) %>% 
  ggplot(aes(x = common_year, y = sft*-1, color = as.factor(VegetationType1))) + 
  geom_line(show.legend = F)

#Combining graphs into one TIFF file for publication.

library(png)
library(grid)
library(ggplot2)
library(gridExtra)

#Directory where soil moisture PNG files are stored
model_dir <- "C:/Users/mlucash/Dropbox (University of Oregon)/Lucash_Lab/Lucash/Alaska_Reburns_Project/Manuscripts/DGS_model_description_paper/Figures/SoilMoistureGraphs/"  
setwd(model_dir)

plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})

ggsave("SoilMoisture.TIFF", marrangeGrob(grobs=plots, nrow=1, ncol=2, dpi=400))
dev.off()

#Directory where soil temp PNG files are stored
model_dir <- "C:/Users/mlucash/Dropbox (University of Oregon)/Lucash_Lab/Lucash/Alaska_Reburns_Project/Manuscripts/DGS_model_description_paper/Figures/SoilTempGraphs/"  
setwd(model_dir)

#all_maps <- list.files(paste0(model_dir), pattern = ".png", recursive = T, full = T)
plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})

ggsave("SoilTemp.TIFF", marrangeGrob(grobs=plots, nrow=1, ncol=2, dpi=400))
dev.off()

#########################################################################
# Graphs used for Sims_FinalTHU
ggplot(Data_frame_input, aes(x=(common_year), y=mean_VWC, group=Scenario_replicate)) +
  scale_color_manual(values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_VWC, colour=Scenario_replicate))+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))


all_data_ribbon<-ddply(Data_frame_input, .(common_year, Scenario), summarize,
                       mean_VWC = mean(VWC),
                       SD_VWC = sd(VWC),
                       SE_VWC = sd(VWC)/sqrt(length(VWC)),
                       mean_ALT = mean(-1*max_alt),
                       SD_ALT = sd(-1*max_alt),
                       SE_SLT = sd(-1*max_alt)/sqrt(length(max_alt)))

all_data_ribbon$Scenario <- factor(all_data_ribbon$Scenario, levels = c("Historical", "ClimateChange"))

plt.cols.short <- c("grey30", "darkorange") #Number corresponds to scenarios
legend_title<-'Climate Scenarios'

ggplot(all_data_ribbon, aes(x=(common_year), y=mean_VWC, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_VWC, colour=Scenario))+
  #geom_smooth(method = "glm", se = FALSE, color = "grey60")+
  geom_ribbon(aes(ymin=mean_VWC-SD_VWC, ymax=mean_VWC+SD_VWC, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Volumetric soil moisture") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

ggplot(all_data_ribbon, aes(x=(common_year), y=mean_ALT, group=Scenario)) +
  #+scale_color_manual(legend_title,values=plt.cols.short)+
  geom_line(aes(x=(common_year), y=mean_ALT, colour=Scenario))+
  #geom_line(aes(x=ActualYear, y=mean_VWC, group=Scenario), color="grey") +
  geom_ribbon(aes(ymin=mean_ALT-SD_ALT, ymax=mean_ALT+SD_ALT, fill=Scenario), alpha=0.25, show.legend = FALSE)+ 
  ylim(-4,0) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values = plt.cols.short) +
  scale_fill_manual(values = plt.cols.short)+
  theme_classic()+xlab(NULL)+ylab("Seasonally Freezing Thickness (m)") + theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(1990, 2040, 10))

##########################################################################

