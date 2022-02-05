library(raster)
library(rgdal)

SetLandisCRS <- function (from, to) {
  extent(from) <- extent(to)
  projection(from) <- projection(to)
  return(from)
}


sim_dir <-"D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"
setwd(sim_dir)

example_scenario<-c("Calib_Landscape_Scrapple_220119_CC_A")

climate_map<-(paste0(sim_dir, example_scenario, "/ClimateMap_10regions_DaltonArea3.tif")) #landis input file (ecoregion map)
spatial_reference <- raster(climate_map) #rasterize the input file.

Scenario_LUT <- read.csv ("Scenarios_DGS_Paper.csv")
scen_list<-(Scenario_LUT[c(1:5, 26:30),1]) #ran 6-25

for (t in 1:length(scen_list)){
  scenario<-scen_list[t]
DGS_biomass_dir <- paste(sim_dir, scenario, "/DGS/", sep="") #directory of biomass raster
#all_sp_biomass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.
thu_map_files <- list.files(paste0(DGS_biomass_dir), pattern = "THU", recursive = T, full = T)

for (s in 1:length(thu_map_files)){ #for each thu...
  thu_LANDIS<-thu_map_files[s]
  thu_LANDIS_eachRaster<-raster(thu_LANDIS)
  thu_LANDIS_name<-paste0("THU-",s,".img")
  dir.create(file.path(paste0(sim_dir, scenario, "/DGS_rp/")), showWarnings = FALSE)
  new_thu_dir <- paste(sim_dir, scenario, "/DGS_rp/", sep="") #directory of biomass raster
  thu_LANDIS_newName<-(paste(new_thu_dir,thu_LANDIS_name,sep=""))

projectedLandisOutputRaster <- SetLandisCRS(thu_LANDIS_eachRaster, spatial_reference)
writeRaster(projectedLandisOutputRaster, thu_LANDIS_newName,datatype='INT4S', overwrite=TRUE)
}
}
rm(t)
rm(s)
  print("done")
plot(projectedLandisOutputRaster)
