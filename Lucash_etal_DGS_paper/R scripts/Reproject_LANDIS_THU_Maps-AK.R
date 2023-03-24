# R script to reprojecting all the THU maps so they can be used for future analysis.

library(raster)

SetLandisCRS <- function (from, to) {
  extent(from) <- extent(to)
  projection(from) <- projection(to)
  return(from)
}

time_step<-seq(0, 50,by=1) 
time_step<-c(1, 50) 

sim_dir<-("E:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/")  #New computer external HD
sim_dir<-("C:/Users/mlucash/Alaska_Reburns_Project_Sims/")  #New computer external HD
setwd(sim_dir)

scen_list<-c("Calib_Landscape_Scrapple_230302_CC_A")

climate_map<-(paste0(sim_dir, scen_list, "/ClimateMap_10regions_DaltonArea3.tif")) #landis input file (ecoregion map)
spatial_reference <- raster(climate_map) #rasterize the input file.

Scenario_LUT <- read.csv (paste0(model_dir,"Scenarios_DGS_Paper.csv"))
full_scen_list<-(Scenario_LUT[c(31:40),1]) #historical + CC


for (t in 1:length(full_scen_list)){
  scenario<-full_scen_list[t]
sp_biomass_dir <- paste(sim_dir, scenario, "/DGS/", sep="") #directory of biomass raster
all_sp_biomass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.

for (s in 1:length(all_sp_biomass_files)){ #for each species...
  #sp_biomass_dir <- paste(sim_dir, scenario, "/biomass/", sep="") #directory of biomass raster
  #all_sp_biomass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.
    spp_LANDIS<-all_sp_biomass_files[s]
  spp_LANDIS_eachRaster<-raster(paste(sim_dir,scenario,"/DGS/", spp_LANDIS,sep=""))
  spp_LANDIS_newName<-(paste(sim_dir,scenario,"/DGS_rp/", spp_LANDIS,sep=""))

projectedLandisOutputRaster <- SetLandisCRS(spp_LANDIS_eachRaster, spatial_reference)
writeRaster(projectedLandisOutputRaster, spp_LANDIS_newName,datatype='INT4S', overwrite=TRUE)
}
}
rm(t)
rm(s)
  print("done")
