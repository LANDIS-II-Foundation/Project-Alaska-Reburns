
## Script used to reproject all the reclass maps.

library(raster)

SetLandisCRS <- function (from, to) {
  extent(from) <- extent(to)
  projection(from) <- projection(to)
  return(from)
}

time_step<-seq(0, 50,by=1) 
#time_step<-c(0, 50) 

sim_dir<-("G:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/")  #New computer external HD
setwd(sim_dir)

#scen_list<-c("Calib_Landscape_Scrapple_210327_CC_A")
Scenario_LUT <- read.csv ("Scenarios_DGS_Paper_NoLimits.csv")
scen_list<-(Scenario_LUT[1:5,1])

climate_map<-(paste0(sim_dir, "/Calib_Landscape_Scrapple_220119A/ClimateMap_10regions_DaltonArea3.tif")) #landis input file (ecoregion map)
spatial_reference <- raster(climate_map) #rasterize the input file.

for (t in 1:length(scen_list)){
  scenario<-scen_list[t]
sp_biomass_dir <- paste(sim_dir, scenario, "/Reclass/", sep="") #directory of biomass raster
all_reclass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.

for (s in 1:length(all_reclass_files)){ #for each species...
  reclass_LANDIS<-all_reclass_files[s]
  reclass_LANDIS_eachRaster<-raster(paste(sim_dir,scenario,"/Reclass/", reclass_LANDIS,sep=""))
  reclassLANDIS_newName<-(paste(sim_dir,scenario,"/Reclass_rp/", reclass_LANDIS,sep=""))

projectedLandisOutputRaster <- SetLandisCRS(reclass_LANDIS_eachRaster, spatial_reference)
writeRaster(projectedLandisOutputRaster, reclassLANDIS_newName,datatype='INT4S', overwrite=TRUE)
}
}
rm(t)
rm(s)
  print("done")
