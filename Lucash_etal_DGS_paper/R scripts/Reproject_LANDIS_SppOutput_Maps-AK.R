library(raster)

SetLandisCRS <- function (from, to) {
  extent(from) <- extent(to)
  projection(from) <- projection(to)
  return(from)
}

sim_dir <-"D:/Lucash/ResearchAssistantProfessor/Alaska_Reburns_Project/Sims_DGS_methods_paper/Jan_2022_Sims/"
setwd(sim_dir)

#historical scenario
scen_list<-c("Calib_Landscape_Scrapple_220119_CC_A")

climate_map<-(paste0(sim_dir, scen_list,"/ClimateMap_10regions_DaltonArea3.tif")) #landis input file (ecoregion map)
spatial_reference <- raster(climate_map) #rasterize the input file.

Scenario_LUT <- read.csv ("Scenarios_DGS_Paper.csv")
scen_list<-(Scenario_LUT[c(1:5, 26:30),1]) #ran 6-25

for (t in 1:length(scen_list)){
  scenario<-scen_list[t]
sp_biomass_dir <- paste(sim_dir, scenario, "/biomass/", sep="") #directory of biomass raster
#all_sp_biomass_files <- list.files(sp_biomass_dir) #all the species biomass files +total biomass file.
all_total_biomass_files <- list.files(paste0(sp_biomass_dir), pattern = "TotalBiomass-", recursive = T, full = T)

#for (s in 1:length(all_sp_biomass_files)){ #for each species...
  for (s in 1:length(all_total_biomass_files)){ #for each total biomass map...
    biomass_LANDIS<-all_total_biomass_files[s]
    biomass_LANDIS_eachRaster<-raster(biomass_LANDIS)
    biomass_LANDIS_name<-paste0("TotalBiomass-",s-1,".img")
    dir.create(file.path(paste0(sim_dir, scenario, "/biomass_rp/")), showWarnings = FALSE)
    new_biomass_dir <- paste(sim_dir, scenario, "/biomass_rp/", sep="") #directory of biomass raster
    biomass_LANDIS_newName<-(paste(new_biomass_dir,biomass_LANDIS_name,sep=""))

projectedLandisOutputRaster <- SetLandisCRS(biomass_LANDIS_eachRaster, spatial_reference)
writeRaster(projectedLandisOutputRaster, biomass_LANDIS_newName,datatype='INT4S', overwrite=TRUE)
}
}
rm(t)
rm(s)
  print("done")
