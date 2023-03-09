Within each veg_type_by_year.zip: 

series of csvs named according to their scenario and timestep-

hist_ncar = Historical (1970-1999) NCAR-CCSM4
fut_gfdl = Future (2000-2100) GFDL-CM3
fut_ncar = Future (2000-2100) NCAR-CCSM4

1 csv file per simulation year, populated with relative species biomass information and resulting veg type assignment for each cell
cell_num = cell number 
each_time = timestep
species names = gives biomass of all cohorts of that species in the give cell
conifer = combined conifer species biomass
decid_broadleaf = combined broadleaf deciduous species biomass
total = total tree biomass
perc_conifer = percent conifer biomass / total tree biomass
perc_decid = percent deciduous biomass / total tree biomass
veg_type = vegetation type assigned based on relative dominance (conifer, deciduous broadleaf, mixed conifer-deciduous broadleaf, or non-forest)
	conifer veg type --> perc_conifer > 66.66%
	deciduous broadleaf veg type --> perc_decid > 66.66%
	mixed deciduous broadleaf veg type --> perc_decid > 33.33% & perc_decid < 66.66%
	non-forest --> total = 0 (i.e. no tree biomass in cell)