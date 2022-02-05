# Make a figure that shows calibrated and observed soil moisture and temperature for all three study sites. 
library(tidyverse)
library(lubridate)
library(data.table)
library(ggthemes)
library(patchwork)
library(pals)

project_dir <- "C:/Users/mlucash/Dropbox (University of Oregon)/NSF ReburnsAK LANDIS Results/NSF ReburnsAK LANDIS Results"
#project_dir <- "~/Dropbox/NSF ReburnsAK LANDIS Results (1)"
sites <- c("SmithLake2", "UP1A", "US-Rpf")

site_dirs <- list.files(project_dir, pattern = str_c(sites, collapse = "|"),
                        full = T)
result_dirs <- map_chr(site_dirs, list.files, pattern = "2022_0120", full = T, recursive = T, include.dirs = T) # works as long as no other files share this pattern

site_info <- data.frame(sites = sites) %>% 
  mutate(result_dirs = result_dirs,
         year = list(2014:2018, 2009:2011, 2012:2016),
         temp_depths = list(c(0.1, 1, 3), c(0.1, 0.5, 1), c(0.1, 0.5, 0.9)),
         moi_depths = list(c(0.2, 0.4, 0.85), c(0.1, 0.2, 0.5), c(0.02, 0.3))) %>% 
  split(.$sites)

# Smith Lake 2 observations: 
sl2_moi <- read_csv(paste0(site_dirs[1], "/calibration_data/observed_soil_moi.csv")) %>% 
  select(-source) %>% 
  rename(value = VWC) %>% 
  mutate(variable = "VWC") %>% 
  filter(depth %in% unlist(site_info$SmithLake2$moi_depths))
sl2_temp <- read_csv(paste0(site_dirs[1], "/calibration_data/Smith_lake_2_heave_corrected_interp1d.csv")) %>% # from Dmitry, Jan 13 2022
  pivot_longer(-Time, names_to = "depth", values_to = "value") %>% 
  mutate(depth = as.numeric(str_remove(depth, "m"))) %>% 
  mutate(date = mdy(Time)) %>% 
  select(-Time) %>% 
  mutate(variable = "soil_temp") %>% 
  mutate(depth = ifelse(depth == 1.06, 1.0, depth))
  
sl2_obs <- bind_rows(sl2_moi, sl2_temp) %>% 
  mutate(site = sites[1]) %>% 
  mutate(year = year(date))

sl2_depths <- data.frame(variable = rep("soil_temp", 3),
                         depth = unlist(site_info$SmithLake2$temp_depths)) %>% 
  bind_rows(data.frame(variable = rep("VWC", 3),
                       depth = unlist(site_info$SmithLake2$moi_depths)))

sl2_obs <- inner_join(sl2_obs, sl2_depths, by = c("variable", "depth")) 

# UP1A read in observations: 
up1a_temp <- read_csv(paste0(site_dirs[2], "/calibration_data/3_ST_UP1A_2008-2016.txt")) %>% 
  filter(depth != "Surface") %>% 
  mutate(depth = as.numeric(depth)/100) %>% 
  group_by(date, depth) %>% 
  summarise(value = mean(tempC, na.rm = T)) %>% 
  mutate(variable = "soil_temp")

up1a_moi <- read_csv(paste0(site_dirs[2], "/calibration_data/5_SM_UP1A_2003-2016.txt")) %>% 
  filter(depth != "Surface") %>% 
  mutate(depth = as.numeric(depth)/100) %>% 
  group_by(date, depth) %>% 
  summarise(value = mean(VWC, na.rm = T)) %>% 
  mutate(variable = "VWC")

up1a_obs <- bind_rows(up1a_temp, up1a_moi) %>% 
  mutate(site = sites[[2]]) %>% 
  mutate(year = year(date))

up1a_depths <- data.frame(variable = rep("soil_temp", 3),
                         depth = unlist(site_info$UP1A$temp_depths)) %>% 
  bind_rows(data.frame(variable = rep("VWC", 3),
                       depth = unlist(site_info$UP1A$moi_depths)))

up1a_obs <- inner_join(up1a_obs, up1a_depths, by = c("variable", "depth")) 

# US-Rpf read in observations
usrpf_obs <- read_csv(paste0(site_dirs[3], "/calibration_data/US_Rpf_observed_soils.csv"),
                      col_types = "Tddd") %>% 
  mutate(depth = depth/100) %>% 
  mutate(date = date(date_time)) %>% 
  group_by(date, depth) %>% 
  summarise(VWC = mean(VWC, na.rm = T),
            soil_temp = mean(temperature, na.rm = T)) %>% # these should maybe be end of day? 
  ungroup() %>% 
  pivot_longer(cols = c("VWC", "soil_temp"), names_to = "variable", values_to = "value") %>% 
  mutate(site = sites[[3]]) %>% 
  mutate(year = year(date))

rpf_depths <- data.frame(variable = rep("soil_temp", 3),
                         depth = unlist(site_info$`US-Rpf`$temp_depths)) %>% 
  bind_rows(data.frame(variable = rep("VWC", 2),
                      depth = unlist(site_info$`US-Rpf`$moi_depths)))

usrpf_obs <- inner_join(usrpf_obs, rpf_depths, by = c("variable", "depth"))

# Combine observations:
obs <- bind_rows(up1a_obs, sl2_obs) %>% bind_rows(usrpf_obs)%>% 
  mutate(case = "Observed")
                        

# Read in modeled soil moisture and temperature for all three sites.
modeled_dat <- map(site_info, function(site1){
  x <- as.character(site1$sites)
  
  # Liquid water content:  
  out_liquid <- list.files(site1$result_dirs[[1]], full = T, recursive = T, pattern = "liquid")
  sizes <- file.size(out_liquid)
  out_liquid <- out_liquid[which.max(sizes)] # select the biggest output file. 
  
  moi <- fread(out_liquid) %>% 
    select(-which(duplicated(colnames(.)))) %>% # somehow 10 cm column name is duplicated at US Rpf. 
    slice(186:nrow(.)) %>% # get rid of burn-in for first six months. 
    mutate(date = ymd(paste0(YR, "-01-01")) + DY - 1) %>%
    dplyr::select(-DY, -HR, -YR) %>% 
    pivot_longer(-date, names_to = "depth", values_to = "value") %>% 
    mutate(case = "Modeled",
           site = x,
           variable = "VWC") %>% 
    mutate(depth = as.numeric(depth)) %>% 
    mutate(depth = ifelse(depth == 0.31, 0.3, depth)) %>% 
    mutate(depth = ifelse(depth == 0.02, NA, depth)) %>% 
    filter(depth %in% site1$moi_depths[[1]],
           year(date) %in% site1$year[[1]]) 
  
  # Now temperature: - use the same THU as for liquid.
  thu_dir <- basename(dirname(out_liquid))
  out_temp <- list.files(paste0(site1$result_dirs[[1]], "/Outputs/GIPL"), full = T, recursive = T,
                         pattern = thu_dir) # everything in GIPL outputs
  out_temp <- str_subset(out_temp, "Snow", negate = T)
  
  soiltemp_gipl <- data.table::fread(out_temp, skip = 0, header = T) 
  names(soiltemp_gipl)[c(1, 2)] <- c("year", "index")
  soiltemp_gipl <- soiltemp_gipl %>% 
    slice(186:nrow(.)) %>% # get rid of burn-in, first six months.
    mutate(day_index = 1:nrow(.)) %>% 
    mutate(date = min(moi$date) + day_index - 1) %>% 
    dplyr::select(-day_index, -index, -year) %>% 
    pivot_longer(-date, names_to = "depth", values_to = "value") %>% 
    mutate(depth = round(as.numeric(depth), 2)) %>% 
    mutate(depth = ifelse(depth == 0.12, 0.1, depth)) %>% 
    filter(depth %in% site1$temp_depths[[1]],
           year(date) %in% site1$year[[1]]) %>% 
    mutate(case = "Modeled",
           site = x,
           variable = "soil_temp")

   mod_dat <- bind_rows(moi, soiltemp_gipl)
  
  return(mod_dat)
}) %>% 
  bind_rows() %>% 
  mutate(year = year(date))
  
# Combine. 
all_dat <- bind_rows(modeled_dat, obs) %>% 
  filter(!is.na(depth)) 

# Only keep years of interest
plot_years <- map(site_info, function(x){
  data.frame(year = x$year[[1]],
             site = rep(x$sites, length(x$year[[1]])))
}) %>% bind_rows()
plot_dat <- inner_join(all_dat, plot_years)

# Plot. 
moi_plot <- plot_dat %>% 
  filter(variable == "VWC") %>% 
  mutate(value = ifelse(site == "UP1A" & value > 0.5, NA, value)) %>% # get rid of one spike in UP1A that screws up y axis
  ggplot(aes(x = date, y = value, color = as.factor(depth),
             linetype = case)) + 
  geom_line(show.legend = T) + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_color_manual(values = tol(6)[6:1]) + 
  scale_x_date(expand = c(0, 0)) + 
  facet_wrap(~ site, ncol = 1, scales = "free", strip.position = "left") + 
  theme_few() + 
  labs(x = "", y = expression(theta[VWC]), color = "depth", linetype = "") + 
  theme(strip.placement = "outside") 

moi_plot

pdf(paste0(project_dir, "/figures/DGS_moi_calibration.pdf"),
    width = 9, height = 6)
png(paste0(project_dir, "/figures/DGS_moi_calibration.png"),
    width = 800, height = 600, units="px")
tiff(paste0(project_dir, "/figures/DGS_moi_calibration.tiff"),
    width = 800, height = 600, units="px")
moi_plot
dev.off()

temp_plot <- plot_dat %>% 
  filter(variable == "soil_temp") %>% 
  ggplot(aes(x = date, y = value, color = as.factor(depth),
             linetype = case)) + 
  geom_line(show.legend = T) + 
  scale_linetype_manual(values = c(2, 1)) + 
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") + 
  scale_color_manual(values = tol(6)[6:1]) + 
  scale_x_date(expand = c(0, 0)) + 
  facet_wrap(~site, ncol = 1, scales = "free", strip.position = "left") + 
  labs(x = "", y = "Soil temperature (°C)", color = "Depth (m)", linetype = "") +
  theme_few() + 
  theme(strip.placement = "outside") 

temp_plot

pdf(paste0(project_dir, "/figures/DGS_temp_calibration.pdf"),
    width = 9, height = 6)
png(paste0(project_dir, "/figures/DGS_temp_calibration.png"),
    width = 800, height = 600, units="px")
tiff(paste0(project_dir, "/figures/DGS_temp_calibration.TIFF"),
    width = 800, height = 600, units="px")
temp_plot
dev.off()

# Calculate RMSE for all years --------
# Fow VWC, remove cases in Oct-April - this is how we did it for birch paper. 
rmse <- function(obs, mod){
  x1 <- obs-mod
  x2 <- x1^2 # square each diff
  num <- sum(x2, na.rm = T)
  ans <- sqrt(num/length(obs))
  return(ans)
}

all_dat %>% 
  mutate(month = month(date)) %>% 
  mutate(value = ifelse(month %in% c(11, 12, 1, 2, 3) & variable == "VWC", NA, value)) %>% 
  pivot_wider(names_from = "case", values_from = "value") %>% 
  group_by(depth, site, variable) %>% 
  summarise(rmse = rmse(Observed, Modeled)) %>% 
  ungroup() %>% 
  arrange(site, variable, depth)

