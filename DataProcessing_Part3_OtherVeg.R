
vegdata <- read_excel("./EWPW_mastersheet2025_23oct2025.xlsx", sheet = "veg") # read in Kara's data
vegdata <- as.data.frame(vegdata) # turn into data.frame
#View(vegdata)

# create blank df to hold data
big_veg_df <- data.frame(point_id = character(),
                         date = character(),
                         studyarea = character(),
                         time = numeric(),
                         lat = numeric(),
                         long = numeric(),
                         response = numeric(),
                         nudds = numeric(),
                         perc_bare = numeric(),
                         perc_ll = numeric(),
                         perc_grass = numeric(),
                         perc_forb = numeric(),
                         perc_vine = numeric(),
                         perc_Rubus = numeric())

for(i in 1:nrow(vegdata)){
  # i = 1
  
  # isolate point i's veg data
  veg_i <- vegdata[i,]
  
  # we want this!
  # Bird_ID
  point_id_i <- veg_i$point_id
  
  # date
  date_i <-  veg_i$`Observation Date (M/DD)`
  
  # study area
  studyarea_i <-  ifelse(veg_i$Lat > 40.5, "moshannon", "michaux")
  
  # time
  time_i <- veg_i$`Observation Time (24 hr)`
  
  # lat
  lat_i <- veg_i$Lat
  
  # long
  long_i <- veg_i$Long
  
  # type (used vs random)
  response_i <- ifelse(veg_i$`Survey Type` == "used", 1, 0)
  
  # AvgNudds
  nudds_i <- mean(veg_i$NUDDNorth, veg_i$NUDDEast, veg_i$NUDDSouth, veg_i$NUDDWest)
  
  # Avg bare ground
  perc_bare_i <- mean(veg_i$`NDF% bare ground`, veg_i$`SDF% bare ground`)
  
  # Avg leaf litter
  perc_ll_i <- mean(veg_i$`NDF% leaf litter`, veg_i$`SDF% leaf litter`)
  
  # Avg grass
  perc_grass_i <- mean(veg_i$`NDF%grass`, veg_i$`SDF%grass`)
  
  # Avg forb
  perc_forb_i <- mean(veg_i$`NDF%forb`, veg_i$`SDF%forb`)
  
  # Avg vine
  perc_vine_i <- mean(veg_i$`NDF%vine`, veg_i$`SDF%vine`)
  
  # Avg Rubus
  perc_Rubus_i <- mean(veg_i$`NDF%Rubus`, veg_i$`SDF%Rubus`)
  
  new_veg_row <- data.frame(point_id = point_id_i,
                            date = date_i,
                            studyarea = studyarea_i,
                            time = time_i,
                            lat = lat_i,
                            long = long_i,
                            response = response_i,
                            nudds = nudds_i,
                            perc_bare = perc_bare_i,
                            perc_ll = perc_ll_i,
                            perc_grass = perc_grass_i,
                            perc_forb = perc_forb_i,
                            perc_vine = perc_vine_i,
                            perc_Rubus = perc_Rubus_i)
  big_veg_df <- rbind(big_veg_df, new_veg_row)
  print(paste0("row number ", i, " is done!"))
  Sys.sleep(0.005)
}

write.csv(big_veg_df, "./otherveg_cleaned.csv", row.names = F)
