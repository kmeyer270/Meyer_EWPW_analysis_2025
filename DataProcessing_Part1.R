library(readxl)

vegdata <- read_excel("./EWPW_mastersheet2025_23oct2025.xlsx", sheet = "veg") # read in Kara's data
vegdata <- as.data.frame(vegdata) # turn into data.frame
View(vegdata)

#prism
# 31 - 42 = basal area
# 43 = snag count
# 44 = edge tree count

# north
# 45 - 68 = small woody stem
# 70 - 93 = med woody stem
# 95 - 118 = tall woody stem

# south
# 120 - 143 = small woody stem
# 145 - 168 = med woody stem
# 170 - 193 = tall woody stem

# blank data.frame() to hold data
data_new <- data.frame("point_id" = character(0), # use row number as point_id for now
                       "class" = character(0),"direction" = character(0),
                       "species" = character(0), "count" = character(0))

# for() loop that runs through all points and summarizes the woody stem data
for(i in 2:nrow(vegdata)){
  
  # i = 2 # test the for() loop using i = 2
  
  # data.frame for new chunk
  data_i <- data.frame("point_id" = character(0), # use row number as point_id for now
    "class" = character(0),"direction" = character(0),
                       "species" = character(0), "count" = character(0))
  
  # Basal area
  BA <- vegdata[i, 31:42] # isolate all BA data
    BA_sp <- BA[,c(1,3,5,7,9,11)] # the odd values are species IDs
    BA_n <- BA[,c(2,4,6,8,10,12)] # the even values are counts
    BA_parts <- data.frame( # make a data frame for all the data
        point_id = rep(i, 6), # use row number as point_id for now
        class = rep("basalarea", 6), # all basal area rows get "basalarea"
        direction = rep("none", 6), # direction = "none" for basal area
        species = as.character(BA_sp), # species from above
        count = as.character(BA_n)) # counts from above
    BA_parts <- BA_parts[BA_parts$species != "NA", ] # ditch the rows with NA in species
    BA_parts
    
  # Snags    
  snag <- vegdata[i, 43]
    snag_parts <- data.frame( # make a data frame for all the data
      point_id = i, # use row number as point_id for now
      class = "snag", # all snag rows get "snag"
      direction = "none", # direction = "none" for snag
      species = "snag", # species = snag
      count = snag) # count from above
    snag_parts[, 5][snag_parts[, 5] == "NA"] <- "0" # replace NA values with 0 for snag
    snag_parts
    
  # Edge Trees  
  edge <- vegdata[i, 44]
    edge_parts <- data.frame( # make a data frame for all the data
      point_id = i, # use row number as point_id for now
      class = "basalarea_edge", # all edgetree rows get "basalarea_edge"
      direction = "none", # direction = "none" for edge trees
      species = "nodata", # species = nodata
      count = edge) # count from above
    edge_parts[, 5][edge_parts[, 5] == "NA"] <- "0" # replace NA values with 0 for edge trees
    edge_parts
    
  # NORTH - Small Woody Stems
  nss <- vegdata[i, 45:68]  # isolate all data
  nss_sp <- nss[,c(1,3,5,7,9,11,13,15,17,19,21,23)] # the odd values are species IDs
  nss_n <- nss[,c(2,4,6,8,10,12,14,16,18,20,22,24)] # the even values are counts
  nss_parts <- data.frame( # make a data frame for all the data
    point_id = rep(i, 12), # use row number as point_id for now
    class = rep("small_woody_stems", 12),
    direction = rep("north", 12),
    species = as.character(nss_sp), # species from above
    count = as.character(nss_n)) # counts from above
  nss_parts
  
  # NORTH - Medium Woody Stems
  nms <- vegdata[i, 70:93]  # isolate all data
  nms_sp <- nms[,c(1,3,5,7,9,11,13,15,17,19,21,23)] # the odd values are species IDs
  nms_n <- nms[,c(2,4,6,8,10,12,14,16,18,20,22,24)] # the even values are counts
  nms_parts <- data.frame( # make a data frame for all the data
    point_id = rep(i, 12), # use row number as point_id for now
    class = rep("medium_woody_stems", 12),
    direction = rep("north", 12),
    species = as.character(nms_sp), # species from above
    count = as.character(nms_n)) # counts from above
  nms_parts

  # NORTH - Large Woody Stems
  nls <- vegdata[i, 95:118] # isolate all data
  nls_sp <- nls[,c(1,3,5,7,9,11,13,15,17,19,21,23)] # the odd values are species IDs
  nls_n <- nls[,c(2,4,6,8,10,12,14,16,18,20,22,24)] # the even values are counts
  nls_parts <- data.frame( # make a data frame for all the data
    point_id = rep(i, 12), # use row number as point_id for now
    class = rep("large_woody_stems", 12),
    direction = rep("north", 12),
    species = as.character(nls_sp), # species from above
    count = as.character(nls_n)) # counts from above
  nls_parts
  
  # SOUTH - Small Woody Stems
  sss <- vegdata[i, 120:143]  # isolate all data
  sss_sp <- sss[,c(1,3,5,7,9,11,13,15,17,19,21,23)] # the odd values are species IDs
  sss_n <- sss[,c(2,4,6,8,10,12,14,16,18,20,22,24)] # the even values are counts
  sss_parts <- data.frame( # make a data frame for all the data
    point_id = rep(i, 12), # use row number as point_id for now
    class = rep("small_woody_stems", 12),
    direction = rep("south", 12),
    species = as.character(sss_sp), # species from above
    count = as.character(sss_n)) # counts from above
  sss_parts
  
  # SOUTH - Medium Woody Stems
  sms <- vegdata[i, 145:168] # isolate all data
  sms_sp <- sms[,c(1,3,5,7,9,11,13,15,17,19,21,23)] # the odd values are species IDs
  sms_n <- sms[,c(2,4,6,8,10,12,14,16,18,20,22,24)] # the even values are counts
  sms_parts <- data.frame( # make a data frame for all the data
    point_id = rep(i, 12), # use row number as point_id for now
    class = rep("medium_woody_stems", 12),
    direction = rep("south", 12),
    species = as.character(sms_sp), # species from above
    count = as.character(sms_n)) # counts from above  
  sms_parts
  
  # SOUTH - Large Woody Stems
  sls <- vegdata[i, 170:193] # isolate all data
  sls_sp <- sls[,c(1,3,5,7,9,11,13,15,17,19,21,23)] # the odd values are species IDs
  sls_n <- sls[,c(2,4,6,8,10,12,14,16,18,20,22,24)] # the even values are counts
  sls_parts <- data.frame( # make a data frame for all the data
    point_id = rep(i, 12), # use row number as point_id for now
    class = rep("large_woody_stems", 12),
    direction = rep("south", 12),
    species = as.character(sls_sp), # species from above
    count = as.character(sls_n)) # counts from above  
  sls_parts
  
  # re-assemble new chunk
  newchunk <- rbind(BA_parts, snag_parts, edge_parts, nss_parts, nms_parts, nls_parts,
                    sss_parts, sms_parts, sls_parts)
  newchunk  <- newchunk[newchunk[, 5] != "NA", ] # remove any NA values in "count"
  data_i <- rbind(data_i, newchunk)
  data_i
  
  # add new chunk to big data frame
  data_new <- rbind(data_new, data_i)
  data_new
  
  # print progress and give a short pause
  print(paste0("point ID number ", i, " is done!"))
  Sys.sleep(0.01)
}

#########
######### Cleaning Up the Species
#########

unique(data_new$species)

data_new$species <- gsub("Quercusrubra", "Quercus_rubra", data_new$species)
data_new$species <- gsub("QuercusmontaNA", "Quercus_montana", data_new$species)
data_new$species <- gsub("Nyssasylvatica", "Nyssa_sylvatica", data_new$species)
data_new$species <- gsub("Acerrubrum", "Acer_rubrum", data_new$species)
data_new$species <- gsub("HamamelisvirginiaNA", "Hamamelis_virginiana", data_new$species)
data_new$species <- gsub("Serviceberry", "Amelanchier_sp", data_new$species)
# etc