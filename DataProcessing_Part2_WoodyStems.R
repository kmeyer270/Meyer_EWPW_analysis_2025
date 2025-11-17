# we want to create a file that contains:
# Bird_ID
# date
# study area
# time
# lat
# long
# type (used vs random)
# AvgNudds
# perc_bare
# perc_LL
# perc_grass
# ...
# perc_woody
# AvgPrism
# Avg#SmStem
# Avg#MedStem
# Avg#LgStem

# Let's begin with the woody stem data
woodystems <-read.csv("./woodystems_raw.csv") # 548 veg plots
head(woodystems)

# create blank df to hold data
big_woody_df <- data.frame(point_id = numeric(), 
                           BasalArea = numeric(),
                           AvgNoSmStem = numeric(),
                           AvgNoMedStem = numeric(),
                           AvgNoLgStem = numeric())

for(i in 1:length(unique(woodystems$point_id))){
  # i = 101
  
  # isolate point i's woody stem data
  woodystems_i <- subset(woodystems, point_id == i)
  
  # point ID
  point_id_i = woodystems_i$point_id[1]
  
  # how many edge trees
  edgetrees <- nrow(subset(woodystems_i, class == "basalarea_edge"))
  
  # how many edge trees get counted?
  Countededgetrees <- ifelse(edgetrees == 0, # if # edge trees = 0...
                             0,              # ...# counted edge trees = 0!
                             ifelse(edgetrees %% 2 == 0, # ...but if # edge trees is not 0, check if even...
                                    edgetrees/2, # if even, # counted edge trees = # edge trees / 2, otherwise,
                                    (edgetrees-1)/2)) # counted edge trees = # edge trees-1 / 2!
  
  
  # how many in trees?
  intrees <- nrow(subset(woodystems_i, class == "basalarea"))
  
  # Basal area
  BasalArea_i = (intrees + Countededgetrees)*10
  
  # Small woody stem count
  AvgNoSmStem_i_raw = subset(woodystems_i, class == "small_woody_stems")
  AvgNoSmStem_i = sum(AvgNoSmStem_i_raw$count)
  
  # Medium woody stem count
  AvgNoMedStem_i_raw = subset(woodystems_i, class == "medium_woody_stems")
  AvgNoMedStem_i = sum(AvgNoMedStem_i_raw$count)

  # Large woody stem count
  AvgNoLgStem_i_raw = subset(woodystems_i, class == "large_woody_stems")
  AvgNoLgStem_i = sum(AvgNoLgStem_i_raw$count)
  
  NewRow = data.frame(point_id = point_id_i, 
                      BasalArea = BasalArea_i,
                      AvgNoSmStem = AvgNoSmStem_i,
                      AvgNoMedStem = AvgNoMedStem_i,
                      AvgNoLgStem = AvgNoLgStem_i)
  big_woody_df <- rbind(big_woody_df, NewRow)
  print(paste0("new row ", i, " is done!"))
  Sys.sleep(0.01)
}
big_woody_df
par(mfrow = c(2,2))
hist(big_woody_df$AvgNoSmStem)
hist(big_woody_df$AvgNoMedStem)
hist(big_woody_df$AvgNoLgStem)
hist(big_woody_df$BasalArea)
par(mfrow = c(1,1))
write.csv(big_woody_df, "./woodystems_cleaned.csv", row.names = F)

