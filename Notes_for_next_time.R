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
# Avg#SmSap
# Avg#MedSap
# Avg#LgSap
# Avg#SmShrub
# Avg#MedShrub
# Avg#LgShrub

# Let's begin with the woody stem data
woodystems <-read.csv("./woodystems_raw.csv")
head(woodystems)

# create blank df to hold data
big_woody_df <- data.frame(point_id = 0, 
                           AvgPrism = 0,
                           AvgNoSmSap = 0,
                           AvgNoMedSap = 0,
                           AvgNoLgSap = 0,
                           AvgNoSmShrub = 0,
                           AvgNoMedShrub = 0,
                           AvgNoLgShrub = 0)

for(i in 1:length(unique(woodystems$point_id))){
  # i = 100
  
  woodystems_i <- subset(woodystems, point_id == i)
  
  point_id_i = woodystems_i$point_id
  AvgPrism_i = 0
  AvgNoSmSap_i = 0
  AvgNoMedSap_i = 0
  AvgNoLgSap_i = 0
  AvgNoSmShrub_i = 0
  AvgNoMedShrub_i = 0
  AvgNoLgShrub_i = 0

  
}


