retro <- read.csv(file.choose())
retro$GmRuns <- retro$visitor_score + retro$home_score

#Slice the data to get total umpire runs, games, and park specific runs/games
hpUmps <- subset(retro, select = c(hp_ump_name, park, GmRuns))
hpUmpsTotRuns <- ddply(hpUmps, .(hp_ump_name), summarise ,TotalRuns=sum(GmRuns))
hpUmpsPakrkRuns <- ddply(hpUmps, .(hp_ump_name, park), summarise ,TotalParkRuns=sum(GmRuns))
hpUmpsTotGames <- ddply(hpUmps, .(hp_ump_name), summarise, TotalGames=length((hp_ump_name)))
hpUmpsParkGames <- ddply(hpUmps, .(hp_ump_name, park), summarise ,ParkGms=length(hp_ump_name))

#Join data to get all info into one dataframe
hpUmpRunsByPark <- left_join(hpUmpsParkGames, hpUmpsPakrkRuns, by = c("hp_ump_name","park")) #get park specific runs
hpUmpRunsByPark <- left_join(hpUmpRunsByPark, hpUmpsTotGames, by = "hp_ump_name" ) #get total games by umpire
hpUmpRunsByPark <- left_join(hpUmpRunsByPark, hpUmpsTotRuns, by = "hp_ump_name") #get total runs by ump

#calcuate overall runs/game and park specific runs/game
hpUmpRunsByPark$TotRunsGm <- round(hpUmpRunsByPark$TotalRuns / hpUmpRunsByPark$TotalGames,2) 
hpUmpRunsByPark$ParkRunsGm <- round(hpUmpRunsByPark$TotalParkRuns / hpUmpRunsByPark$ParkGms,2)

#calculate park speific and total runs/gm differental
hpUmpRunsByPark$ParkDiff <- hpUmpRunsByPark$ParkRunsGm - hpUmpRunsByPark$TotRunsGm

#Subset into coors only dataframe
coors <- subset(hpUmpRunsByPark, park == 'DEN02')




write.csv(hpUmpRunsByPark,"hpUmpRunsByPark_2010_2015.csv")


