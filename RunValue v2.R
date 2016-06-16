#####################################################################################################
#                                 How Many Wins are 10 random runs worth?                           # 
#                                 Code written by: Jake Witteveen                                   #
#####################################################################################################


#read in header for retrosheet data
header <- read.csv("retrosheet data attributes.csv")
headList <- header[,1]

#set working dir and obtain list of files
setwd("xxx") #set working dir to wherever the files are stored
seasonFiles <- list.files()

##################################Combining season level data into one DataFrame##################
#create empty dataframe for season game logs
seasonFull <- data.frame(year=character(),
                         game_number=integer(),
                         team=factor(),
                         opponent=factor(),
                         team_score=integer(),
                         opponent_score=integer())

#Loop to read all of the season logs in working dir
for (seasons in seasonFiles) {
  season <- read.csv(seasons, header = FALSE, col.names = headList)
  season$year <- substr(season$date,0,4)

  #create dataframe for all home teams
  seasonHome <- subset(season,
                      select = c("year","game_number","home","visitor","home_score","visitor_score"))
  colnames(seasonHome)[c(3:6)] <- c("team","opponent","team_score","opponent_score")

 #create dataframe for all away teams
  seasonAway <- subset(season,
                       select = c("year","game_number","visitor","home","visitor_score","home_score"))
  colnames(seasonAway)[c(3:6)] <- c("team","opponent","team_score","opponent_score")

  #combine home and away team game logs
  seasonBind <- bind_rows(seasonHome,seasonAway)
  
  #add new season to total dataframe
  seasonFull <- bind_rows(seasonFull,seasonBind)

}

#add W/L to season dataframe
seasonFull$game_result <- ifelse(seasonFull$team_score>seasonFull$opponent_score, "W","L")

##################################EXTRA RUN SIMULATION########################################
years <- (unique(seasonFull$year)) #get number of years in dataframe
incWinYear <- data.frame(yr=factor(),lgWinInc=integer()) #empty dataframe for league wide benifit by year

for (yr in years){

incWinsDF <- data.frame(tm=factor(),avgIncWins=integer()) #create empty dataframe for average inc team wins
seasonFullYr <- subset(seasonFull, year == yr) #get dataframe for specific season
teams <- (unique(seasonFullYr$team)) #get list of teams in the seasonFull dataframe

  #create loop to preform simulation for each team
  for (tm in teams){
    teamObs <- data.frame(sim=integer(),addWins=integer()) #create empty dataframe for team simulations
    teamDF <- subset(seasonFullYr, team == tm) #subset season data for specific team
    rows <- nrow(teamDF) #count number of rows (shoud be 162, one for each game of the season)
  
    
    #simulation loop
    set.seed(1234)
    for (i in 1:1000) {
  
      sampleIndex <- sample(c(1:rows), 10, replace = FALSE) #randomly select 10 games for extra run
      extraRun <- teamDF[sampleIndex,] #get subset of 10 games
      addWins <- sum((extraRun$team_score + 1 - extraRun$opponent_score) == 0) * 0.5 #give team extra run and assume team will win 50% of games that will go into extras
  
      sim <- i
      x <- data.frame(sim,addWins)
      teamObs <- bind_rows(teamObs,x) #add each simulation to team dataframe
  
      }

    avgIncWins <- mean(teamObs$addWins)
    y <- data.frame(tm,avgIncWins)
    incWinsDF <- bind_rows(incWinsDF,y) #add each average team result to current season dataframe
  
  }

lgWinInc <- mean(incWinsDF$avgIncWins)
z <- data.frame(yr, lgWinInc)
incWinYear <- bind_rows(incWinYear,z) #add average year result to overall season dataframe

}

##################################Computing Summary Stats by Year###################################

seasonStats <- incWinYear #create new dataframe
colnames(seasonStats)[c(1:2)] <- c("year","winValue") #rename columns


seasonFull$oneRun <- ifelse(abs(seasonFull$team_score-seasonFull$opponent_score)==1,1,0) #add one run game flag
temp <- ddply(seasonFull, "year", summarize,
                    total_runs=sum(team_score),
                    games=NROW(year),
                    oneRun=sum(oneRun)/2
              )
temp$runsGm <- temp$total_runs / temp$games
temp$oneRunPct <- temp$oneRun / (temp$games/2)

seasonStats <- left_join(seasonStats, temp, by = "year")
seasonStats$runsWin <- 10/seasonStats$winValue

#look at correlation matrix
cor(seasonStats[,c(2:4,6,7)])[1,c(4,5)]
cor(seasonStats[,c(2:7)])

valuePlot <- (ggplot(seasonStats,aes(winValue,oneRunPct)) + geom_point(col="firebrick3") +
               labs(title='Value of Ten Runs vs. One Run Gm Pct', x='10 Run Win Value', y='One Run Gm Pct') + 
               theme(plot.title = element_text(size=12, face = "bold"),
                     axis.title.x = element_text(size=10),
                     axis.title.y = element_text(size=10))+
                stat_smooth(method = lm, se=FALSE, col="black"))
valuePlot

rsePlot <- (ggplot(seasonStats,aes(runsGm,oneRunPct)) + geom_point(col="dodgerblue") +
                labs(title='Runs/Game vs. One Run Gm Pct', x='Runs/Game', y='One Run Gm Pct') + 
                theme(plot.title = element_text(size=12, face = "bold"),
                      axis.title.x = element_text(size=10),
                      axis.title.y = element_text(size=10))+
                stat_smooth(method = lm, se=FALSE, col="black"))
rsePlot




#save data frames used
saveRDS(seasonFull, file = "1990-2015 Game Results.rds")
saveRDS(seasonStats, file = "1990-2015 Seasn Summaries.rds")

#seasonFullYr <- readRDS("seasonFullYr.rds") #read back in data
#seasonStats <- readRDS("seasonStats.rds") #read back in data

