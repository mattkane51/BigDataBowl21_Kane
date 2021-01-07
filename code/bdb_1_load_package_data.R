# install packages
{
  install.packages('gganimate')
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyverse')
  install.packages('mvtnorm') 
  install.packages('tidyr')
  install.packages('ggnewscale')
  install.packages('purrr')
  install.packages('rlang')
  install.packages('stringr')
  install.packages('magick')
}

# load packages
{
  library(gganimate)
  library(ggplot2)
  library(dplyr)
  library(tidyverse)
  library(mvtnorm) 
  library(tidyr)
  library(ggnewscale)
  library(purrr)
  library(rlang)
  library(stringr)
  library(magick)
}

# load & alter play / game data
{
  play = read.csv('D:/Downloads/NFL/plays.csv')
  game = read.csv('D:/Downloads/NFL/games.csv')
  targ = read.csv('D:/Downloads/targetedReceiver.csv')
  
  game$homeTeamAbbr = as.character(game$homeTeamAbbr)
  game$visitorTeamAbbr = as.character(game$visitorTeamAbbr)
  play$possessionTeam = as.character(play$possessionTeam)
  play$possTeam = as.character(play$possessionTeam)
  play$sideTeam = as.character(play$yardlineSide)
  play$yardline = play$yardlineNumber
  play$time = as.character(play$gameClock)
  play$times = play$playResult
  
  play = play %>%
    mutate(yardline = if_else(sideTeam == possTeam, 100-yardlineNumber, 1*yardlineNumber)) %>%
    mutate(times = ifelse(quarter == 1 | quarter == 3, 
                          900+60*as.numeric(substr(time,1,2))+as.numeric(substr(time,4,5)),
                          60*as.numeric(substr(time,1,2))+as.numeric(substr(time,4,5))))
}
