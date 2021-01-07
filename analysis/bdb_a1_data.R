
w1 = read.csv("D:/Downloads/final_week_1.csv")
w2 = read.csv("D:/Downloads/final_week_2.csv")
w3 = read.csv("D:/Downloads/final_week_3.csv")
w4 = read.csv("D:/Downloads/final_week_4.csv")
w5 = read.csv("D:/Downloads/final_week_5.csv")
w6 = read.csv("D:/Downloads/final_week_6.csv")
w7 = read.csv("D:/Downloads/final_week_7.csv")
w8 = read.csv("D:/Downloads/final_week_8.csv")
w9 = read.csv("D:/Downloads/final_week_9.csv")
w10 = read.csv("D:/Downloads/final_week_10.csv")
w11 = read.csv("D:/Downloads/final_week_11.csv")
w12 = read.csv("D:/Downloads/final_week_12.csv")
w13 = read.csv("D:/Downloads/final_week_13.csv")
w14 = read.csv("D:/Downloads/final_week_14.csv")
w15 = read.csv("D:/Downloads/final_week_15.csv")
w16 = read.csv("D:/Downloads/final_week_16.csv")
w17 = read.csv("D:/Downloads/final_week_17.csv")


wk = rbind(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17)

rm(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17)

playgame = merge(play, game, by = c('gameId'))

playgame$isondef = if_else(playgame$possessionTeam == playgame$homeTeamAbbr, "away", "home")


week_def = merge(wk, playgame, by = c('gameId', 'playId'))

week_def$position = as.character(week_def$position)
week_def$route = as.character(week_def$route)

defplayers = subset(week_def, !is.na(QOSi) & team == isondef) %>%
  distinct(gameId, playId, nflId, frameId, event, position, x, y, QOSi, QOSi_sv) %>%
  select(gameId, playId, nflId, frameId, event, position, x, y, QOSi, QOSi_sv)

defplayers = defplayers %>%
  group_by(gameId, playId, nflId) %>%
  mutate(QOSi_prev = lag(QOSi, n = 1, default = NA), QOSi_sv_prev = lag(QOSi_sv, n = 1, default = NA),
         QOSi_prev3 = lag(QOSi, n = 3, default = NA), QOSi_sv_prev3 = lag(QOSi_sv, n = 3, default = NA))

offplayers = subset(week_def, !is.na(QOSi) & team != isondef) %>%
  distinct(gameId, playId, nflId, frameId, position, x, y, route, QOSi, QOSi_sv) %>%
  select(gameId, playId, nflId, frameId, position, x, y, route, QOSi, QOSi_sv)

offplayers = offplayers %>%
  pivot_wider(names_from = position, values_from = c(nflId, x, y, route, QOSi, QOSi_sv))


for(ps in c('WR', 'QB', 'TE', 'RB', 'FB')) {
  
  for(cl in c('nflId', 'x', 'y', 'route', 'QOSi', 'QOSi_sv')){
    
    clps = paste(cl, '_', ps, sep = '')
    
    offplayers = offplayers %>%
      unnest(cols = c(paste(clps))) %>%
      group_by(gameId, playId, frameId) %>%
      mutate(col=paste(clps, seq_along(!!as.name(clps)), sep = '')) %>%
      spread(key = col, value = paste(clps)) %>%
      right_join(offplayers)
    
    offplayers = offplayers[1:(ncol(offplayers)-1)]
    
  }
  
}

# head(offplayers)

offplayers = offplayers[,c(1:3,(ncol(offplayers)-(15*6)+1):ncol(offplayers))]

pydist = merge(defplayers, offplayers, by = c('gameId', 'playId', 'frameId'))
