
# load data from that week
{
  w = 1
  
  imp_flnm = paste("D:/Downloads/NFL/week", w, ".csv", sep = '')
  all = read.csv(imp_flnm)
  
}



# select play
{
  #automatic
  # gmid = play_used$gameId[i]
  # pyid = play_used$playId[i]
  
  #manual
  gmid = 2018090600
  pyid = 2624
}

# load play data
{
  pyused = subset(play, gameId == gmid & playId == pyid)
  
  p0 = subset(all, gameId == gmid & playId == pyid)
  p1 = subset(all, gameId == gmid & playId == pyid)
  
  if (length(unique(p1$team)) != 3) {next}
  
  gm = subset(game, gameId == gmid)
  py = pyused #subset(play, gameId == gmid & playId == pyid)
  
  #xepa given yardline, and time
  ydln = as.numeric(py$yardline[1])
  ytg = as.numeric(py$yardsToGo[1])
  tms = as.numeric(py$times[1])
  dwn = as.numeric(py$down[1])
}

# space impact equations
{
  colnames(p1)[1] <- 'time'
  
  td = subset(p1, displayName != 'Football')
  bd = subset(p1, select = c('time', 'x', 'y', 'frameId'), displayName == 'Football')
  colnames(bd)[2] = 'bx'
  colnames(bd)[3] = 'by'
  
  dat = left_join(td, bd, by = 'time')
  colnames(dat)[14] <- "frameId"
  
  dat$s_x = dat$s * sin(dat$dir*3.1415/180)
  dat$s_y = dat$s * cos(dat$dir*3.1415/180)
  
  dat$s_rat = (dat$s/14)^2
  
  dat$mu_x = dat$x + dat$s_x /2
  dat$mu_y = dat$y + dat$s_y /2
  
  dat$dx_m = (dat$x-dat$bx)*0.9144
  dat$dy_m = (dat$y-dat$by)*0.9144
  dat$dist = sqrt((dat$dx_m)^2+(dat$dy_m)^2)
  dat$ri = 4 + (((sqrt((dat$dx_m)^2+(dat$dy_m)^2))^3) / ((18^3)/ 6))
  
  dat = dat %>%
    mutate(ri = if_else(ri > 10, 10, ri)*1.09361)
}

# calculate impact for the play
{
  mxp1 = 120 #max(p1$x)
  mnp1 = 0 #min(p1$x)
  
  mxplt = ifelse(mxp1 > 100, 120, mxp1 + 20)  
  mnplt = ifelse(mnp1 < 20, 0, mnp1 - 20)
  
  pitch <- expand.grid(seq(mnplt, mxplt, length.out = (mxplt-mnplt)*2), seq(0, 160/3, length.out = 107)) %>%
    rename(x = Var1, y = Var2)  
  
  # for plots / images / gifs (higher resolution for gradients)
  
  # pitch <- expand.grid(seq(0, 120, length.out = 200), seq(0, 160/3, length.out = 200)) %>%
  #   rename(x = Var1, y = Var2)
  
  pitch_control_player <- dat %>%
    select(frameId, x, y, mu_x, mu_y, dir, ri, s_rat, nflId, team) %>%
    #run func
    pmap_df(., calc_PC, pitch_area = pitch) %>%
    #sum by team and area
    group_by(nflId, team, x, y, frameId) %>%
    summarise(team_sum = mean(I)) %>%
    pivot_wider(names_from = c(team, nflId), values_from = team_sum)
  
  
  teamcol <- list(
    away = colnames(pitch_control_player[ , grepl( "away" , names( pitch_control_player ) ) ]),
    home = colnames(pitch_control_player[ , grepl( "home" , names( pitch_control_player ) ) ])
  )
  
  # awaycol = colnames(pitch_control_player[ , grepl( "away" , names( pitch_control_player ) ) ])
  
  sum_funs <- map(teamcol, ~ parse_expr(paste(.x, collapse = "+")))
  
  pitch_control <- mutate(pitch_control_player, !!!sum_funs)
  
  ishmpos = py$possessionTeam[1] == gm$homeTeamAbbr[1]
  
  if(ishmpos == TRUE) {
    
    pitch_control <- pitch_control  %>%
      #   #?? - logistic function
      mutate(PC = 1 / (1 + exp(away - home)))
    
  } else {
    
    pitch_control <- pitch_control  %>%
      #   #?? - logistic function
      mutate(PC = 1 / (1 + exp(home - away)))
    
  }
}

# calculate field value on the play
{
  
  mod_input = data.frame("yardline" = NA, "times" = NA, "yardsToGo" = NA)
  mod_input$yardline[1] = ydln
  mod_input$times[1] = tms
  mod_input$yardsToGo[1] = ytg
  # mod_input
  
  #predict the value of a touchdown
  mod_result = data.frame("yardmarker" = seq(0,120), "xepa" = NA)
  
  #predict first down
  mod_3 = data.frame("yardline" = seq(1,ydln-ytg), "times" = tms, 'yardsToGo' = ytg)
  fd_results = data.frame("yardmarker" = seq(1,ydln-ytg), "xepa" = NA)
  
  #predict second down
  mod_4 = data.frame("yardline" = ydln, "times" = tms, "playResult" = ydln-seq(ydln-ytg+1,100), "yardsToGo" = ytg)
  nd_results = data.frame("yardmarker" = seq(ydln-ytg+1,100), "xepa" = NA)
  
  
  if(ydln == ytg) {
    
    if(dwn == 1) {
      
      # touchdown value
      td_val = predict(mod_ft, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #second down
      nd_results = nd_results %>% mutate(xepa = predict(mod_fg, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else if (dwn == 2) {
      
      # touchdown value
      td_val = predict(mod_st, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #second down
      nd_results = nd_results %>% mutate(xepa = predict(mod_sg, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else if (dwn == 3) {
      
      # touchdown value
      td_val = predict(mod_tt, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #second down
      nd_results = nd_results %>% mutate(xepa = predict(mod_tg, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else if (dwn == 4) {
      
      # touchdown value
      td_val = predict(mod_ut, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #second down
      nd_results = nd_results %>% mutate(xepa = predict(mod_ug, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else {
      
      NA
      
    }
    
    mod_result = merge(mod_result, nd_results, by = 'yardmarker', all.x = TRUE)
    names(mod_result)[2] <- "xTD"
    names(mod_result)[3] <- "xNxtDw"
    
    mod_result$xEPA = coalesce(mod_result$xTD, mod_result$xNxtDw)            
    
    mod_result = subset(mod_result, select = c('yardmarker', 'xEPA'))
    mod_result$xEPA[112:121] = mod_result$xEPA[111]
    
  } else {
    
    if(dwn == 1) {
      
      # touchdown value
      td_val = predict(mod_ft, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #first down
      fd_results = fd_results %>% mutate(xepa = td_val - predict(mod_ft, mod_3),
                                         yardmarker = yardmarker + 10)
      
      #second down
      nd_results = nd_results %>% mutate(xepa = predict(mod_fd, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else if (dwn == 2) {
      
      # touchdown value
      td_val = predict(mod_st, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #first down
      fd_results = fd_results %>% mutate(xepa = td_val - predict(mod_st, mod_3),
                                         yardmarker = yardmarker + 10)
      
      #third down
      nd_results = nd_results %>% mutate(xepa = predict(mod_sd, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else if (dwn == 3) {
      
      # touchdown value
      td_val = predict(mod_tt, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #first down
      fd_results = fd_results %>% mutate(xepa = td_val - predict(mod_tt, mod_3),
                                         yardmarker = yardmarker + 10)
      
      #fourth down
      nd_results = nd_results %>% mutate(xepa = predict(mod_td, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else if (dwn == 4) {
      
      # touchdown value
      td_val = predict(mod_ut, mod_input)
      mod_result = mod_result %>% mutate(xepa = ifelse(yardmarker <= 10, td_val, NA))
      
      #first down
      fd_results = fd_results %>% mutate(xepa = td_val - predict(mod_ut, mod_3),
                                         yardmarker = yardmarker + 10)
      
      #turnover on downs
      nd_results = nd_results %>% mutate(xepa = predict(mod_ud, mod_4),
                                         yardmarker = yardmarker + 10)
      
    } else {
      
      NA
      
    }
    
    mod_result = merge(mod_result, fd_results, by = 'yardmarker', all.x = TRUE)
    names(mod_result)[2] <- "xTD"
    names(mod_result)[3] <- "xFstDw"
    mod_result = merge(mod_result, nd_results, by = 'yardmarker', all.x = TRUE)
    names(mod_result)[4] <- "xNxtDw"
    
    mod_result$xEPA = coalesce(mod_result$xTD, mod_result$xFstDw, mod_result$xNxtDw)            
    
    mod_result = subset(mod_result, select = c('yardmarker', 'xEPA'))
    mod_result$xEPA[112:121] = mod_result$xEPA[111]
    
  }
  
  if(py$yardline[1] + py$absoluteYardlineNumber[1] == 110) {
    mod_result$yardmarker = 120-mod_result$yardmarker
  }
  
  if(py$possTeam[1] == gm$visitorTeamAbbr[1]) {
    mod_result$xEPA = -mod_result$xEPA
  }
  
  
  pitch_control$yardmarker = round(pitch_control$x,0)
  
  pitch_control = merge(pitch_control, mod_result, by = 'yardmarker', all.x = TRUE)
  
  pitch_control$pv = pitch_control$xEPA/16 + 0.5
  
  pitch_control$qos = pitch_control$pv * pitch_control$PC
  
}

# calculate player value on the play
{
  
  if(ishmpos == TRUE) {
    player_wnwout <- pitch_control %>%
      
      mutate(across(c(teamcol$home), ~ #ifelse(ishmpos,
                      ((1 / (1 + exp(away - home)))^3)*((1 / (1 + exp(away - home)))-(1 / (1 + exp(away - home + .))))
                    ,
                    .names="{.col}"))  %>%
      
      mutate(across(c(teamcol$away), ~ #ifelse(ishmpos,
                      (1/(1.5*sqrt(2*3.1415)))*exp(-((1 / (1 + exp(away - home)))-.5)^2/.045)*((1 / (1 + exp(away - home)))-(1 / (1 + exp(away - home - .))))
                    ,
                    .names="{.col}")) 
    
  } else {
    player_wnwout <- pitch_control %>%
      mutate(across(c(teamcol$home), ~ #ifelse(ishmpos,
                      (1/(1.5*sqrt(2*3.1415)))*exp(-((1 / (1 + exp(home - away)))-.5)^2/.045)*((1 / (1 + exp(home - away)))-(1 / (1 + exp(home - away - .))))
                    ,
                    .names="{.col}"))   %>%
      
      mutate(across(c(teamcol$away), ~ #ifelse(ishmpos,
                      ((1 / (1 + exp(home - away)))^3)*((1 / (1 + exp(home - away + .)))-(1 / (1 + exp(home - away))))
                    ,
                    .names="{.col}"))
    
  }
  
  #taking into account field position .... alter the pv area to decrease the importance like: (((pv-.5)^2)+.5)
  player_wnwout2 <- player_wnwout %>%
    mutate_at(vars(contains('away_')), funs(. * (((pv-.5)^2)+.5))) %>%
    mutate_at(vars(contains('home_')), funs(. * (((pv-.5)^2)+.5)))
  
  wnwout_player <- player_wnwout %>%
    group_by(frameId) %>%
    summarise_at(vars(contains('_')), funs(mean)) %>%
    mutate_at(vars(contains('_')), funs(. * 100 * ((mxplt-mnplt)/120)))
  
  wnwout_player2 <- player_wnwout2 %>%
    group_by(frameId) %>%
    summarise_at(vars(contains('_')), funs(mean)) %>%
    mutate_at(vars(contains('_')), funs(. * 100 * ((mxplt-mnplt)/120)))
  
  
  qp = wnwout_player %>%
    pivot_longer(cols = contains('_'), names_to = "nflId", values_to = "QOSi_sv")
  
  # need QOSi_fv (quality of space individual - field value)
  # could just join later
  
  qp2 = wnwout_player2 %>%
    pivot_longer(cols = contains('_'), names_to = "nflId", values_to = "QOSi")
  
  qp$nflId = substr(qp$nflId, 6, length(qp$nflId))
  qp2$nflId = substr(qp2$nflId, 6, length(qp2$nflId))
  
  qp = merge(qp, qp2, by = c('nflId', 'frameId'))
  qp$QOSi_fv = ((((qp$QOSi / qp$QOSi_sv) - .5) ^ 0.5) + 0.5)
  qp$QOSi_sv = round(abs(qp$QOSi_sv),4)
  qp$QOSi_fv = round(abs(qp$QOSi_fv),4)
  qp$QOSi = round(abs(qp$QOSi),4)
  
  qp = merge(td, qp, by = c('nflId', 'frameId'), all.x = TRUE)
  
  # highlight player if value over ____ (0.075)
  qp = qp %>%
    mutate(sze = ifelse(abs(QOSi) > 0.075, 6, 3))
  
}




# colors for the plot
{
  # clrs = subset(mtchp_clr, mtchp_clr$home == gm$homeTeamAbbr[1] &  mtchp_clr$away == gm$visitorTeamAbbr[1])
  # 
  # td = td %>%
  #   mutate(jrsyclr = ifelse(team == 'home', clrs$hjer[1], clrs$ajer[1])) %>%
  #   mutate(nmbrclr = ifelse(team == 'home', clrs$hnum[1], clrs$anum[1]))
  
  py_colors <- team_colors(h_team_ = gm$homeTeamAbbr, a_team_ = gm$visitorTeamAbbr, diverge_ = T)
  
}

# field values
{
  ymin <- 0
  ymax <- 160/3
  xmin <- 0
  xmax <- 120
  
  hash.top <- 38.35
  hash.bottom <- 12
  hash.width <- 3.3
  df.hash <- expand.grid(y = c(0, 18, ymax-18, ymax), x = (10:110))
}

# animated plot - Space Value (SV)
{
  p_SV <- ggplot() + 
    #pitch layout background
    geom_rect(data=df.hash, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="#88c999", alpha=0.2) +
    
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "darkgreen") + 
    
    annotate("segment", y = ymin, 
             x = seq(max(10, xmin), min(xmax, 110), by = 5), 
             yend =  ymax, 
             xend = seq(max(10, xmin), min(xmax, 110), by = 5), color="white") + 
    
    annotate("text", x = df.hash$x[df.hash$y < 55/2],
             y = df.hash$y[df.hash$y < 55/2], label = "_", hjust = 0, vjust = -0.2, 
             angle=90, color="white") +
    
    annotate("text", x = df.hash$x[df.hash$y > 55/2],
             y = df.hash$y[df.hash$y > 55/2], label = "_", hjust = 1, vjust = -0.2, 
             angle=90, color="white") +
    
    annotate("text", y = rep(hash.bottom - 2, 11), x = seq(10, 110, by = 10),
             label = c(" G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G "),
             angle = 0, size = 4, color="white") +
    
    annotate("text", y = rep((ymax - hash.bottom + 3), 11), x = seq(10, 110, by = 10), 
             label = c("G  ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), " G"), 
             angle = 180, size = 4, color="white") +
    
    geom_tile(data = pitch_control, aes(x = x, y = y, fill = PC), alpha = 0.7) + #change to fill = PC / qos
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.55) +
    #players for each team
    #also add in little vector arrows
    # geom_segment(data = td, aes(x = x, y = y, colour = team),
    # size = 1 #, arrow = arrow(length = unit(0.01, "npc"))
    # ) +
    #ball location
    geom_segment(data = qp, aes(x = x, y = y, 
                                xend = 10*dis*sin(dir*3.1415/180)+x, yend = 10*dis*cos(dir*3.1415/180)+y),
                 size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    geom_point(data = bd, aes(x = bx, y = by),
               colour = "black", fill = "white", shape = 17, size = 2, stroke = 1) +
    # geom_point(data = qp, aes(x = x, y = y, size = I(sze)+3, group = nflId), col = 'green',
    #            pch = 19) +
    # away team locs and jersey numbers
    geom_point(
      data = qp %>% dplyr::filter(team == 'away'),
      mapping = aes(x = x, y = y),
      fill = "#f8f9fa", colour = py_colors$away_2,
      shape = 21, alpha = 1, size = 6, stroke = 1.5
    ) +
    geom_text(
      data = qp %>% dplyr::filter(team == 'away'),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      colour = py_colors$away_1, size = 3
    ) +
    # home team locs and jersey numbers
    geom_point(
      data = qp %>% dplyr::filter(team == 'home'),
      mapping = aes(x = x, y = y),
      fill = py_colors$home_1, colour = py_colors$home_2,
      shape = 21, alpha = 1, size = 6, stroke = 1.5
    ) +
    geom_text(
      data = qp %>% dplyr::filter(team == 'home'),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      colour = py_colors$home_2, size = 3 
    ) +
    # scale_size_manual(values = c(7, 4), guide = FALSE) + 
    # scale_shape_manual(values = c(19, 19), guide = FALSE) +
    
    #pitch control raster
    
    ylim(ymin, ymax) +
    ggtitle("Space Occupation Value (SV)") +
    coord_fixed() +  
    theme_void()   + 
    theme(plot.title = element_text(size = 10, hjust = 0.5)) +
    transition_time(frameId)  +
    ease_aes('linear') +
    NULL
}

# animated plot - field value (FV)
{
  p_FV <- ggplot() + 
    #pitch layout background
    geom_rect(data=df.hash, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="#88c999", alpha=0.2) +
    
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "darkgreen") + 
    
    annotate("segment", y = ymin, 
             x = seq(max(10, xmin), min(xmax, 110), by = 5), 
             yend =  ymax, 
             xend = seq(max(10, xmin), min(xmax, 110), by = 5), color="white") + 
    
    annotate("text", x = df.hash$x[df.hash$y < 55/2],
             y = df.hash$y[df.hash$y < 55/2], label = "_", hjust = 0, vjust = -0.2, 
             angle=90, color="white") +
    
    annotate("text", x = df.hash$x[df.hash$y > 55/2],
             y = df.hash$y[df.hash$y > 55/2], label = "_", hjust = 1, vjust = -0.2, 
             angle=90, color="white") +
    
    annotate("text", y = rep(hash.bottom - 2, 11), x = seq(10, 110, by = 10),
             label = c(" G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G "),
             angle = 0, size = 4, color="white") +
    
    annotate("text", y = rep((ymax - hash.bottom + 3), 11), x = seq(10, 110, by = 10), 
             label = c("G  ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), " G"), 
             angle = 180, size = 4, color="white") +
    
    geom_tile(data = pitch_control, aes(x = x, y = y, fill = pv), alpha = 0.7) + #change to fill = PC / qos
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.55) +
    #players for each team
    #also add in little vector arrows
    # geom_segment(data = td, aes(x = x, y = y, colour = team),
    # size = 1 #, arrow = arrow(length = unit(0.01, "npc"))
    # ) +
    #ball location
    # geom_segment(data = qp, aes(x = x, y = y, 
    #                             xend = 10*dis*sin(dir*3.1415/180)+x, yend = 10*dis*cos(dir*3.1415/180)+y),
    #              size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    # geom_point(data = bd, aes(x = bx, y = by),
    #            colour = "black", fill = "white", shape = 17, size = 2, stroke = 1) +
  # # geom_point(data = qp, aes(x = x, y = y, size = I(sze)+3, group = nflId), col = 'green',
  # #            pch = 19) +
  # # away team locs and jersey numbers
  # geom_point(
  #   data = qp %>% dplyr::filter(team == 'away'),
  #   mapping = aes(x = x, y = y),
  #   fill = "#f8f9fa", colour = py_colors$away_2,
  #   shape = 21, alpha = 1, size = 6, stroke = 1.5
  # ) +
  # geom_text(
  #   data = qp %>% dplyr::filter(team == 'away'),
  #   mapping = aes(x = x, y = y, label = jerseyNumber),
  #   colour = py_colors$away_1, size = 3
  # ) +
  # # home team locs and jersey numbers
  # geom_point(
  #   data = qp %>% dplyr::filter(team == 'home'),
  #   mapping = aes(x = x, y = y),
  #   fill = py_colors$home_1, colour = py_colors$home_2,
  #   shape = 21, alpha = 1, size = 6, stroke = 1.5
  # ) +
  # geom_text(
  #   data = qp %>% dplyr::filter(team == 'home'),
  #   mapping = aes(x = x, y = y, label = jerseyNumber),
  #   colour = py_colors$home_2, size = 3 
  # ) +
  # # scale_size_manual(values = c(7, 4), guide = FALSE) + 
  # scale_shape_manual(values = c(19, 19), guide = FALSE) +
  
  #pitch control raster
  
  ylim(ymin, ymax) +
    ggtitle(paste(as.character(py$playDescription[1]), " \n Field Value (FV)", sep = '')) +
    coord_fixed() +  
    theme_void()   + 
    theme(plot.title = element_text(size = 9, hjust = 0.5)) +
    transition_time(frameId)  +
    ease_aes('linear') +
    NULL
}

# animated plot - Quality of Space (QOS)
{
  p_QOS <- ggplot() + 
    #pitch layout background
    geom_rect(data=df.hash, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="#88c999", alpha=0.2) +
    
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "darkgreen") + 
    
    annotate("segment", y = ymin, 
             x = seq(max(10, xmin), min(xmax, 110), by = 5), 
             yend =  ymax, 
             xend = seq(max(10, xmin), min(xmax, 110), by = 5), color="white") + 
    
    annotate("text", x = df.hash$x[df.hash$y < 55/2],
             y = df.hash$y[df.hash$y < 55/2], label = "_", hjust = 0, vjust = -0.2, 
             angle=90, color="white") +
    
    annotate("text", x = df.hash$x[df.hash$y > 55/2],
             y = df.hash$y[df.hash$y > 55/2], label = "_", hjust = 1, vjust = -0.2, 
             angle=90, color="white") +
    
    annotate("text", y = rep(hash.bottom - 2, 11), x = seq(10, 110, by = 10),
             label = c(" G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G "),
             angle = 0, size = 4, color="white") +
    
    annotate("text", y = rep((ymax - hash.bottom + 3), 11), x = seq(10, 110, by = 10), 
             label = c("G  ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), " G"), 
             angle = 180, size = 4, color="white") +
    
    geom_tile(data = pitch_control, aes(x = x, y = y, fill = qos), alpha = 0.7) + #change to fill = PC / qos
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.25) +
    #players for each team
    #also add in little vector arrows
    # geom_segment(data = td, aes(x = x, y = y, colour = team),
    # size = 1 #, arrow = arrow(length = unit(0.01, "npc"))
    # ) +
    #ball location
    geom_segment(data = qp, aes(x = x, y = y, 
                                xend = 10*dis*sin(dir*3.1415/180)+x, yend = 10*dis*cos(dir*3.1415/180)+y),
                 size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
    geom_point(data = bd, aes(x = bx, y = by),
               colour = "black", fill = "white", shape = 17, size = 2, stroke = 1) +
    # geom_point(data = qp, aes(x = x, y = y, size = I(sze)+3, group = nflId), col = 'green',
    #            pch = 19) +
    # away team locs and jersey numbers
    geom_point(
      data = qp %>% dplyr::filter(team == 'away'),
      mapping = aes(x = x, y = y),
      fill = "#f8f9fa", colour = py_colors$away_2,
      shape = 21, alpha = 1, size = 6, stroke = 1.5
    ) +
    geom_text(
      data = qp %>% dplyr::filter(team == 'away'),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      colour = py_colors$away_1, size = 3
    ) +
    # home team locs and jersey numbers
    geom_point(
      data = qp %>% dplyr::filter(team == 'home'),
      mapping = aes(x = x, y = y),
      fill = py_colors$home_1, colour = py_colors$home_2,
      shape = 21, alpha = 1, size = 6, stroke = 1.5
    ) +
    geom_text(
      data = qp %>% dplyr::filter(team == 'home'),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      colour = py_colors$home_2, size = 3 
    ) +
    # scale_size_manual(values = c(7, 4), guide = FALSE) + 
    # scale_shape_manual(values = c(19, 19), guide = FALSE) +
    
    #pitch control raster
    
    ylim(ymin, ymax) +
    ggtitle("Quality of Space Occupation (QOS)") +
    coord_fixed() +  
    theme_void()   + 
    theme(plot.title = element_text(size = 10, hjust = 0.5)) +
    transition_time(frameId)  +
    ease_aes('linear') +
    NULL
}



# save animated plot
flnm = paste("D:/Downloads/animation_SV_",gmid,"_",pyid,".gif", sep = '')
{
  play.length.ex <- length(unique(p1$frameId))
  
  # alter fps for faster or slow plot... 
  # 10 = live time (nfl captures at 10 fps)
  # 5 = slower / good to understand play
  # 1 = decrepit/extremely slow
  anim_SV = animate(p_SV, fps = 10, nframe = play.length.ex, width = 450, height = 250)
  anim_FV = animate(p_FV, fps = 10, nframe = play.length.ex, width = 450, height = 250)
  anim_QOS = animate(p_QOS, fps = 10, nframe = play.length.ex, width = 450, height = 250)
  
  cmb_gif <- image_append(c(anim_SV[1], anim_FV[1], anim_QOS[1]))
  
  for(i in 2:play.length.ex){
    combined <- image_append(c(anim_SV[i], anim_FV[i], anim_QOS[i]))
    cmb_gif <- c(cmb_gif, combined)
  }
  
  anim_save(flnm, cmb_gif)
}
