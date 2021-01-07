# start weekly loop

for(w in 1:17) {
  
  # load week data
  {
    imp_flnm = paste("D:/Downloads/NFL/week", w, ".csv", sep = '')
    all = read.csv(imp_flnm)
    
  }
  
  # set up dataframe to load final calculations into 
  {
    QOS_dataset = as.data.frame(all[1,])
    
    excol <- data.frame(NA, NA, NA, NA)
    names(excol) <- c("QOSi_sv","QOSi","QOSi_fv","sze")
    
    QOS_dataset = cbind(QOS_dataset, excol)
    QOS_dataset = QOS_dataset[0,]
    
  }
  
  # trim out post-pass data ... only looking at the play from snap till the ball is thrown or QB is sacked
  {
    fb = subset(all, select = c('gameId', 'playId', 'frameId'), displayName == "Football" & 
                  (event == "pass_forward" | event == "qb_sack" | event == "qb_spike" | event == "qb_strip_sack" ) )
    colnames(fb)[3] <- 'MaxFrameId'
    fb$MaxFrameId = fb$MaxFrameId + 3
    
    bs = subset(all, select = c('gameId', 'playId', 'frameId'), displayName == "Football" & 
                  (event == "ball_snap" ))
    colnames(bs)[3] <- 'MinFrameId'
    
    # the subset here takes out plays with no time / gameclock / yardline listed 
    # because it would mess with the model in determining time of game as well as other important play details used in model
    # unfortunately this factors out DPI's
    play_used = merge(play, bs, by = c('gameId', 'playId'))     # all.x = TRUE
    play_used = merge(play_used, fb, by = c('gameId', 'playId'))     # all.x = TRUE
    
  }
  
  # alter null values in columns (need absoluteyardlinenumber and time for models)
  # without this step... no DPI or any plays with a penalty would be captured
  {
    play_used = play_used %>%
      mutate(times = ifelse(!is.na(times), times,
                            ifelse((quarter == 1 | quarter == 3) & is.na(preSnapVisitorScore), 
                                   900+60*as.numeric(ifelse(str_extract(string = playDescription, pattern = "(?<=\\().*(?=\\:)") == "", 0,
                                                            str_extract(string = playDescription, pattern = "(?<=\\().*(?=\\:)")))+
                                     as.numeric(substr(str_extract(string = playDescription, pattern = "(?<=\\:).*(?=\\) )"),1,2)),
                                   60*as.numeric(ifelse(str_extract(string = playDescription, pattern = "(?<=\\().*(?=\\:)") == "", 0,
                                                        str_extract(string = playDescription, pattern = "(?<=\\().*(?=\\:)")))+
                                     as.numeric(substr(str_extract(string = playDescription, pattern = "(?<=\\:).*(?=\\) )"),1,2)))))
    
    dpi_help = subset(play_used, select = c('gameId', 'possessionTeam', 'quarter', 'absoluteYardlineNumber', 'yardline'))
    
    dpi_help = subset(dpi_help, !is.na(dpi_help$absoluteYardlineNumber)) %>%
      group_by(gameId, possessionTeam, quarter) %>%
      summarize(meanAYN = mean(absoluteYardlineNumber, na.rm=TRUE), meany = mean(yardline, na.rm= TRUE)) %>%
      mutate(isTF = ifelse(meanAYN + meany == 110, TRUE, FALSE))
    
    play_used = merge(play_used, subset(dpi_help, select = c('gameId', 'possessionTeam', 'quarter','isTF')),
                      by = c('gameId', 'possessionTeam', 'quarter'))
    
    play_used = play_used %>%
      mutate(absoluteYardlineNumber = ifelse(isTF == TRUE, 110-yardline, yardline + 10))
  }
  
  # start loop here
  stm = proc.time()
  
  for(i in 1:nrow(play_used)) {
    
    # start time
    ptm = proc.time()
    
    # select play
    {
      #automatic
      gmid = play_used$gameId[i]
      pyid = play_used$playId[i]
      
      #manual
      # gmid = 2018090600 #2018090902
      # pyid = 2624 #2435
    }
    
    # load play data
    {
      pyused = subset(play_used, gameId == gmid & playId == pyid)
      frmn = pyused$MinFrameId[1]
      frmx = pyused$MaxFrameId[1]
      
      p0 = subset(all, gameId == gmid & playId == pyid & (frameId < frmn | frameId >= frmx))
      p1 = subset(all, gameId == gmid & playId == pyid & frameId >= frmn & frameId <= frmx)
      
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
      mxp1 = max(p1$x)
      mnp1 = min(p1$x)
      
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
      mod_input
      
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
    
    # store into database for further analysis
    {
      
      p0 = cbind(p0, excol)
      p2 = rbind(p0, qp)
      QOS_dataset = rbind(QOS_dataset,p2)
      
    }
    
    # update position in loop
    {
      update_name = paste("Week ", w, " - Play Number ", i, sep = '')
      update_name2 = paste(py$playDescription[1])
      
      print(update_name)
      print(update_name2)
      print(cbind((proc.time() - ptm)[3],(proc.time() - stm)[3]))
      
    }
  }
  # end loop
  
  # save final results
  {
    sv_flnm = paste("D:/Downloads/final_week_", w, ".csv", sep = '')
    write.csv(QOS_dataset, sv_flnm, row.names = FALSE)
  }
  
}
# end week loop

