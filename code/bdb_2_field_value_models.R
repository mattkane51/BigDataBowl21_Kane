# subset data for field value
{
  #starting point
  play_ft = subset(play, down == 1 &
                     passResult != 'IN' & epa > -2.5 & playResult == yardline)
  
  #first and goal
  play_fg = subset(play, down == 1 & yardsToGo == yardline & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5)
  
  # first down under 10
  play_fd = subset(play, down == 1 & yardline >= 10  & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5 & !is.na(times) & times > 0)
  
  
  # second down 
  play_st = subset(play, down == 2 & 
                     passResult != 'IN' & epa > -2.5 & playResult == yardline)
  
  play_sg = subset(play, down == 2 & yardsToGo == yardline & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5)
  
  play_sd = subset(play, down == 2 & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5 & !is.na(times) & times > 0)
  
  
  # third down 
  play_tt = subset(play, down == 3 & 
                     passResult != 'IN' & epa > -2.5 & playResult == yardline)
  
  play_tg = subset(play, down == 3 & yardsToGo == yardline & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5)
  
  play_td = subset(play, down == 3 & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5 & !is.na(times) & times > 0)
  
  # fourth down 
  play_ut = subset(play, down == 4 &
                     passResult != 'IN' & epa > -2.5 & playResult == yardline)
  
  play_ug = subset(play, down == 4 & yardsToGo == yardline & playResult < yardsToGo &
                     passResult != 'IN' & !is.na(times))
  
  play_ud = subset(play, down == 4 & playResult < yardsToGo &
                     passResult != 'IN' & epa > -2.5 & !is.na(times) & times > 0)
  
  
}

# run models for field value
{
  # models are overfitted... but we are modeling a model so we just want the best approx of that original xepa model
  
  # touchdown models for each down
  
  mod_ft = lm(data = play_ft, epa ~ yardline + I(yardline^2) + I(yardline^3) +
                log(times) + I(times^2) + I(times^3) + yardsToGo)
  
  mod_st = lm(data = play_st, epa ~ yardline + I(yardline^2) + I(yardline^3) +
                log(times) + I(times^2) + I(times^3) + yardsToGo)
  
  mod_tt = lm(data = play_tt, epa ~ yardline + I(yardline^2) + I(yardline^3) +
                log(times) + I(times^2) + I(times^3) + yardsToGo)
  
  mod_ut = lm(data = play_ut, epa ~ yardline + I(yardline^2) + I(yardline^3) +
                log(times) + I(times^2) + I(times^3) + yardsToGo)
  
  # first/second/third and goal models
  
  mod_fg = lm(data = play_fg, epa ~  yardline + I(yardline^2) + log(times) +
                playResult + I(playResult^2))
  
  mod_sg = lm(data = play_sg, epa ~  yardline + I(yardline^2) + log(times) +
                playResult + I(playResult^2))
  
  mod_tg = lm(data = play_tg, epa ~  yardline + I(yardline^2) + log(times) +
                playResult + I(playResult^2))
  
  mod_ug = lm(data = play_ug, epa ~  yardline + I(yardline^2) + log(times) +
                playResult + I(playResult^2))
  
  
  # first/second/third and not-goal models
  
  mod_fd = lm(data = play_fd, epa ~  I(yardline^2) + I(yardline^3) + log(times) +
                I(times^2) + playResult + yardsToGo)
  
  mod_sd = lm(data = play_sd, epa ~  I(yardline^2) + I(yardline^3) + log(times) +
                I(times^2) + playResult + yardsToGo)
  
  mod_td = lm(data = play_td, epa ~  I(yardline^2) + I(yardline^3) + log(times) +
                I(times^2) + playResult + yardsToGo)
  
  mod_ud = lm(data = play_ud, epa ~  I(yardline^2) + I(yardline^3) + log(times) +
                I(times^2) + playResult + yardsToGo)
  
  
}