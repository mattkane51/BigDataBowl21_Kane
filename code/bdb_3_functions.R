#team colors function
team_colors <- function(team_colors_=NULL, h_team_, a_team_, diverge_=FALSE) {
  colors_url <- "https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"
  
  if (is.null(team_colors_)) {
    team_colors_ <- suppressMessages(readr::read_tsv(colors_url))
  }
  
  h_team_color1 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color1)
  h_team_color2 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color2)
  a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color1)
  a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color2)
  
  if (diverge_ == TRUE) {
    h_team_color1_family <- team_colors_ %>% filter(teams == h_team_) %>% select(color1_family) %>% pull()
    a_team_color1_family <- team_colors_ %>% filter(teams == a_team_) %>% select(color1_family) %>% pull()
    
    if (h_team_color1_family == a_team_color1_family) {
      a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% select(color2) %>% pull()
      a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% select(color1) %>% pull()
    }
  }
  
  df_colors <- tibble(
    home_1 = h_team_color1, home_2 = h_team_color2, away_1 = a_team_color1, away_2 = a_team_color2
  )
  
  
  return(df_colors)
}

#function to calculate I as in equation 1/13
calc_I <- function(pitch_area, x, y, mu_x, mu_y, Sigma) {
  #create vectors
  mu <- c(mu_x, mu_y)
  player_loc <- c(x, y)
  
  numerator <- dmvnorm(as.matrix(pitch_area), mu, Sigma)
  denominator <- dmvnorm(t(matrix(player_loc)), mu, Sigma)
  #and normalise
  norm_pdf = numerator/denominator
  return(norm_pdf)
}

calc_PC <- function(frameId, x, y, mu_x, mu_y, dir, ri, s_rat, team, nflId, pitch_area) {
  
  
  R = matrix(c(cos(dir*3.1415/180), sin(dir*3.1415/180), -sin(dir*3.1415/180), cos(dir*3.1415/180)), nrow = 2)
  
  S = matrix(c((ri * (1 + s_rat)/2), 0, 0, (ri * (1 - s_rat)/2)), nrow = 2)
  
  inv_R = solve(R)
  Sigma = R %*% S %*% S %*% inv_R
  
  pitch_area$I <- calc_I(as.matrix(pitch), x, y, mu_x, mu_y, Sigma)
  pitch_area$team <- team
  pitch_area$frameId <- frameId
  pitch_area$nflId <- nflId
  return(pitch_area)
  
}
