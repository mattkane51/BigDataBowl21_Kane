off_form = play %>%
  inner_join(game, by = c('gameId')) %>%
  mutate(defTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
  select(gameId, playId, typeDropback, offenseFormation, possessionTeam, defTeam)

dem6 = pydist %>%
  left_join(subset(vnt, select = c('frameId', 'gameId', 'playId'), event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  left_join(off_form, by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -10 & frameId.x <= frameId.y + 3 & !is.na(SOL)) %>%
  group_by(defTeam, gameId, playId, istarg, typeDropback, offenseFormation) %>%
  summarise(SOL = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv), SOL = ifelse(SOL > 0, 1, 0)) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  select (defTeam, SOL, passResult, istarg, epa, QOSi, typeDropback, offenseFormation) %>%
  filter(!is.na(SOL) &!is.na(istarg)) %>%
  group_by(defTeam) %>%
  summarise(comp = sum(ifelse(passResult == 'C' & istarg == 1, 1, 0)), targ = sum(ifelse(istarg == 1, 1, 0)),
            ntarg = sum(ifelse(istarg == 0, 1, 0)),
            n = n(), QOSi_t = sum(ifelse(istarg == 1, QOSi, 0))/targ,
            QOSi_n = sum(ifelse(istarg == 0, QOSi, 0))/ntarg,
            #targ_pct = targ/n,
            comp_SOL = sum(ifelse(SOL == 1 & passResult == 'C' & istarg == 1, 1, 0)),
            targ_SOL = sum(ifelse(SOL == 1 & istarg == 1, 1, 0)),
            epa_t = sum(ifelse(istarg == 1, epa, 0))/targ,
            epa_n = sum(ifelse(istarg == 0, epa, 0))/ntarg,
            SOL_t = sum(ifelse(SOL == 1 & istarg == 1, 1, 0)), SOL_t_pct = SOL_t/targ,
            SOL_n = sum(ifelse(SOL == 1 & istarg == 0, 1, 0)), SOL_n_pct = SOL_n/ntarg,
            comp_pct = comp/targ)


tbl3 = dem6 %>%
  select(defTeam, ntarg, comp_pct, QOSi_t, SOL_t_pct, QOSi_n, SOL_n_pct)

colnames(tbl3)[2] <- "n"
tbl3$SOL_t_pct = paste(100*round(tbl3$SOL_t_pct,3),'%', sep = '')
tbl3$SOL_n_pct = paste(100*round(tbl3$SOL_n_pct,3),'%', sep = '')
tbl3$comp_pct = paste(100*round(tbl3$comp_pct,3),'%', sep = '')
tbl3$QOSi_t = round(tbl3$QOSi_t,3)
tbl3$QOSi_n = round(tbl3$QOSi_n,3)


title3 <- textGrob("Team Overall Ratings - Target v Non Target",gp=gpar(fontsize=15))
{
  g3 <- tableGrob(tbl3, rows = NULL)
  # title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
  # vjust=0, gp=gpar(fontsize=20))
  g3 <- gtable_add_grob(g3,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 2, b = nrow(g3), l = 1, r = ncol(g3))
  g3 <- gtable_add_grob(g3,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 1, l = 1, r = ncol(g3))
  g3 <- gtable_add_grob(g3,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 33, l = 2, r = 2)
  g3 <- gtable_add_grob(g3,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 33, l = 3, r = 3)
  g3 <- gtable_add_grob(g3,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 33, l = 4, r = 4)
  g3 <- gtable_add_grob(g3,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 33, l = 6, r = 6)
  
  
}

g3 <- grid.arrange(g3, top = title3)




dem7 = pydist %>%
  left_join(subset(vnt, select = c('frameId', 'gameId', 'playId'), event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  left_join(off_form, by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -10 & frameId.x <= frameId.y + 3 & !is.na(SOL)) %>%
  group_by(defTeam, gameId, playId, istarg, typeDropback, offenseFormation) %>%
  summarise(SOL = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv), SOL = ifelse(SOL > 0, 1, 0)) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  select (defTeam, SOL, passResult, istarg, epa, QOSi, typeDropback, offenseFormation) %>%
  filter(!is.na(SOL) &!is.na(istarg)) %>%
  group_by(defTeam, typeDropback, offenseFormation) %>%
  summarise(comp = sum(ifelse(passResult == 'C' & istarg == 1, 1, 0)), targ = sum(ifelse(istarg == 1, 1, 0)),
            ntarg = sum(ifelse(istarg == 0, 1, 0)),
            n = n(), QOSi_t = sum(ifelse(istarg == 1, QOSi, 0))/targ,
            QOSi_n = sum(ifelse(istarg == 0, QOSi, 0))/ntarg,
            #targ_pct = targ/n,
            comp_SOL = sum(ifelse(SOL == 1 & passResult == 'C' & istarg == 1, 1, 0)),
            targ_SOL = sum(ifelse(SOL == 1 & istarg == 1, 1, 0)),
            epa_t = sum(ifelse(istarg == 1, epa, 0))/targ,
            epa_n = sum(ifelse(istarg == 0, epa, 0))/ntarg,
            SOL_t = sum(ifelse(SOL == 1 & istarg == 1, 1, 0)), SOL_t_pct = SOL_t/targ,
            SOL_n = sum(ifelse(SOL == 1 & istarg == 0, 1, 0)), SOL_n_pct = SOL_n/ntarg,
            comp_pct = comp/targ)



tbl4 = dem7 %>%
  select(defTeam, offenseFormation, typeDropback, ntarg, comp_pct, QOSi_t, SOL_t_pct, QOSi_n, SOL_n_pct) %>%
  filter((defTeam == 'KC' | defTeam == 'NE' | defTeam == 'CHI' | defTeam == "ATL") & ntarg >= 7) %>%
  arrange(defTeam, offenseFormation, typeDropback)


colnames(tbl4)[2] <- "Formation"
colnames(tbl4)[3] <- "Dropback"
colnames(tbl4)[4] <- "n"
tbl4$SOL_t_pct = paste(100*round(tbl4$SOL_t_pct,3),'%', sep = '')
tbl4$SOL_n_pct = paste(100*round(tbl4$SOL_n_pct,3),'%', sep = '')
tbl4$comp_pct = paste(100*round(tbl4$comp_pct,3),'%', sep = '')
tbl4$QOSi_t = round(tbl4$QOSi_t,3)
tbl4$QOSi_n = round(tbl4$QOSi_n,3)





title4 <- textGrob("Team Ratings by Dropback & Formation - ATL/CHI/KC/NE",gp=gpar(fontsize=15))
{
  g4 <- tableGrob(tbl4, rows = NULL)
  # title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
  # vjust=0, gp=gpar(fontsize=20))
  g4 <- gtable_add_grob(g4,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 2, b = nrow(g4), l = 1, r = ncol(g4))
  g4 <- gtable_add_grob(g4,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 1, l = 1, r = ncol(g4))
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 36, l = 2, r = 2)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 36, l = 3, r = 3)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 36, l = 4, r = 4)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 36, l = 5, r = 5)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 36, l = 6, r = 6)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 36, l = 8, r = 8)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 11, b = 11, l = 1, r = 9)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 20, b = 20, l = 1, r = 9)
  g4 <- gtable_add_grob(g4,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 28, b = 28, l = 1, r = 9)
  
  
}

g4 <- grid.arrange(g4, top = title4)


g_all3 = arrangeGrob(g3, g4, top=textGrob("Figure 6",gp=gpar(fontsize=10,font=3)), ncol = 2)

ggsave(g_all3, filename="D:/Downloads/figure6.png", width = 20, height = 11)

