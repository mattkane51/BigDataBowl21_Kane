dem4 = pydist %>%
  left_join(subset(vnt, select = c('frameId', 'gameId', 'playId'), event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -10 & frameId.x <= frameId.y + 3 & !is.na(SOL)) %>%
  group_by(nflId, gameId, playId, istarg) %>%
  summarise(SOL = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv), SOL = ifelse(SOL > 0, 1, 0)) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  select (nflId, SOL, passResult, istarg, epa, QOSi) %>%
  filter(!is.na(SOL) &!is.na(istarg)) %>%
  group_by(nflId) %>%
  summarise(comp = sum(ifelse(passResult == 'C' & istarg == 1, 1, 0)), targ = sum(ifelse(istarg == 1, 1, 0)),
            n = n(), QOSi = mean(QOSi),targ_pct = targ/n,
            comp_SOL = sum(ifelse(SOL == 1 & passResult == 'C' & istarg == 1, 1, 0)),
            targ_SOL = sum(ifelse(SOL == 1 & istarg == 1, 1, 0)),
            epa = sum(ifelse(istarg == 1, epa, 0))/targ,
            SOL = sum(ifelse(SOL == 1, 1, 0)), SOL_pct = SOL/n,
            comp_pct = comp/targ)%>%
  left_join(pyrs, by = 'nflId')

tbl1 = subset(dem4, select = c('displayName', 'position', 'n', 'SOL_pct', 'comp_pct', 'targ_pct', 'QOSi', 'epa'),
              displayName == "Marcus Peters" | displayName == "Richard Sherman" |
                displayName == "Mackensie Alexander" | displayName == "Desmond King" |
                displayName == "Jamal Adams"| displayName == 'Adrian Phillips' |
                displayName == "Ha Ha Clinton-Dix" | displayName == "Budda Baker" | displayName == "Eddie Jackson")

colnames(tbl1)[1] <- "name"
tbl1$position = as.character(tbl1$position)
tbl1$SOL_pct = paste(100*round(tbl1$SOL_pct,3),'%', sep = '')
tbl1$comp_pct = paste(100*round(tbl1$comp_pct,3),'%', sep = '')
tbl1$targ_pct = paste(100*round(tbl1$targ_pct,3),'%', sep = '')
tbl1$QOSi = round(tbl1$QOSi,3)
tbl1$epa = round(tbl1$epa,3)

tbl1 = tbl1 %>%
  arrange(position, SOL_pct)


title1 <- textGrob("Overall Ratings - Tops & Bottoms",gp=gpar(fontsize=15))
{
g1 <- tableGrob(tbl1, rows = NULL)
# title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
# vjust=0, gp=gpar(fontsize=20))
g1 <- gtable_add_grob(g1,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 1, l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
                   grobs = segmentsGrob( # line across the bottom
                     x0 = unit(0,"npc"),
                     y0 = unit(0,"npc"),
                     x1 = unit(1,"npc"),
                     y1 = unit(0,"npc"),
                     gp = gpar(lwd = 2.0)),
                   t = 5, b = 5, l = 1, r = 8)
g1 <- gtable_add_grob(g1,
                      grobs = segmentsGrob( # line across the bottom
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(0,"npc"),
                        y1 = unit(1,"npc"),
                        gp = gpar(lwd = 2.0)),
                      t = 1, b = 10, l = 2, r = 2)
g1 <- gtable_add_grob(g1,
                      grobs = segmentsGrob( # line across the bottom
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(0,"npc"),
                        y1 = unit(1,"npc"),
                        gp = gpar(lwd = 2.0)),
                      t = 1, b = 10, l = 3, r = 3)
g1 <- gtable_add_grob(g1,
                      grobs = segmentsGrob( # line across the bottom
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(0,"npc"),
                        y1 = unit(1,"npc"),
                        gp = gpar(lwd = 2.0)),
                      t = 1, b = 10, l = 4, r = 4)
g1 <- gtable_add_grob(g1,
                      grobs = segmentsGrob( # line across the bottom
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(0,"npc"),
                        y1 = unit(1,"npc"),
                        gp = gpar(lwd = 2.0)),
                      t = 1, b = 10, l = 7, r = 7)
g1 <- gtable_add_grob(g1,
                      grobs = segmentsGrob( # line across the bottom
                        x0 = unit(0,"npc"),
                        y0 = unit(0,"npc"),
                        x1 = unit(1,"npc"),
                        y1 = unit(0,"npc"),
                        gp = gpar(lwd = 2.0)),
                      t = 8, b = 8, l = 1, r = 8)
}
g1 <- grid.arrange(g1, top = title1)


#SOL for players by route around when the pass is thrown

dem5 = pydist %>%
  left_join(subset(vnt, select = c('frameId', 'gameId', 'playId'), event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -10 & frameId.x <= frameId.y + 3 & !is.na(SOL)) %>%
  group_by(nflId, gameId, playId, istarg, route_min1) %>%
  summarise(SOL = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv), SOL = ifelse(SOL > 0, 1, 0)) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  select (nflId, SOL, passResult, istarg, epa, QOSi, route_min1) %>%
  filter(!is.na(SOL) &!is.na(istarg)) %>%
  group_by(nflId, route_min1) %>%
  summarise(comp = sum(ifelse(passResult == 'C' & istarg == 1, 1, 0)), targ = sum(ifelse(istarg == 1, 1, 0)),
            n = n(), QOSi = mean(QOSi),targ_pct = targ/n,
            comp_SOL = sum(ifelse(SOL == 1 & passResult == 'C' & istarg == 1, 1, 0)),
            targ_SOL = sum(ifelse(SOL == 1 & istarg == 1, 1, 0)),
            epa = sum(ifelse(istarg == 1, epa, 0))/targ,
            SOL = sum(ifelse(SOL == 1, 1, 0)), SOL_pct = SOL/n,
            comp_pct = comp/targ)%>%
  left_join(pyrs, by = 'nflId')

tbl2 = subset(dem5, select = c('displayName', 'position', 'route_min1', 'n', 'SOL_pct', 'comp_pct', 'targ_pct', 'QOSi', 'epa'),
              (displayName == 'Marcus Peters' & route_min1 == 'GO') |
                (displayName == 'Jalen Ramsey' & route_min1 == 'GO') |
                (displayName == 'Mackensie Alexander' & route_min1 == 'GO') |
                (displayName == 'Janoris Jenkins' & route_min1 == 'SLANT') |
                (displayName == 'Byron Jones' & route_min1 == 'SLANT') |
                (displayName == 'Shawn Williams' & route_min1 == 'CROSS') |
                (displayName == 'Malcolm Jenkins' & route_min1 == 'CROSS') |
                (displayName == 'Tyrann Mathieu' & route_min1 == 'HITCH') |
                (displayName == 'Budda Baker' & route_min1 == 'HITCH'))
                
colnames(tbl2)[1] <- "name"
colnames(tbl2)[3] <- "route"
tbl2$position = as.character(tbl2$position)
tbl2$SOL_pct = paste(100*round(tbl2$SOL_pct,3),'%', sep = '')
tbl2$comp_pct = paste(100*round(tbl2$comp_pct,3),'%', sep = '')
tbl2$targ_pct = paste(100*round(tbl2$targ_pct,3),'%', sep = '')
tbl2$QOSi = round(tbl2$QOSi,3)
tbl2$epa = round(tbl2$epa,3)


tbl2 = tbl2 %>%
  arrange(route, SOL_pct)


title2 <- textGrob("Ratings by Route - Tops & Bottoms",gp=gpar(fontsize=15))
{
  g2 <- tableGrob(tbl2, rows = NULL)
  # title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
  # vjust=0, gp=gpar(fontsize=20))
  g2 <- gtable_add_grob(g2,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 2, b = nrow(g2), l = 1, r = ncol(g2))
  g2 <- gtable_add_grob(g2,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 1, l = 1, r = ncol(g2))
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(1,"npc"),
                          y1 = unit(0,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 3, b = 3, l = 1, r = 9)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(1,"npc"),
                          y1 = unit(0,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 6, b = 6, l = 1, r = 9)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 10, l = 2, r = 2)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 10, l = 3, r = 3)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 10, l = 4, r = 4)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 10, l = 5, r = 5)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 10, l = 8, r = 8)
  g2 <- gtable_add_grob(g2,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(1,"npc"),
                          y1 = unit(0,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 8, b = 8, l = 1, r = 9)
}
g2 <- grid.arrange(g2, top = title2)




g_all2 = arrangeGrob(g1, g2, top=textGrob("Figure 5",gp=gpar(fontsize=10,font=3)), ncol = 2)

ggsave(g_all2, filename="D:/Downloads/figure5.png", width = 16, height = 4)



