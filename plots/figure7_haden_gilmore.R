# install.packages('ggpubr')
library(ggpubr)

dbpn1 = 'Stephon Gilmore'
dbpn2 = 'Joe Haden'

dbid1 = as.numeric(subset(pyrs, displayName == dbpn1)[1,1])
dbid2 = as.numeric(subset(pyrs, displayName == dbpn2)[1,1])

grph = subset(pydist, nflId == dbid1 | nflId == dbid2)

grph = grph %>%
  inner_join(subset(vnt, select = c('frameId', 'gameId', 'playId'), event == 'ball_snap'), by = c('gameId', 'playId')) %>%
  inner_join(subset(vnt, select = c('frameId', 'gameId', 'playId'), event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  inner_join(pyrs, by = 'nflId') %>%
  mutate(frame_pct = (frameId.x - frameId.y)/(frameId-frameId.y)) %>%
  filter(route_min1 == 'GO' | route_min1 == 'HITCH' | route_min1 == 'SLANT' |
           route_min1 == 'IN' | route_min1 == 'OUT' | route_min1 == 'POST')

grph$istarg = as.factor(grph$istarg)
grph$SOL = as.factor(grph$SOL)

grph = grph %>%
  filter(istarg != 'NA' & !is.na(nflId_min1))

g7 = ggplot(grph, aes(x=frame_pct, y=QOSi)) +
  # geom_point(aes(color = istarg)) +
  geom_smooth(aes(group = displayName, color = displayName), se=TRUE) +
  theme_light() +
  facet_wrap(~ route_min1, nrow = 3)


tbl6 = dem4 %>%
  filter((nflId == dbid1 | nflId == dbid2)) %>%
  select(displayName, position, n, SOL_pct, comp_pct, targ_pct, QOSi, epa)


colnames(tbl6)[1] <- "name"
tbl6$position = as.character(tbl6$position)
tbl6$SOL_pct = paste(100*round(tbl6$SOL_pct,3),'%', sep = '')
tbl6$comp_pct = paste(100*round(tbl6$comp_pct,3),'%', sep = '')
tbl6$targ_pct = paste(100*round(tbl6$targ_pct,3),'%', sep = '')
tbl6$QOSi = round(tbl6$QOSi,3)
tbl6$epa = round(tbl6$epa,3)

title6 <- textGrob("Haden v Gilmore - Overall",gp=gpar(fontsize=15))
{
  g6 <- tableGrob(tbl6, rows = NULL)
  # title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
  # vjust=0, gp=gpar(fontsize=20))
  g6 <- gtable_add_grob(g6,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 2, b = nrow(g6), l = 1, r = ncol(g6))
  g6 <- gtable_add_grob(g6,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 1, l = 1, r = ncol(g6))
  g6 <- gtable_add_grob(g6,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(1,"npc"),
                          y1 = unit(0,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 2, b = 2, l = 1, r = 8)
  g6 <- gtable_add_grob(g6,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 3, l = 2, r = 2)
  g6 <- gtable_add_grob(g6,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 3, l = 3, r = 3)
  g6 <- gtable_add_grob(g6,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 3, l = 4, r = 4)
  g6 <- gtable_add_grob(g6,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 3, l = 7, r = 7)
}
g6 <- grid.arrange(g6, top = title6)




tbl5 = dem5 %>%
  filter((route_min1 == 'GO' | route_min1 == 'HITCH' | route_min1 == 'SLANT' |
            route_min1 == 'IN' | route_min1 == 'OUT' | route_min1 == 'POST') &
           (nflId == dbid1 | nflId == dbid2))

tbl5 = tbl5 %>%
  select(displayName, position, route_min1, n, SOL_pct, comp_pct, targ_pct, QOSi, epa)
tbl5 = tbl5[,-c(1)]

colnames(tbl5)[1] <- "name"
colnames(tbl5)[3] <- "route"
tbl5$position = as.character(tbl5$position)
tbl5$SOL_pct = paste(100*round(tbl5$SOL_pct,3),'%', sep = '')
tbl5$comp_pct = paste(100*round(tbl5$comp_pct,3),'%', sep = '')
tbl5$targ_pct = paste(100*round(tbl5$targ_pct,3),'%', sep = '')
tbl5$QOSi = round(tbl5$QOSi,3)
tbl5$epa = round(tbl5$epa,3)

title5 <- textGrob("Haden vs Gilmore by Route",gp=gpar(fontsize=15))
{
  g5 <- tableGrob(tbl5, rows = NULL)
  # title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
  # vjust=0, gp=gpar(fontsize=20))
  g5 <- gtable_add_grob(g5,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 2, b = nrow(g5), l = 1, r = ncol(g5))
  g5 <- gtable_add_grob(g5,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 1, l = 1, r = ncol(g5))
  g5 <- gtable_add_grob(g5,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 13, l = 2, r = 2)
  g5 <- gtable_add_grob(g5,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 13, l = 3, r = 3)
  g5 <- gtable_add_grob(g5,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 13, l = 4, r = 4)
  g5 <- gtable_add_grob(g5,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 13, l = 5, r = 5)
  g5 <- gtable_add_grob(g5,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(0,"npc"),
                          y1 = unit(1,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 1, b = 13, l = 8, r = 8)
  g5 <- gtable_add_grob(g5,
                        grobs = segmentsGrob( # line across the bottom
                          x0 = unit(0,"npc"),
                          y0 = unit(0,"npc"),
                          x1 = unit(1,"npc"),
                          y1 = unit(0,"npc"),
                          gp = gpar(lwd = 2.0)),
                        t = 7, b = 7, l = 1, r = 9)
}
g5 <- grid.arrange(g5, top = title5)




g_all0 = ggarrange(g7,                                           
          ggarrange(g6, g5, nrow = 2), 
          ncol = 2) 


ggsave(g_all0, filename="D:/Downloads/figure7.png", width = 18, height = 9)



