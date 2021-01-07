#install.packages('grid')
library(grid)


targ_SOL_df = pydist %>%
  left_join(subset(vnt, event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -3 & frameId.x <= frameId.y + 3 & !is.na(SOL)) %>%
  group_by(nflId, gameId, playId, istarg, passResult, epa) %>%
  summarise(SOL = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv),
            SOL = ifelse(SOL > 0, 1, 0), iscmp = ifelse(passResult == 'C', 1, 0)) 

log_mod <- glm(istarg ~ QOSi + SOL, family = 'binomial', data = targ_SOL_df)

istar_tbl = signif(exp(cbind(coef = coef(log_mod), confint(log_mod))),2)



log_mod2 <- glm(iscmp ~ QOSi + SOL, family = 'binomial', data = subset(targ_SOL_df, istarg == 1))

iscmp_tbl = signif(exp(cbind(coef = coef(log_mod2), confint(log_mod2))),2)





tbl1 = pydist %>%
  left_join(subset(vnt, event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -3 & frameId.x <= frameId.y + 3 & !is.na(SOL) & !is.na(istarg)) %>%
  group_by(nflId, gameId, playId, istarg, passResult, epa) %>%
  summarise(SOL_ = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv),
            SOL_ = ifelse(SOL_ > 0, 1, 0)) %>%
  group_by(SOL_) %>%
  summarise(n= n(), SOL = sum(SOL_)/n(),
            epa = mean(!is.na(ifelse(istarg == 1, epa, NA))),
            comp = sum(ifelse(passResult == 'C' & istarg == 1, 1, 0)), targ = sum(ifelse(istarg == 1, 1, 0)),
            comp_pct = comp/targ, targ_pct = targ/n) %>%
  select(SOL, n, epa, comp_pct, targ_pct)

tbl1$SOL = ifelse(tbl1$SOL == 1, 'Yes', 'No')
tbl1$comp_pct = paste(100*round(tbl1$comp_pct,3),'%', sep = '')
tbl1$targ_pct = paste(100*round(tbl1$targ_pct,3),'%', sep = '')
tbl1$epa = round(tbl1$epa,3)


tbl2 = pydist %>%
  left_join(subset(vnt, event == 'pass_forward'), by = c('gameId', 'playId')) %>%
  inner_join(cmp, by = c('gameId', 'playId')) %>%
  filter(frameId.x >= frameId.y -3 & frameId.x <= frameId.y + 3 & !is.na(SOL) & !is.na(istarg)) %>%
  group_by(nflId, gameId, playId, istarg, passResult, epa) %>%
  summarise(SOL = sum(SOL), QOSi = mean(QOSi), QOSi_sv = mean(QOSi_sv),
            SOL = ifelse(SOL > 0, 1, 0)) %>%
  group_by(istarg) %>%
  summarise(n= n(), SOL_pct = sum(SOL)/n(), QOSI = mean(QOSi))

tbl2$istarg = ifelse(tbl2$istarg == 1, 'Targeted', 'No')
tbl2$SOL_pct = paste(100*round(tbl2$SOL_pct,3),'%', sep = '')
tbl2$QOSI = round(tbl2$QOSI,3)

# tt2 <- ttheme_default(
#   main = 'uh'
# )
# tt3 <- ttheme_minimal(
#   core=list(bg_params = list(fill = blues9[1:4], col=NA),
#             fg_params=list(fontface=3)),
#   colhead=list(fg_params=list(col="navyblue", fontface=4L)),
#   rowhead=list(fg_params=list(col="orange", fontface=3L)))
# 
# grid.arrange(
#   tableGrob(round(istar_tbl,2), theme=tt2),
#   tableGrob(round(iscmp_tbl,5), theme=tt2),
#   nrow=1)

title <- textGrob("LogReg for Completion %",gp=gpar(fontsize=15),just = c('center', 'top'))

g <- tableGrob(istar_tbl)
# title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
                  # vjust=0, gp=gpar(fontsize=20))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
g <- grid.arrange(g, top = title)

title2 <- textGrob("LogReg for Target %",gp=gpar(fontsize=15))

g2 <- tableGrob(iscmp_tbl)
# title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
# vjust=0, gp=gpar(fontsize=20))
g2 <- gtable_add_grob(g2,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g2), l = 1, r = ncol(g2))
g2 <- gtable_add_grob(g2,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g2))
g2 <- grid.arrange(g2, top = title2)


title3 <- textGrob("Averages given SOL",gp=gpar(fontsize=15))

g3 <- tableGrob(tbl1, rows = NULL)
# title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
# vjust=0, gp=gpar(fontsize=20))
g3 <- gtable_add_grob(g3,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 2, b = nrow(g3), l = 1, r = ncol(g3))
g3 <- gtable_add_grob(g3,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 1, l = 1, r = ncol(g3))
g3 <- grid.arrange(g3, top = title3)


title4 <- textGrob("Averages given IsTarget",gp=gpar(fontsize=15))

g4 <- tableGrob(tbl2, rows = NULL)
# title <- textGrob("Title", y=unit(0.5,"npc") + 0.5*grobHeight(g), 
# vjust=0, gp=gpar(fontsize=20))
g4 <- gtable_add_grob(g4,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g4), l = 1, r = ncol(g4))
g4 <- gtable_add_grob(g4,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g4))
g4 <- grid.arrange(g4, top = title4)

# par(bg = 'white')

g_all = arrangeGrob(g,g3,g2,g4, top=textGrob("Figure 4",gp=gpar(fontsize=10,font=3)))

ggsave(g_all, filename="D:/Downloads/figure4.png", width = 9, height = 4)

dev.off()
