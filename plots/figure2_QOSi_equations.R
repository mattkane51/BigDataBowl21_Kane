# install.packages('transformr')
library(transformr)

eq_df1 = data_frame(
  a = seq(0, 1, 1/101)
)

eq_df2 = data.frame(
  x = seq(0, 1, 0.001)
)

eq_df = merge(eq_df1, eq_df2, all = TRUE)
eq_df$y = (eq_df$x^3)*(eq_df$x-eq_df$a)
eq_df$Team = "red"

eq_df3 = merge(eq_df1, eq_df2, all = TRUE)
eq_df3$y = (1/(1.5*sqrt(2*3.1415)))*exp(-((eq_df3$x-.5)^2)/.045)*((eq_df3$a-eq_df3$x))
eq_df3$Team = "blue"

eq_df = rbind(eq_df, eq_df3)


eq_an = ggplot(data = eq_df, mapping = aes(x=x, y=y, group = Team, colour = Team)) +
  geom_line(aes(group = paste(Team,a)), lwd = 1.5, linetype = 1) +
  # geom_line(lwd = 1.5) +
  scale_colour_identity("Team", guide="legend", 
                        labels = c("Offense", "Defense"),
                        breaks = c("red", "blue")) +

  ylim(-0.2, 0.2) +
  xlab("Offensive Space Value w/ Player") +
  ylab("Individual Quality of Space (QOSi)") +
  # ggtitle("QOSi equations for offense/defense") +
  theme_light() +
  
  transition_time(a) +
  labs(title = "QOSi equations for offense/defense",
       caption = paste("SV w/out player =","{round(frame_time,2)}")) +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 1.2))

  
an_eq = animate(eq_an, fps = 10)

anim_save("D:/downloads/QOSi_equations_anim.gif", an_eq)
