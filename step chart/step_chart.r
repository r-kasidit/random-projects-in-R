# inspired by https://nightingaledvs.com/step-charts/?mc_cid=fa7faa1d04&mc_eid=0b1961a6ab

library(tidyverse)
library(ggthemes)

set.seed(070823)

data = data.frame(year = 1999:2023, 
                  rpg = round(rnorm(25,4.65,0.5),2))

for (i in 1:25) {
  if (i %in% c(1,25)){
    data$low[i] =  data$rpg[i]
    data$up[i] = NA
  } else {
    data$low[i] = ifelse((data$rpg[i-1]>data$rpg[i])&(data$rpg[i]<data$rpg[i+1]),
                      data$rpg[i],NA) 
    data$up[i] = ifelse((data$rpg[i-1]>data$rpg[i])&(data$rpg[i]<data$rpg[i+1]),
                        NA,data$rpg[i]) 
  }
}

line_col = '#6082B6'
inside_point_fill= '#B6D0E2'

ggplot() +
  geom_step(data=data, aes(x=year-0.5, y=rpg), color=line_col, size=1) +
  geom_segment(aes(x= 2022.47, xend=2023.5 , y=  data$rpg[25], yend =data$rpg[25]), color=line_col, size=1)+
  geom_point(data=data, aes(x=year, y=rpg), fill=inside_point_fill, color=line_col, 
             shape=21, size=2.5, stroke = 1.5) +
  geom_text(data=data, aes(x=year, y=rpg, label=up), vjust=-1, size=3, color=line_col, fontface = "bold")+
  geom_text(data=data, aes(x=year, y=rpg, label=low), vjust=2, size=3, color=line_col, fontface = "bold")+
  scale_x_continuous(breaks = 1999:2023)+
  theme_solarized()+
  theme(panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linetype = 3, color='grey'),
        plot.title = element_text(face = "bold", hjust = 0, size=18),
        plot.subtitle = element_text(hjust = 0, size=12),
        axis.title.y = element_text(hjust = 0, size = 9, face = 'bold'),
        axis.text.x = element_text(size=7, face = 'bold'),
        axis.text.y = element_text(size=7, face='bold'))+
  labs(x='', y='Run per game', title = 'Baseball data: step chart',
       subtitle = 'A step chart showing number of runs per game for each MLB season')
  
ggsave('step_chart.jpg', width=11, height = 6, dpi=200)

