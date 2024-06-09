library(ggplot2)
library(ggforce)


data <- data.frame(
  angle = 1:90,
  cell = sample(1:10, size =90, replace = TRUE)
)

alpha <- pi / 6
# Maximum radius to draw grid lines, set limits ...
# I set it equal to the maximum value.
max_r <- 10
padding_limits <- 0.5
angle_line = seq(from=0,to=3,length.out=181)
angle_line_full = angle_line[seq(1,181,2)]
angle_line_half = angle_line[seq(2,181,2)]
angle_text_full = 0:90

dist = 30
dist_cal_full = sprintf('%.2f', round(tan(0:89*(pi / (180)))*dist, digit=3))
dist_cal_full = paste(dist_cal_full, 'ม.')
dist_cal_full = c(dist_cal_full, 'ไม่สามารถหาค่าได้')

dist_cal_half = sprintf('%.2f', round(tan(0.5:89.5*(pi / (180)))*dist, digit=3))
dist_cal_half = paste(dist_cal_half, 'ม.')


p_title = paste('ระยะทาง',
                dist,
                'เมตรจากต้นไม้')
p_name = paste0('quarter_circle_',dist,'.jpg')

ggplot(data) +
  # Grid lines
  geom_arc(
    data = data.frame(
      start = 0, end = 1/2 * pi,
      r = c(0, max_r) # add seq for several rings
    ),
    aes(x0 = 0, y0 = 0, r = r,
      start = start, end = end),
  ) +
  # Axes
  annotate(
    geom = "segment", x = 0, y = 0, linewidth=0.5,
    xend = max_r * cos(alpha * c(0, 3)),
    yend = max_r * sin(alpha * c(0, 3)),
  ) +
  #  1 degree rays from origin
  annotate(
    geom = "segment", x = 0, y = 0,linewidth=0.15,
    xend = max_r * cos(alpha * angle_line_full),
    yend = max_r * sin(alpha * angle_line_full),
  ) +
  # 0.5 degree rays
  annotate(
    geom = "segment", x = 0, y = 0,linewidth=0.05,
    xend = max_r * cos(alpha * angle_line_half),
    yend = max_r * sin(alpha * angle_line_half),
    linetype = "dashed"
  ) +
  # Arc labels
  # outer degree
  annotate(
    geom = "text",angle = angle_text_full,
    x = (max_r + 0.2) * cos(alpha * angle_line_full),
    y = (max_r + 0.2) * sin(alpha * angle_line_full),
    label = paste0(round(30 * angle_line_full, digits = 0), "°") 
  ) +
  # distance 
  annotate(
    geom = "label",angle = angle_text_full,
    label.size=NA,fill="white",size=4,
    x = (0.85*max_r) * cos(alpha * angle_line_full),
    y = (0.85*max_r) * sin(alpha * angle_line_full),
    label = dist_cal_full 
  ) +
  annotate(
    geom = "label",angle = angle_text_full[-1]+0.5,
    label.size=NA,fill="white",size=2.5,
    x = (0.75*max_r) * cos(alpha * angle_line_half),
    y = (0.75*max_r) * sin(alpha * angle_line_half),
    label = dist_cal_half 
  ) +
  coord_equal(
    xlim = c(0, max_r + padding_limits),
    ylim = c(0, max_r + padding_limits),
    expand = FALSE, clip = "off"
  ) +
  scale_x_continuous(breaks = c(0,10))+
  scale_y_continuous(breaks = c(0,10))+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(face = 'bold', size=20)
  )+
  labs(x='',y='', 
       title = p_title)
ggsave(p_name, width=15, height = 15, dpi=600)

