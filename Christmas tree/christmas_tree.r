library(tidyverse)
data = data.frame(x=seq(-5, 5), y=0:10)
snow = data.frame(x=runif(100,-5,5), y=runif(100,0,10))
layer = 20
triangles = data.frame(
  group = rep(seq(1, layer), 3),
  pointx= c(seq(min(data$x), mean(data$x)-0.5, length.out=layer), 
            rep(0,layer),
            seq(max(data$x), mean(data$x+0.5), length.out=layer)),
  pointy= c(seq(min(data$y) +2, max(data$y)-2, length.out=layer), 
            seq(min(data$y) +3,max(data$y)-1, length.out=layer+1)[-1], 
            seq(min(data$y) +2, max(data$y)-2, length.out=layer))
            )
pres_xposition = rnorm(10,0,2)
pres_width = runif(10,0.3,0.7)
pres_height = runif(10,0,1)
  
present = data.frame(
    group = seq(1,10),
    x_min= pres_xposition-pres_width, 
    x_max= pres_xposition+pres_width, 
    y_min= rep(0,10),
    y_max= pres_height,
    x_min_rib = pres_xposition - pres_width/4,
    x_max_rib = pres_xposition + pres_width/4
  )
x = NULL
list_value = list(NULL)
list_layer_x = list(NULL)
list_layer_y= list(NULL)
  
for (i in 1:layer) {
    j = (layer:1)[1]
    x = runif(j,
              triangles[which(triangles$group==i), 2][1],
              triangles[which(triangles$group==i), 2][3])
    list_layer_x[[length(list_layer_x) +1]] <- rep(i,j)
    list_layer_y[[length(list_layer_y) +1]] <- rep(triangles[which(triangles$group==1), 3][1],j) 
    list_value[[length(list_value) +1]] <- x
      }
  
ornaments = data.frame(
    layer = unlist(list_layer_x),
    x_position = unlist(list_value),
    y_position = unlist(list_layer_y),
    size = rpois (length(unlist(list_layer_y)),0.5)+1,
    color = sample(c('red', 'blue', 'yellow'), length(unlist(list_layer_y)), replace = T)
       )
  
rand_drop = sample (1:dim(ornaments)[1], round(dim(ornaments)[1]/2, digits = 0), replace = F)
  
ggplot() +
  geom_rect(data=data, aes(xmin=mean(x)-0.75, xmax=mean(x) +0.75, ymin=min (y), ymax=max (y)-7), fill='tan3') + 
  annotate('point', x = 0, y = max(triangles$pointy), shape=24, fill='yellow',size=8, color='yellow') + 
  annotate('point', x = 0, y = max(triangles$pointy), shape=25, fill='yellow',size=8, color='yellow') + 
  geom_polygon(data=triangles, aes(x=pointx, y=pointy, group=group), fill='green4')+
  geom_jitter(data=ornaments[rand_drop, ], aes(x=x_position, y=y_position, size=size, color=color))+ 
  scale_color_brewer(palette = 'Y1Gn')+
  geom_rect(data=present, aes(xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, fill=factor (group)))+ 
  geom_rect(data=present, aes(xmin=x_min_rib, xmax=x_max_rib, ymin=y_min, ymax=y_max), fill='yellow') + 
  geom_point(data=snow, aes(x=x, y=y), shape='*', color="white", size=6)+
  theme_void()+
  theme(legend.position= 'none',
      panel.background = element_rect(fill="#BFD5E3"))
#ggsave('cs_tree.jpeg', dpi=300, width=5, height = 5)


