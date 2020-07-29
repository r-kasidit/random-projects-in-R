#######################################################################
###########################     NMDS    ###############################
######################################################################

#remove lists
rm(list=ls())

#library packages
library(vegan)
library(tidyverse)


#import datasets

spe = read.csv("SPE_BL.csv")
envi = read.csv("EVR_BL.csv")
rownames(envi) = envi[,1] # first col as row names
envi = envi[,-1] 

head(spe)# it is already abundance data
head(envi)

#do the nMDS 
meta.spe = t(spe) # transpose dataset 
                  # b/c, in community or distance analysis, 
                  # col must be the communities.
colnames(meta.spe) = meta.spe[1,] # first row as colnames
meta.spe = meta.spe[-1,] 
as.data.frame(meta.spe)
as.matrix(meta.spe)

nmds.spe = metaMDS(meta.spe, distance = "bray")


nmds.point = as.data.frame(meta.nmds$points)
nmds.spe = as.data.frame(meta.nmds$species)

nmds.plot = as.data.frame(rbind(meta.nmds$points,meta.nmds$species))
ggplot(nmds.point,aes(x=MDS1,y=MDS2))+
  geom_point()+
  stat_ellipse()+
  annotate(geom = "text", x=nmds.point$MDS1,y=nmds.point$MDS2+0.05,
           label = rownames(nmds.point), size= 3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x="NMDS1",y="NMDS2")
