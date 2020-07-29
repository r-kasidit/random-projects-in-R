#remove lists
rm(list=ls())

#library packages
library(vegan)
library(tidyverse)
library(plyr)

#import datasets
ASH = read_csv("ASH.csv")
MHT = read_csv("MHT.csv")
MTK = read_csv("MTK.csv")
PTY = read_csv("PTY.csv")
SGK = read_csv("SGK.csv")
SGT = read_csv("SGT.csv")
SXV = read_csv("SXV.csv")
envi =read.csv("variable.csv")

ASH = ASH[,2:27]
MHT = MHT[,2:27]
MTK = MTK[,2:27]
PTY = PTY[,2:27]
SGK = SGK[,2:27]
SGT = SGT[,2:27]
SXV = SXV[,2:27]

#sum each species abundance into one and combind those into a dataframe
meta.data = as.data.frame(
  rbind(colSums(ASH),colSums(MHT),colSums(MTK),
        colSums(PTY),colSums(SGK),colSums(SGT),colSums(SXV)))


#set name of communities
rownames(meta.data) = c("ASH","MHT","MTK","PTY","SGK","SGT","SXV")

#calculate dissimilarity index
#                    to compare species dissimilarity btwn communities
#                             more value, lesser number of share sp. btwn comm.
#firstly, i create data frame of present-absent species in comm.

simi=NULL
for(i in 1:dim(meta.data)[1]){ 
  output=NULL
  for(j in 1:dim(meta.data)[2]){
    a = if (meta.data[i,j]!=0){print(1)} 
    else {print(0)}
    output = cbind(output,a)
  }
  simi=rbind(simi,output)
}
colnames(simi) = colnames(meta.data)
rownames(simi) = rownames(meta.data)

#then, do the similarity index
#i use Sorensen method (bray-cultis) 
# may be other method more suitable, buti dont know which one should be,
#i choose sorensen b/c of my limited knowledge
diss.bray<-vegdist(simi, method = "bray",binary = T); diss.bray
#reasonably, for similarity index
sim.bray = 1-vegdist(simi, method = "bray",binary = T); sim.bray

##########################################################################
#############################  CCA  #####################################

ENVI = envi[,c(1,3,5,8,11,16)]

ENVI.mean = aggregate(ENVI[,2:6], by=list(Category=ENVI$X), FUN=mean)
ENVI.mean = ENVI.mean[,2:6]
rownames(ENVI.mean) = rownames(meta.data)

####change colname

meta.data1 = meta.data
  colnames(meta.data1) = 1:26

meta.cca = cca(meta.data1,ENVI.mean)

anova.cca(meta.cca, test= "F")

plot(meta.cca)
