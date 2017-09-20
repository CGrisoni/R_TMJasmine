setwd("/Users/CGrisoni/Documents/workspace/TM Jasmine/04_Traitement des données et matching")
data = read.csv( "/Users/CGrisoni/Documents/workspace/TM Jasmine/02_Extraction des données NIST/data_all.csv" ) ## 48434, 16 var


library(dplyr)
library(ggplot2)
library(mosaic)
library(ggtern)
library( reshape2 )
library( data.table )

library(fmsb)

# calculs de base sur la liste des données
data = mutate( data, xc = x.minut - x.centre )
data = mutate( data, yc = y.minut - y.centre ) 
data = mutate( data, angle.correction = rad2deg(angle.correction))
data = mutate( data, t.minut=(t.minut+180)%%360)
data = mutate( data, tc = t.minut - angle.correction)
data = mutate( data, angle.bc = ((360-angle.b)-(360-angle.c)))
data = mutate( data, theta = tc - phi) #nouveau: est adapté à l'angle de position de la minutie

for (i in 1:nrow(data)) {
  if (data[i,20] < 0) {
    data[i,20] = data[i,20] + 360 
  } 
  if (data[i,16] < 0){
    data[i,16] = data[i,16] + 360
  }
}


#### Classer les minuties selon zone et distance: ----
# minutie en aab, abb, bbc, bcc, cca, caa

for (i in 1:nrow(data)) {
  if (data[i,3]== "delta"){
    
    if (data[i,15] > data[i,11] & data[i,15] < (data[i,11]+data[i,12])/2) {data[i,21] = "aab"}
    if (data[i,15] > (data[i,11]+data[i,12])/2 & data[i,15] < data[i,12]) {data[i,21] = "abb"}
    
    if (data[i,15] > data[i,12] & data[i,15] < (data[i,12]+data[i,13])/2) {data[i,21] = "bbc"}
    if (data[i,15] > (data[i,12]+data[i,13])/2 & data[i,15] < data[i,13]) {data[i,21] = "bcc"}
    
    if ((data[i,13]+(450-data[i,13])/2)%%360 > 90 & data[i,15] > data[i,13] & data[i,15] < (data[i,13]+(450-data[i,13])/2)%%360 |
        (data[i,13]+(450-data[i,13])/2)%%360 < 90 & data[i,15] < (data[i,13]+(450-data[i,13])/2)%%360) {data[i,21] = "cca"}
    
    if ((data[i,13]+(450-data[i,13])/2)%%360 > 90 & (data[i,15] < 90 | data[i,15] > data[i,13]) |
        (data[i,13]+(450-data[i,13])/2)%%360 < 90 & data[i,15] > (data[i,13]+(450-data[i,13])/2)%%360 & data[i,15] < 90) {data[i,21] = "caa"}

    if (data[i,15] == data[i,11]) {data[i,21] = "a"}
    if (data[i,15] == data[i,12]) {data[i,21] = "b"}
    if (data[i,15] == data[i,13]) {data[i,21] = "c"}
    
    if (data[i,15] == (data[i,11]+data[i,12])/2) {data[i,21] = "ab"}
    if (data[i,15] == (data[i,12]+data[i,13])/2) {data[i,21] = "bc"}
    if (data[i,15] == (data[i,13]+(450-data[i,13])/2)%%360) {data[i,21] = "ca"}}}

a=0;b=0;c=0;ab=0;bc=0;ca=0

for (i in 1:nrow(data)) {
  if (data[i,3]== "delta"){
    if(data[i,21]=="a"){a=a+1}
    else if (data[i,21]=="b"){b=b+1}
    else if (data[i,21]=="c"){c=c+1}
    else if (data[i,21]=="ab"){ab=ab+1}
    else if (data[i,21]=="bc"){bc=bc+1}
    else if (data[i,21]=="ca"){ca=ca+1}
    }}


#minutie en groupe de radius:
#0.5: x <= 0.5, 0.7: 0.5< x <= 0.7, 0.9: 0.7< x <= 0.9, 1: x>0.9
for (i in 1:nrow(data)){
  if (data[i,3] != "zone"){
    if (data[i,14] <= 5){
      data[i,22] = 0.5
    } 
    if (data[i,14] <= 7 & data[i,14] > 5){
      data[i,22] = 0.7
    }
    if (data[i,14] <= 9 & data[i,14] > 7){
      data[i,22] = 0.9
    }
    if (data[i,14] > 9){
      data[i,22] = 1
    }
  }
}

## Radar Chart
data.aab = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.5)

radar.aab = as.data.frame(matrix(0,ncol=36))

for (i in data.aab[,16]){
  print(i)
  radar.aab[1,round(i/10)%%36+1]=radar.aab[1,round(i/10)%%36+1]+1
}

radar.aab = radar.aab / sum(radar.aab)*100
 
radar.aab=rbind(rep(20,10) ,rep(0,10) , radar.aab)

colnames(radar.aab)=seq(0,350,10)

radarchart(radar.aab)

radarchart( radar.aab  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

