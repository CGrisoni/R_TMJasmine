setwd("/Users/CGrisoni/Documents/workspace/TM Jasmine/04_Traitement des données et matching")
data = read.csv( "/Users/CGrisoni/Documents/workspace/TM Jasmine/02_Extraction des données NIST/data_all.csv" ) ## 48434, 16 var


library(dplyr)
library(ggplot2)
library(mosaic)
library(ggtern)
library( reshape2 )
library( data.table )

library(fmsb) # radar chart
library(plotrix) # polar plot

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

# a=0;b=0;c=0;ab=0;bc=0;ca=0
# 
# for (i in 1:nrow(data)) {
#   if (data[i,3]== "delta"){
#     if(data[i,21]=="a"){a=a+1}
#     else if (data[i,21]=="b"){b=b+1}
#     else if (data[i,21]=="c"){c=c+1}
#     else if (data[i,21]=="ab"){ab=ab+1}
#     else if (data[i,21]=="bc"){bc=bc+1}
#     else if (data[i,21]=="ca"){ca=ca+1}
#     }}


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
radar.angle <- function (data, zone, main, angle, dist, title) {
  tmp = subset(data, type==zone & d.g==main & V21==angle & V22==dist) ; print(nrow(tmp))
  if (nrow(tmp) != 0) {
  radar.tmp = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp)=c(seq(90,350,10),seq(0,80,10))
  
  for (i in tmp[,19]) { radar.tmp[1,round(i/10-9)%%36+1] = radar.tmp[1,round(i/10-9)%%36+1]+1 } ## tmp[,16] c'est theta, 19 pour tc
  
  radar.tmp = radar.tmp / sum(radar.tmp)*100 ; 
  radar.tmp=rbind(rep(20,10) ,rep(0,10) , radar.tmp)
  
  radarchart( radar.tmp  , axistype=1 , 
              pcol=rgb(0,0,1,0.9) , pfcol=rgb(0,0,1,0.5) , #custom polygon
              cglcol="grey", cglty=1, axislabcol="grey", seg = 5, caxislabels=seq(0,25,5), cglwd=0.8, pty = 32, plty=1,  #custom the grid
              vlcex=0.8 , centerzero=TRUE , #custom labels
              title = title )
  } else {print("No Value")}}



radar.angle(data, "delta propre", "droit", "aab", 0.5, "aab 0.5" )
radar.angle(data, "delta propre", "droit", "aab", 0.7, "aab 0.7" )
radar.angle(data, "delta propre", "droit", "aab", 0.9, "aab 0.9" )
radar.angle(data, "delta propre", "droit", "aab", 1.0, "aab 1.0" )

radar.angle(data, "delta propre", "droit", "abb", 0.5, "abb 0.5" )
radar.angle(data, "delta propre", "droit", "abb", 0.7, "abb 0.7" )
radar.angle(data, "delta propre", "droit", "abb", 0.9, "abb 0.9" )
radar.angle(data, "delta propre", "droit", "abb", 1.0, "abb 1.0" )

radar.angle(data, "delta propre", "droit", "bbc", 0.5, "bbc 0.5" )
radar.angle(data, "delta propre", "droit", "bbc", 0.7, "bbc 0.7" )
radar.angle(data, "delta propre", "droit", "bbc", 0.9, "bbc 0.9" )
radar.angle(data, "delta propre", "droit", "bbc", 1.0, "bbc 1.0" )

radar.angle(data, "delta propre", "droit", "bcc", 0.5, "bcc 0.5" )
radar.angle(data, "delta propre", "droit", "bcc", 0.7, "bcc 0.7" )
radar.angle(data, "delta propre", "droit", "bcc", 0.9, "bcc 0.9" )
radar.angle(data, "delta propre", "droit", "bcc", 1.0, "bcc 1.0" )

radar.angle(data, "delta propre", "droit", "cca", 0.5, "cca 0.5" )
radar.angle(data, "delta propre", "droit", "cca", 0.7, "cca 0.7" )
radar.angle(data, "delta propre", "droit", "cca", 0.9, "cca 0.9" )
radar.angle(data, "delta propre", "droit", "cca", 1.0, "cca 1.0" )

radar.angle(data, "delta propre", "droit", "caa", 0.5, "caa 0.5" )
radar.angle(data, "delta propre", "droit", "caa", 0.7, "caa 0.7" )
radar.angle(data, "delta propre", "droit", "caa", 0.9, "caa 0.9" )
radar.angle(data, "delta propre", "droit", "caa", 1.0, "caa 1.0" )




radar2.angle <- function (data, zone, main, angle, dist, title) {
  if (angle=="A"){ print ("A")
    tmp1 = subset(data, type==zone & d.g==main & V21=="aab" & V22==dist) ; print(nrow(tmp1))
    tmp2 = subset(data, type==zone & d.g==main & V21=="caa" & V22==dist) ; print(nrow(tmp2)) }
  
  if (angle=="B"){
    tmp1 = subset(data, type==zone & d.g==main & V21=="abb" & V22==dist) ; print(nrow(tmp1))
    tmp2 = subset(data, type==zone & d.g==main & V21=="bbc" & V22==dist) ; print(nrow(tmp2)) }
  
  if (angle=="C"){
    tmp1 = subset(data, type==zone & d.g==main & V21=="cca" & V22==dist) ; print(nrow(tmp1))
    tmp2 = subset(data, type==zone & d.g==main & V21=="bcc" & V22==dist) ; print(nrow(tmp2)) }
  

  radar.tmp1 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp1)=c(seq(90,350,10),seq(0,80,10))
  radar.tmp2 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp2)=c(seq(90,350,10),seq(0,80,10))
  
  if (angle=="A"){
    for (i in tmp1[,19]) { radar.tmp1[1,round(i/10-9)%%36+1] = radar.tmp1[1,round(i/10-9)%%36+1]+1 }
    for (i in tmp2[,19]) { radar.tmp2[1,round(i/10-9)%%36+1] = radar.tmp2[1,round(i/10-9)%%36+1]+1 }
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2,c(30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) }
  
  if (angle=="B"){
    for (i in 1:nrow(tmp1)) { radar.tmp1[1,round((tmp1[i,19]-tmp1[i,12]+210)/10-9)%%36+1] = radar.tmp1[1,round((tmp1[i,19]-tmp1[i,12]+210)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp2)) { radar.tmp2[1,round((tmp2[i,19]-tmp2[i,12]+210)/10-9)%%36+1] = radar.tmp2[1,round((tmp2[i,19]-tmp2[i,12]+210)/10-9)%%36+1]+1 }
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2,c(0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0)) }
  
  if (angle=="C"){
    for (i in 1:nrow(tmp1)) { radar.tmp1[1,round((tmp1[i,19]-tmp1[i,13]+330)/10-9)%%36+1] = radar.tmp1[1,round((tmp1[i,19]-tmp1[i,13]+330)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp2)) { radar.tmp2[1,round((tmp2[i,19]-tmp2[i,13]+330)/10-9)%%36+1] = radar.tmp2[1,round((tmp2[i,19]-tmp2[i,13]+330)/10-9)%%36+1]+1 }
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2,c(0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0)) }
    
  radarchart( radar.tmp  , axistype=1 , 
              pcol=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9),rgb(0,0,0)) , pfcol=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5),rgb(0,0,0)) , #custom polygon
              cglcol="grey", cglty=1, axislabcol="grey", seg=6, caxislabels=seq(0,30,5), cglwd=0.8, pty = 32, plty=1, plwd=c(1,1,3), #custom the grid
              vlcex=0.8 , centerzero=TRUE, #custom labels
              title = title )
}




radar2.angle(data, "delta propre", "droit", "A", 0.5, "Angle a 0.5" )
radar2.angle(data, "delta propre", "droit", "B", 0.5, "Angle b 0.5" )
radar2.angle(data, "delta propre", "droit", "C", 0.5, "Angle c 0.5" )



radar3.angle <- function (data, zone, main, angle, title) {
  
  if (angle=="aab"){ 
    tmp1 = subset(data, type==zone & d.g==main & V21=="aab" & V22==0.5) 
    tmp2 = subset(data, type==zone & d.g==main & V21=="aab" & V22==0.7)
    tmp3 = subset(data, type==zone & d.g==main & V21=="aab" & V22==0.9) 
    tmp4 = subset(data, type==zone & d.g==main & V21=="aab" & V22==1.0) }
  
  if (angle=="abb"){ 
    tmp1 = subset(data, type==zone & d.g==main & V21=="abb" & V22==0.5) 
    tmp2 = subset(data, type==zone & d.g==main & V21=="abb" & V22==0.7)
    tmp3 = subset(data, type==zone & d.g==main & V21=="abb" & V22==0.9) 
    tmp4 = subset(data, type==zone & d.g==main & V21=="abb" & V22==1.0)}
  
  if (angle=="bbc"){ 
    tmp1 = subset(data, type==zone & d.g==main & V21=="bbc" & V22==0.5) 
    tmp2 = subset(data, type==zone & d.g==main & V21=="bbc" & V22==0.7)
    tmp3 = subset(data, type==zone & d.g==main & V21=="bbc" & V22==0.9) 
    tmp4 = subset(data, type==zone & d.g==main & V21=="bbc" & V22==1.0) }
  
  if (angle=="bcc"){ 
    tmp1 = subset(data, type==zone & d.g==main & V21=="bcc" & V22==0.5) 
    tmp2 = subset(data, type==zone & d.g==main & V21=="bcc" & V22==0.7)
    tmp3 = subset(data, type==zone & d.g==main & V21=="bcc" & V22==0.9) 
    tmp4 = subset(data, type==zone & d.g==main & V21=="bcc" & V22==1.0) }
  
  if (angle=="cca"){ 
    tmp1 = subset(data, type==zone & d.g==main & V21=="cca" & V22==0.5) 
    tmp2 = subset(data, type==zone & d.g==main & V21=="cca" & V22==0.7)
    tmp3 = subset(data, type==zone & d.g==main & V21=="cca" & V22==0.9) 
    tmp4 = subset(data, type==zone & d.g==main & V21=="cca" & V22==1.0) }
  
  if (angle=="caa"){ 
    tmp1 = subset(data, type==zone & d.g==main & V21=="caa" & V22==0.5) 
    tmp2 = subset(data, type==zone & d.g==main & V21=="caa" & V22==0.7)
    tmp3 = subset(data, type==zone & d.g==main & V21=="caa" & V22==0.9) 
    tmp4 = subset(data, type==zone & d.g==main & V21=="caa" & V22==1.0) }
  
  print(nrow(tmp1));print(nrow(tmp2));print(nrow(tmp3));print(nrow(tmp4))
  
  
  radar.tmp1 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp1)=c(seq(90,350,10),seq(0,80,10))
  radar.tmp2 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp2)=c(seq(90,350,10),seq(0,80,10))
  radar.tmp3 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp3)=c(seq(90,350,10),seq(0,80,10))
  radar.tmp4 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp4)=c(seq(90,350,10),seq(0,80,10))
  
  print("Test 0")
  
  if (angle=="aab" | angle=="caa"){ 
    for (i in tmp1[,19]) { radar.tmp1[1,round(i/10-9)%%36+1] = radar.tmp1[1,round(i/10-9)%%36+1]+1 }
    for (i in tmp2[,19]) { radar.tmp2[1,round(i/10-9)%%36+1] = radar.tmp2[1,round(i/10-9)%%36+1]+1 }
    for (i in tmp3[,19]) { radar.tmp3[1,round(i/10-9)%%36+1] = radar.tmp3[1,round(i/10-9)%%36+1]+1 }
    for (i in tmp4[,19]) { radar.tmp4[1,round(i/10-9)%%36+1] = radar.tmp4[1,round(i/10-9)%%36+1]+1 }
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    radar.tmp3 = radar.tmp3 / sum(radar.tmp3)*100 ; radar.tmp4 = radar.tmp4 / sum(radar.tmp4)*100
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2, radar.tmp3, radar.tmp4, c(30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) }
  
  if (angle=="abb" | angle == "bbc"){print("Test 1")
    for (i in 1:nrow(tmp1)) { radar.tmp1[1,round((tmp1[i,19]-tmp1[i,12]+210)/10-9)%%36+1] = radar.tmp1[1,round((tmp1[i,19]-tmp1[i,12]+210)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp2)) { radar.tmp2[1,round((tmp2[i,19]-tmp2[i,12]+210)/10-9)%%36+1] = radar.tmp2[1,round((tmp2[i,19]-tmp2[i,12]+210)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp3)) { radar.tmp3[1,round((tmp3[i,19]-tmp3[i,12]+210)/10-9)%%36+1] = radar.tmp3[1,round((tmp3[i,19]-tmp3[i,12]+210)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp4)) { radar.tmp4[1,round((tmp4[i,19]-tmp4[i,12]+210)/10-9)%%36+1] = radar.tmp4[1,round((tmp4[i,19]-tmp4[i,12]+210)/10-9)%%36+1]+1 }
    
    print("Test 2")
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    radar.tmp3 = radar.tmp3 / sum(radar.tmp3)*100 ; radar.tmp4 = radar.tmp4 / sum(radar.tmp4)*100
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2 , radar.tmp3, radar.tmp4 ,c(0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0)) }
  
  if (angle=="bcc" | angle=="cca"){print("Test 1")
    for (i in 1:nrow(tmp1)) { radar.tmp1[1,round((tmp1[i,19]-tmp1[i,13]+330)/10-9)%%36+1] = radar.tmp1[1,round((tmp1[i,19]-tmp1[i,13]+330)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp2)) { radar.tmp2[1,round((tmp2[i,19]-tmp2[i,13]+330)/10-9)%%36+1] = radar.tmp2[1,round((tmp2[i,19]-tmp2[i,13]+330)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp3)) { radar.tmp3[1,round((tmp3[i,19]-tmp3[i,13]+330)/10-9)%%36+1] = radar.tmp3[1,round((tmp3[i,19]-tmp3[i,13]+330)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp4)) { radar.tmp4[1,round((tmp4[i,19]-tmp4[i,13]+330)/10-9)%%36+1] = radar.tmp4[1,round((tmp4[i,19]-tmp4[i,13]+330)/10-9)%%36+1]+1 }
    
    print("Test 2")
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    radar.tmp3 = radar.tmp3 / sum(radar.tmp3)*100 ; radar.tmp4 = radar.tmp4 / sum(radar.tmp4)*100
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2, radar.tmp3, radar.tmp4 ,c(0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0)) }
  
  radarchart( radar.tmp  , axistype=1 , 
              pcol=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9),rgb(0,1,0,0.9),rgb(0,0,0,0.7),rgb(0,0,0)) , pfcol=c(rgb(1,0,0,0.7), rgb(0,0,1,0.5),rgb(0,1,0,0.3),rgb(0,0,0,0.1),rgb(0,0,0)) , #custom polygon
              cglcol="grey", cglty=1, axislabcol="grey", seg=6, caxislabels=seq(0,30,5), cglwd=0.8, pty = 32, plty=1, plwd=c(1,1,1,1,3), #custom the grid
              vlcex=0.8 , centerzero=TRUE, #custom labels
              title = title )
}


pdf("Radar Chart.pdf",paper="a4",width=7.8,height=11.2)
layout(matrix(c(seq(1,6,2),seq(2,6,2)),nrow=3,ncol=2))
par(oma=c(0,0,1.5,0)+0.1,mar=c(2,0,1,0)+0.1)

radar3.angle(data, "delta propre", "droit", "aab", "aab delta propre droit")
radar3.angle(data, "delta propre", "droit", "caa", "caa delta propre droit")
radar3.angle(data, "delta propre", "droit", "abb", "abb delta propre droit")
radar3.angle(data, "delta propre", "droit", "bbc", "bbc delta propre droit")
radar3.angle(data, "delta propre", "droit", "bcc", "bcc delta propre droit")
radar3.angle(data, "delta propre", "droit", "cca", "cca delta propre droit")

radar3.angle(data, "delta propre", "gauche", "aab", "aab delta propre gauche")
radar3.angle(data, "delta propre", "gauche", "caa", "caa delta propre gauche")
radar3.angle(data, "delta propre", "gauche", "abb", "abb delta propre gauche")
radar3.angle(data, "delta propre", "gauche", "bbc", "bbc delta propre gauche")
radar3.angle(data, "delta propre", "gauche", "bcc", "bcc delta propre gauche")
radar3.angle(data, "delta propre", "gauche", "cca", "cca delta propre gauche")

radar3.angle(data, "delta de pouce", "droit", "aab", "aab delta propre droit")
radar3.angle(data, "delta de pouce", "droit", "caa", "caa delta propre droit")
radar3.angle(data, "delta de pouce", "droit", "abb", "abb delta propre droit")
radar3.angle(data, "delta de pouce", "droit", "bbc", "bbc delta propre droit")
radar3.angle(data, "delta de pouce", "droit", "bcc", "bcc delta propre droit")
radar3.angle(data, "delta de pouce", "droit", "cca", "cca delta propre droit")

radar3.angle(data, "delta de pouce", "gauche", "aab", "aab delta de pouce gauche")
radar3.angle(data, "delta de pouce", "gauche", "caa", "caa delta de pouce gauche")
radar3.angle(data, "delta de pouce", "gauche", "abb", "abb delta de pouce gauche")
radar3.angle(data, "delta de pouce", "gauche", "bbc", "bbc delta de pouce gauche")
radar3.angle(data, "delta de pouce", "gauche", "bcc", "bcc delta de pouce gauche")
radar3.angle(data, "delta de pouce", "gauche", "cca", "cca delta de pouce gauche")

dev.off()

# 
# data.aab1 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.5)
# radar.aab1 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab1)=c(seq(90,350,10),seq(0,80,10))
# 
# for (i in data.aab1[,19]){
#   radar.aab1[1,round(i/10-9)%%36+1]=radar.aab1[1,round(i/10-9)%%36+1]+1}
# 
# data.caa1 = subset(data, type=="delta propre" & d.g=="droit" & V21=="caa" & V22==0.5)
# radar.caa1 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.caa1)=c(seq(90,350,10),seq(0,80,10))
# 
# for (i in data.caa1[,19]){
#   radar.caa1[1,round(i/10-9)%%36+1]=radar.caa1[1,round(i/10-9)%%36+1]+1}
# 
# radar.aab1 = radar.aab1 / sum(radar.aab1)*100
# radar.caa1 = radar.caa1 / sum(radar.caa1)*100
# 
# radar.aab1=rbind(rep(25,12.5) ,rep(0,12.5) , radar.aab1, radar.caa1,c(25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# 
# radarchart( radar.aab1  , axistype=1 ,
#             pcol=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9),rgb(0,0,0)) , pfcol=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5),rgb(0,0,0)) , #custom polygon
#             cglcol="grey", cglty=1, axislabcol="grey", seg=5, caxislabels=seq(0,25,5), cglwd=0.8, pty = 32, plty=1, plwd=c(1,1,3), #custom the grid
#             vlcex=0.8 , centerzero=TRUE, #custom labels
#             title = "Axis a 0.5" )




data.aab1 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.5)
radar.aab1 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab1)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab1[,19]){
  radar.aab1[1,round(i/10-9)%%36+1]=radar.aab1[1,round(i/10-9)%%36+1]+1}

data.aab2 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.7)
radar.aab2 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab2)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab2[,19]){
  radar.aab2[1,round(i/10-9)%%36+1]=radar.aab2[1,round(i/10-9)%%36+1]+1}

data.aab3 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.9)
radar.aab3 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab3)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab3[,19]){
  radar.aab3[1,round(i/10-9)%%36+1]=radar.aab3[1,round(i/10-9)%%36+1]+1}

data.aab4 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==1.0)
radar.aab4 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab4)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab4[,19]){
  radar.aab4[1,round(i/10-9)%%36+1]=radar.aab4[1,round(i/10-9)%%36+1]+1}

radar.aab1 = radar.aab1 / sum(radar.aab1)*100
radar.aab2 = radar.aab2 / sum(radar.aab2)*100
radar.aab3 = radar.aab3 / sum(radar.aab3)*100
radar.aab4 = radar.aab4 / sum(radar.aab4)*100

radar.aab1=rbind(rep(30,15) ,rep(0,15) , radar.aab1, radar.aab2, radar.aab3, radar.aab4,c(30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

radarchart( radar.aab1  , axistype=1 ,
            pcol=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9),rgb(0,1,0,0.9),rgb(0,0,0,0.7),rgb(0,0,0)) , pfcol=c(rgb(1,0,0,0.7), rgb(0,0,1,0.5),rgb(0,1,0,0.3),rgb(0,0,0,0.1),rgb(0,0,0)) , #custom polygon
            cglcol="grey", cglty=1, axislabcol="grey", seg=6, caxislabels=seq(0,30,5), cglwd=0.8, pty = 32, plty=1, plwd=c(1,1,1,1,3), #custom the grid
            vlcex=0.8 , centerzero=TRUE, #custom labels
            title = "Axis a 0.5" )

#----------------------------------------------------------------------------------------------------------------------------------------




testpos<-c(seq(90,359),seq(0,89))




data.aab1 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.5)
radar.aab1 = as.data.frame(matrix(0,ncol=360)) ; colnames(radar.aab1)=c(seq(90,359),seq(0,89))

for (i in data.aab1[,19]){
  radar.aab1[1,round(i-90)%%360+1]=radar.aab1[1,round(i-90)%%360+1]+1}

polar.plot(radar.aab1,testpos,main="Polar Plot", lwd=3,line.col=4)





data.aab2 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.7)
radar.aab2 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab2)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab2[,19]){
  radar.aab2[1,round(i/10-9)%%36+1]=radar.aab2[1,round(i/10-9)%%36+1]+1}

data.aab3 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==0.9)
radar.aab3 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab3)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab3[,19]){
  radar.aab3[1,round(i/10-9)%%36+1]=radar.aab3[1,round(i/10-9)%%36+1]+1}

data.aab4 = subset(data, type=="delta propre" & d.g=="droit" & V21=="aab" & V22==1.0)
radar.aab4 = as.data.frame(matrix(0,ncol=36)) ; colnames(radar.aab4)=c(seq(90,350,10),seq(0,80,10))

for (i in data.aab4[,19]){
  radar.aab4[1,round(i/10-9)%%36+1]=radar.aab4[1,round(i/10-9)%%36+1]+1}

radar.aab1 = radar.aab1 / sum(radar.aab1)*100
radar.aab2 = radar.aab2 / sum(radar.aab2)*100
radar.aab3 = radar.aab3 / sum(radar.aab3)*100
radar.aab4 = radar.aab4 / sum(radar.aab4)*100

radar.aab1=rbind(rep(30,15) ,rep(0,15) , radar.aab1, radar.aab2, radar.aab3, radar.aab4,c(30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

radarchart( radar.aab1  , axistype=1 ,
            pcol=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9),rgb(0,1,0,0.9),rgb(0,0,0,0.7),rgb(0,0,0)) , pfcol=c(rgb(1,0,0,0.7), rgb(0,0,1,0.5),rgb(0,1,0,0.3),rgb(0,0,0,0.1),rgb(0,0,0)) , #custom polygon
            cglcol="grey", cglty=1, axislabcol="grey", seg=6, caxislabels=seq(0,30,5), cglwd=0.8, pty = 32, plty=1, plwd=c(1,1,1,1,3), #custom the grid
            vlcex=0.8 , centerzero=TRUE, #custom labels
            title = "Axis a 0.5" )