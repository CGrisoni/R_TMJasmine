setwd("/Users/CGrisoni/Documents/workspace/TM Jasmine/04_Traitement des données et matching")
data = read.csv( "/Users/CGrisoni/Documents/workspace/TM Jasmine/02_Extraction des données NIST/data_all.csv" ) ## 48434, 16 var


library(dplyr) # mutate, 
library(ggplot2)
library(mosaic) # rad2deg, 
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
data = mutate( data, angledif.bc = ((360-angle.b)-(360-angle.c)))
data = mutate( data, theta = tc - phi) #nouveau: est adapté à l'angle de position de la minutie

# Modification / ajout CG
data = mutate( data, phi = (phi + angle.correction - 90)%%360) #intégration de la correction pour phi
data = mutate( data, angle.a = angle.a -90)
data = mutate( data, angle.b = angle.b -90)
data = mutate( data, angle.c = (angle.c -90)%%360)

data = mutate( data, angle.ab = (angle.a + angle.b)/2)
data = mutate( data, angle.bc = (angle.b + angle.c)/2)
data = mutate( data, angle.ca = (angle.c + (360 - angle.c)/2))

for (i in 1:nrow(data)) {
  if (data[i,"angledif.bc"] < 0) {
    data[i,"angledif.bc"] = data[i,"angledif.bc"] + 360 
  } 
  if (data[i,"theta"] < 0){
    data[i,"theta"] = data[i,"theta"] + 360
  }
}


#### Classer les minuties selon zone et distance: ----
# minutie en aab, abb, bbc, bcc, cca, caa QUI FONCTIONNE

old <- Sys.time() # get start time

for (i in 1:nrow(data)) {
  if (data[i,"delta"] == "delta"){
    if      (data[i,"angle.a"]  < data[i,"phi"] & data[i,"phi"] < data[i,"angle.ab"]) {data[i,24] = "aab"}
    else if (data[i,"angle.ab"] < data[i,"phi"] & data[i,"phi"] < data[i,"angle.b"])  {data[i,24] = "abb"}
    
    else if (data[i,"angle.b"]  < data[i,"phi"] & data[i,"phi"] < data[i,"angle.bc"]) {data[i,24] = "bbc"}
    else if (data[i,"angle.bc"] < data[i,"phi"] & data[i,"phi"] < data[i,"angle.c"])  {data[i,24] = "bcc"}
    
    else if (data[i,"angle.c"]  < data[i,"phi"] & data[i,"phi"] < data[i,"angle.ca"]) {data[i,24] = "cca"}
    else if (data[i,"angle.ca"] < data[i,"phi"] & data[i,"phi"] < data[i,"angle.a"]+360)  {data[i,24] = "caa"}
    
    else if (data[i,"phi"] == data[i,"angle.a"]) {data[i,24] = "a"}
    else if (data[i,"phi"] == data[i,"angle.b"]) {data[i,24] = "b"}
    else if (data[i,"phi"] == data[i,"angle.c"]) {data[i,24] = "c"}
    
    else if (data[i,"phi"] == data[i,"angle.ab"]) {data[i,24] = "ab"}
    else if (data[i,"phi"] == data[i,"angle.bc"]) {data[i,24] = "bc"}
    else if (data[i,"phi"] == data[i,"angle.ca"]) {data[i,24] = "ca"}
    
    else {print ("Error")}
    }}

new <- Sys.time() - old # calculate difference
print(new) # print in nice format

for (i in 1:nrow(data)) {#print(i)
  if (data[i,"delta"] == "delta"){
    if (data[i,24] == "aab"){
      if(-5 <= data[i,"xc"])                            {data[i,25] = 1} #17 = xc ; 18 = yc
      else if(-10 <= data[i,"xc"] & data[i,"xc"] < -5)  {data[i,25] = 2}
      else if(data[i,"xc"] < -10)                       {data[i,25] = 3}}
    
    else if (data[i,24] == "abb"){
      if(data[i,"yc"] <= data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) + 5/sin(deg2rad(data[i,"angle.b"])))                    {data[i,25] = 1}
      else if(          (data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) + 5/sin(deg2rad(data[i,"angle.b"]))) < data[i,"yc"] & 
         data[i,"yc"] <= data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) + 10/sin(deg2rad(data[i,"angle.b"])))    {data[i,25] = 2}
      else if(          (data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) + 10/sin(deg2rad(data[i,"angle.b"]))) < data[i,"yc"])    {data[i,25] = 3}}
    
    else if (data[i,24] == "bbc"){
      if(data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) - 5/sin(deg2rad(data[i,"angle.b"])) <= data[i,"yc"])                    {data[i,25] = 1}
      else if(          (data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) -10/sin(deg2rad(data[i,"angle.b"]))) < data[i,"yc"] & 
                        data[i,"yc"] <= data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) - 5/sin(deg2rad(data[i,"angle.b"])))    {data[i,25] = 2}
      else if(          (data[i,"yc"] < data[i,"xc"]*tan(deg2rad(data[i,"angle.b"]+90)) - 10/sin(deg2rad(data[i,"angle.b"]))))    {data[i,25] = 3}}
    
    else if (data[i,24] == "bcc"){
      if(data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) + 5/sin(deg2rad(data[i,"angle.c"])) <= data[i,"yc"])                    {data[i,25] = 1}
      else if(          (data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) + 10/sin(deg2rad(data[i,"angle.c"]))) < data[i,"yc"] & 
                        data[i,"yc"] <= data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) + 5/sin(deg2rad(data[i,"angle.c"])))    {data[i,25] = 2}
      else if(          (data[i,"yc"] < data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) + 10/sin(deg2rad(data[i,"angle.c"]))))    {data[i,25] = 3}}
    
    else if (data[i,24] == "cca"){
      if(data[i,"yc"] <= data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) - 5/sin(deg2rad(data[i,"angle.c"])))                    {data[i,25] = 1}
      else if(          (data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) - 5/sin(deg2rad(data[i,"angle.c"]))) < data[i,"yc"] & 
                        data[i,"yc"] <= data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) - 10/sin(deg2rad(data[i,"angle.c"])))    {data[i,25] = 2}
      else if(          (data[i,"xc"]*tan(deg2rad(data[i,"angle.c"]+90)) - 10/sin(deg2rad(data[i,"angle.c"]))) < data[i,"yc"])    {data[i,25] = 3}}
    
    else if (data[i,24] == "caa"){
      if(data[i,"xc"] <= 5)                           {data[i,25] = 1}
      else if(5 < data[i,"xc"] & data[i,"xc"] <= 10)  {data[i,25] = 2}
      else if(10 < data[i,"xc"])                      {data[i,25] = 3}}
  }}

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




## Radar Chart

radar3.angle <- function (data, zone, main, angle, title) {
  
  # for (i in c("aab","abb","bbc","bcc","cca","caa")){
  #   if (angle==i){ 
  #     tmp1 = subset(data, type==zone & d.g==main & V24=="aab" & V25==1) 
  #     tmp2 = subset(data, type==zone & d.g==main & V24=="aab" & V25==2)
  #     tmp3 = subset(data, type==zone & d.g==main & V24=="aab" & V25==3)}
  # } ### à mettre tout à la fin
  
  if (angle=="aab"){
    tmp1 = subset(data, type==zone & d.g==main & V24=="aab" & V25==1)
    tmp2 = subset(data, type==zone & d.g==main & V24=="aab" & V25==2)
    tmp3 = subset(data, type==zone & d.g==main & V24=="aab" & V25==3)}

  if (angle=="abb"){
    tmp1 = subset(data, type==zone & d.g==main & V24=="abb" & V25==1)
    tmp2 = subset(data, type==zone & d.g==main & V24=="abb" & V25==2)
    tmp3 = subset(data, type==zone & d.g==main & V24=="abb" & V25==3)}

  if (angle=="bbc"){
    tmp1 = subset(data, type==zone & d.g==main & V24=="bbc" & V25==1)
    tmp2 = subset(data, type==zone & d.g==main & V24=="bbc" & V25==2)
    tmp3 = subset(data, type==zone & d.g==main & V24=="bbc" & V25==3)}

  if (angle=="bcc"){
    tmp1 = subset(data, type==zone & d.g==main & V24=="bcc" & V25==1)
    tmp2 = subset(data, type==zone & d.g==main & V24=="bcc" & V25==2)
    tmp3 = subset(data, type==zone & d.g==main & V24=="bcc" & V25==3)}

  if (angle=="cca"){
    tmp1 = subset(data, type==zone & d.g==main & V24=="cca" & V25==1)
    tmp2 = subset(data, type==zone & d.g==main & V24=="cca" & V25==2)
    tmp3 = subset(data, type==zone & d.g==main & V24=="cca" & V25==3)}

  if (angle=="caa"){
    tmp1 = subset(data, type==zone & d.g==main & V24=="caa" & V25==1)
    tmp2 = subset(data, type==zone & d.g==main & V24=="caa" & V25==2)
    tmp3 = subset(data, type==zone & d.g==main & V24=="caa" & V25==3)}
  
  print(nrow(tmp1));print(nrow(tmp2));print(nrow(tmp3))
  
  
  radar.tmp1 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp1)=c(seq(90,350,10),seq(0,80,10))
  radar.tmp2 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp2)=c(seq(90,350,10),seq(0,80,10))
  radar.tmp3 = as.data.frame(matrix(0,ncol=36)) ;colnames(radar.tmp3)=c(seq(90,350,10),seq(0,80,10))
  
  print("Test 0")
  
  if (angle=="aab" | angle=="caa"){ 
    for (i in tmp1[,19]) { radar.tmp1[1,round(i/10-9)%%36+1] = radar.tmp1[1,round(i/10-9)%%36+1]+1 }
    for (i in tmp2[,19]) { radar.tmp2[1,round(i/10-9)%%36+1] = radar.tmp2[1,round(i/10-9)%%36+1]+1 }
    if (nrow(tmp3 != 0)) {for (i in tmp3[,19]) { radar.tmp3[1,round(i/10-9)%%36+1] = radar.tmp3[1,round(i/10-9)%%36+1]+1 }}
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    if (nrow(tmp3 != 0)) {radar.tmp3 = radar.tmp3 / sum(radar.tmp3)*100}
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2, radar.tmp3, c(30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) }
  
  if (angle=="abb" | angle == "bbc"){print("Test 1")
    for (i in 1:nrow(tmp1)) { radar.tmp1[1,round((tmp1[i,19]-tmp1[i,12]+210)/10-9)%%36+1] = radar.tmp1[1,round((tmp1[i,19]-tmp1[i,12]+210)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp2)) { radar.tmp2[1,round((tmp2[i,19]-tmp2[i,12]+210)/10-9)%%36+1] = radar.tmp2[1,round((tmp2[i,19]-tmp2[i,12]+210)/10-9)%%36+1]+1 }
    if (nrow(tmp3 != 0)) {for (i in 1:nrow(tmp3)) { radar.tmp3[1,round((tmp3[i,19]-tmp3[i,12]+210)/10-9)%%36+1] = radar.tmp3[1,round((tmp3[i,19]-tmp3[i,12]+210)/10-9)%%36+1]+1 }}
    
    print("Test 2")
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    if (nrow(tmp3 != 0)) {radar.tmp3 = radar.tmp3 / sum(radar.tmp3)*100}
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2 , radar.tmp3, c(0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0)) }
  
  if (angle=="bcc" | angle=="cca"){print("Test 1")
    for (i in 1:nrow(tmp1)) { radar.tmp1[1,round((tmp1[i,19]-tmp1[i,13]+330)/10-9)%%36+1] = radar.tmp1[1,round((tmp1[i,19]-tmp1[i,13]+330)/10-9)%%36+1]+1 }
    for (i in 1:nrow(tmp2)) { radar.tmp2[1,round((tmp2[i,19]-tmp2[i,13]+330)/10-9)%%36+1] = radar.tmp2[1,round((tmp2[i,19]-tmp2[i,13]+330)/10-9)%%36+1]+1 }
    if (nrow(tmp3 != 0)) {for (i in 1:nrow(tmp3)) { radar.tmp3[1,round((tmp3[i,19]-tmp3[i,13]+330)/10-9)%%36+1] = radar.tmp3[1,round((tmp3[i,19]-tmp3[i,13]+330)/10-9)%%36+1]+1 }}
    
    print("Test 2")
    
    radar.tmp1 = radar.tmp1 / sum(radar.tmp1)*100 ; radar.tmp2 = radar.tmp2 / sum(radar.tmp2)*100 
    if (nrow(tmp3 != 0)) {radar.tmp3 = radar.tmp3 / sum(radar.tmp3)*100}
    radar.tmp=rbind(rep(30,15) ,rep(0,15) , radar.tmp1, radar.tmp2, radar.tmp3, c(0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0)) }
  
  radarchart( radar.tmp  , axistype=1 , 
              pcol=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9),rgb(0,1,0,0.9),rgb(0,0,0)) , pfcol=c(rgb(1,0,0,0.7), rgb(0,0,1,0.5),rgb(0,1,0,0.3),rgb(0,0,0)) , #custom polygon
              cglcol="grey", cglty=1, axislabcol="grey", seg=6, caxislabels=seq(0,30,5), cglwd=0.8, pty = 32, plty=1, plwd=c(1,1,1,3), #custom the grid
              vlcex=0.8 , centerzero=TRUE, #custom labels
              title = title )
}

old <- Sys.time() # get start time

pdf("Radar Chart.pdf",paper="a4",width=7.8,height=11.2)
#layout(matrix(c(seq(1,6,2),seq(2,6,2)),nrow=3,ncol=2))
layout(matrix(c(1,4,1,2,4,5,2,1,5,3,1,6,3,7,6,1,7,1),nrow=3))
par(oma=c(0,0,1.5,0)+0.1,mar=c(2,0,1,0)+0.1)

plot.new()

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

new <- Sys.time() - old # calculate difference
print(new) # print in nice format
