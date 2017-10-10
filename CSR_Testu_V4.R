# CSR Test

setwd("/Users/CGrisoni/Documents/workspace/TM Jasmine/src")
data = read.csv( "/Users/CGrisoni/Documents/workspace/TM Jasmine/02_Extraction des données NIST/data_all.csv" )

library(ggplot2)
library(spatstat)
library(gridExtra) # tableGrob
library(plotrix)
library(dplyr)
library(mosaic)
library(ggtern)
library( reshape2 )
library( data.table )  


# calculs de base sur la liste des données----
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



#separation des données par zone et main----
pouceD = subset(data, type=="delta de pouce" & d.g=="droit")
pouceG = subset(data, type=="delta de pouce" & d.g=="gauche")
propreD = subset(data, type=="delta propre" & d.g=="droit")
propreG = subset(data, type=="delta propre" & d.g=="gauche")
controlD = subset(data, type=="zone de controle" & d.g=="droit")
controlG = subset(data, type=="zone de controle" & d.g=="gauche")

# Individual plot and Kest ---

pdfCSR <- function(data,name){
  r<- seq(0,7.5, by=0.01) ; CSR=0 ; cluster = 0 ; dispersed = 0 ; cluster2 = 0 ; dispersed2 = 0
  test = data.frame()
  
  pdf(paste(name,".pdf",sep=""),paper="a4",width=7.8,height=11.2,title="name")
  
    layout(matrix(c(seq(1,15,2),seq(2,16,2),seq(17,31,2),seq(18,32,2),seq(33,47,2),seq(34,48,2)),nrow=8,ncol=6))
    par(oma=c(0,0,1.5,0)+0.1,mar=c(2,0,1,0)+0.1)

    for (i in unique(data[,1])){
      datau = data.frame()
        for (j in 1:nrow(data)){
          if (data[j,1] == i){datau=rbind(datau,data[j,])}}
            ppp = ppp(datau[,17],datau[,18],disc(7.5))
            plot(ppp,main=i,cex=0.5)
            k=Kest(ppp,correction="Ripley",var.approx = TRUE,ratio= TRUE)
            edge =  c(-15,15) # c(-ceiling(max(c(2*sqrt(k$rip),k$t-pi * k$r^2))),ceiling(max(c(2*sqrt(k$rip),k$t-pi * k$r^2))))
  
            plot(k,. - pi * r^2 ~ r,ylim=edge,main="",ylab="",xlab="",legend=FALSE)
            lines(matrix(c(k$r,2*sqrt(k$rip)),ncol=2),lty=2,col="red")
            lines(matrix(c(k$r,-2*sqrt(k$rip)),ncol=2),lty=2,col="red")
            
            mtext(name, outer=TRUE,  cex=1, line=0)
  
            n=1 ; d=0 ; c=0 ; csr=0 ; d2=0 ; c2=0
            for (r in k$r) {
              if (k$iso[n] - pi * r^2 < -2*sqrt(k$rip[n])) {d=d+1}
              else if (k$iso[n] - pi * r^2 > 2*sqrt(k$rip[n])) {c=c+1}
              else csr=csr+1
              
              if (k$iso[n] - pi * r^2 < -2*sqrt(k$rip[n]) & k$r[n]>2.5) {d2=d2+1}
              else if (k$iso[n] - pi * r^2 > 2*sqrt(k$rip[n]) & k$r[n]>2.5) {c2=c2+1}
              
              n=n+1
              if (csr==length(k$r)){CSR=CSR+1}
              
            }
            #print (d) ; print (csr) ; print(c)
            test=rbind(test,c(round(d/length(k$r)*100),round(csr/length(k$r)*100),round(c/length(k$r)*100)))
            if (c>d){cluster=cluster + 1}
            else if (c<d){dispersed=dispersed+1}
            
            if (c2>d2){cluster2=cluster2 + 1}
            else if (c2<d2){dispersed2=dispersed2+1}
    }

   colnames(test)=c("Dis", "CSR", "Clu")
    
    t1=tableGrob(test[seq(1,20),],rows=NULL)
    t2=tableGrob(test[seq(21,40),],rows=NULL)
    t3=tableGrob(test[seq(41,60),],rows=NULL)
    t4=tableGrob(test[seq(61,80),],rows=NULL)
    t5=tableGrob(test[seq(81,100),],rows=NULL)
    grid.arrange(t1,t2,t3,t4,t5, ncol=5)
    
    mtext(name, outer=TRUE,  cex=1, line=-10)
    mtext(paste(CSR," total CSR distributions, ",cluster," clustering and ",dispersed," dispersed",sep=""),outer = TRUE, cex=1,line=-68)
    mtext(paste("Over 2.5 mm, ",cluster2," clustering and ",dispersed2," dispersed",sep=""),outer = TRUE, cex=1,line=-70)
    
  dev.off()
  #print(CSR) 
  }

pdfCSR(pouceD,"PouceD") # 17
pdfCSR(pouceG,"PouceG") # 7
pdfCSR(propreD,"PropreD") # 29
pdfCSR(propreG,"PropreG") # 24
pdfCSR(controlD,"ControlD") # 69
pdfCSR(controlG,"ControlG") # 74





pdfCSR <- function(data,name){
  r<- seq(0,7.5, by=0.01) ; CSR=0 ; cluster = 0 ; dispersed = 0 ; cluster2 = 0 ; dispersed2 = 0
  test = data.frame()
  
  pdf(paste(name,"1.pdf",sep=""),paper="a4r",width=11.2,height=7.8,title="name")
  
  layout(matrix(c(seq(1,9,2),seq(2,10,2),seq(11,19,2),seq(12,20,2),seq(21,29,2),seq(22,30,2),seq(31,39,2),seq(32,40,2)),nrow=5,ncol=8))
  par(oma=c(0,0,1.5,0)+0.1,mar=c(2,1.5,1,0)+0.1)
  
  for (i in unique(data[,1])){
    datau = data.frame()
    for (j in 1:nrow(data)){
      if (data[j,1] == i){datau=rbind(datau,data[j,])}}
    ppp = ppp(datau[,17],datau[,18],disc(7.5))
    plot(ppp,main=i,cex=0.5)
    k=Kest(ppp,correction="Ripley",var.approx = TRUE,ratio= TRUE)
    edge =  c(-15,15) # c(-ceiling(max(c(2*sqrt(k$rip),k$t-pi * k$r^2))),ceiling(max(c(2*sqrt(k$rip),k$t-pi * k$r^2))))
    
    plot(k,. - pi * r^2 ~ r,ylim=edge,main="",ylab="",xlab="",legend=FALSE)
    lines(matrix(c(k$r,2*sqrt(k$rip)),ncol=2),lty=2,col="red")
    lines(matrix(c(k$r,-2*sqrt(k$rip)),ncol=2),lty=2,col="red")
    
    mtext(name, outer=TRUE,  cex=1, line=0)
    
    n=1 ; d=0 ; c=0 ; csr=0 ; d2=0 ; c2=0
    for (r in k$r) {
      if (k$iso[n] - pi * r^2 < -2*sqrt(k$rip[n])) {d=d+1}
      else if (k$iso[n] - pi * r^2 > 2*sqrt(k$rip[n])) {c=c+1}
      else csr=csr+1
      
      if (k$iso[n] - pi * r^2 < -2*sqrt(k$rip[n]) & k$r[n]>2.5) {d2=d2+1}
      else if (k$iso[n] - pi * r^2 > 2*sqrt(k$rip[n]) & k$r[n]>2.5) {c2=c2+1}
      
      n=n+1
      if (csr==length(k$r)){CSR=CSR+1}
    }

    test=rbind(test,c(i,round(d/length(k$r)*100,1),round(csr/length(k$r)*100,1),round(c/length(k$r)*100,1)))
    if (c>d){cluster=cluster + 1}
    else if (c<d){dispersed=dispersed+1}
    
    if (c2>d2){cluster2=cluster2 + 1}
    else if (c2<d2){dispersed2=dispersed2+1}
  }
  
  colnames(test)=c("ID","Dis", "CSR", "Clu")
  
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.75)),
    colhead = list(fg_params=list(cex = 1.0)),
    rowhead = list(fg_params=list(cex = 1.0)))
  
  t1=tableGrob(test[seq(1,20),],rows=NULL, theme = mytheme)
  t2=tableGrob(test[seq(21,40),],rows=NULL, theme = mytheme)
  t3=tableGrob(test[seq(41,60),],rows=NULL, theme = mytheme)
  t4=tableGrob(test[seq(61,80),],rows=NULL, theme = mytheme)
  t5=tableGrob(test[seq(81,100),],rows=NULL, theme = mytheme)
  grid.arrange(t1,t2,t3,t4,t5, ncol=5)
  
  mtext(name, outer=TRUE,  cex=1, line=-0)
  mtext(paste(CSR," total CSR distributions, ",cluster," clustering and ",dispersed," dispersed",sep=""),outer = TRUE, cex=1,line=-52)
  mtext(paste("Over 2.5 mm, ",cluster2," clustering and ",dispersed2," dispersed",sep=""),outer = TRUE, cex=1,line=-54)
  
  dev.off()
  #print(CSR) 
}

pdfCSR(pouceD,"PouceD") # 17
pdfCSR(pouceG,"PouceG") # 7
pdfCSR(propreD,"PropreD") # 29
pdfCSR(propreG,"PropreG") # 24
pdfCSR(controlD,"ControlD") # 69
pdfCSR(controlG,"ControlG") # 74
