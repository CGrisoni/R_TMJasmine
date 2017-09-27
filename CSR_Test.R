# CSR Test

setwd("/Users/CGrisoni/Documents/workspace/TM Jasmine/src")
data = read.csv( "/Users/CGrisoni/Documents/workspace/TM Jasmine/02_Extraction des données NIST/data_all.csv" )

library(ggplot2)
library(spatstat)
library(gridExtra)
library(plotrix)


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

#Graphique delta propre vs zone de controle / Graphique 1----
data %>%
  subset (type !="delta de pouce") %>%
  ggplot(aes(x=xc,y=yc, colour=tc, bic=100))+
  geom_point(alpha=0.5)+
  facet_grid( type ~ d.g)+
  scale_color_continuous( name = "Direction des minuties \npar rapport à l'axe 0")+
  ggtitle("Distribution et direction des minuties sur la palmaire\nZone de delta vs Zone de contrôle \n ") +
  theme(plot.title = element_text(lineheight=0.8, face="bold", hjust=0.5, size=20)) +
  xlab(" Axe x") +
  ylab("Axe y")+
  theme(text=element_text(family="Times"))

#separation des données par zone et main----
pouceD = subset(data, type=="delta de pouce" & d.g=="droit")
pouceG = subset(data, type=="delta de pouce" & d.g=="gauche")
propreD = subset(data, type=="delta propre" & d.g=="droit")
propreG = subset(data, type=="delta propre" & d.g=="gauche")
controlD = subset(data, type=="zone de controle" & d.g=="droit")
controlG = subset(data, type=="zone de controle" & d.g=="gauche")



#Carré -15,15 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDc30 = ppp(pouceD[,17],pouceD[,18], c(-15,15), c(-15,15))                    #Tous les points /8782 - 8782 || T : R = 0.70589, p-value < 2.2e-16 --> clustered (R<1)
ppouceGc30 = ppp(pouceG[,17],pouceG[,18], c(-15,15), c(-15,15))                    #Tous les points /8947 - 8947 || T : R = 0.72671, p-value < 2.2e-16 --> clustered (R<1)
ppropreDc30 = ppp(propreD[,17],propreD[,18], c(-15,15), c(-15,15))                 #Tous les points /9244 - 9244 || T : R = 0.80102, p-value < 2.2e-16 --> clustered (R<1)
ppropreGc30 = ppp(propreG[,17],propreG[,18], c(-15,15), c(-15,15))                 #Tous les points /8935 - 8935 || T : R = 0.79807, p-value < 2.2e-16 --> clustered (R<1)
pcontrolDc30 = ppp(controlD[,17],controlD[,18], c(-15,15), c(-15,15))              #Tous les points /6282 - 6282 || T : R = 0.83162, p-value < 2.2e-16 --> clustered (R<1)
pcontrolGc30 = ppp(controlG[,17],controlG[,18], c(-15,15), c(-15,15))              #Tous les points /6244 - 6244 || T : R = 0.82823, p-value < 2.2e-16 --> clustered (R<1)

testc30=matrix(nrow=6,ncol=10);rownames(testc30)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testc30)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testc30[1,1]=nrow(pouceD);testc30[2,1]=nrow(pouceG);testc30[3,1]=nrow(propreD);testc30[4,1]=nrow(propreG);testc30[5,1]=nrow(controlD);testc30[6,1]=nrow(controlG)
testc30[1,2]=nrow(as.data.frame(ppouceDc30));testc30[2,2]=nrow(as.data.frame(ppouceGc30));testc30[3,2]=nrow(as.data.frame(ppropreDc30))
testc30[4,2]=nrow(as.data.frame(ppropreGc30));testc30[5,2]=nrow(as.data.frame(pcontrolDc30));testc30[6,2]=nrow(as.data.frame(pcontrolGc30))


testc30[1,3]=round(clarkevans.test(ppouceDc30)$statistic,4) ; testc30[2,3]=round(clarkevans.test(ppouceGc30)$statistic,4)
testc30[3,3]=round(clarkevans.test(ppropreDc30)$statistic,4) ; testc30[4,3]=round(clarkevans.test(ppropreGc30)$statistic,4)
testc30[5,3]=round(clarkevans.test(pcontrolDc30)$statistic,4) ; testc30[6,3]=round(clarkevans.test(pcontrolGc30)$statistic,4)

testc30[1,4]=clarkevans.test(ppouceDc30)$p.value ; testc30[2,4]=clarkevans.test(ppouceGc30)$p.value
testc30[3,4]=clarkevans.test(ppropreDc30)$p.value ; testc30[4,4]=clarkevans.test(ppropreGc30)$p.value
testc30[5,4]=clarkevans.test(pcontrolDc30)$p.value ; testc30[6,4]=clarkevans.test(pcontrolGc30)$p.value

testc30[1,5]=round(hopskel.test(ppouceDc30)$statistic,5) ; testc30[2,5]=round(hopskel.test(ppouceGc30)$statistic,5) ; testc30[3,5]=round(hopskel.test(ppropreDc30)$statistic,4)
testc30[4,5]=round(hopskel.test(ppropreGc30)$statistic,5) ; testc30[5,5]=round(hopskel.test(pcontrolDc30)$statistic,5) ; testc30[6,5]=round(hopskel.test(pcontrolGc30)$statistic,4)

testc30[1,6]=hopskel.test(ppouceDc30)$p.value ; testc30[2,6]=hopskel.test(ppouceGc30)$p.value ; testc30[3,6]=hopskel.test(ppropreDc30)$p.value
testc30[4,6]=hopskel.test(ppropreGc30)$p.value ; testc30[5,6]=hopskel.test(pcontrolDc30)$p.value ; testc30[6,6]=hopskel.test(pcontrolGc30)$p.value

testc30[1,7]=round(hopskel.test(ppouceDc30,method="asymptotic")$statistic,4) ; testc30[2,7]=round(hopskel.test(ppouceGc30,method="asymptotic")$statistic,4)
testc30[3,7]=round(hopskel.test(ppropreDc30,method="asymptotic")$statistic,4) ; testc30[4,7]=round(hopskel.test(ppropreGc30,method="asymptotic")$statistic,4)
testc30[5,7]=round(hopskel.test(pcontrolDc30,method="asymptotic")$statistic,4) ; testc30[6,7]=round(hopskel.test(pcontrolGc30,method="asymptotic")$statistic,4)

testc30[1,8]=hopskel.test(ppouceDc30,method="asymptotic")$p.value ; testc30[2,8]=hopskel.test(ppouceGc30,method="asymptotic")$p.value
testc30[3,8]=hopskel.test(ppropreDc30,method="asymptotic")$p.value ; testc30[4,8]=hopskel.test(ppropreGc30,method="asymptotic")$p.value
testc30[5,8]=hopskel.test(pcontrolDc30,method="asymptotic")$p.value ; testc30[6,8]=hopskel.test(pcontrolGc30,method="asymptotic")$p.value

testc30[1,9]=round(hopskel.test(ppouceDc30,method="MonteCarlo")$statistic,4) ; testc30[2,9]=round(hopskel.test(ppouceGc30,method="MonteCarlo")$statistic,4)
testc30[3,9]=round(hopskel.test(ppropreDc30,method="MonteCarlo")$statistic,4) ; testc30[4,9]=round(hopskel.test(ppropreGc30,method="MonteCarlo")$statistic,4)
testc30[5,9]=round(hopskel.test(pcontrolDc30,method="MonteCarlo")$statistic,4) ; testc30[6,9]=round(hopskel.test(pcontrolGc30,method="MonteCarlo")$statistic,4)

testc30[1,10]=hopskel.test(ppouceDc30,method="MonteCarlo")$p.value ; testc30[2,10]=hopskel.test(ppouceGc30,method="MonteCarlo")$p.value
testc30[3,10]=hopskel.test(ppropreDc30,method="MonteCarlo")$p.value ; testc30[4,10]=hopskel.test(ppropreGc30,method="MonteCarlo")$p.value
testc30[5,10]=hopskel.test(pcontrolDc30,method="MonteCarlo")$p.value ; testc30[6,10]=hopskel.test(pcontrolGc30,method="MonteCarlo")$p.value


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDc30,main="Delta de pouce",cex.main=1);plot(ppouceGc30,main="")
plot(ppropreDc30,main="Delta propre");plot(ppropreGc30,main="")
plot(pcontrolDc30,main="Zone de contrôle");plot(pcontrolGc30,main="")
title("Carré de 30x30 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testc30


plot(tableGrob(testc30))


par(mfrow = c(1,1))
#Carré -10,10 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDc20 = ppp(pouceD[,17],pouceD[,18], c(-10,10), c(-10,10))              #1780 points en dehors /8782 - 7002 || T : R = 0.89803, p-value < 2.2e-16
ppouceGc20 = ppp(pouceG[,17],pouceG[,18], c(-10,10), c(-10,10))              #1883 points en dehors /8947 - 7064 || T : R = 0.91818, p-value < 2.2e-16
ppropreDc20 = ppp(propreD[,17],propreD[,18], c(-10,10), c(-10,10))           #2127 points en dehors /9244 - 7117 || T : R = 0.96566, p-value = 2.983e-08
ppropreGc20 = ppp(propreG[,17],propreG[,18], c(-10,10), c(-10,10))           #1989 points en dehors /8935 - 6946 || T : R = 0.97392, p-value = 3.213e-05
pcontrolDc20 = ppp(controlD[,17],controlD[,18], c(-10,10), c(-10,10))        #1842 points en dehors /6282 - 4440 || T : R = 0.99693, p-value = 0.6953
pcontrolGc20 = ppp(controlG[,17],controlG[,18], c(-10,10), c(-10,10))        #1748 points en dehors /6244 - 4496 || T : R = 1.0046, p-value = 0.5515

testc20=matrix(nrow=6,ncol=10);rownames(testc20)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testc20)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testc20[1,1]=nrow(pouceD);testc20[2,1]=nrow(pouceG);testc20[3,1]=nrow(propreD);testc20[4,1]=nrow(propreG);testc20[5,1]=nrow(controlD);testc20[6,1]=nrow(controlG)
testc20[1,2]=nrow(as.data.frame(ppouceDc20));testc20[2,2]=nrow(as.data.frame(ppouceGc20));testc20[3,2]=nrow(as.data.frame(ppropreDc20))
testc20[4,2]=nrow(as.data.frame(ppropreGc20));testc20[5,2]=nrow(as.data.frame(pcontrolDc20));testc20[6,2]=nrow(as.data.frame(pcontrolGc20))


testc20[1,3]=round(clarkevans.test(ppouceDc20)$statistic,4) ; testc20[2,3]=round(clarkevans.test(ppouceGc20)$statistic,4)
testc20[3,3]=round(clarkevans.test(ppropreDc20)$statistic,4) ; testc20[4,3]=round(clarkevans.test(ppropreGc20)$statistic,4)
testc20[5,3]=round(clarkevans.test(pcontrolDc20)$statistic,4) ; testc20[6,3]=round(clarkevans.test(pcontrolGc20)$statistic,4)

testc20[1,4]=round(clarkevans.test(ppouceDc20)$p.value,4) ; testc20[2,4]=round(clarkevans.test(ppouceGc20)$p.value,4)
testc20[3,4]=round(clarkevans.test(ppropreDc20)$p.value,4) ; testc20[4,4]=round(clarkevans.test(ppropreGc20)$p.value,4)
testc20[5,4]=round(clarkevans.test(pcontrolDc20)$p.value,4) ; testc20[6,4]=round(clarkevans.test(pcontrolGc20)$p.value,4)

testc20[1,5]=round(hopskel.test(ppouceDc20)$statistic,5) ; testc20[2,5]=round(hopskel.test(ppouceGc20)$statistic,5) ; testc20[3,5]=round(hopskel.test(ppropreDc20)$statistic,4)
testc20[4,5]=round(hopskel.test(ppropreGc20)$statistic,5) ; testc20[5,5]=round(hopskel.test(pcontrolDc20)$statistic,5) ; testc20[6,5]=round(hopskel.test(pcontrolGc20)$statistic,4)

testc20[1,6]=round(hopskel.test(ppouceDc20)$p.value,4) ; testc20[2,6]=round(hopskel.test(ppouceGc20)$p.value,4) ; testc20[3,6]=round(hopskel.test(ppropreDc20)$p.value,4)
testc20[4,6]=round(hopskel.test(ppropreGc20)$p.value,4) ; testc20[5,6]=round(hopskel.test(pcontrolDc20)$p.value,4) ; testc20[6,6]=round(hopskel.test(pcontrolGc20)$p.value,4)

testc20[1,7]=round(hopskel.test(ppouceDc20,method="asymptotic")$statistic,4) ; testc20[2,7]=round(hopskel.test(ppouceGc20,method="asymptotic")$statistic,4)
testc20[3,7]=round(hopskel.test(ppropreDc20,method="asymptotic")$statistic,4) ; testc20[4,7]=round(hopskel.test(ppropreGc20,method="asymptotic")$statistic,4)
testc20[5,7]=round(hopskel.test(pcontrolDc20,method="asymptotic")$statistic,4) ; testc20[6,7]=round(hopskel.test(pcontrolGc20,method="asymptotic")$statistic,4)

testc20[1,8]=round(hopskel.test(ppouceDc20,method="asymptotic")$p.value,4) ; testc20[2,8]=round(hopskel.test(ppouceGc20,method="asymptotic")$p.value,4)
testc20[3,8]=round(hopskel.test(ppropreDc20,method="asymptotic")$p.value,4) ; testc20[4,8]=round(hopskel.test(ppropreGc20,method="asymptotic")$p.value,4)
testc20[5,8]=round(hopskel.test(pcontrolDc20,method="asymptotic")$p.value,4) ; testc20[6,8]=round(hopskel.test(pcontrolGc20,method="asymptotic")$p.value,4)

testc20[1,9]=round(hopskel.test(ppouceDc20,method="MonteCarlo")$statistic,4) ; testc20[2,9]=round(hopskel.test(ppouceGc20,method="MonteCarlo")$statistic,4)
testc20[3,9]=round(hopskel.test(ppropreDc20,method="MonteCarlo")$statistic,4) ; testc20[4,9]=round(hopskel.test(ppropreGc20,method="MonteCarlo")$statistic,4)
testc20[5,9]=round(hopskel.test(pcontrolDc20,method="MonteCarlo")$statistic,4) ; testc20[6,9]=round(hopskel.test(pcontrolGc20,method="MonteCarlo")$statistic,4)

testc20[1,10]=round(hopskel.test(ppouceDc20,method="MonteCarlo")$p.value,4) ; testc20[2,10]=round(hopskel.test(ppouceGc20,method="MonteCarlo")$p.value,4)
testc20[3,10]=round(hopskel.test(ppropreDc20,method="MonteCarlo")$p.value,4) ; testc20[4,10]=round(hopskel.test(ppropreGc20,method="MonteCarlo")$p.value,4)
testc20[5,10]=round(hopskel.test(pcontrolDc20,method="MonteCarlo")$p.value,4) ; testc20[6,10]=round(hopskel.test(pcontrolGc20,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDc20,main="Delta de pouce",cex.main=1);plot(ppouceGc20,main="")
plot(ppropreDc20,main="Delta propre");plot(ppropreGc20,main="")
plot(pcontrolDc20,main="Zone de contrôle");plot(pcontrolGc20,main="")
title("Carré de 20x20 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testc20


plot(tableGrob(testc20))

par(mfrow = c(1,1))
#Carré -7.5,7.5 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDc15 = ppp(pouceD[,17],pouceD[,18], c(-7.5,7.5), c(-7.5,7.5))          #3810 points en dehors /8782 - 4972 || T : R = 0.95386, p-value = 4.837e-10
ppouceGc15 = ppp(pouceG[,17],pouceG[,18], c(-7.5,7.5), c(-7.5,7.5))          #3953 points en dehors /8947 - 4994 || T : R = 0.95884, p-value = 2.628e-08
ppropreDc15 = ppp(propreD[,17],propreD[,18], c(-7.5,7.5), c(-7.5,7.5))       #4473 points en dehors /9244 - 4771 || T : R = 0.9732, p-value = 0.0003978
ppropreGc15 = ppp(propreG[,17],propreG[,18], c(-7.5,7.5), c(-7.5,7.5))       #4217 points en dehors /8935 - 4718 || T : R = 0.97515, p-value = 0.001091
pcontrolDc15 = ppp(controlD[,17],controlD[,18], c(-7.5,7.5), c(-7.5,7.5))    #3675 points en dehors /6282 - 2607 || T : R = 0.99799, p-value = 0.8443
pcontrolGc15 = ppp(controlG[,17],controlG[,18], c(-7.5,7.5), c(-7.5,7.5))    #3685 points en dehors /6244 - 2559 || T : R = 1.0225, p-value = 0.02967

testc15=matrix(nrow=6,ncol=10);rownames(testc15)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testc15)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testc15[1,1]=nrow(pouceD);testc15[2,1]=nrow(pouceG);testc15[3,1]=nrow(propreD);testc15[4,1]=nrow(propreG);testc15[5,1]=nrow(controlD);testc15[6,1]=nrow(controlG)
testc15[1,2]=nrow(as.data.frame(ppouceDc15));testc15[2,2]=nrow(as.data.frame(ppouceGc15));testc15[3,2]=nrow(as.data.frame(ppropreDc15))
testc15[4,2]=nrow(as.data.frame(ppropreGc15));testc15[5,2]=nrow(as.data.frame(pcontrolDc15));testc15[6,2]=nrow(as.data.frame(pcontrolGc15))


testc15[1,3]=round(clarkevans.test(ppouceDc15)$statistic,4) ; testc15[2,3]=round(clarkevans.test(ppouceGc15)$statistic,4)
testc15[3,3]=round(clarkevans.test(ppropreDc15)$statistic,4) ; testc15[4,3]=round(clarkevans.test(ppropreGc15)$statistic,4)
testc15[5,3]=round(clarkevans.test(pcontrolDc15)$statistic,4) ; testc15[6,3]=round(clarkevans.test(pcontrolGc15)$statistic,4)

testc15[1,4]=round(clarkevans.test(ppouceDc15)$p.value,4) ; testc15[2,4]=round(clarkevans.test(ppouceGc15)$p.value,4)
testc15[3,4]=round(clarkevans.test(ppropreDc15)$p.value,4) ; testc15[4,4]=round(clarkevans.test(ppropreGc15)$p.value,4)
testc15[5,4]=round(clarkevans.test(pcontrolDc15)$p.value,4) ; testc15[6,4]=round(clarkevans.test(pcontrolGc15)$p.value,4)

testc15[1,5]=round(hopskel.test(ppouceDc15)$statistic,5) ; testc15[2,5]=round(hopskel.test(ppouceGc15)$statistic,5) ; testc15[3,5]=round(hopskel.test(ppropreDc15)$statistic,4)
testc15[4,5]=round(hopskel.test(ppropreGc15)$statistic,5) ; testc15[5,5]=round(hopskel.test(pcontrolDc15)$statistic,5) ; testc15[6,5]=round(hopskel.test(pcontrolGc15)$statistic,4)

testc15[1,6]=round(hopskel.test(ppouceDc15)$p.value,4) ; testc15[2,6]=round(hopskel.test(ppouceGc15)$p.value,4) ; testc15[3,6]=round(hopskel.test(ppropreDc15)$p.value,4)
testc15[4,6]=round(hopskel.test(ppropreGc15)$p.value,4) ; testc15[5,6]=round(hopskel.test(pcontrolDc15)$p.value,4) ; testc15[6,6]=round(hopskel.test(pcontrolGc15)$p.value,4)

testc15[1,7]=round(hopskel.test(ppouceDc15,method="asymptotic")$statistic,4) ; testc15[2,7]=round(hopskel.test(ppouceGc15,method="asymptotic")$statistic,4)
testc15[3,7]=round(hopskel.test(ppropreDc15,method="asymptotic")$statistic,4) ; testc15[4,7]=round(hopskel.test(ppropreGc15,method="asymptotic")$statistic,4)
testc15[5,7]=round(hopskel.test(pcontrolDc15,method="asymptotic")$statistic,4) ; testc15[6,7]=round(hopskel.test(pcontrolGc15,method="asymptotic")$statistic,4)

testc15[1,8]=round(hopskel.test(ppouceDc15,method="asymptotic")$p.value,4) ; testc15[2,8]=round(hopskel.test(ppouceGc15,method="asymptotic")$p.value,4)
testc15[3,8]=round(hopskel.test(ppropreDc15,method="asymptotic")$p.value,4) ; testc15[4,8]=round(hopskel.test(ppropreGc15,method="asymptotic")$p.value,4)
testc15[5,8]=round(hopskel.test(pcontrolDc15,method="asymptotic")$p.value,4) ; testc15[6,8]=round(hopskel.test(pcontrolGc15,method="asymptotic")$p.value,4)

testc15[1,9]=round(hopskel.test(ppouceDc15,method="MonteCarlo")$statistic,4) ; testc15[2,9]=round(hopskel.test(ppouceGc15,method="MonteCarlo")$statistic,4)
testc15[3,9]=round(hopskel.test(ppropreDc15,method="MonteCarlo")$statistic,4) ; testc15[4,9]=round(hopskel.test(ppropreGc15,method="MonteCarlo")$statistic,4)
testc15[5,9]=round(hopskel.test(pcontrolDc15,method="MonteCarlo")$statistic,4) ; testc15[6,9]=round(hopskel.test(pcontrolGc15,method="MonteCarlo")$statistic,4)

testc15[1,10]=round(hopskel.test(ppouceDc15,method="MonteCarlo")$p.value,4) ; testc15[2,10]=round(hopskel.test(ppouceGc15,method="MonteCarlo")$p.value,4)
testc15[3,10]=round(hopskel.test(ppropreDc15,method="MonteCarlo")$p.value,4) ; testc15[4,10]=round(hopskel.test(ppropreGc15,method="MonteCarlo")$p.value,4)
testc15[5,10]=round(hopskel.test(pcontrolDc15,method="MonteCarlo")$p.value,4) ; testc15[6,10]=round(hopskel.test(pcontrolGc15,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDc15,main="Delta de pouce",cex.main=1);plot(ppouceGc15,main="")
plot(ppropreDc15,main="Delta propre");plot(ppropreGc15,main="")
plot(pcontrolDc15,main="Zone de contrôle");plot(pcontrolGc15,main="")
title("Carré de 15x15 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testc15


plot(tableGrob(testc15))

par(mfrow = c(1,1))
#Carré -5,5 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDc10 = ppp(pouceD[,17],pouceD[,18], c(-5,5), c(-5,5))                  #5897 points en dehors /8782 - 2885 || T : 
ppouceGc10 = ppp(pouceG[,17],pouceG[,18], c(-5,5), c(-5,5))                  #6042 points en dehors /8947 - 2905 || T : 
ppropreDc10 = ppp(propreD[,17],propreD[,18], c(-5,5), c(-5,5))               #6660 points en dehors /9244 - 2584 || T : 
ppropreGc10 = ppp(propreG[,17],propreG[,18], c(-5,5), c(-5,5))               #6360 points en dehors /8935 - 2575 || T : 
pcontrolDc10 = ppp(controlD[,17],controlD[,18], c(-5,5), c(-5,5))            #5108 points en dehors /6282 - 1174 || T : 
pcontrolGc10 = ppp(controlG[,17],controlG[,18], c(-5,5), c(-5,5))            #5095 points en dehors /6244 - 1149 || T : 

testc10=matrix(nrow=6,ncol=10);rownames(testc10)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testc10)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testc10[1,1]=nrow(pouceD);testc10[2,1]=nrow(pouceG);testc10[3,1]=nrow(propreD);testc10[4,1]=nrow(propreG);testc10[5,1]=nrow(controlD);testc10[6,1]=nrow(controlG)
testc10[1,2]=nrow(as.data.frame(ppouceDc10));testc10[2,2]=nrow(as.data.frame(ppouceGc10));testc10[3,2]=nrow(as.data.frame(ppropreDc10))
testc10[4,2]=nrow(as.data.frame(ppropreGc10));testc10[5,2]=nrow(as.data.frame(pcontrolDc10));testc10[6,2]=nrow(as.data.frame(pcontrolGc10))


testc10[1,3]=round(clarkevans.test(ppouceDc10)$statistic,4) ; testc10[2,3]=round(clarkevans.test(ppouceGc10)$statistic,4)
testc10[3,3]=round(clarkevans.test(ppropreDc10)$statistic,4) ; testc10[4,3]=round(clarkevans.test(ppropreGc10)$statistic,4)
testc10[5,3]=round(clarkevans.test(pcontrolDc10)$statistic,4) ; testc10[6,3]=round(clarkevans.test(pcontrolGc10)$statistic,4)

testc10[1,4]=round(clarkevans.test(ppouceDc10)$p.value,4) ; testc10[2,4]=round(clarkevans.test(ppouceGc10)$p.value,4)
testc10[3,4]=round(clarkevans.test(ppropreDc10)$p.value,4) ; testc10[4,4]=round(clarkevans.test(ppropreGc10)$p.value,4)
testc10[5,4]=round(clarkevans.test(pcontrolDc10)$p.value,4) ; testc10[6,4]=round(clarkevans.test(pcontrolGc10)$p.value,4)

testc10[1,5]=round(hopskel.test(ppouceDc10)$statistic,5) ; testc10[2,5]=round(hopskel.test(ppouceGc10)$statistic,5) ; testc10[3,5]=round(hopskel.test(ppropreDc10)$statistic,4)
testc10[4,5]=round(hopskel.test(ppropreGc10)$statistic,5) ; testc10[5,5]=round(hopskel.test(pcontrolDc10)$statistic,5) ; testc10[6,5]=round(hopskel.test(pcontrolGc10)$statistic,4)

testc10[1,6]=round(hopskel.test(ppouceDc10)$p.value,4) ; testc10[2,6]=round(hopskel.test(ppouceGc10)$p.value,4) ; testc10[3,6]=round(hopskel.test(ppropreDc10)$p.value,4)
testc10[4,6]=round(hopskel.test(ppropreGc10)$p.value,4) ; testc10[5,6]=round(hopskel.test(pcontrolDc10)$p.value,4) ; testc10[6,6]=round(hopskel.test(pcontrolGc10)$p.value,4)

testc10[1,7]=round(hopskel.test(ppouceDc10,method="asymptotic")$statistic,4) ; testc10[2,7]=round(hopskel.test(ppouceGc10,method="asymptotic")$statistic,4)
testc10[3,7]=round(hopskel.test(ppropreDc10,method="asymptotic")$statistic,4) ; testc10[4,7]=round(hopskel.test(ppropreGc10,method="asymptotic")$statistic,4)
testc10[5,7]=round(hopskel.test(pcontrolDc10,method="asymptotic")$statistic,4) ; testc10[6,7]=round(hopskel.test(pcontrolGc10,method="asymptotic")$statistic,4)

testc10[1,8]=round(hopskel.test(ppouceDc10,method="asymptotic")$p.value,4) ; testc10[2,8]=round(hopskel.test(ppouceGc10,method="asymptotic")$p.value,4)
testc10[3,8]=round(hopskel.test(ppropreDc10,method="asymptotic")$p.value,4) ; testc10[4,8]=round(hopskel.test(ppropreGc10,method="asymptotic")$p.value,4)
testc10[5,8]=round(hopskel.test(pcontrolDc10,method="asymptotic")$p.value,4) ; testc10[6,8]=round(hopskel.test(pcontrolGc10,method="asymptotic")$p.value,4)

testc10[1,9]=round(hopskel.test(ppouceDc10,method="MonteCarlo")$statistic,4) ; testc10[2,9]=round(hopskel.test(ppouceGc10,method="MonteCarlo")$statistic,4)
testc10[3,9]=round(hopskel.test(ppropreDc10,method="MonteCarlo")$statistic,4) ; testc10[4,9]=round(hopskel.test(ppropreGc10,method="MonteCarlo")$statistic,4)
testc10[5,9]=round(hopskel.test(pcontrolDc10,method="MonteCarlo")$statistic,4) ; testc10[6,9]=round(hopskel.test(pcontrolGc10,method="MonteCarlo")$statistic,4)

testc10[1,10]=round(hopskel.test(ppouceDc10,method="MonteCarlo")$p.value,4) ; testc10[2,10]=round(hopskel.test(ppouceGc10,method="MonteCarlo")$p.value,4)
testc10[3,10]=round(hopskel.test(ppropreDc10,method="MonteCarlo")$p.value,4) ; testc10[4,10]=round(hopskel.test(ppropreGc10,method="MonteCarlo")$p.value,4)
testc10[5,10]=round(hopskel.test(pcontrolDc10,method="MonteCarlo")$p.value,4) ; testc10[6,10]=round(hopskel.test(pcontrolGc10,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDc10,main="Delta de pouce",cex.main=1);plot(ppouceGc10,main="")
plot(ppropreDc10,main="Delta propre");plot(ppropreGc10,main="")
plot(pcontrolDc10,main="Zone de contrôle");plot(pcontrolGc10,main="")
title("Carré de 10x10 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testc10


plot(tableGrob(testc10))
par(mfrow = c(1,1))
#Disque 30 ------------------------------------------------------------------------------------------------------------------------------------
ppouceDd30 = ppp(pouceD[,17],pouceD[,18], disc(15))                          #150 points en dehors /8782 - 8632 || T : 
ppouceGd30 = ppp(pouceG[,17],pouceG[,18], disc(15))                          #109 points en dehors /8947 - 8838 || T : 
ppropreDd30 = ppp(propreD[,17],propreD[,18], disc(15))                       #188 points en dehors /9244 - 9056 || T : 
ppropreGd30 = ppp(propreG[,17],propreG[,18], disc(15))                       #131 points en dehors /8935 - 8804 || T : 
pcontrolDd30 = ppp(controlD[,17],controlD[,18], disc(15))                    #128 points en dehors /6282 - 6154 || T : 
pcontrolGd30 = ppp(controlG[,17],controlG[,18], disc(15))                    #120 points en dehors /6244 - 6124 || T : 

testd30=matrix(nrow=6,ncol=10);rownames(testd30)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testd30)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testd30[1,1]=nrow(pouceD);testd30[2,1]=nrow(pouceG);testd30[3,1]=nrow(propreD);testd30[4,1]=nrow(propreG);testd30[5,1]=nrow(controlD);testd30[6,1]=nrow(controlG)
testd30[1,2]=nrow(as.data.frame(ppouceDd30));testd30[2,2]=nrow(as.data.frame(ppouceGd30));testd30[3,2]=nrow(as.data.frame(ppropreDd30))
testd30[4,2]=nrow(as.data.frame(ppropreGd30));testd30[5,2]=nrow(as.data.frame(pcontrolDd30));testd30[6,2]=nrow(as.data.frame(pcontrolGd30))


testd30[1,3]=round(clarkevans.test(ppouceDd30)$statistic,4) ; testd30[2,3]=round(clarkevans.test(ppouceGd30)$statistic,4)
testd30[3,3]=round(clarkevans.test(ppropreDd30)$statistic,4) ; testd30[4,3]=round(clarkevans.test(ppropreGd30)$statistic,4)
testd30[5,3]=round(clarkevans.test(pcontrolDd30)$statistic,4) ; testd30[6,3]=round(clarkevans.test(pcontrolGd30)$statistic,4)

testd30[1,4]=round(clarkevans.test(ppouceDd30)$p.value,4) ; testd30[2,4]=round(clarkevans.test(ppouceGd30)$p.value,4)
testd30[3,4]=round(clarkevans.test(ppropreDd30)$p.value,4) ; testd30[4,4]=round(clarkevans.test(ppropreGd30)$p.value,4)
testd30[5,4]=round(clarkevans.test(pcontrolDd30)$p.value,4) ; testd30[6,4]=round(clarkevans.test(pcontrolGd30)$p.value,4)

testd30[1,5]=round(hopskel.test(ppouceDd30)$statistic,5) ; testd30[2,5]=round(hopskel.test(ppouceGd30)$statistic,5) ; testd30[3,5]=round(hopskel.test(ppropreDd30)$statistic,4)
testd30[4,5]=round(hopskel.test(ppropreGd30)$statistic,5) ; testd30[5,5]=round(hopskel.test(pcontrolDd30)$statistic,5) ; testd30[6,5]=round(hopskel.test(pcontrolGd30)$statistic,4)

testd30[1,6]=round(hopskel.test(ppouceDd30)$p.value,4) ; testd30[2,6]=round(hopskel.test(ppouceGd30)$p.value,4) ; testd30[3,6]=round(hopskel.test(ppropreDd30)$p.value,4)
testd30[4,6]=round(hopskel.test(ppropreGd30)$p.value,4) ; testd30[5,6]=round(hopskel.test(pcontrolDd30)$p.value,4) ; testd30[6,6]=round(hopskel.test(pcontrolGd30)$p.value,4)

testd30[1,7]=round(hopskel.test(ppouceDd30,method="asymptotic")$statistic,4) ; testd30[2,7]=round(hopskel.test(ppouceGd30,method="asymptotic")$statistic,4)
testd30[3,7]=round(hopskel.test(ppropreDd30,method="asymptotic")$statistic,4) ; testd30[4,7]=round(hopskel.test(ppropreGd30,method="asymptotic")$statistic,4)
testd30[5,7]=round(hopskel.test(pcontrolDd30,method="asymptotic")$statistic,4) ; testd30[6,7]=round(hopskel.test(pcontrolGd30,method="asymptotic")$statistic,4)

testd30[1,8]=round(hopskel.test(ppouceDd30,method="asymptotic")$p.value,4) ; testd30[2,8]=round(hopskel.test(ppouceGd30,method="asymptotic")$p.value,4)
testd30[3,8]=round(hopskel.test(ppropreDd30,method="asymptotic")$p.value,4) ; testd30[4,8]=round(hopskel.test(ppropreGd30,method="asymptotic")$p.value,4)
testd30[5,8]=round(hopskel.test(pcontrolDd30,method="asymptotic")$p.value,4) ; testd30[6,8]=round(hopskel.test(pcontrolGd30,method="asymptotic")$p.value,4)

testd30[1,9]=round(hopskel.test(ppouceDd30,method="MonteCarlo")$statistic,4) ; testd30[2,9]=round(hopskel.test(ppouceGd30,method="MonteCarlo")$statistic,4)
testd30[3,9]=round(hopskel.test(ppropreDd30,method="MonteCarlo")$statistic,4) ; testd30[4,9]=round(hopskel.test(ppropreGd30,method="MonteCarlo")$statistic,4)
testd30[5,9]=round(hopskel.test(pcontrolDd30,method="MonteCarlo")$statistic,4) ; testd30[6,9]=round(hopskel.test(pcontrolGd30,method="MonteCarlo")$statistic,4)

testd30[1,10]=round(hopskel.test(ppouceDd30,method="MonteCarlo")$p.value,4) ; testd30[2,10]=round(hopskel.test(ppouceGd30,method="MonteCarlo")$p.value,4)
testd30[3,10]=round(hopskel.test(ppropreDd30,method="MonteCarlo")$p.value,4) ; testd30[4,10]=round(hopskel.test(ppropreGd30,method="MonteCarlo")$p.value,4)
testd30[5,10]=round(hopskel.test(pcontrolDd30,method="MonteCarlo")$p.value,4) ; testd30[6,10]=round(hopskel.test(pcontrolGd30,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDd30,main="Delta de pouce",cex.main=1);plot(ppouceGd30,main="")
plot(ppropreDd30,main="Delta propre");plot(ppropreGd30,main="")
plot(pcontrolDd30,main="Zone de contrôle");plot(pcontrolGd30,main="")
title("Disque de diamètre 30 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testd30


plot(tableGrob(testd30))

par(mfrow = c(1,1))
#Disque 20 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDd20 = ppp(pouceD[,17],pouceD[,18], disc(10))                          #2624 points en dehors /8782 - 6158 || T : 
ppouceGd20 = ppp(pouceG[,17],pouceG[,18], disc(10))                          #2691 points en dehors /8947 - 6256 || T : 
ppropreDd20 = ppp(propreD[,17],propreD[,18], disc(10))                       #3083 points en dehors /9244 - 6161 || T : 
ppropreGd20 = ppp(propreG[,17],propreG[,18], disc(10))                       #2835 points en dehors /8935 - 6100 || T : 
pcontrolDd20 = ppp(controlD[,17],controlD[,18], disc(10))                    #2746 points en dehors /6282 - 3536 || T : 
pcontrolGd20 = ppp(controlG[,17],controlG[,18], disc(10))                    #2665 points en dehors /6244 - 3579 || T : 

testd20=matrix(nrow=6,ncol=10);rownames(testd20)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testd20)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testd20[1,1]=nrow(pouceD);testd20[2,1]=nrow(pouceG);testd20[3,1]=nrow(propreD);testd20[4,1]=nrow(propreG);testd20[5,1]=nrow(controlD);testd20[6,1]=nrow(controlG)
testd20[1,2]=nrow(as.data.frame(ppouceDd20));testd20[2,2]=nrow(as.data.frame(ppouceGd20));testd20[3,2]=nrow(as.data.frame(ppropreDd20))
testd20[4,2]=nrow(as.data.frame(ppropreGd20));testd20[5,2]=nrow(as.data.frame(pcontrolDd20));testd20[6,2]=nrow(as.data.frame(pcontrolGd20))


testd20[1,3]=round(clarkevans.test(ppouceDd20)$statistic,4) ; testd20[2,3]=round(clarkevans.test(ppouceGd20)$statistic,4)
testd20[3,3]=round(clarkevans.test(ppropreDd20)$statistic,4) ; testd20[4,3]=round(clarkevans.test(ppropreGd20)$statistic,4)
testd20[5,3]=round(clarkevans.test(pcontrolDd20)$statistic,4) ; testd20[6,3]=round(clarkevans.test(pcontrolGd20)$statistic,4)

testd20[1,4]=round(clarkevans.test(ppouceDd20)$p.value,4) ; testd20[2,4]=round(clarkevans.test(ppouceGd20)$p.value,4)
testd20[3,4]=round(clarkevans.test(ppropreDd20)$p.value,4) ; testd20[4,4]=round(clarkevans.test(ppropreGd20)$p.value,4)
testd20[5,4]=round(clarkevans.test(pcontrolDd20)$p.value,4) ; testd20[6,4]=round(clarkevans.test(pcontrolGd20)$p.value,4)

testd20[1,5]=round(hopskel.test(ppouceDd20)$statistic,5) ; testd20[2,5]=round(hopskel.test(ppouceGd20)$statistic,5) ; testd20[3,5]=round(hopskel.test(ppropreDd20)$statistic,4)
testd20[4,5]=round(hopskel.test(ppropreGd20)$statistic,5) ; testd20[5,5]=round(hopskel.test(pcontrolDd20)$statistic,5) ; testd20[6,5]=round(hopskel.test(pcontrolGd20)$statistic,4)

testd20[1,6]=round(hopskel.test(ppouceDd20)$p.value,4) ; testd20[2,6]=round(hopskel.test(ppouceGd20)$p.value,4) ; testd20[3,6]=round(hopskel.test(ppropreDd20)$p.value,4)
testd20[4,6]=round(hopskel.test(ppropreGd20)$p.value,4) ; testd20[5,6]=round(hopskel.test(pcontrolDd20)$p.value,4) ; testd20[6,6]=round(hopskel.test(pcontrolGd20)$p.value,4)

testd20[1,7]=round(hopskel.test(ppouceDd20,method="asymptotic")$statistic,4) ; testd20[2,7]=round(hopskel.test(ppouceGd20,method="asymptotic")$statistic,4)
testd20[3,7]=round(hopskel.test(ppropreDd20,method="asymptotic")$statistic,4) ; testd20[4,7]=round(hopskel.test(ppropreGd20,method="asymptotic")$statistic,4)
testd20[5,7]=round(hopskel.test(pcontrolDd20,method="asymptotic")$statistic,4) ; testd20[6,7]=round(hopskel.test(pcontrolGd20,method="asymptotic")$statistic,4)

testd20[1,8]=round(hopskel.test(ppouceDd20,method="asymptotic")$p.value,4) ; testd20[2,8]=round(hopskel.test(ppouceGd20,method="asymptotic")$p.value,4)
testd20[3,8]=round(hopskel.test(ppropreDd20,method="asymptotic")$p.value,4) ; testd20[4,8]=round(hopskel.test(ppropreGd20,method="asymptotic")$p.value,4)
testd20[5,8]=round(hopskel.test(pcontrolDd20,method="asymptotic")$p.value,4) ; testd20[6,8]=round(hopskel.test(pcontrolGd20,method="asymptotic")$p.value,4)

testd20[1,9]=round(hopskel.test(ppouceDd20,method="MonteCarlo")$statistic,4) ; testd20[2,9]=round(hopskel.test(ppouceGd20,method="MonteCarlo")$statistic,4)
testd20[3,9]=round(hopskel.test(ppropreDd20,method="MonteCarlo")$statistic,4) ; testd20[4,9]=round(hopskel.test(ppropreGd20,method="MonteCarlo")$statistic,4)
testd20[5,9]=round(hopskel.test(pcontrolDd20,method="MonteCarlo")$statistic,4) ; testd20[6,9]=round(hopskel.test(pcontrolGd20,method="MonteCarlo")$statistic,4)

testd20[1,10]=round(hopskel.test(ppouceDd20,method="MonteCarlo")$p.value,4) ; testd20[2,10]=round(hopskel.test(ppouceGd20,method="MonteCarlo")$p.value,4)
testd20[3,10]=round(hopskel.test(ppropreDd20,method="MonteCarlo")$p.value,4) ; testd20[4,10]=round(hopskel.test(ppropreGd20,method="MonteCarlo")$p.value,4)
testd20[5,10]=round(hopskel.test(pcontrolDd20,method="MonteCarlo")$p.value,4) ; testd20[6,10]=round(hopskel.test(pcontrolGd20,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDd20,main="Delta de pouce",cex.main=1);plot(ppouceGd20,main="")
plot(ppropreDd20,main="Delta propre");plot(ppropreGd20,main="")
plot(pcontrolDd20,main="Zone de contrôle");plot(pcontrolGd20,main="")
title("Disque de diamètre 20 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testd20


plot(tableGrob(testd20))

par(mfrow = c(1,1))
#Disuqe 15 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDd15 = ppp(pouceD[,17],pouceD[,18], disc(7.5))                          #4447 points en dehors /8782 - 4335 || T : 
ppouceGd15 = ppp(pouceG[,17],pouceG[,18], disc(7.5))                          #4548 points en dehors /8947 - 4399 || T : 
ppropreDd15 = ppp(propreD[,17],propreD[,18], disc(7.5))                       #5208 points en dehors /9244 - 4036 || T : 
ppropreGd15 = ppp(propreG[,17],propreG[,18], disc(7.5))                       #4925 points en dehors /8935 - 4010 || T : 
pcontrolDd15 = ppp(controlD[,17],controlD[,18], disc(7.5))                    #4217 points en dehors /6282 - 2065 || T : 
pcontrolGd15 = ppp(controlG[,17],controlG[,18], disc(7.5))                    #4192 points en dehors /6244 - 2052 || T : 

testd15=matrix(nrow=6,ncol=10);rownames(testd15)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testd15)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testd15[1,1]=nrow(pouceD);testd15[2,1]=nrow(pouceG);testd15[3,1]=nrow(propreD);testd15[4,1]=nrow(propreG);testd15[5,1]=nrow(controlD);testd15[6,1]=nrow(controlG)
testd15[1,2]=nrow(as.data.frame(ppouceDd15));testd15[2,2]=nrow(as.data.frame(ppouceGd15));testd15[3,2]=nrow(as.data.frame(ppropreDd15))
testd15[4,2]=nrow(as.data.frame(ppropreGd15));testd15[5,2]=nrow(as.data.frame(pcontrolDd15));testd15[6,2]=nrow(as.data.frame(pcontrolGd15))


testd15[1,3]=round(clarkevans.test(ppouceDd15)$statistic,4) ; testd15[2,3]=round(clarkevans.test(ppouceGd15)$statistic,4)
testd15[3,3]=round(clarkevans.test(ppropreDd15)$statistic,4) ; testd15[4,3]=round(clarkevans.test(ppropreGd15)$statistic,4)
testd15[5,3]=round(clarkevans.test(pcontrolDd15)$statistic,4) ; testd15[6,3]=round(clarkevans.test(pcontrolGd15)$statistic,4)

testd15[1,4]=round(clarkevans.test(ppouceDd15)$p.value,4) ; testd15[2,4]=round(clarkevans.test(ppouceGd15)$p.value,4)
testd15[3,4]=round(clarkevans.test(ppropreDd15)$p.value,4) ; testd15[4,4]=round(clarkevans.test(ppropreGd15)$p.value,4)
testd15[5,4]=round(clarkevans.test(pcontrolDd15)$p.value,4) ; testd15[6,4]=round(clarkevans.test(pcontrolGd15)$p.value,4)

testd15[1,5]=round(hopskel.test(ppouceDd15)$statistic,5) ; testd15[2,5]=round(hopskel.test(ppouceGd15)$statistic,5) ; testd15[3,5]=round(hopskel.test(ppropreDd15)$statistic,4)
testd15[4,5]=round(hopskel.test(ppropreGd15)$statistic,5) ; testd15[5,5]=round(hopskel.test(pcontrolDd15)$statistic,5) ; testd15[6,5]=round(hopskel.test(pcontrolGd15)$statistic,4)

testd15[1,6]=round(hopskel.test(ppouceDd15)$p.value,4) ; testd15[2,6]=round(hopskel.test(ppouceGd15)$p.value,4) ; testd15[3,6]=round(hopskel.test(ppropreDd15)$p.value,4)
testd15[4,6]=round(hopskel.test(ppropreGd15)$p.value,4) ; testd15[5,6]=round(hopskel.test(pcontrolDd15)$p.value,4) ; testd15[6,6]=round(hopskel.test(pcontrolGd15)$p.value,4)

testd15[1,7]=round(hopskel.test(ppouceDd15,method="asymptotic")$statistic,4) ; testd15[2,7]=round(hopskel.test(ppouceGd15,method="asymptotic")$statistic,4)
testd15[3,7]=round(hopskel.test(ppropreDd15,method="asymptotic")$statistic,4) ; testd15[4,7]=round(hopskel.test(ppropreGd15,method="asymptotic")$statistic,4)
testd15[5,7]=round(hopskel.test(pcontrolDd15,method="asymptotic")$statistic,4) ; testd15[6,7]=round(hopskel.test(pcontrolGd15,method="asymptotic")$statistic,4)

testd15[1,8]=round(hopskel.test(ppouceDd15,method="asymptotic")$p.value,4) ; testd15[2,8]=round(hopskel.test(ppouceGd15,method="asymptotic")$p.value,4)
testd15[3,8]=round(hopskel.test(ppropreDd15,method="asymptotic")$p.value,4) ; testd15[4,8]=round(hopskel.test(ppropreGd15,method="asymptotic")$p.value,4)
testd15[5,8]=round(hopskel.test(pcontrolDd15,method="asymptotic")$p.value,4) ; testd15[6,8]=round(hopskel.test(pcontrolGd15,method="asymptotic")$p.value,4)

testd15[1,9]=round(hopskel.test(ppouceDd15,method="MonteCarlo")$statistic,4) ; testd15[2,9]=round(hopskel.test(ppouceGd15,method="MonteCarlo")$statistic,4)
testd15[3,9]=round(hopskel.test(ppropreDd15,method="MonteCarlo")$statistic,4) ; testd15[4,9]=round(hopskel.test(ppropreGd15,method="MonteCarlo")$statistic,4)
testd15[5,9]=round(hopskel.test(pcontrolDd15,method="MonteCarlo")$statistic,4) ; testd15[6,9]=round(hopskel.test(pcontrolGd15,method="MonteCarlo")$statistic,4)

testd15[1,10]=round(hopskel.test(ppouceDd15,method="MonteCarlo")$p.value,4) ; testd15[2,10]=round(hopskel.test(ppouceGd15,method="MonteCarlo")$p.value,4)
testd15[3,10]=round(hopskel.test(ppropreDd15,method="MonteCarlo")$p.value,4) ; testd15[4,10]=round(hopskel.test(ppropreGd15,method="MonteCarlo")$p.value,4)
testd15[5,10]=round(hopskel.test(pcontrolDd15,method="MonteCarlo")$p.value,4) ; testd15[6,10]=round(hopskel.test(pcontrolGd15,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDd15,main="Delta de pouce",cex.main=1);plot(ppouceGd15,main="")
plot(ppropreDd15,main="Delta propre");plot(ppropreGd15,main="")
plot(pcontrolDd15,main="Zone de contrôle");plot(pcontrolGd15,main="")
title("Disque de diamètre 15 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testd15


plot(tableGrob(testd15))

par(mfrow = c(1,1))
#Disque 10 -------------------------------------------------------------------------------------------------------------------------------------
ppouceDd10 = ppp(pouceD[,17],pouceD[,18], disc(5))                          #6367 points en dehors /8782 - 2415 || T : 
ppouceGd10 = ppp(pouceG[,17],pouceG[,18], disc(5))                          #6485 points en dehors /8947 - 2462 || T : 
ppropreDd10 = ppp(propreD[,17],propreD[,18], disc(5))                       #7027 points en dehors /9244 - 2217 || T : 
ppropreGd10 = ppp(propreG[,17],propreG[,18], disc(5))                       #6721 points en dehors /8935 - 2214 || T : 
pcontrolDd10 = ppp(controlD[,17],controlD[,18], disc(5))                    #5348 points en dehors /6282 - 934 || T : 
pcontrolGd10 = ppp(controlG[,17],controlG[,18], disc(5))                    #5338 points en dehors /6244 - 906 || T : 

testd10=matrix(nrow=6,ncol=10);rownames(testd10)=c("PouceD","PouceG","PropreD","PropreG","ControlD","ControlG")
colnames(testd10)=c("Minutiae","Used","ClarkEvans R","p","HopkinsSkellam A","p","HopkinsSkellamAs A","p","HopkinsSkellamMC A","p")
testd10[1,1]=nrow(pouceD);testd10[2,1]=nrow(pouceG);testd10[3,1]=nrow(propreD);testd10[4,1]=nrow(propreG);testd10[5,1]=nrow(controlD);testd10[6,1]=nrow(controlG)
testd10[1,2]=nrow(as.data.frame(ppouceDd10));testd10[2,2]=nrow(as.data.frame(ppouceGd10));testd10[3,2]=nrow(as.data.frame(ppropreDd10))
testd10[4,2]=nrow(as.data.frame(ppropreGd10));testd10[5,2]=nrow(as.data.frame(pcontrolDd10));testd10[6,2]=nrow(as.data.frame(pcontrolGd10))


testd10[1,3]=round(clarkevans.test(ppouceDd10)$statistic,4) ; testd10[2,3]=round(clarkevans.test(ppouceGd10)$statistic,4)
testd10[3,3]=round(clarkevans.test(ppropreDd10)$statistic,4) ; testd10[4,3]=round(clarkevans.test(ppropreGd10)$statistic,4)
testd10[5,3]=round(clarkevans.test(pcontrolDd10)$statistic,4) ; testd10[6,3]=round(clarkevans.test(pcontrolGd10)$statistic,4)

testd10[1,4]=round(clarkevans.test(ppouceDd10)$p.value,4) ; testd10[2,4]=round(clarkevans.test(ppouceGd10)$p.value,4)
testd10[3,4]=round(clarkevans.test(ppropreDd10)$p.value,4) ; testd10[4,4]=round(clarkevans.test(ppropreGd10)$p.value,4)
testd10[5,4]=round(clarkevans.test(pcontrolDd10)$p.value,4) ; testd10[6,4]=round(clarkevans.test(pcontrolGd10)$p.value,4)

testd10[1,5]=round(hopskel.test(ppouceDd10)$statistic,5) ; testd10[2,5]=round(hopskel.test(ppouceGd10)$statistic,5) ; testd10[3,5]=round(hopskel.test(ppropreDd10)$statistic,4)
testd10[4,5]=round(hopskel.test(ppropreGd10)$statistic,5) ; testd10[5,5]=round(hopskel.test(pcontrolDd10)$statistic,5) ; testd10[6,5]=round(hopskel.test(pcontrolGd10)$statistic,4)

testd10[1,6]=round(hopskel.test(ppouceDd10)$p.value,4) ; testd10[2,6]=round(hopskel.test(ppouceGd10)$p.value,4) ; testd10[3,6]=round(hopskel.test(ppropreDd10)$p.value,4)
testd10[4,6]=round(hopskel.test(ppropreGd10)$p.value,4) ; testd10[5,6]=round(hopskel.test(pcontrolDd10)$p.value,4) ; testd10[6,6]=round(hopskel.test(pcontrolGd10)$p.value,4)

testd10[1,7]=round(hopskel.test(ppouceDd10,method="asymptotic")$statistic,4) ; testd10[2,7]=round(hopskel.test(ppouceGd10,method="asymptotic")$statistic,4)
testd10[3,7]=round(hopskel.test(ppropreDd10,method="asymptotic")$statistic,4) ; testd10[4,7]=round(hopskel.test(ppropreGd10,method="asymptotic")$statistic,4)
testd10[5,7]=round(hopskel.test(pcontrolDd10,method="asymptotic")$statistic,4) ; testd10[6,7]=round(hopskel.test(pcontrolGd10,method="asymptotic")$statistic,4)

testd10[1,8]=round(hopskel.test(ppouceDd10,method="asymptotic")$p.value,4) ; testd10[2,8]=round(hopskel.test(ppouceGd10,method="asymptotic")$p.value,4)
testd10[3,8]=round(hopskel.test(ppropreDd10,method="asymptotic")$p.value,4) ; testd10[4,8]=round(hopskel.test(ppropreGd10,method="asymptotic")$p.value,4)
testd10[5,8]=round(hopskel.test(pcontrolDd10,method="asymptotic")$p.value,4) ; testd10[6,8]=round(hopskel.test(pcontrolGd10,method="asymptotic")$p.value,4)

testd10[1,9]=round(hopskel.test(ppouceDd10,method="MonteCarlo")$statistic,4) ; testd10[2,9]=round(hopskel.test(ppouceGd10,method="MonteCarlo")$statistic,4)
testd10[3,9]=round(hopskel.test(ppropreDd10,method="MonteCarlo")$statistic,4) ; testd10[4,9]=round(hopskel.test(ppropreGd10,method="MonteCarlo")$statistic,4)
testd10[5,9]=round(hopskel.test(pcontrolDd10,method="MonteCarlo")$statistic,4) ; testd10[6,9]=round(hopskel.test(pcontrolGd10,method="MonteCarlo")$statistic,4)

testd10[1,10]=round(hopskel.test(ppouceDd10,method="MonteCarlo")$p.value,4) ; testd10[2,10]=round(hopskel.test(ppouceGd10,method="MonteCarlo")$p.value,4)
testd10[3,10]=round(hopskel.test(ppropreDd10,method="MonteCarlo")$p.value,4) ; testd10[4,10]=round(hopskel.test(ppropreGd10,method="MonteCarlo")$p.value,4)
testd10[5,10]=round(hopskel.test(pcontrolDd10,method="MonteCarlo")$p.value,4) ; testd10[6,10]=round(hopskel.test(pcontrolGd10,method="MonteCarlo")$p.value,4)


layout(matrix(c(1,2,7,3,4,7,5,6,7),nrow=3,ncol=3))
par(oma = c(0,0,5,0) + 0.1,mar = c(0,0,0.9,0) + 0.1)
plot(ppouceDd10,main="Delta de pouce",cex.main=1);plot(ppouceGd10,main="")
plot(ppropreDd10,main="Delta propre");plot(ppropreGd10,main="")
plot(pcontrolDd10,main="Zone de contrôle");plot(pcontrolGd10,main="")
title("Disque de diamètre 10 mm ?", cex.main=2.5, outer=TRUE, ylab="",cex.lab=2)
#testd10


plot(tableGrob(testd10))

par(mfrow = c(1,1))
#-------------------------------------------------------------------------------------------------------------------------------------



ppouceDd20 = ppp(pouceD[,17],pouceD[,18], disc(7.5))
ppouceGd20 = ppp(pouceG[,17],pouceG[,18], disc(7.5))
ppropreDd20 = ppp(propreD[,17],propreD[,18], disc(7.5))
ppropreGd20 = ppp(propreG[,17],propreG[,18], disc(7.5))
pcontrolDd20 = ppp(controlD[,17],controlD[,18], disc(7.5))
pcontrolGd20 = ppp(controlG[,17],controlG[,18], disc(7.5))



k=Kest(pcontrolGd20,correction="Ripley",var.approx = TRUE,ratio= TRUE)
edge =  c(-10,10) # c(-ceiling(max(c(2*sqrt(k$rip),k$t-pi * k$r^2))),ceiling(max(c(2*sqrt(k$rip),k$t-pi * k$r^2))))

plot(k,. - pi * r^2 ~ r,ylim=edge,main="",ylab="",xlab="",legend=FALSE)
lines(matrix(c(k$r,2*sqrt(k$rip)),ncol=2),lty=2,col="red")
lines(matrix(c(k$r,-2*sqrt(k$rip)),ncol=2),lty=2,col="red")

plot(pcontrolGd20)


