plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="aab" & V22==1)[,17:18], 
       col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="aab" & V22==2)[,17:18], 
       col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="aab" & V22==3)[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="abb" & V22==1)[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="abb" & V22==2)[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="abb" & V22==3)[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bbc" & V22==1)[,17:18], 
     col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bbc" & V22==2)[,17:18], 
     col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bbc" & V22==3)[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bcc" & V22==1)[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bcc" & V22==2)[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bcc" & V22==3)[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="cca" & V22==1)[,17:18], 
     col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="cca" & V22==2)[,17:18], 
     col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="cca" & V22==3)[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="caa" & V22==1)[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="caa" & V22==2)[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="caa" & V22==3)[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))

# ----

plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="aab")[,17:18], 
     col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="abb")[,17:18], 
     col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bbc")[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="bcc")[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="cca")[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V21=="caa")[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))


for (i in 1:nrow(data)) {
  if (data[i,3]== "delta"){
    if(((data[i,13]+90)%%360)/2 >= 90){print(i)}}}

i=3888
if (((data[i,13]+90)%%360)/2 < 90 & 
    
    ((data[i,13] > 90 & (data[i,15] > data[i,13] | data[i,15] < ((data[i,13]+90)%%360)/2)) |
     
     (data[i,13] < 90 & data[i,15] > data[i,13] & data[i,15] < ((data[i,13]+90)%%360)/2 ) )) {print("cca")}
