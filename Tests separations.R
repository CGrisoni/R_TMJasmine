plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="aab" & V25==1)[,17:18], 
       col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="aab" & V25==2)[,17:18], 
       col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="aab" & V25==3)[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="abb" & V25==1)[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="abb" & V25==2)[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="abb" & V25==3)[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="bbc" & V25==1)[,17:18], 
     col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="bbc" & V25==2)[,17:18], 
     col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="bbc" & V25==3)[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="bcc" & V25==1)[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="bcc" & V25==2)[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="bcc" & V25==3)[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="cca" & V25==1)[,17:18], 
     col="blue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="cca" & V25==2)[,17:18], 
     col="green",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="cca" & V25==3)[,17:18], 
     col="red",xlim=c(-20,20),ylim=c(-20,20))

par(new=T)

plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="caa" & V25==1)[,17:18], 
     col="darkblue",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="caa" & V25==2)[,17:18], 
     col="darkgreen",xlim=c(-20,20),ylim=c(-20,20))
par(new=T)
plot(subset(data,type=="delta propre" & d.g=="droit" & V24=="caa" & V25==3)[,17:18], 
     col="darkred",xlim=c(-20,20),ylim=c(-20,20))

# 
# ----
# for (i in 1:nrow(data)) {
#   if (data[i,3]== "delta"){
#     if(((data[i,13]+90)%%360)/2 >= 90){print(i)}}}
# 
# i=3888
# if (((data[i,13]+90)%%360)/2 < 90 & 
#     
#     ((data[i,13] > 90 & (data[i,15] > data[i,13] | data[i,15] < ((data[i,13]+90)%%360)/2)) |
#      
#      (data[i,13] < 90 & data[i,15] > data[i,13] & data[i,15] < ((data[i,13]+90)%%360)/2 ) )) {print("cca")}


for (i in 1:nrow(data)){
  if (data[i,"delta"]=="delta"){
  if (data[i,"angle.b"]<90 | data[i,"angle.b"]>180){print(i)}
}}

layout=matrix(c(1:100),ncol=10,nrow=10)
for (i in unique(subset(data, type=="delta propre" & d.g=="droit")[,1])){print(i)
  plot(subset(data,No.==i&type=="delta propre" & d.g=="droit" & V24=="abb" & V25==1)[,17:18], 
       col="darkblue",xlim=c(-20,20),ylim=c(-20,20),main=i)
  par(new=T)
  plot(subset(data,No.==i&type=="delta propre" & d.g=="droit" & V24=="abb" & V25==2)[,17:18], 
       col="darkgreen",xlim=c(-20,20),ylim=c(-20,20),main=i)
  par(new=T)
  plot(subset(data,No.==i&type=="delta propre" & d.g=="droit" & V24=="abb" & V25==3)[,17:18], 
       col="darkred",xlim=c(-20,20),ylim=c(-20,20),main=i)
  abline(0,tan(90-abs(data[which(data$No. == i)[1][1],"angle.b"]-180)))
  abline(5,tan(90-abs(data[which(data$No. == i)[1][1],"angle.b"]-180)))
  abline(10,tan(90-abs(data[which(data$No. == i)[1][1],"angle.b"]-180)))
}


z = subset(data, type=="delta propre" & d.g=="droit")
layout=matrix(c(1:100),ncol=10,nrow=10)
for (i in unique(z[,1])){print(i)
  plot(subset(z,No.==i & V24=="abb" & V25==1)[,17:18], 
       col="darkblue",xlim=c(-20,20),ylim=c(-20,20),main=i)
  par(new=T)
  plot(subset(z,No.==i & V24=="abb" & V25==2)[,17:18], 
       col="darkgreen",xlim=c(-20,20),ylim=c(-20,20),main=i)
  par(new=T)
  plot(subset(z,No.==i & V24=="abb" & V25==3)[,17:18], 
       col="darkred",xlim=c(-20,20),ylim=c(-20,20),main=i)
  abline(0,tan(90-abs(z[which(z$No. == i)[1][1],"angle.b"]-180)))
  abline(5,tan(90-abs(z[which(z$No. == i)[1][1],"angle.b"]-180)))
  abline(10,tan(90-abs(z[which(z$No. == i)[1][1],"angle.b"]-180)))
}

### ca fonctionne !!!!
z = subset(data, type=="delta propre" & d.g=="droit")

pdf("test.pdf",paper="a4",width=7.8,height=11.2,title="name")
layout(matrix(c(1:25),nrow=5,ncol=5))
par(oma=c(0,0,1.5,0)+0.1,mar=c(2,0,1,0)+0.1)


for (i in unique(z[,1])){
  zu=data.frame()
  for (j in 1:nrow(z)){print (z[j,"angle.b"])
    if (z[j,1] == i){
      zu=rbind(zu,z[j,])}}
      
  plot(subset(zu, V24=="abb" & V25==1)[,17:18], 
           col="darkblue",xlim=c(-20,20),ylim=c(-20,20),main=i)
      par(new=T)
      plot(subset(zu, V24=="abb" & V25==2)[,17:18], 
           col="darkgreen",xlim=c(-20,20),ylim=c(-20,20),main=i)
      par(new=T)
      plot(subset(zu, V24=="abb" & V25==3)[,17:18], 
           col="darkred",xlim=c(-20,20),ylim=c(-20,20),main=i)
      abline(0,tan(deg2rad(90-abs(z[which(z$No. == i)[1][1],"angle.b"]-180))))
      abline(5,tan(deg2rad(90-abs(z[which(z$No. == i)[1][1],"angle.b"]-180))))
      abline(10,tan(deg2rad(90-abs(z[which(z$No. == i)[1][1],"angle.b"]-180))))
    }
dev.off()