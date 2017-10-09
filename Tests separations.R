delta=c("delta propre", "delta de pouce") ; main = c("droit","gauche")
zone = c("aab","abb","bbc","bcc","cca","caa") ; dist = c(1,2,3)

for (a in delta){
  for (b in main){par(new=F)
    for (c in zone){
      if (c=="aab" | c=="bbc" | c=="caa"){col=""}
      else {col="dark"}
      for (d in dist){
        if (d==1){plot(subset(data,type==a & d.g==b & V24==c & V25==d)[,17:18], col=paste(col,"red",sep=""),xlim=c(-20,20),ylim=c(-20,20),main=paste(a,b))}
        else if (d==2){plot(subset(data,type==a & d.g==b & V24==c & V25==d)[,17:18], col=paste(col,"blue",sep=""),xlim=c(-20,20),ylim=c(-20,20))}
        else {plot(subset(data,type==a & d.g==b & V24==c & V25==d)[,17:18], col=paste(col,"green",sep=""),xlim=c(-20,20),ylim=c(-20,20))}
        par(new=T)
      }
      
      }}}








### ca fonctionne !!!! ----
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
      abline(0,tan(deg2rad(z[which(z$No. == i)[1][1],"angle.b"]+90)))
      abline(5,tan(deg2rad(z[which(z$No. == i)[1][1],"angle.b"]+90)))
      abline(10,tan(deg2rad(z[which(z$No. == i)[1][1],"angle.b"]+90)))
    }
dev.off()
