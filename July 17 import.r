
#load data
library(jpeg)
library(png)
library(xlsx)
library(rioja)

#functions
good<-function(x){
  if(is.null(dim(x))){
    x[x>0]
  }else{
    x[,colSums(x)>0]
  }
}

find.dup<-function(data, round=-1, pa=FALSE, thresh=0,...){
  data0<-data
  if(round>0)data<-round(data, round)
  if(pa)data<-data>0
  
  dup<-as.matrix(dist(data))
  dup[!upper.tri(dup)]<-NA
  hist(dup,...)
  abline(v=thresh, col=2)
  
  wdup<-which(dup<=thresh, arr.ind=TRUE)
  wdup
  min(dup, na.rm=TRUE)
  lapply(1:nrow(wdup), function(n){
    good(data0[wdup[n,],])
  })
}



#calibration set
chiros2 <- read.xlsx('july 17/Transfer function species.xls', sheetIndex = 1)
rownames(chiros2)<-chiros2[,1]
chiros2[,1]<-NULL
tt2 <- chiros2$Temp
chiros2$Temp<-NULL
#fossil
zab2 <- read.xlsx('july 17/Zabinskie percentages for reconstruction.xls', sheetIndex = 1)
chron2<-zab2[,1]
zab2[,1] <- NULL
zab2$Total<-NULL

#original data
#calibration set
chiros <- read.xlsx('Data training Poland-Quebec.xlsx', sheetIndex = 1)
rownames(chiros)<-chiros[,1]
chiros[,1]<-NULL
tt <- chiros$Temp
chiros$Temp<-NULL
#fossil
zab <- read.xlsx('Zabinskie chiro 1886AD.xlsx', sheetIndex = 1)
chron<-zab$Chron
zab$Chron <- NULL


#compare with C2 file
c2train<-read.table("exC2/chiros combined.csv", header=TRUE, sep=",")
c2fos<-read.table("exC2/good 2015.csv", header=TRUE, sep=",")
c2fos$F52<-NULL
all.equal(chron,c2fos$CodeNum)
c2fos<-c2fos[,-(1:3)]


#august data (version number 5)
zab5 <- read.xlsx('july 17/Zabinskie percentages for reconstruction2.xls', sheetIndex = 1)
zab5[,1]<-NULL
count5<-zab5[,ncol(zab5)]
zab5<-zab5[,-ncol(zab5)]


#september data (version 6)
zab6 <- read.xlsx('july 17/Zabinskie percentages for reconstruction2 (1).xls', sheetIndex = 1)
zab6[,1]<-NULL
count6<-zab6[,ncol(zab6)]
zab6<-zab6[,-ncol(zab6)]



#october data
zab7<-read.xlsx('july 17/counts.xls', sheetIndex = 2)
counts7<-read.xlsx('july 17/counts.xls', sheetIndex = 1)

zab7[,1]<-NULL
counts7[,1]<-NULL
count7<-counts7[,ncol(counts7)]
zab7<-zab7[,-ncol(zab7)]
counts7<-counts7[,-ncol(counts7)]


#second october data
counts8<-read.xlsx('july 17/counts.xls', sheetIndex = 1)

counts8[,1]<-NULL
count8<-counts8[,ncol(counts8)]
counts8<-counts8[,-ncol(counts8)]

#NOAA 
counts9<-read.table("noaa/zabinskie2015chir.txt", header = TRUE, sep="\t", skip=96, comment="")
c9tot<-counts9$Total
counts9$Total<-NULL
c9age<-counts9$age_AD
counts9$age_AD<-NULL

pc9<-read.table("noaa/zabinskie2015chirpct.txt", header = TRUE, sep="\t", skip=96, comment="")
pc9$Nb.head.capsules <-NULL
pc9$age_AD<-NULL
