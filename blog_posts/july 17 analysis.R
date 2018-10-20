#1) are newly archived data idential to initial archived or C2 file
#2) do they generate new fig5 + reconstruction
#3) duplicate samples

source ("July 17 import.r")


#Calibration
head(c2train)
dim(c2train)
dim(chiros2)
names(chiros2)%in%names(c2train)
names(c2train)%in%names(chiros2)

sum(sapply(names(chiros2), function(x)sum(chiros2[,x]!=c2train[,x])))
#identical

#fossil
dim(c2fos)

dim(zab2)
setdiff(names(zab2), names(c2fos))
setdiff(names(c2fos), names(zab2))

setdiff(names(zab), names(c2fos))
setdiff(names(c2fos), names(zab))
colSums(zab[,!names(zab)%in%names(zab2)]>0)
good(zab$Parochlus)

chron2[!chron2%in%chron]
chron[!chron%in%chron2]
which(!chron2%in%chron)
count5[!chron2%in%chron]
plot(chron2,count5, col=ifelse(chron2%in%chron,1,2), xlab="Year CE", ylab="Count sum")
legend("topright", legend=c("both versions", "long version only"), pch=1, col=c(2,1))



plot(chron2[-1], -diff(chron2), xlab="Year CE", ylab="Time span, years")
points(chron[-1], -diff(chron), col=2, pch=20)
abline(v=c(1949,1915, 1899))
legend("topright", legend=c("short version", "long version"), pch=c(20,1), col=c(2,1))

dim(zab5)
quantile(count5)

mean(count5>=50)

chron[75]
chron2[75]


sapply(names(zab2),function(nam){ 
  #x11()
  plot(chron2,zab2[,nam], type="l",main=nam, ylim=range(c2fos[,nam],zab2[,nam],zab[,nam]))
  lines(chron,c2fos[,nam]-0.1, col=2)
  lines(chron,zab[,nam]-0.2, col=4)
  lines(chron2, zab5[,nam]+0.3, col=3)
  lines(chron2, zab6[,nam]+0.2, col=5)
  lines(chron2, zab7[,nam]+0.1, col=6)
  rug(1931, col=3)
})




#test for duplicates
dup<-as.matrix(dist(zab2))
dup[!upper.tri(dup)]<-NA
wdup<-which(dup==0, arr.ind=TRUE)
wdup#none
min(dup, na.rm=TRUE)
hist(dup)
wdup<-which(dup<1, arr.ind=TRUE)
wdup
zab2[wdup,colSums(zab2[wdup,])>0]


dup<-as.matrix(dist(round(zab2,0)))
dup[!upper.tri(dup)]<-NA
wdup<-which(dup==0, arr.ind=TRUE)
wdup#one pair within rounding error
min(dup, na.rm=TRUE)
hist(dup)
wdup<-which(dup<5, arr.ind=TRUE)
wdup
tmp<-zab2[wdup,colSums(zab2[wdup,])>0]
tmp[order(tmp[,1]),] #basically duplicates



#singletons
min2<-apply(zab2,1,function(x)min(x[x>0]))<=2 #only 28/84 samples can have singletons
which(min2)
sum(min2)

#flat
zabeven<-apply(zab2,1, function(r){r<-r[r>0]; all(r==min(r))})#samples with all taxa having identical
sum(zabeven)
apply(zab2[zabeven,], 1,sort, decreasing=TRUE)[1:10,]
zab2[zabeven,colSums(zab2[zabeven,])>0]

#near integer
near.integer <- apply(zab2[!(min2 | zabeven), ], 1, function(r) {
  # samples with all taxa having integer multiple abundances of rarest taxa
  r <- r[r > 0]
  f <- r/min(r)
  fd <- abs(f - round(f))
  max(fd) < 1e-04 #rounding error tolerance
})
sum(near.integer)  #12 
length(near.integer)
apply(zab2[as.numeric(names(which(!near.integer))), ], 1, sort, decreasing = TRUE)[1:20,]


#reconstruction
wapls.chiros <- WAPLS(sqrt(chiros2),tt2)

cv.wapls.loo <- crossval(wapls.chiros) #leave-one-out cross-validation
performance(cv.wapls.loo)$crossval[1:3,2:1]

# make reconstruction
rec <- predict(wapls.chiros, newdata = sqrt(zab2))
rec5 <- predict(wapls.chiros, newdata = sqrt(zab5))
# import jpeg
LT15.6 <- readJPEG("1-s2.0-S0277379115000086-gr6.jpg", native = TRUE)
# overlay reconstruction on LT fig 6
par(mar = c(4, 4, 1, 1), mgp = c(1.5, 0.5, 0))
plot(chron2, rec$fit[, 2], type = "o", xlab = "", ylab = "", col = 2, xlim = c(1890, 2010), xaxs = "i", ylim = c(13, 22), yaxs = "i")
par(xpd = TRUE)
rasterImage(LT15.6, 1879, 10.8, 2012, 22.15) #positioned to fit axes
par(new = TRUE)
plot(chron2, rec$fit[, 2], type = "o", xlab = "", ylab = "", col = 2, xlim = c(1890, 2010), xaxs = "i", ylim = c(13, 22), yaxs = "i")
lines(chron2, rec5$fit[, 2], type = "o",  col = 4)

#identical within expected error


#inclusion rule breakers
colSums(zab2>0)#number occurences
which(colSums(zab2>0)<3)

min(apply(zab2,2,function(x)max(x[x>0]))) #all OK

#zab 2 vs zab5
identical(names(zab2), names(zab5))
summary(as.matrix((zab2)-(zab5)))


hist(zab5$Number)
mean(zab5$Number<50)
summary(zab5$Number)

#flat
zab5even<-apply(zab5,1, function(r){r<-r[r>0]; all(r==min(r))})#samples with all taxa having identical
sum(zab5even)
apply(zab5[zab5even,], 1,sort, decreasing=TRUE)[1:10,]
zab2[zabeven,colSums(zab2[zabeven,])>0]
 count5[zab5even]
 
dmultinom(rep(3,7),p=rep(1/7,7))
dmultinom(rep(7,7),p=rep(1/7,7))
which.min(count5)
sort(count5)

zab5[29,zab5[29,]>0]

zab5[29,zab5[29,]>0]/100*19

good(zab5[count5==20,])/100*20

good<-function(x)x[,colSums(x)>0]
good(zab5[count5==21,])

round(good(zab5[count5==21,])/100*21,2)

round(good(zab5[count5==22,])/100*22,2)

apply(round(zab5/100*count5,2),1,sort, decreasing=TRUE)[1:20,]



minc<-function(pc)seq(0.5,50,.5)/pc*100
zab5



c/tc*100=pc
c/pc*100=tc

sapply(good(zab5[89,] ), minc)
round(sapply(good(zab5[89,] ), function(x)x/100*seq(0.5,43)),2)

 
#version 5 vs version 6
identical(names(zab5), names(zab6))
dim(zab5)
dim(zab6)
identical(round(count5,1),round( count6,1))
which(round(zab5,1)!=round(zab6,1), TRUE)

zab5[28,2]
zab6[28,2]
which(abs(zab5-zab6)>0.17, TRUE)
rbind(good(zab5[8,]),good(zab6[8,]))
count6[8]
good(zab6[8,])*41/100
0.5/41*100

which(abs(zab5-zab6)>0.2, TRUE)

max(abs(zab5-zab6))
table(round(unlist(abs(zab5-zab6)),2))
rbind(good(zab5[76,]),good(zab6[76,]))

good(zab5[76,])
100/count6[76]
100/min(good(zab5[76,]))
.5/(100/min(good(zab5[76,]))) *100

#zab7
plot(count6-count7)
table(unlist(counts7))
sort(unique(unlist(counts7)))
sum(!(counts7*2)%%1==0)
sum(counts7!=0)

identical(zab6, zab7)
identical(names(zab6), names(zab7))
range(zab6-zab7)
sum(zab6!=zab7)#zab7 is not rounded
plot(unlist(zab6), unlist(zab7))
abline(0,1)

which(abs(zab7-zab6)>2, TRUE)
good(rbind(zab7[37,], zab6[37,]))
good(rbind(zab7[71,], zab6[71,], zab5[71,], zab2[71,]))
plot(t(good(rbind(zab7[71,], zab6[71,]))))
abline(0,1)

identical(zab5, zab7)
range(zab5-zab7)
sum(zab5!=zab7)
sum(c2fos!=zab7)
plot(unlist(zab5), unlist(zab7))
plot(unlist(zab6), unlist(zab5))



#check counts7 ~= zab7 


plot(unlist(counts7/rowSums(counts7)*100),unlist(zab7))
range(counts7/rowSums(counts7)*100-zab7)#exact
good(counts7[abs(count7-21)<1,])


sum(abs(zab6-zab7))
sum(abs(zab5-zab7))
sum(abs(zab5-zab6))
sum(abs(zab2-zab7))
sum(abs(zab2-zab6))
sum(abs(zab2-zab5))

dim(zab2)
dim(zab7)
dim(c2fos)
dim(zab)


#test for duplicates

x11();par(mfrow=c(6,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
find.dup(zab, pa=TRUE,thresh=1)
find.dup(c2fos, pa=TRUE,thresh=1)
find.dup(zab2, pa=TRUE,thresh=1)
find.dup(zab5, pa=TRUE,thresh=1)
find.dup(zab6, pa=TRUE,thresh=1)
find.dup(zab7, pa=TRUE,thresh=1)

find.dup(zab7, pa=FALSE,thresh=1, ylim=c(0,10))
find.dup(zab6, pa=FALSE,thresh=1, ylim=c(0,10))
find.dup(zab2, pa=FALSE,thresh=1, ylim=c(0,10))
find.dup(zab, pa=FALSE,thresh=1, ylim=c(0,10))

x11(width=13, height=7)
strat.plot(zab7,yvar = chron2,scale.percent = TRUE,y.rev = FALSE )

#counts 8
identical(counts7, counts8)#TRUE AMAZING

apply(zab7, 1, function(x)good(matrix(x, nrow=1)))

plot(count5,apply(zab7, 1, function(x)min(x[x>0]))*count5/100)
plot(chron2,apply(zab7, 1, function(x)min(x[x>0]))*count5/100)

plot(count5,apply(counts7, 1, function(x)min(x[x>0])))

plot(chron2,apply(counts7, 1, function(x)min(x[x>0])), xlab="Year CE", ylab="Minimum count, heads")

table(round(apply(counts7, 1, function(x)min(x[x>0])),1))
apply(counts7, 1, function(x)min(x[x>0]))
mean(apply(counts7, 1, function(x)min(x[x>0]))>1.1)
rowSums(good(counts7[apply(counts7, 1, function(x)min(x[x>0]))>3.3,])>0)
apply(counts7[apply(counts7, 1, function(x)min(x[x>0]))>1.1,], 1, function(x){
  z<-round(as.vector(good(x)),1)
  all(z%%min(z)==0)
})


names(chiros2)
good(chiros2[1,])

#simulation of high min counts
simMinCount<-replicate(10000,min(table(sample(ncol(chiros2), prob=chiros2[1,], size=50, replace=TRUE))))
table(simMinCount)

sum(counts7*2!=round(counts7*2))
colSums(counts7*2!=round(counts7*2))

count5[2]
good(counts7[2,])
good(zab6[2,])*count6[2]/100

good(zab6[2,])
count5==count6
cbind(count5, count6)[count5!=count6,]
good(counts7[2,])
good(zab6[2,])*count6[2]/100
chron2[2]

31*round(3/31,3)
31*floor(x = 3/31*1000)/1000
31*round(3/31.5,3)


#noaa (v9)
quantile(as.matrix(counts9/c9tot*100-pc9))#should be max diff of 0.05
sum(abs(counts9-counts8))
sum(abs(counts9-round(counts8,1)))#matches!
head(counts8)

quantile(as.matrix(counts8/c9tot*100-pc9))#should be max diff of 0.05
sort(colSums(counts9>0))
