#load data
library(readxl)

#functions
good <- function(x) {
  if (is.null(dim(x))) {
    x[x > 0]
  } else{
    x[, colSums(x) > 0]
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

#published version 

#original data uploaded January 2015
#fossil
zab2 <- read_excel('data/Zabinskie chiro 1886AD.xlsx', sheet = 1)
chron2 <- zab2$Chron
zab2$Chron <- NULL
names(zab2) <- make.names(names(zab2))

#compare with C2 file - emailed 15th June 2015
zab3 <- read.table("data/Good 2015.csv", header = TRUE, sep = ",")
zab3$F52 <- NULL
all.equal(chron2, zab3$CodeNum)#same chronology as zab2
zab3 <- zab3[, -(1:3)]


#July 17th
zab4 <- read_excel('data/Zabinskie percentages for reconstruction.xls', sheet = 1)
chron4 <- zab4[, 1]
all.equal(chron2, chron4)
zab4[, 1] <- NULL
zab4$Total <- NULL#sum percent
names(zab4) <- make.names(names(zab4))


#august data (version number 5)
zab5 <- read_excel('data/Zabinskie percentages for reconstruction2.xls', sheet = 1)
all.equal(chron4, zab5[, 1])
zab5[, 1] <- NULL
count5 <- zab5$Number
zab5$Number <- NULL
names(zab5) <- make.names(names(zab5))



#september data (version 6)
zab6 <- read_excel('data/Zabinskie percentages for reconstruction2 (1).xls', sheet = 1)
all.equal(chron4, zab6[, 1])
zab6[, 1] <- NULL
count6 <- zab6$Total
zab6$Total <- NULL
names(zab6) <- make.names(names(zab6))



#october data
zab7 <- read_excel('data/Counts.xls', sheet = 2)
counts7 <- read_excel('data/Counts.xls', sheet = 1)
all.equal(chron4, zab7[, 1])
zab7[, 1] <- NULL
counts7[, 1] <- NULL
count7 <- counts7[, ncol(counts7)]
zab7 <- zab7[, -ncol(zab7)]
counts7 <- counts7[, -ncol(counts7)]
names(zab7) <- make.names(names(zab7))
names(counts7) <- make.names(names(counts7))


#second october data
counts8 <- read_excel('data/Counts-1.xls', sheet = 1)
all.equal(chron4, counts8[, 1])
counts8[, 1] <- NULL
count8 <- counts8$Total
counts8$Total <- NULL
names(counts8) <- make.names(names(counts8))


#NOAA 
counts9<-read.table("data/zabinskie2015chir.txt", header = TRUE, sep="\t", skip=96, comment="")
all.equal(chron4, counts9$age_AD)
count9 <- counts9$Total
counts9$Total <- NULL
counts9$age_AD <- NULL
names(counts9) <- make.names(names(counts9))


zab9<-read.table("data/zabinskie2015chirpct.txt", header = TRUE, sep = "\t", skip = 96, comment = "")
zab9$Nb.head.capsules <- NULL
zab9$age_AD <- NULL


#NOAA Excel
fname <- "data/zabinskie2015cit.xls"
excel_sheets(fname)

zab10 <- read_excel(fname, sheet = "Chironomids Zabinsk percentages")
all.equal(chron4, zab10[, 1])
zab10[, 1] <- NULL
names(zab10)<-make.names(names(zab10))
zab10$Nb.head.capsules <- NULL

all.equal(zab9, as.data.frame(zab10))
all.equal(zab9, as.data.frame(zab7))

counts10 <- read_excel(fname, sheet = "Chironomids Zabinskie counts")
all.equal(chron4, counts10[, 1])
counts10[, 1] <- NULL
names(counts10)<-make.names(names(counts10))
count10 <- counts10$Total
counts10$Total <- NULL


#count sums comparison
allCount <- data.frame(count5, count6, count7, count8, count9, count10)
which(rowSums(abs(round(allCount, 1)-rowMeans(round(allCount, 1)))) != 0)

#species list comparison
sapply(list(zab2, zab3, zab4, zab5, zab6, zab7, counts8, zab9), ncol)
sapply(list(zab2, zab3, zab4, zab5, zab6, zab7, counts8, zab9, zab10, counts10), function(f)all.equal(names(f), names(zab3)))
setdiff(names(zab4), names(zab10))
setdiff(names(zab10), names(zab4))#just a spelling change


#chronology
setdiff(chron2, chron4)
setdiff(chron4, chron2)

#zab2 vs zab3
setdiff(names(zab3), names(zab2))
lost <- setdiff(names(zab2), names(zab3))
colSums(zab2[, lost]>0) ##mostly rare
good(zab2[zab2$Parochlus>0,])

all.equal(as.data.frame(zab2)[, !names(zab2) %in% lost], zab3)
diff23 <- rowSums(abs(zab2[, !names(zab2) %in% lost]-zab3))
sum(diff23 == 0)
nrow(zab2)
max(diff23)
chron2[which.max(diff23)]
good(rbind(zab2[which.max(diff23), !names(zab2) %in% lost],
zab3[which.max(diff23),]))

#zab3 vs zab4
diff34 <- rowSums(abs(zab3[chron2 %in% intersect(chron2, chron4),]-zab4[chron4 %in% intersect(chron2, chron4),]))
sum(round(diff34,2) == 0)

intersect(chron2, chron4)[which.max(diff34)]
getYear(intersect(chron2, chron4)[which.max(diff34)])


#zab4 vs zab5
diff45 <- rowSums(abs(zab4-zab5))
round(diff45,2)
sum(round(diff45,2) != 0)
count5
nrow(zab4)
getYear(chron4[which.max(diff45)])

#zab5 vs zab6
diff56 <- rowSums(abs(zab5-zab6))
round(diff56,2)
sum(round(diff56,2) != 0)
getYear(chron4[which.max(diff56)])

#zab6 vs zab7
diff67 <- rowSums(abs(zab6-zab7))
round(diff67,2)
sum(round(diff67,2) != 0)
getYear(chron4[which.max(diff67)])

#zab7 vs zab8
diff78 <- rowSums(abs(round(counts7,1)-counts8))
diff78

#v7/8 vs v9
rowSums(abs(counts8-counts10))
diff89 <- rowSums(abs(counts8-counts9))
diff89
counts9

diff79<-rowSums(abs(round(zab7,1)-zab9))
sort(round(diff79,2))
getYear(chron4[which.max(diff79)])


#rare taxa
sort(colSums(zab9>0))


library(plyr)
allpc <- ldply(setNames(list(zab2, zab3, zab4, zab5, zab6, zab7, zab9), c("zab2", "zab3", "zab4", "zab5", "zab6", "zab7", "zab9")),function(x){
  names(x)[names(x) == "Paratendipes.nubisquama"] <- "Paratendipes.nudisquama"
  if(nrow(x) == length(chron2)){
    x <- cbind(chron = chron2, x)
  } else {
    x <- cbind(chron = chron4, x)
  }
  x
})
allpc[is.na(allpc)] <- 0
dim(allpc)

getYear <- function(y, dat = allpc){
  dat_y <- subset(dat, chron == y)
  cbind(.id = dat_y$.id, good(dat_y[,-1]))
}
getYear(2005)
getYear(2006)

library(vegan)
maxdist <- ldply(as.list(intersect(chron2, chron4)), function(y) {
  c(y, round(max(vegdist(getYear(y)[,-(1:2)])), 2))
})
x11();plot(maxdist)
ldply(as.list(intersect(chron2, chron4)), function(y){
  c(y, ncol(getYear(y)[, -(1:2)]))
})


getYear(1986)
getYear(2002)

library(ggplot2)


ggplot(allpc, aes(x = chron, y = Smittia, colour = .id)) + geom_path()

library("reshape2")
ggplot(melt(getYear(2006)[,-2]), aes(x = variable, y = value, colour = .id)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(subset(melt(getYear(1940)[,-2]), value >0), aes(x = .id, y = value)) + geom_bar(stat = "identity")  + facet_grid(variable~.) +  theme( strip.text.y = element_text(angle = 0, hjust = 1)) + labs(x = "Version", y = "Percent") + ylim(0, NA)
ggsave("zabinskie1940.png")

sapply(sort(union(chron2, chron4)), function(y) {
  g <- ggplot(subset(melt(getYear(y)[, -2]), value > 0), aes(x = .id, y = value)) + geom_bar(stat = "identity")  +
    facet_grid(variable ~ .) + 
    theme(strip.text.y = element_text(angle = 0, hjust = 1)) + 
    labs(x = "Version", y = "Percent", title = y) + 
    ylim(0, NA)
  print(g)
})


#reconstructions
excel_sheets("data/zabinskie2015cit.xls")
spp<- read_excel("data/zabinskie2015cit.xls", "Training species")
env <- read_excel(fname, sheet = "Training temperature")
recon <- read_excel(fname, sheet = "Reconstruction ")
names(recon) <- c("date", "temperature")

rownames(spp) <- spp[, 1]
spp[, 1] <- NULL
rownames(env) <- env[, 1]
env <- env[, 2, drop = FALSE]

lowCount <- c("SAL", "LEK", "TRZ", "WAS", "SZOS", "GOR", "KOS", "ZAB")
spp <- spp[!rownames(spp) %in% lowCount, ]
env <- env[!rownames(env) %in% lowCount, , drop  = FALSE]
identical(rownames(spp), rownames(env))
env <- env$Temp
names(spp) <- make.names(names(spp))
library(rioja)
setdiff(names(allpc), names(spp))

mod <- WAPLS(sqrt(spp[, colSums(spp > 0) >= 3]), env)
recons <- cbind(allpc[,1:2], recon =  predict(mod, sqrt(allpc[, -(1:2)]))$fit[, 2])
ggplot() + geom_path( aes(x = chron, y = recon, colour = .id), subset(recons, .id == "zab9")) +geom_point( aes(x = chron, y = recon, colour = .id), subset(recons, .id == "zab9")) + geom_path(aes(x = date, y = temperature), data = recon)


plot(recon$temperature, subset(recons, .id == "zab9")$recon)
abline(0,1)
cor(recon$temperature, subset(recons, .id == "zab9")$recon)
abline(1,1, lty = 1)
