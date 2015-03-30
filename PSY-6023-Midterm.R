rm(list = ls(all.names = TRUE))
library(foreign)
library(psych)
library(psy)
library(stats)
library(CTT)
library(sem)
library(GPArotation)
library(MBESS)
library(lavaan)
library(epicalc)

# Reading in and prepping data
ds<-read.csv("Midterm.csv")

namevector<-c(
  "DS1", "DS2", "DS3", "DS4", "DS5", "DS6", "DS7", "DS8", "DS9", "DS10", "DS11", "DS12", "DS13", "DS14", 
  "LS1", "LS2", "LS3", "LS4", "LS5", "LS6", "LS7", "LS8", "LS9", "LS10", 
  "RR1", "RR2", "RR3", "RR4", "RR5", "RR6", "RR7", "RR8", "RR9", "RR10", 
  "GAR1", "GAR2", "GAR3", "GAR4", "GAR5", "GAR6", "GAR7", "GAR8", 
  "BAR1", "BAR2", "BAR3", "BAR4", "BAR5", "BAR6",
  "Sex", "Age")

colnames(ds)<-namevector

ds$DS4<- ifelse(ds$DS4==1, 4,
                ifelse(ds$DS4==2, 3,
                       ifelse(ds$DS4==3, 2, 1)))

ds$DS7<- ifelse(ds$DS7==1, 4,
                ifelse(ds$DS7==2, 3,
                       ifelse(ds$DS7==3, 2, 1)))

ds$DS12<- ifelse(ds$DS12==1, 4,
                ifelse(ds$DS12==2, 3,
                       ifelse(ds$DS12==3, 2, 1)))

ds$DS14<- ifelse(ds$DS14==1, 4,
                ifelse(ds$DS14==2, 3,
                       ifelse(ds$DS14==3, 2, 1)))


DS<-ds[,c(1:14, 49, 50)]
LS<-ds[,c(15:24, 49, 50)]
RR<-ds[,c(25:34, 49,50)]
GAR<-ds[,c(35:42, 49,50)]
BAR<-ds[,c(43:50)]

for(i in 1:length(DS$DS1)){
  for(j in 1:14){
   DS[i,j]<-ifelse(DS[i,j] > 4, NA, DS[i,j])
  }
}
DS<-DS[complete.cases(DS),]

for(i in 1:length(LS$LS1)){
  for(j in 1:10){
    LS[i,j]<-ifelse(LS[i,j] > 5 || LS[i,j]<0, NA, LS[i,j])
  }
}

LS<-LS[complete.cases(LS),]
table(DS$DS11)

for(i in 1:length(RR$RR1)){
  for(j in 1:10){
    RR[i,j]<-ifelse(RR[i,j] > 4 || RR[i,j]<0, NA, RR[i,j])
  }
}

RR<-RR[complete.cases(RR),]
table(RR$RR1)
length(RR$RR1)

for(i in 1:length(GAR$GAR1)){
  for(j in 1:8){
    GAR[i,j]<-ifelse(GAR[i,j] > 4 || GAR[i,j]<0, NA, GAR[i,j])
  }
}

GAR<-GAR[complete.cases(GAR),]
table(GAR$GAR1)

for(i in 1:length(BAR$BAR1)){
  for(j in 1:6){
    BAR[i,j]<-ifelse(BAR[i,j] > 4 || BAR[i,j]<0, NA, BAR[i,j])
  }
}

BAR<-BAR[complete.cases(BAR),]
table(BAR$BAR1)

#Question 1:
#Calculate Cronbach's alpha for each scale
#alpha(ds, keys=c("DS4", "DS7", "DS12", "DS14"))

cacDS<-cronbach(DS[,1:14])
cacDS$alpha

cacLS<-cronbach(LS[,1:10])
cacLS$alpha

cacRR<-cronbach(RR[,1:10])
cacRR$alpha

cacGAR<-cronbach(GAR[,1:8])
cacGAR$alpha

cacBAR<-cronbach(BAR[,1:6])
cacBAR$alpha

#Question 2: Calculate cronbach's alpha by hand
#Scale 1: DS
covDS<-cov(DS[,1:14])
covDS<-round(covDS, 3)

for(i in 1:length(DS$DS1)){
  DS$DSC[i]=sum(DS[i,1:14])
}

var(DS$DSC)
sumVarDS<-sum(.317, .655, .661, .581, .277, .2, .512, .294, .149, .485, .338, .863, .503, .897)

cacDShand<-(14/13)*(1-(sumVarDS/as.numeric(var(DS$DSC))))
cacDS$alpha

#Scale 2: LS
covLS<-cov(LS[,1:10])
covLS<-round(covLS, 2)
covLS

for(i in 1:length(LS$LS1)){
  LS$LSC[i]=sum(as.numeric(LS[i,1:10]))
}

var(LS$LSC)

sumVarLS<-sum(.729, .875, 1.273, 1.27, .485, .427, .229, .499, .587, .508)

(10/9)*(1-(sumVarLS/as.numeric(var(LS$LSC))))
cacLS$alpha


#Scale 3: RR
covRR<-cov(RR[,1:10])
covRR<-round(covRR, 2)
covRR

for(i in 1:length(RR$RR1)){
  RR$RRC[i]=sum(RR[i,1:10])
}
head(RR)

var(RR$RRC)

sumVarRR<-sum(1.623, .724, 1.537, .695, .529, 1.081, .365, .591, .818, .573)

(10/9)*(1-(sumVarRR/as.numeric(var(RR$RRC))))
cacRR$alpha

#Scale 4: GAR
covGAR<-cov(GAR[,1:8])
covGAR<-round(covGAR, 2)
covGAR

for(i in 1:length(GAR$GAR1)){
  GAR$GARC[i]=sum(GAR[i,1:8])
}

var(GAR$GARC)
sumVarGAR<-sum(1.311, 1.531, 1.119, .937, 1.473, 1.337, 1.239, 1.239, 1.372)

(8/7)*(1-(sumVarGAR/as.numeric(var(GAR$GARC))))
cacGAR$alpha

#Scale 5: BAR
covBAR<-cov(BAR[,1:6])
covBAR<-round(covBAR, 2)
covBAR

for(i in 1:length(BAR$BAR1)){
  BAR$BARC[i]=sum(BAR[i,1:6])
}
head(BAR)

var(BAR$BARC)
sumVarBAR<-sum(.861, 1.003, 1.059, 1.329, 1.181, 1.338)

(6/5)*(1-(sumVarBAR/as.numeric(var(BAR$BARC))))
cacBAR$alpha


#Question 4:
#Split-half reliability of each scale. 

DShalf<-splitHalf(DS)
DShalf<-DShalf$meanr
spearman.brown(r.xx=as.numeric(DShalf), 2, "n")

LShalf<-splitHalf(LS)
LShalf<-LShalf$meanr
spearman.brown(r.xx=as.numeric(LShalf), 2, "n")

RRhalf<-splitHalf(RR)
RRhalf<-RRhalf$meanr
spearman.brown(r.xx=as.numeric(RRhalf), 2, "n")

GARhalf<-splitHalf(GAR)
GARhalf<-GARhalf$meanr
spearman.brown(r.xx=as.numeric(GARhalf), 2, "n")

BARhalf<-splitHalf(BAR)
BARhalf<-BARhalf$meanr
spearman.brown(r.xx=as.numeric(BARhalf), 2, "n")

#Question 5
factanalDS<-factanal(DS[,1:14], factors=1)

factanal(LS[,1:10], factors=1)

factanal(RR[,1:10], factors=1)

factanal(GAR[,1:8], factors=1)

factanal(BAR[,1:6], factors=1)

#Question 6
DSomega<-ci.reliability(data=DS[,1:14])
DSomega$est
DSomega$ci.lower
DSomega$ci.upper

LSomega<-ci.reliability(data=LS[,1:10])
LSomega$est
LSomega$ci.lower
LSomega$ci.upper

RRomega<-ci.reliability(data=RR[,1:10])
RRomega$est
RRomega$ci.lower
RRomega$ci.upper


GARomega<-ci.reliability(data=GAR[,1:8])
GARomega$est
GARomega$ci.lower
GARomega$ci.upper

BARomega<-ci.reliability(data=BAR[,1:6])
BARomega$est
BARomega$ci.lower
BARomega$ci.upper

#Question 7 
DShalf<-splitHalf(DS)
DShalf<-DShalf$meanr
spearman.brown(r.xx=as.numeric(DShalf), input=0.7, "r")
.36*14

DSNew<-DS[,-c(6, 9, 11, 14, 15,16,17)]
cacDSNew<-cronbach(DSNew)
cacDSNew
cacDS
head(DSNew)

DSNewhalf<-splitHalf(DSNew)
DSNewhalf<-DSNewhalf$meanr
spearman.brown(r.xx=as.numeric(DSNewhalf), input=2, "n")

alphaBestDS<-alpha(vars=c(DS1, DS2, DS3, DS4, DS5, DS6, DS7, DS8, DS9, DS10, DS11, DS12, DS13, DS14), dataFrame=DS)
alphaBestDS$alpha.if.removed

alphaBestLS<-alpha(vars=c(LS1, LS2, LS3, LS4, LS5, LS6, LS7, LS8, LS9, LS10), dataFrame=LS)
alphaBestLS$alpha.if.removed
alphaBestLS$alpha

LSNew<-LS[,c(1, 3:5, 8:10)]
cacLSNew<-cronbach(LSNew)
cacLSNew
cacLS


LSNewhalf<-splitHalf(LSNew)
LSNewhalf<-LSNewhalf$meanr
spearman.brown(r.xx=as.numeric(LSNewhalf), input=2, "n")

alphaLSNew<-alpha(vars=c(LS1, LS2, LS3, LS4, LS5, LS6, LS LS8, LS9, LS10), dataFrame=LSNew)

alphaItemsGAR<-alpha(vars=c(GAR1, GAR2, GAR3, GAR4, GAR5, GAR6, GAR7, GAR8), dataFrame=GAR)
alphaItemsGAR$alpha.if.removed

GARNew<-GAR[,-c(7:11)]
cacGARNew<-cronbach(GARNew)
cacGARNew$alpha

head(BAR)
alphaItemsBAR<-alpha(vars=c(BAR1, BAR2, BAR3, BAR4, BAR5, BAR6), dataFrame=BAR)
alphaItemsBAR$alpha.if.removed

BARNew<-BAR[,-c(3, 7:9)]
head(BARNew)
cacBARNew<-cronbach(BARNew)
cacBARNew$alpha

alphaItemsRR<-alpha(vars=c(RR1, RR2, RR3, RR4, RR5, RR6, RR7, RR8, RR9, RR10), dataFrame=RR)
alphaItemsRR$alpha.if.removed
cacRR
RRNew<-RR[,-c(3, 7:9)]
head(RRNew)
cacRRNew<-cronbach(RRNew)
cacRRNew$alpha

head(LSNew)
LSNewOmega<-ci.reliability(data=DSNew)
LSomega$est
LSomega$ci.lower
LSomega$ci.upper
