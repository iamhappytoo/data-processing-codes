###Question 1a###
autocf <- seq(-0.9,0.9,0.1)
ar.sim=array(0,dim=c(length(autocf),100))
for(i in 1:length(autocf)){
  ar.sim[i,1] <- runif(1,-10,10)
  for(j in 2:100){
    ar.sim[i,j] <- ar.sim[i,j-1]*autocf[i]+rnorm(1,0,1)
  }
  #ar.sim[i,] <- arima.sim(model=list(ar=c(autocf[i])),n=100)
}
###Question 1b and 1c###
effn <- rep(0,length(autocf))
varxbar=rep(0,length(autocf))
for(i in 1:length(autocf)){
  ar.sim=array(0,dim=c(10000,100))
  xbar=rep(0,10000)
  s2=rep(0,10000)
  for(j in 1:10000){
    ar.sim[j,1]=runif(1,-10,10)
    for(k in 2:100){
      ar.sim[j,k]=ar.sim[j,k-1]*autocf[i]+rnorm(1,0,1)
    }
    xbar[j]=mean(ar.sim[j,])
    s2[j]=var(ar.sim[j,])
  }
  varxbar[i]=var(xbar)
  s2avg=mean(s2)
  effn[i]=s2avg/varxbar[i]
}
plot(x=autocf,y=varxbar,type='l',lwd=3,xlab="autocorrelation coefficients",ylab="empirical variance")
title(lwd=2, main="empirical variance at different autocorrelation coefficients")
plot(x=autocf,y=effn,type='l',lwd=3,xlab="autocorrelation coefficients",ylab="effective sample size")
title(lwd=2, main="effective sample size at different autocorrelation coefficients")
###Question 1d###
sizen=rep(0,length(size))
for (i in 1:length(size)){
  sizen[i]=100*(1-autocf[i])/(1+autocf[i])
}
plot(effn,type='l',col="black",lwd=3)
lines(sizen,col="green",lwd=3)
legend("topright",c("My estimated","5.12 estimated"),col=c("black","green"),lwd=3)
title(lwd=2, main="Two estimated effective sample sizes")
##The two estimated effective sample sizes agrees very well.
###Question 2a###
acf=0.5
ar1=rep(0,100)
ar1[1]=rnorm(1,0,4/3)
for(i in 2:100){
  ar1[i]=ar1[i-1]*acf+rnorm(1,0,1)
}
###Question 2b###
##if using y=ax+b, a is the record index, then the a is the empirical trend we are looking for, according to OLS, a=cov(x,y)/var(x)
index=seq(1,100,1)
index=index-mean(index)
COV<- function(x,y) {
  if(length(x)!=length(y)) {stop('x must have the same length as y ')}
  x.bar <- mean(x)
  y.bar <- mean(y)
  N <- length(x)
  Cov <- (sum((x-x.bar)*(y-y.bar))) / (N-1)
  return(Cov)
}
##Calculate trend, named a
a <- COV(index,ar1)/var(index)
##Calculate the other parameter in OLS, named b, then y=ax+b
b <- mean(ar1)-a*mean(index)
estimator <- a*index+b
n=100
variance <- 1/(n-2)*sum((ar1-estimator)^2)/sum((index)*(index))
###Question 2c###
###lag-1 autocorrelation coefficient###
xminus <- ar1[1:99]
xplus <- ar1[2:100]
r1 <- sum((xminus-mean(xminus))*(xplus-mean(xplus)))/((sqrt(sum((xminus-mean(xminus))^2)))*(sqrt(sum((xminus-mean(xminus))^2))))
n1 <- n*(1-r1)/(1+r1)
expon <- 2/3*(1-n1/n) ###Now the Wilks 5.36 becomes L=(101-L)^(expon)
fL <- function(L,expon){
  n=100
  result <- L-(n-L+1)^expon
  return(result)
}
##Check the approximate location of root of fL##
L=seq(-100,100,0.1)
plot(L-(n-L+1)^expon,type='l') ##root of fL is between 5 and 7
eps=1e-7
max=max(L)
min=min(L)
tmp=0.5*(max+min)
check <- fL(tmp,expon)
while(abs(check)>eps){
  if(check<0){
    min=tmp
    tmp=0.5*(max+min)
  }else{
    max=tmp
    tmp=0.5*(max+min)
  }    
  check <- fL(tmp,expon)
}
L <- round(tmp) 
###Check the autocorrelation properties of the data 
acff <- acf(ar1)
r <- rep(0,floor(100/L)-1)
for(step in 1:floor((100/L))-1){
  B1 <- ar1[(L*(step-1)+1):(L*step)]
  B2 <- ar1[(L*step+1):(L*(step+1))]
  r[step]=sum((B1-mean(B1))*(B2-mean(B2)))/((sqrt(sum((B1-mean(B1))^2)))*(sqrt(sum((B1-mean(B1))^2))))
}
###block bootstrap for 10000 times without overlap###
nblock <- floor(100/L)
stdp <- sample(1:100,10000,replace=T)
trends=rep(0,10000)
for(i in 1:10000){
  newar=rep(0,100)
  newar[1:(100-stdp[i]+1)]=ar1[stdp[i]:100]
  newar[(100-stdp[i]+2):100]=ar1[1:(stdp[i]-1)]
  sel=sample(1:nblock,nblock,replace=T)
  boot=rep(0,96)
  for(j in 1:nblock){
    boot[((j-1)*L+1):(j*L)]=newar[((sel[j]-1)*L+1):(sel[j]*L)]
  }
  sel=sample(1:nblock,1)
  boot[(j*L+1):100]=newar[((sel-1)*L+1):((sel-1)*L+1+100-j*L-1)]
  trends[i] <- COV(index[1:100],boot)/var(index[1:100])
}
var(trends)
###block bootstrap for 10000 times with overlap###
trends=rep(0,10000)
nblock <- floor(100-L+1)
arrayblock <- array(0,dim=c(100-L+1,L))
for(i in 1:(100-L+1)){
  arrayblock[i,]=ar1[i:(i+L-1)]
}
for(i in 1:10000){
  selline <- sample(1:nblock,(floor(100/L)+1),replace=T)
  tmp=arrayblock[selline[1],]
  for(j in 2:length(selline)){
    tmp=c(tmp,arrayblock[selline[j],])
  }
  boot=tmp[1:100]
  trends[i] <- COV(index[1:100],boot)/var(index[1:100])
}
var(trends)
###Check the bootstrap method using r built-in package###
library(boot)
trend <- function(data){
  index <- seq(1,100,1)
  mm <- COV(index,data)/var(index)
  return(mm)
}
test <- tsboot(ar1,trend,R=10000,l=L,sim="fixed",endcorr=FALSE,n.sim=100,orig.t=FALSE)
var(test$t)
###So I am going to choose the overlapped method of bootstrap###
variance_c <- var(trends)
###Question 2d###
tt=rep(0,10000) ##This is used for storing the trend for random datasets
for(i in 1:10000){
  datai=rep(0,100)
  datai[1]=rnorm(1,0,4/3)
  for(j in 2:100){
    datai[j]=datai[j-1]*0.5+rnorm(1,0,1)
  }
  tt[i]=trend(datai)
}
var(tt)
variance_d <- var(tt)
###CHeck the results using built-in function of AR(1)
mmm=rep(0,10000)
tt1=rep(0,10000)
for(i in 1:10000){
  datai <- arima.sim(model=list(ar=0.5),n=100)
  tt1[i]=trend(datai)
  mmm[i]=datai[1]
}
var(tt1)

###Question 3a###
setwd("C:/Users/BANZH/Downloads/")
Tmax=read.table("USW00023174_TMAX_clean.csv",sep=",",header=T)
Tmin=read.table("USW00023174_TMIN_clean.csv",sep=",",header=T)
ind1 <- which(Tmax[,5]!=-999.9)
ind2 <- which(Tmin[,5]!=-999.9)
ind <- intersect(ind1,ind2)
Tmax=Tmax[ind,]
Tmin=Tmin[ind,]
##Calculate summer-season CDD, first select summer-season
Tmax_JJA <- Tmax[which((Tmax[,2]==6)|(Tmax[,2]==7)|(Tmax[,2]==8)),5]
Tmin_JJA <- Tmin[which((Tmin[,2]==6)|(Tmin[,2]==7)|(Tmin[,2]==8)),5]
Date_JJA <- Tmax[which((Tmax[,2]==6)|(Tmax[,2]==7)|(Tmax[,2]==8)),1:3]
##Calculate winter-season HDD, first select winter-season
Tmax_DJF <- Tmax[which((Tmax[,2]==12)|(Tmax[,2]==1)|(Tmax[,2]==2)),5]
Tmin_DJF <- Tmin[which((Tmin[,2]==12)|(Tmin[,2]==1)|(Tmin[,2]==2)),5]
Date_DJF <- Tmax[which((Tmax[,2]==12)|(Tmax[,2]==1)|(Tmax[,2]==2)),1:3]
CDD=rep(0,length(Tmax_JJA))
HDD=rep(0,length(Tmax_DJF))
for(i in 1:length(CDD)){
  CDD[i]=max(0.5*(Tmax_JJA[i]+Tmin_JJA[i])-20,0)
}
for(i in 1:length(HDD)){
  HDD[i]=max(15-0.5*(Tmax_DJF[i]+Tmin_DJF[i]),0)
}
CDD_wd <- cbind(Date_JJA,CDD)
HDD_wd <- cbind(Date_DJF,HDD)
lJJA <- length(unique(Date_JJA[,1]))-1
lDJF <- length(unique(Date_DJF[,1]))-1  
seas_CDD <- rep(0,lJJA)
seas_HDD <- rep(0,lDJF)
for(i in 1:lJJA){
  seas_CDD[i]=sum(CDD_wd[which(CDD_wd[,1]==unique(Date[,1])[i+1]),4])  
}
for(i in 1:lDJF){
  D <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i]))*100+12),]
  J <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i+1])*100+1)),]
  F <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i+1])*100+2)),]
  seas_HDD[i]=sum(c(D[,4],J[,4],F[,4]))
}
plot(x=unique(Date_JJA[,1])[2:length(unique(Date_JJA[,1]))],y=seas_CDD,type='l',lwd=2,ylim=c(0,500),xlab="year",ylab="CDD or HDD")
lines(x=unique(Date_DJF[,1])[2:length(unique(Date_DJF[,1]))],y=seas_HDD,col="red",lwd=2)
legend("topright",c("JJA_CDD","DJF_HDD"),col=c("black","red"),lwd=2)
title(lwd=2, main="seasonal sum of CDD and HDD")

###Question 3b###
###missing data makes the sum of CDD and HDD in seasonal calculation inaccurate. Instead I calculate average, and 
###Delete the days either contains missing Tmax or Tmin.
for(i in 1:lJJA){
  seas_CDD[i]=mean(CDD_wd[which(CDD_wd[,1]==unique(Date[,1])[i]),4])  
}
for(i in 1:lDJF){
  D <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i]))*100+12),]
  J <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i+1])*100+1)),]
  F <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i+1])*100+2)),]
  seas_HDD[i]=mean(c(D[,4],J[,4],F[,4]))
}
plot(x=unique(Date_JJA[,1])[2:length(unique(Date_JJA[,1]))],y=seas_CDD,type='l',lwd=2,ylim=c(0,5),xlab="year",ylab="CDD or HDD")
lines(x=unique(Date_DJF[,1])[2:length(unique(Date_DJF[,1]))],y=seas_HDD,col="red",lwd=2)
legend("topright",c("JJA_CDD","DJF_HDD"),col=c("black","red"),lwd=2)
title(lwd=2, main="seasonal mean of CDD and HDD")
###Question 3c###
par(mfrow=c(1,3))
hist(seas_CDD)
dens_CDD <- density(seas_CDD,adjust=1,kernel=c("gaussian"))
hist(seas_HDD)
dens_HDD <- density(seas_HDD,adjust=1,kernel=c("gaussian"))
plot(dens_CDD,lwd=3,main="kernel density plot of seasonal CDD and HDD")
lines(dens_HDD,lwd=3,col="red")
legend("topright",c("CDD","HDD"),col=c("black","red"),lwd=3)
###Question 3d###
par(mfrow=c(1,1))
seas_wd_CDD=cbind(unique(Date[,1])[2:length(unique(Date[,1]))],seas_CDD)
seas_wd_HDD=cbind(unique(Date[,1])[2:length(unique(Date[,1]))],seas_HDD)
bf80_seas_CDD=seas_wd_CDD[which(seas_wd_CDD[,1]<1980),]
af80_seas_CDD=seas_wd_CDD[which(seas_wd_CDD[,1]>1980),]
bf80_seas_HDD=seas_wd_HDD[which(seas_wd_HDD[,1]<1980),]
af80_seas_HDD=seas_wd_HDD[which(seas_wd_HDD[,1]<1980),]
##check for CDD if their mean are the same##
mean1 <- mean(bf80_seas_CDD[,2])
mean2 <- mean(af80_seas_CDD[,2])
denom <- sqrt(var(bf80_seas_CDD[,2])/length(bf80_seas_CDD[,2])+var(af80_seas_CDD[,2])/length(af80_seas_CDD[,2]))
Z <- (mean1-mean2)/denom
p=pnorm(Z,sd=1,mean=0)
print(paste0("p(z<Z)=",p,">0.025"))
##CDD has no trend##
##check for HDD if their mean are the same##
mean1 <- mean(bf80_seas_HDD[,2])
mean2 <- mean(af80_seas_HDD[,2])
denom <- sqrt(var(bf80_seas_HDD[,2])/length(bf80_seas_HDD[,2])+var(af80_seas_HDD[,2])/length(af80_seas_HDD[,2]))
Z <- (mean1-mean2)/denom
p=pnorm(Z,sd=1,mean=0)
print(paste0("p(z<Z)=",p,">0.025"))
##HDD has no trend##
###Question 3e###
par(mfrow=c(1,2))
###Fit regression model to HDD and CDD
len=length(unique(Date[,1]))
predictor <- unique(Date[,1])[2:len]-mean(unique(Date[,1])[2:len])
Y_HDD <- seas_HDD
##Calculate trend, named a
a_HDD <- COV(predictor,Y_HDD)/var(predictor)
##Calculate the other parameter in OLS, named b, then y=ax+b
b_HDD <- mean(Y_HDD)-a*mean(predictor)
estimator_HDD <- a_HDD*predictor+b_HDD
plot(Y_HDD,col="blue",type="p")
lines(estimator_HDD,lwd=3,col="red")
Y_CDD <- seas_CDD
##Calculate trend, named a
a_CDD <- COV(predictor,Y_CDD)/var(predictor)
##Calculate the other parameter in OLS, named b, then y=ax+b
b_CDD <- mean(Y_CDD)-a*mean(predictor)
estimator_CDD <- a_CDD*predictor+b_CDD
plot(Y_CDD,col="blue",type="p")
lines(estimator_CDD,lwd=3,col="red")
###Check if there are significant trends in both CDD and HDD, build statistics to check the significance
n=length(Y_HDD)
A_HDD=(a_HDD-0)/sqrt(1/(n-2)*sum((Y_HDD-estimator_HDD)^2)/sum((index-mean(index))*(index-mean(index))))
n=length(Y_CDD)
A_CDD=(a_CDD-0)/sqrt(1/(n-2)*sum((Y_CDD-estimator_CDD)^2)/sum((index-mean(index))*(index-mean(index))))
hddp=pnorm(A_HDD,sd=1,mean=0)
print(paste0("p(z<A_HDD)=",hddp,"<0.025")) ##has trend
cddp=1-pnorm(A_CDD,sd=1,mean=0)
print(paste0("p(z>A_CDD)=",cddp,"<0.025")) ##has trend
##So there are significant trends.

###Question 4###
#Choose NINO3.4, NINO4 (1870-now, HadISST1)
#SOI(1866-now,Jones), 
#PDO(Mantua, U. Washington)
#NAO Gibraltar-Stykkisholmur
#AMO derived from HadSST SST SST EQ-60°N, 0°-80°W minus SST 60°S-60°N
#CRUTEM4 global land temperature
###Download and clean data###
nyr=2018-1944+1
month=rep(seq(1,12),nyr)
NINO3.4=read.table("NINO3.4.txt",skip=40)
NINO3.4=NINO3.4[which(NINO3.4[,1]>=1944),]
year=c(floor(NINO3.4[,1]),2018)
NINO3.4=cbind(year[1:length(NINO3.4[,2])],month[1:length(NINO3.4[,2])],NINO3.4[,2])
NINO4=read.table("NINO4.txt",skip=40)
NINO4=NINO4[which(NINO4[,1]>=1944),]
NINO4=cbind(year[1:length(NINO4[,2])],month[1:length(NINO4[,2])],NINO4[,2])
SOI=read.table("SOI Jones.txt",skip=40)
getindex <- function(index){
  index1=index[which(index[,1]>=1944),]
  tmp=rep(0,12*nyr)
  for(i in 1:nyr){
    tmp[((i-1)*12+1):(i*12)]=as.numeric(index1[i,2:13])
  }
  index2=cbind(year[1:length(tmp)],month[1:length(tmp)],tmp)
}
SOI=getindex(SOI)
PDO=read.table("PDO.txt",skip=40)
PDO=getindex(PDO)
NAO=read.table("NAO.txt",skip=40)
NAO=getindex(NAO)
AMO=read.table("AMO.txt",skip=40)
AMO=getindex(AMO)
landT=read.table("landtemp.txt",skip=40)
landT=landT[which(landT[,1]>=1944),]
###create predictors for summer CDD###
getMAM <- function(index){
  tmp=index[which((index[,2]==3)|(index[,2]==4)|(index[,2]==5)),]
  l=length(unique(tmp[,1]))-1
  out=rep(0,l)
  for(i in 1:l){
    out[i]=mean(tmp[which(tmp[,1]==unique(tmp[,1])[i+1]),3])
  }
  out1 <- cbind(unique(tmp[,1])[2:(l+1)],out)
  return(out1)
}
getSON <- function(index){
  index=index[which(index[,3]!=-999.9),]
  tmp=index[which((index[,2]==9)|(index[,2]==10)|(index[,2]==11)),]
  l=length(unique(tmp[,1]))-1
  out=rep(0,l)
  for(i in 1:l){
    out[i]=mean(tmp[which(tmp[,1]==unique(tmp[,1])[i]),3])
  }
  out1 <- cbind(unique(tmp[,1])[2:(l+1)],out)
  return(out1)
}
#1.Nino3.4 averaged over MAM
NINO3.4MAM=getMAM(NINO3.4)
#2.SOI averaged over MAM
SOIMAM=getMAM(SOI)
#3.PDO averaged over MAM
PDOMAM=getMAM(PDO)
#4.landT averaged over MAM
landTMAM=getMAM(landT)
NINO4MAM=getMAM(NINO4)
NAOMAM=getMAM(NAO)
AMOMAM=getMAM(AMO)
###create predictors for winter HDD###
#1.Nino4 averaged over SON
NINO4SON=getSON(NINO4)
#2.NAO averaged over SON
NAOSON=getSON(NAO)
#3.AMO averaged over SON
AMOSON=getSON(AMO)
#4.landT averaged over MAM
landTSON=getSON(landT)
seas_HDD=cbind(landTSON[,1],seas_HDD)
seas_CDD=cbind(landTSON[,1],seas_CDD)
par(mfrow=c(2,4))
plot(NINO3.4MAM,type='l',xlab="year", ylab="NINO3.4-MAM", lwd=2, main="NINO3.4-MAM")
plot(SOIMAM,type='l',xlab="year", ylab="SOI-MAM", lwd=2, main="SOI-MAM")
plot(PDOMAM,type='l',xlab="year", ylab="PDO-MAM", lwd=2, main="PDO-MAM")
plot(landTMAM,type='l',xlab="year", ylab="landT-MAM", lwd=2, main="landT-MAM")
plot(NINO4SON,type='l',xlab="year", ylab="NINO4-SON", lwd=2, main="NINO4-SON")
plot(NAOSON,type='l',xlab="year", ylab="NAO-SON", lwd=2, main="NAO-SON")
plot(AMOSON,type='l',xlab="year", ylab="AMO-SON", lwd=2, main="AMO-SON")
plot(landTSON,type='l',xlab="year", ylab="landT-SON", lwd=2, main="landT-SON")
###Question 4b###
MAM <- cbind(NINO3.4MAM[,2],SOIMAM[,2], PDOMAM[,2],landTMAM[,2])
k=ncol(MAM)
n=nrow(MAM)
cov(MAM)
MAM_mean <- matrix(data=1, nrow=n) %*% cbind(mean(NINO3.4MAM[,2]),mean(SOIMAM[,2]),mean(PDOMAM[,2]),mean(landTMAM[,2])) 
DMAM=MAM-MAM_mean
CMAM=(n-1)^-1*t(DMAM)%*%DMAM
SON <- cbind(NINO4SON[,2],NAOSON[,2],AMOSON[,2],landTSON[,2])
k=ncol(SON)
n=nrow(SON)
cov(SON)
SON_mean <- matrix(data=1, nrow=n) %*% cbind(mean(NINO4SON[,2]),mean(NAOSON[,2]),mean(AMOSON[,2]),mean(landTSON[,2])) 
DSON=SON-SON_mean
CSON=(n-1)^-1*t(DSON)%*%DSON
nameSON=c("NINO4-SON","NAO-SON","AMO-SON","landT-SON")
nameMAM=c("NINO3.4-MAM","SOI-MAM","PDO-MAM","landT-MAM")
colnames(CSON)=nameSON
rownames(CSON)=nameSON
colnames(CMAM)=nameMAM
rownames(CMAM)=nameMAM
CMAM
CSON
##There is no significant colinearity between my predictors. 
#heatmap(CMAM,Colv=NA,Rowv=NA,scale="column",cexRow=1,cexCol = 1,RowSideColors=heat.colors(4))
#heatmap(CSON,Colv=NA,Rowv=NA,scale="column",cexRow=1,cexCol=1)
###Question 4c###
par(mfrow=c(1,1))
###For CDD###
df_CDD <- as.data.frame(cbind(seas_CDD[,2],NINO3.4MAM,SOIMAM[,2],PDOMAM[,2],landTMAM[,2]))
df_CDD <- df_CDD[which(df_CDD[,2]<2000),]  ##withheld year2000 and after year 2000
colnames(df_CDD) <- c("CDD","time","NINO3.4","SOI","PDO","landT")
md1 <-lm(CDD~NINO3.4,data=df_CDD)
summary(md1)
md2 <-lm(CDD~time,data=df_CDD)
summary(md2)
md3 <-lm(CDD~SOI,data=df_CDD)
summary(md3)
md4 <-lm(CDD~PDO,data=df_CDD)
summary(md4)
md5 <-lm(CDD~landT,data=df_CDD)
summary(md5)
###The time predictor has the smallest MSE, largest R2 and the largest F ratio, so choose time as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <- lm(CDD~time+NINO3.4,data=df_CDD)
md2 <- lm(CDD~time+SOI,data=df_CDD)
md3 <- lm(CDD~time+PDO,data=df_CDD)
md4 <- lm(CDD~time+landT,data=df_CDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
###The landT predictor has the smallest MSE, largest R2 and the highest F ratio this time, so choose landT as the second predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~time+landT+NINO3.4,data=df_CDD)
md2 <-lm(CDD~time+landT+SOI,data=df_CDD)
md3 <-lm(CDD~time+landT+PDO,data=df_CDD)
summary(md1)
summary(md2)
summary(md3)
###The NINO3.4 predictor has the smallest MSE, largest R2 and highest F ratio this time, so we choose NINO3.4 as the third predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~time+landT+NINO3.4+SOI,data=df_CDD)
md2 <-lm(CDD~time+landT+NINO3.4+PDO,data=df_CDD)
summary(md1)
summary(md2)
###The SOI predictor has smaller MSE, larger R2 and higher F ratio, so we choose SOI as the fourth predictor.
###The remaining predictor cannot reduce the R2 by an amount larger than 0.05%, so we stop adding predictor here, use md1 as the final model
plot(df_CDD$CDD)
lines(md1$fitted.values,col=3,lwd=3)
summary(md1)
###Uncertainty in my parameters can be seen in the Coefficients part of the summary. 

###For HDD###
df_HDD <- as.data.frame(cbind(seas_HDD[,2],NINO4SON,NAOSON[,2],AMOSON[,2],landTSON[,2]))
df_HDD <- df_HDD[which(df_HDD[,2]<2000),]  ##withheld year2000 and after year 2000
colnames(df_HDD) <- c("HDD","time","NINO4","NAO","AMO","landT")
md1 <-lm(HDD~NINO4,data=df_HDD)
summary(md1)
md2 <-lm(HDD~time,data=df_HDD)
summary(md2)
md3 <-lm(HDD~NAO,data=df_HDD)
summary(md3)
md4 <-lm(HDD~AMO,data=df_HDD)
summary(md4)
md5 <-lm(HDD~landT,data=df_HDD)
summary(md5)
###The time predictor has the smallest MSE, largest R2 and the largest F ratio, so choose time as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <- lm(HDD~time+NINO4,data=df_HDD)
md2 <- lm(HDD~time+NAO,data=df_HDD)
md3 <- lm(HDD~time+AMO,data=df_HDD)
md4 <- lm(HDD~time+landT,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
###The NINO4 predictor has the smallest MSE, largest R2 and the highest F ratio this time, so choose NINO4 as the second predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~time+NINO4+NAO,data=df_HDD)
md2 <-lm(HDD~time+NINO4+AMO,data=df_HDD)
md3 <-lm(HDD~time+NINO4+landT,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
###The landT predictor has the smallest MSE, largest R2 and highest F ratio this time, so we choose landT as the third predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~time+NINO4+landT+NAO,data=df_HDD)
md2 <-lm(HDD~time+NINO4+landT+AMO,data=df_HDD)
summary(md1)
summary(md2)
###The NAO predictor has smaller MSE, larger R2 and higher F ratio, so we choose NAO as the fourth predictor.
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~time+NINO4+landT+NAO+AMO,data=df_HDD)
summary(md1)
plot(df_HDD$HDD)
lines(md1$fitted.values,col=3,lwd=3)
summary(md1)
###Uncertainty in my parameters can be seen in the Coefficients part of the summary. 
###Question 4d###
###Add MAM average Tmin, MAM average Tmax in Los Angeles to CDD predictor, and SON average Tmin, SON average Tmax in Los Angeles to HDD predictor
getMAM <- function(index){
  tmp=index[which((index[,2]==3)|(index[,2]==4)|(index[,2]==5)),]
  l=length(unique(tmp[,1]))
  out=rep(0,l)
  for(i in 1:l){
    out[i]=mean(tmp[which(tmp[,1]==unique(tmp[,1])[i]),3])
  }
  out1 <- cbind(unique(tmp[,1]),out)
  return(out1)
}
getSON <- function(index){
  index=index[which(index[,3]!=-999.9),]
  tmp=index[which((index[,2]==9)|(index[,2]==10)|(index[,2]==11)),]
  l=length(unique(tmp[,1]))-1
  out=rep(0,l)
  for(i in 1:l){
    out[i]=mean(tmp[which(tmp[,1]==unique(tmp[,1])[i]),3])
  }
  out1 <- cbind(unique(tmp[,1])[2:(l+1)],out)
  return(out1)
}
Tmin_MAM=getMAM(Tmin[c(1,2,5)])
Tmax_MAM=getMAM(Tmax[c(1,2,5)])
Tmin_SON=getSON(Tmin[c(1,2,5)])
Tmax_SON=getSON(Tmax[c(1,2,5)])
###Do the stepwise again for both CDD and HDD
###For CDD###
df_CDD <- as.data.frame(cbind(seas_CDD[,2],NINO3.4MAM,SOIMAM[,2],PDOMAM[,2],landTMAM[,2],Tmin_MAM[,2],Tmax_MAM[,2]))
df_CDD <- df_CDD[which(df_CDD[,2]<2000),]  ##withheld year2000 and after year 2000
colnames(df_CDD) <- c("CDD","time","NINO3.4","SOI","PDO","landT","Tmin","Tmax")
md1 <-lm(CDD~NINO3.4,data=df_CDD)
summary(md1)
md2 <-lm(CDD~time,data=df_CDD)
summary(md2)
md3 <-lm(CDD~SOI,data=df_CDD)
summary(md3)
md4 <-lm(CDD~PDO,data=df_CDD)
summary(md4)
md5 <-lm(CDD~landT,data=df_CDD)
summary(md5)
md6 <-lm(CDD~Tmin,data=df_CDD)
summary(md6)
md7 <-lm(CDD~Tmax,data=df_CDD)
summary(md7)
###The Tmin predictor has the smallest MSE, largest R2 and the largest F ratio, so choose Tmin as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~Tmin+NINO3.4,data=df_CDD)
md2 <-lm(CDD~Tmin+time,data=df_CDD)
md3 <-lm(CDD~Tmin+SOI,data=df_CDD)
md4 <-lm(CDD~Tmin+PDO,data=df_CDD)
md5 <-lm(CDD~Tmin+landT,data=df_CDD)
md6 <-lm(CDD~Tmin+Tmax,data=df_CDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
summary(md5)
summary(md6)
###The SOI predictor has the smallest MSE, largest R2 and the largest F ratio, so choose SOI as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~Tmin+SOI+NINO3.4,data=df_CDD)
md2 <-lm(CDD~Tmin+SOI+time,data=df_CDD)
md3 <-lm(CDD~Tmin+SOI+PDO,data=df_CDD)
md4 <-lm(CDD~Tmin+SOI+landT,data=df_CDD)
md5 <-lm(CDD~Tmin+SOI+Tmax,data=df_CDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
summary(md5)
###The Tmax predictor has the smallest MSE, largest R2 and the largest F ratio, so choose Tmax as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~Tmin+SOI+Tmax+NINO3.4,data=df_CDD)
md2 <-lm(CDD~Tmin+SOI+Tmax+time,data=df_CDD)
md3 <-lm(CDD~Tmin+SOI+Tmax+PDO,data=df_CDD)
md4 <-lm(CDD~Tmin+SOI+Tmax+landT,data=df_CDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
###The landT predictor has the smallest MSE, largest R2 and the largest F ratio, so choose landT as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~Tmin+SOI+Tmax+landT+NINO3.4,data=df_CDD)
md2 <-lm(CDD~Tmin+SOI+Tmax+landT+time,data=df_CDD)
md3 <-lm(CDD~Tmin+SOI+Tmax+landT+PDO,data=df_CDD)
summary(md1)
summary(md2)
summary(md3)
###The time predictor has the smallest MSE, largest R2 and the largest F ratio, so choose time as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(CDD~Tmin+SOI+Tmax+landT+time+NINO3.4,data=df_CDD)
md2 <-lm(CDD~Tmin+SOI+Tmax+landT+time+PDO,data=df_CDD)
summary(md1)
summary(md2)
###The PDO predictor has the smallest MSE, largest R2 and the largest F ratio, so choose PDO as the first predictor
###The remaining predictor cannot reduce the R2 by an amount larger than 0.05%, so we choose to stop to add predictors
plot(df_CDD$CDD)
lines(md2$fitted.values,col=5,lwd=3)
###The NINO3.4 is no longer included now, choose md2 as the final model
mdCDD <- md2
###For HDD###
df_HDD <- as.data.frame(cbind(seas_HDD[,2],NINO4SON,NAOSON[,2],AMOSON[,2],landTSON[,2],Tmin_SON[,2],Tmax_SON[,2]))
df_HDD <- df_HDD[which(df_HDD[,2]<2000),]  ##withheld year2000 and after year 2000
colnames(df_HDD) <- c("HDD","time","NINO4","NAO","AMO","landT","Tmin","Tmax")
md1 <-lm(HDD~time,data=df_HDD)
md2 <-lm(HDD~NINO4,data=df_HDD)
md3 <-lm(HDD~NAO,data=df_HDD)
md4 <-lm(HDD~AMO,data=df_HDD)
md5 <-lm(HDD~landT,data=df_HDD)
md6 <-lm(HDD~Tmin,data=df_HDD)
md7 <-lm(HDD~Tmax,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
summary(md5)
summary(md6)
summary(md7)
###The Tmin predictor has the smallest MSE, largest R2 and the largest F ratio, so choose Tmin as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~Tmin+time,data=df_HDD)
md2 <-lm(HDD~Tmin+NINO4,data=df_HDD)
md3 <-lm(HDD~Tmin+NAO,data=df_HDD)
md4 <-lm(HDD~Tmin+AMO,data=df_HDD)
md5 <-lm(HDD~Tmin+landT,data=df_HDD)
md6 <-lm(HDD~Tmin+Tmax,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
summary(md5)
summary(md6)
###The time predictor has the smallest MSE, largest R2 and the largest F ratio, so choose time as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~Tmin+time+NINO4,data=df_HDD)
md2 <-lm(HDD~Tmin+time+NAO,data=df_HDD)
md3 <-lm(HDD~Tmin+time+AMO,data=df_HDD)
md4 <-lm(HDD~Tmin+time+landT,data=df_HDD)
md5 <-lm(HDD~Tmin+time+Tmax,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
summary(md5)
###The NINO4 predictor has the smallest MSE, largest R2 and the largest F ratio, so choose NINO4 as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~Tmin+time+NINO4+NAO,data=df_HDD)
md2 <-lm(HDD~Tmin+time+NINO4+AMO,data=df_HDD)
md3 <-lm(HDD~Tmin+time+NINO4+landT,data=df_HDD)
md4 <-lm(HDD~Tmin+time+NINO4+Tmax,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
summary(md4)
###The landT predictor has the smallest MSE, largest R2 and the largest F ratio, so choose landT as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~Tmin+time+NINO4+landT+AMO,data=df_HDD)
md2 <-lm(HDD~Tmin+time+NINO4+landT+NAO,data=df_HDD)
md3 <-lm(HDD~Tmin+time+NINO4+landT+Tmax,data=df_HDD)
summary(md1)
summary(md2)
summary(md3)
###The NAO predictor has the smallest MSE, largest R2 and the largest F ratio, so choose NAO as the first predictor
###The remaining predictor would reduce the R2 by an amount larger than 0.05%, so we choose to continue adding predictors
md1 <-lm(HDD~Tmin+time+NINO4+landT+NAO+AMO,data=df_HDD)
md2 <-lm(HDD~Tmin+time+NINO4+landT+NAO+Tmax,data=df_HDD)
summary(md1)
summary(md2)
###The AMO predictor has the smallest MSE, largest R2 and the largest F ratio, so choose AMO as the first predictor
###The remaining predictor cannot reduce the R2 by an amount larger than 0.05%, so we choose to stop to add predictors
###The Tmax is no longer included now, choose md1 as the final model
mdHDD <-md1
###Question 4e###
###For CDD###
df_CDD <- as.data.frame(cbind(seas_CDD[,2],NINO3.4MAM,SOIMAM[,2],PDOMAM[,2],landTMAM[,2],Tmin_MAM[,2],Tmax_MAM[,2]))
summ <-summary(mdCDD)
colnames(df_CDD) <- c("CDD","time","NINO3.4","SOI","PDO","landT","Tmin","Tmax")
b=summ$coefficients[,1]
sd=summ$coefficients[,2]
tval=summ$coefficients[,3]
cf_lower=b-sd
cf_upper=b+sd#confint(mdCDD)[,2]
checkCDD <-seas_CDD[which(seas_CDD[,1]>=2000),2]
pdTminCDD=Tmin_MAM[which(Tmin_MAM[,1]>=2000),2]
pdTmaxCDD=Tmax_MAM[which(Tmax_MAM[,1]>=2000),2]
pdtimeCDD=Tmin_MAM[which(Tmin_MAM[,1]>=2000),1]
pdSOICDD=SOIMAM[which(SOIMAM[,1]>=2000),2]
pdlandTCDD=landTMAM[which(landTMAM[,1]>=2000),2]
pdPDOCDD=PDOMAM[which(PDOMAM[,1]>=2000),2]
pdfactorsCDD=as.data.frame(cbind(pdTminCDD,pdSOICDD,pdTmaxCDD,pdlandTCDD,pdtimeCDD,pdPDOCDD))
pd <- function(b,pdfactor){
  pd=b[1]+b[2]*pdfactor[,1]+b[3]*pdfactor[,2]+b[4]*pdfactor[,3]+b[5]*pdfactor[,4]+b[6]*pdfactor[,5]+b[7]*pdfactor[,6]
}
pdmdCDD=pd(b,pdfactorsCDD)
pdmd_lowerCDD=pd(cf_lower,pdfactorsCDD)
pdmd_upperCDD=pd(cf_upper,pdfactorsCDD)
plot(checkCDD,ylim=c(-45,45))
lines(pdmdCDD,col=3,lwd=3)
lines(pdmd_lowerCDD,col=5,lwd=3)
lines(pdmd_upperCDD,col=5,lwd=3)
colnames(pdfactorsCDD) <- c("Tmin","SOI","Tmax","landT","time","PDO")
pdCDD <- predict(mdCDD,df_CDD[which(df_CDD[,2]>=2000),])#pdfactors)

plot(pdCDD,type='l')
points(df_CDD$CDD)

###For HDD###
df_HDD <- as.data.frame(cbind(seas_HDD[,2],NINO4SON,NAOSON[,2],AMOSON[,2],landTSON[,2],Tmin_SON[,2],Tmax_SON[,2]))
summ <-summary(mdHDD)
colnames(df_HDD) <- c("HDD","time","NINO4","NAO","AMO","landT","Tmin","Tmax")
b=summ$coefficients[,1]
sd=summ$coefficients[,2]
tval=summ$coefficients[,3]
cf_lower=b-sd
cf_upper=b+sd#confint(mdHDD)[,2]
checkHDD <-seas_HDD[which(seas_HDD[,1]>=2000),2]
pdTminHDD=Tmin_SON[which(Tmin_SON[,1]>=2000),2]
pdAMOHDD=AMOSON[which(AMOSON[,1]>=2000),2]
pdtimeHDD=Tmin_SON[which(Tmin_SON[,1]>=2000),1]
pdNAOHDD=NAOSON[which(NAOSON[,1]>=2000),2]
pdlandTHDD=landTSON[which(landTSON[,1]>=2000),2]
pdNINO4HDD=NINO4SON[which(NINO4SON[,1]>=2000),2]
pdfactorsHDD=as.data.frame(cbind(pdTminHDD,pdtimeHDD,pdNINO4HDD,pdlandTHDD,pdNAOHDD,pdAMOHDD))
pd <- function(b,pdfactor){
  pd=b[1]+b[2]*pdfactor[,1]+b[3]*pdfactor[,2]+b[4]*pdfactor[,3]+b[5]*pdfactor[,4]+b[6]*pdfactor[,5]+b[7]*pdfactor[,6]
}
pdmdHDD=pd(b,pdfactorsHDD)
pdmd_lowerHDD=pd(cf_lower,pdfactorsHDD)
pdmd_upperHDD=pd(cf_upper,pdfactorsHDD)
plot(checkHDD,ylim=c(-45,50))
lines(pdmdHDD,col=3,lwd=3)
lines(pdmd_lowerHDD,col=5,lwd=3)
lines(pdmd_upperHDD,col=5,lwd=3)
colnames(pdfactorsHDD) <- c("Tmin","SOI","Tmax","landT","time","PDO")
pdHDD <- predict(mdHDD,df_HDD[which(df_HDD[,2]>=2000),])#pdfactors)

plot(pdHDD,type='l')
points(df_HDD$HDD)
###Question 4f###
###For CDD###
#1
df_CDDshort=df_CDD[which(df_CDD[,2]<2000),]
pdCDDtrain=predict(mdCDD,df_CDDshort)
epsiCDD=df_CDDshort[,1]-pdCDDtrain
paramCDD=array(0,dim=c(10000,7))
for(i in 1:10000){
  CDDtraintmp=pdCDDtrain+sample(epsiCDD,size=length(epsiCDD),replace=TRUE)
  df_CDDshort[,1]=CDDtraintmp
  md <-lm(CDD~Tmin+SOI+Tmax+landT+time+PDO,data=df_CDDshort)
  paramCDD[i,]=md$coefficients
}
par(mfrow=c(2,4))
hist(paramCDD[,1],prob=TRUE,xlab="Intercept",main="distribution of intercept",sub=paste0("sd=",sd(paramCDD[,1])))
abline(v=confint(mdCDD)[1,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[1,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,1]),col=1,lwd=3)
hist(paramCDD[,2],prob=TRUE,xlab="Tmin",main="distribution of Tmin",sub=paste0("sd=",sd(paramCDD[,2])))
abline(v=confint(mdCDD)[2,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[2,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,2]),col=2,lwd=3)
hist(paramCDD[,3],prob=TRUE,xlab="SOI",main="distribution of SOI",sub=paste0("sd=",sd(paramCDD[,3])))
abline(v=confint(mdCDD)[3,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[3,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,3]),col=3,lwd=3)
hist(paramCDD[,4],prob=TRUE,xlab="Tmax",main="distribution of Tmax",sub=paste0("sd=",sd(paramCDD[,4])))
abline(v=confint(mdCDD)[4,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[4,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,4]),col=4,lwd=3)
hist(paramCDD[,5],prob=TRUE,xlab="landT",main="distribution of landT",sub=paste0("sd=",sd(paramCDD[,5])))
abline(v=confint(mdCDD)[5,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[5,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,5]),col=5,lwd=3)
hist(paramCDD[,6],prob=TRUE,xlab="time",main="distribution of time",sub=paste0("sd=",sd(paramCDD[,6])))
abline(v=confint(mdCDD)[6,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[6,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,6]),col=6,lwd=3)
hist(paramCDD[,7],prob=TRUE,xlab="PDO",main="distribution of PDO",sub=paste0("sd=",sd(paramCDD[,7])))
abline(v=confint(mdCDD)[7,1],lwd=3,col="red",lty=2)
abline(v=confint(mdCDD)[7,2],lwd=3,col="red",lty=2)
lines(density(paramCDD[,7]),col=8,lwd=3)
###For HDD###
#1
df_HDDshort=df_HDD[which(df_HDD[,2]<2000),]
pdHDDtrain=predict(mdHDD,df_HDDshort)
epsiHDD=df_HDDshort[,1]-pdHDDtrain
paramHDD=array(0,dim=c(10000,7))
for(i in 1:10000){
  HDDtraintmp=pdHDDtrain+sample(epsiHDD,size=length(epsiHDD),replace=TRUE)
  df_HDDshort[,1]=HDDtraintmp
  md <-lm(HDD~Tmin+time+NINO4+landT+NAO+AMO,data=df_HDDshort)
  paramHDD[i,]=md$coefficients
}
par(mfrow=c(2,4))
hist(paramHDD[,1],prob=TRUE,xlab="Intercept",main="distribution of intercept",sub=paste0("sd=",sd(paramHDD[,1])))
abline(v=confint(mdHDD)[1,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[1,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,1]),col=1,lwd=3)
hist(paramHDD[,2],prob=TRUE,xlab="Tmin",main="distribution of Tmin",sub=paste0("sd=",sd(paramHDD[,2])))
abline(v=confint(mdHDD)[2,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[2,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,2]),col=2,lwd=3)
hist(paramHDD[,3],prob=TRUE,xlab="time",main="distribution of time",sub=paste0("sd=",sd(paramHDD[,3])))
abline(v=confint(mdHDD)[3,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[3,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,3]),col=3,lwd=3)
hist(paramHDD[,4],prob=TRUE,xlab="NINO4",main="distribution of NINO4",sub=paste0("sd=",sd(paramHDD[,4])))
abline(v=confint(mdHDD)[4,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[4,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,4]),col=4,lwd=3)
hist(paramHDD[,5],prob=TRUE,xlab="landT",main="distribution of landT",sub=paste0("sd=",sd(paramHDD[,5])))
abline(v=confint(mdHDD)[5,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[5,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,5]),col=5,lwd=3)
hist(paramHDD[,6],prob=TRUE,xlab="NAO",main="distribution of NAO",sub=paste0("sd=",sd(paramHDD[,6])))
abline(v=confint(mdHDD)[6,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[6,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,6]),col=6,lwd=3)
hist(paramHDD[,7],prob=TRUE,xlab="AMO",main="distribution of AMO",sub=paste0("sd=",sd(paramHDD[,7])))
abline(v=confint(mdHDD)[7,1],lwd=3,col="red",lty=2)
abline(v=confint(mdHDD)[7,2],lwd=3,col="red",lty=2)
lines(density(paramHDD[,7]),col=8,lwd=3)
###Question 4g###
