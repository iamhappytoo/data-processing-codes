library(stringr)
#library(doParallel)
setwd("/mnt/nfs/home2/zban/snotel_screening/")
#listall=read.csv('SelectedStations_AllCategories.csv')
listall=read.csv('listall.csv')
# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer
# registerDoParallel(cl)
# nrow(listall)
for(i in 1:nrow(listall)){
  nam=paste0(listall[i,3],"_",listall[i,2])
  swe=read.csv(paste0('SWE/',nam,'.csv'))
  p=read.csv(paste0('Precip/',nam,'.csv'))
  t=read.csv(paste0('Temp/',nam,'.csv'))
  ##unify the year month day
  #missing values inserted into the station records for the first
  #15 days of October, the whole PRE and SWE records for the entire
  #year is missing. 
  datswe=as.Date(swe$dates,'%m/%d/%Y')
  ymdswe <- data.frame(date = datswe,
                   year = as.numeric(format(datswe, format = "%Y")),
                   month = as.numeric(format(datswe, format = "%m")),
                   day = as.numeric(format(datswe, format = "%d")))
  swe$dates=ymdswe$date
  swe=cbind(swe,ymdswe[,2:4])
  datp=as.Date(p$dates,'%m/%d/%Y')
  ymdp <- data.frame(date = datswe,
                    year = as.numeric(format(datswe, format = "%Y")),
                    month = as.numeric(format(datswe, format = "%m")),
                    day = as.numeric(format(datswe, format = "%d")))
  p$dates=ymdp$date
  p=cbind(p,ymdp[,2:4])
  datt=str_split_fixed(t$dates," ",2)
  datt1=as.Date(datt[,1],'%d-%b-%Y')
  t$ymd=datt1
  t$hr=datt[,2]
  newt1=t[,c(3,4,2)]
  ymdt1 <- data.frame(date = newt1$ymd,
                      year = as.numeric(format(newt1$ymd, format = "%Y")),
                      month = as.numeric(format(newt1$ymd, format = "%m")),
                      day = as.numeric(format(newt1$ymd, format = "%d")))
  newt=cbind(ymdt1,newt1[,2:3])
  
  # newt<- aggregate(cbind(Temp_K) ~ ymd,t,mean)
  # ymdt <- data.frame(date = newt$ymd,
  #                    year = as.numeric(format(newt$ymd, format = "%Y")),
  #                    month = as.numeric(format(newt$ymd, format = "%m")),
  #                    day = as.numeric(format(newt$ymd, format = "%d")))
  # newt=cbind(newt,ymdt[,2:4])
  # colnames(newt)=c('dates','Temp_K','year','month','day')
  ##Screening policy: only the station that pass screening will be saved in the new folder
  ##initial check
  ##The site with no snow (max swe <10mm) from Nov to June
  novjun=c(11,12,1,2,3,4,5,6)
  ckswe=swe[which(swe$month%in%novjun),2]
  ckswe=ckswe[!is.na(ckswe)]
  if(max(ckswe)==0){ 
    next
  }
  ##a.	If missing values inserted into the station records for the 
  #first 15 days of October, the whole PRE and SWE records for the
  #entire year is missing. 
  day=seq(1:15)
  sel=which((swe$month==10) &(swe$day%in%day))
  swe1=swe[sel,]
  sel=which((p$month==10) &(p$day%in%day))
  p1=p[sel,]
  yrtorm=unique(swe1$year[which(is.na(swe1$SWE_cm))])##select the years that meet a.
  for(y in 1:length(yrtorm)){
    ind=which((swe$year==yrtorm[y])&(swe$mon>=10))
    ind1=which((swe$year==(yrtorm[y]+1))&(swe$mon<=9))
    swe[c(ind,ind1),2]=NaN
  }
  yrtorm=unique(p1$year[which(is.na(p1$pcp_cm))])##select the years that meet a.
  for(y in 1:length(yrtorm)){
    ind=which((p$year==yrtorm[y])&(p$mon>=10))
    ind1=which((p$year==(yrtorm[y]+1))&(p$mon<=9))
    p[c(ind,ind1),2]=NaN
  }
  ##b.If Oct 1st PRE>5inches, all PRE values for that station and 
  #year were coded as missing. ##because the pre is already increment, it is not needed.
  # sel=which((p$month==10)&(p$day==1))
  # p1=p[sel,]
  # ind=which(p$year%in%p1$year[which(p1$pcp_cm>5)])
  # p[ind,2]=NaN
  ##c.	Double check after a) and b), whether all negative SWE and
  #PRE values have been coded as missing (which should be), otherwise,
  #code them as missing.
  pretrsd=-0.5*2.54
  swe[which(swe$SWE_cm<0),2]=NaN
  p$ques=rep(0,nrow(p));
  p$ques[which(p$pcp_cm<pretrsd)]=1 #Negative PRE1 up to -0.5 inches were 
  #allowed to pass, more negative increments were flagged as questionable. 
  #d.	Adjust erroneous precipitation values: 
  #1. if day N-1 and N+1 have same PRE, but PRE on day N is lower or higher, because the 
  #set day N PRE to day N-1 PRE. But because the SNOTEL seems to be providing the daily value already
  #I skip this step.
  # ind=which(!is.na(p$pcp_cm))
  # p1=p[ind,]
  # for(j in 2:nrow(p1)){
  #   ##check consecutive days
  #   if(((as.Date(p1[j+1,1])-as.Date(p1[j,1]))==1) &((as.Date(p1[j,1])-as.Date(p1[j-1,1]))==1)&(p1[j-1,2]==p1[j+1,2])){
  #     if(p1[j,2]>p1[j-1,2]){p1[j,2]==p1[j-1,2]}
  #   }
  # }
  #2.	if PRE difference between day N+1 and N-1 are positive, 
  #but PRE on day N is less than on day N-1 or greater than on N+1 => 
  #  if the positive difference between day N+1 PRE and day N-1 PRE <0.5 inches, 
  #the day N precipitation should be set to the average of the N+1 and N-1 values.(did not apply since Pre is not accum value)
  ##get swe1 as increment values, pre is already increment values, so use pre instead.
  swe1=swe[1:(nrow(swe)-1),]
  swe1[,2]=NaN
  for(j in 2:nrow(swe)){
      ##check consecutive days
      if(((as.Date(swe[j,1])-as.Date(swe[j-1,1]))==1)&(!is.na(swe[j-1,2]))&(!is.na(swe[j,2]))){
        swe1[j-1,2]=swe[j,2]-swe[j-1,2]
      }
  }
  #1.	SWE1 values with an absolute value > 10inches are flagged as questionable.
  swe1$ques=rep(0,nrow(swe1))
  swe1$ques[which(swe1[,2]>25.4)]=1
  #2.	A large snow accumulation event (SWE1>2.5inches) was followed on the next
  #day by a large loss event (SWE1< -2.5 inches), or conversely 
  #(large loss followed by large accumulation) with the same 2.5 limit, 
  #flag as questionable. (If storm comes into play, reduce false-alarm. I added this in the lit but didn't do it, because there are some precipitation check later on, also because hard to identify "storm").
  uplm=2.54*2.5;lwlm=-2.54*2.5
  ind=which(swe1[,2]>uplm)
  ind1=which(swe1[,2]<lwlm)
  date1=swe1$dates[ind];date2=swe1$dates[ind1];
  if(length(date1)*length(date2)>0){
    for(m in 1:length(date1)){
      for(n in 1:length(date2)){
        if((abs(as.Date(date1[m])-as.Date(date2[n])))==1){
          swe1[ind[m],6]=1; swe[ind1[n],6]=1;
        }
      }
    }
  }
  ##3.	Positive PRE1 over 10 inches are flagged as questionable.
  p$ques[which(p$pcp_cm>25.4)]=1
  #f.	For temperature: 
  newt$Temp_C=newt$Temp_K-273.15
  df1<- aggregate(cbind(Temp_C) ~ date,newt,mean)  ##get Tmean,default is to ignore missing values in the given variables.
  df2<- aggregate(cbind(Temp_C) ~ date,newt,max)  ##get Tmax
  df3 <- aggregate(cbind(Temp_C) ~ date,newt,min) ##get Tmin
  ymddf <- data.frame(date = df1$date,
                      year = as.numeric(format(df1$date, format = "%Y")),
                      month = as.numeric(format(df1$date, format = "%m")),
                      day = as.numeric(format(df1$date, format = "%d")))
  df1=cbind(ymddf,df1[,2])
  df2=cbind(ymddf,df2[,2])
  df3=cbind(ymddf,df3[,2])
  #if maximum, minimum, or mean T was >40C or <-40C, 
  #all temperature values for that day were flagged 
  #(because the sensor are the same). 
  newt$ques=rep(0,nrow(newt));
  df1$ques=rep(0,nrow(df1));
  df2$ques=rep(0,nrow(df2));
  df3$ques=rep(0,nrow(df3));
  uplm=40;lwlm=-40;
  datena=unique(newt$date[which((newt$Temp_C>uplm) | (newt$Temp_C<lwlm))])
  newt$ques[which(newt$date%in%datena)]=1
  datena=unique(df1$date[which(df1$Temp_C>uplm)|(df1$Temp_C<lwlm)])
  df1$ques[which(df1$date%in%datena)]=1
  datena=unique(df2$date[which(df2$Temp_C>uplm)|(df2$Temp_C<lwlm)])
  df2$ques[which(df2$date%in%datena)]=1
  datena=unique(df3$date[which(df3$Temp_C>uplm)|(df3$Temp_C<lwlm)])
  df3$ques[which(df3$date%in%datena)]=1
  #g.	Preparation for next round of screening: using the above 
  #procedures generated not flagged data to compile long-term 
  #monthly means and standard deviations for each station based on
  #positive and negative SWE1 values separately and nonzero PRE1 
  #values as well as Tmax, Tmin, and Tmean. 
  #1.	If a positive SWE1 value was more than five standard deviations above or below the respective monthly mean, it was flagged. Similarly, for negative SWE1 values.See here, for very negative SWE1 values, if storm comes into play, to reduce false-alarm.
  #2.	Erroneous PRE1 values are also checked based on the five standard deviation check. 
  ##swe1, add count, sd, and mean columns for each record corresponding month
  #first do a square root transformation
  swe1$SWE_transf=sign(swe1$SWE_cm)*sqrt(abs(swe1$SWE_cm*10))
  sd=rep(NaN,nrow(swe1));mean1=sd;count=sd;md=swe1$month*100+swe1$day
  ym=swe1$year*100+swe1$month;uqym=unique(ym)
  for(r in 1:length(uqym)){
    yr=floor(uqym[r]/100);mon=uqym[r]%%100;
    if(mon==1){#remembered to remove the flagged ones in calculation
      ind=which((!is.na(swe1$SWE_transf))&(swe1$ques==0)&((swe1$year==(yr-1))&(md>=1215))|((swe1$year==yr)&(md<=0215)))
    }else if(mon<12){
      ind=which((!is.na(swe1$SWE_transf))&(swe1$ques==0)&(swe1$year==yr)&((md>=((mon-1)*100+15))&(md<=((mon+1)*100+15))))
    }else{
      ind=which((!is.na(swe1$SWE_transf))&(swe1$ques==0)&((swe1$year==yr)&(md>=1115))|((swe1$year==yr+1)&(md<=115)))
    }##Need to avoid NaN in calculation.
    ind1=which((swe1$year==yr)&(swe1$month==mon))
    sd[ind1]=sd(swe1[ind,7])
    mean1[ind1]=mean(swe1[ind,7])
    count[ind1]=length(ind)
  }
  swe1$sd=sd;swe1$mean=mean1;swe1$count=count;
  swe1$ques[which((abs(swe1$SWE_transf-swe1$mean)>abs(swe1$sd*5))&(swe1$count>=30))]=1;
  swe1$sd5_out=rep(0,nrow(swe1));swe1$sd3_out=rep(0,nrow(swe1))
  swe1$sd5_out[which(((swe1$SWE_transf-swe1$mean)>swe1$sd*5)&(swe1$count>=30))]=1
  swe1$sd3_out[which(((swe1$SWE_transf-swe1$mean)>swe1$sd*3)&(swe1$count>=30))]=1
  ##p, add count, sd, and mean columns for each record corresponding month
  #first do a square root transformation
  p$pcp_transf=sign(p$pcp_cm)*sqrt(abs(p$pcp_cm*10))
  sd=rep(NaN,nrow(p));mean1=sd;count=sd;md=p$month*100+p$day
  ym=p$year*100+p$month;uqym=unique(ym)
  for(r in 1:length(uqym)){
    yr=floor(uqym[r]/100);mon=uqym[r]%%100;
    if(mon==1){#remember to remove the flagged ones in calculation
          ind=which((!is.na(p$pcp_transf))&(p$ques==0)&((p$year==(yr-1))&(md>=1215))|((p$year==yr)&(md<=0215)))
        }else if(mon<12){
          ind=which((!is.na(p$pcp_transf))&(p$ques==0)&(p$year==yr)&((md>=((mon-1)*100+15))&(md<=((mon+1)*100+15))))
        }else{
          ind=which((!is.na(p$pcp_transf))&(p$ques==0)&((p$year==yr)&(md>=1115))|((p$year==yr+1)&(md<=115)))
        }
    ind1=which((p$year==yr)&(p$month==mon))
    sd[ind1]=sd(p[ind,7])
    mean1[ind1]=mean(p[ind,7])
    count[ind1]=length(ind)
  }
  p$sd=sd;p$mean=mean1;p$count=count;
  p$ques[which((abs(p$pcp_transf-p$mean)>abs(p$sd*5))&(p$count>=30))]=1;
  p$sd5_out=rep(0,nrow(p));p$sd3_out=rep(0,nrow(p));
  p$sd5_out[which(((p$SWE_transf-p$mean)>p$sd*5)&(p$count>=30))]=1
  p$sd3_out[which(((p$SWE_transf-p$mean)>p$sd*3)&(p$count>=30))]=1
  ##3-6.	If a positive SWE1 value more than five standard deviations out was
  #paired with a positive deviation flag of PRE1 (5 deviations), they are both
  #recording a valid extreme event and the flags for both variables were 
  #removed. similarly, 5_sd, 3_sd pair too.
  #8.	If some months and stations don't have enough cases of positive or negative 
  #SWE1 values to compute means and sd (a minimum of 30 cases for each 
  #calculation). For these cases the quality control is based only on the 
  #initial limits checks described above.
  indswe3sd=which(swe1$sd3_out==1)
  indswe5sd=which(swe1$sd5_out==1)
  indp3sd=which(p$sd3_out==1)
  indp5sd=which(p$sd5_out==1)
  datswe3sd=as.Date(swe1$dates[indswe3sd])
  datswe5sd=as.Date(swe1$dates[indswe5sd])
  datp3sd=as.Date(p$dates[indp3sd])
  datp5sd=as.Date(p$dates[indp5sd])
  if(length(datswe5sd)*length(datp5sd)>0){
    for(m in 1:length(datswe5sd)){
      for(n in 1:length(datp5sd)){
        if((datswe5sd[m]-datp5sd[n])==0){
          swe1$ques[indswe5sd[m]]=0
          p$ques[indp5sd[n]]=0
        }
      }
    }
  }
  if(length(datswe5sd)*length(datp3sd)>0){
    for(m in 1:length(datswe5sd)){
      for(n in 1:length(datp3sd)){
        if((datswe5sd[m]-datp3sd[n])==0){
          swe1$ques[indswe5sd[m]]=0
          p$ques[indp3sd[n]]=0
        }
      }
    }
  }
  if(length(datswe3sd)*length(datp5sd)>0){
    for(m in 1:length(datswe3sd)){
      for(n in 1:length(datp5sd)){
        if((datswe3sd[m]-datp5sd[n])==0){
          swe1$ques[indswe3sd[m]]=0
          p$ques[indp5sd[n]]=0
        }
      }
    }
  }
  ##7. Temperature data were flagged using a tighter three-sd check. 
  colnames(df1)=c("date","year","month","day","Tmean","ques")
  colnames(df2)=c("date","year","month","day","Tmax","ques")
  colnames(df3)=c("date","year","month","day","Tmin","ques")
  sdmean=rep(NaN,nrow(df1));sdmax=sdmean;sdmin=sdmean;
  meanTmean=sdmean;meanTmax=sdmean;meanTmin=sdmean;
  countmean=sdmean;countmax=sdmean;countmin=sdmean;
  md=newt$month*100+newt$day;
  Tmax=sdmean; Tmin=sdmean; Tmean=sdmean; 
  ym=newt$year*100+newt$month;uqym=unique(ym)
  mddf=df1$month*100+df1$day
  for(r in 1:length(uqym)){
    yr=floor(uqym[r]/100);mon=uqym[r]%%100;
    if(mon==1){#remember to remove the flagged and NaN ones in calculation
      #ind=which((newt$ques==0)&((newt$year==(yr-1))&(md>=1215))|((newt$year==yr)&(md<=0215)))
      inddf1=which((!is.na(df1$Tmean))&(df1$ques==0)&((df1$year==(yr-1))&(mddf>=1215))|((df1$year==yr)&(mddf<=0215)))
      inddf2=which((!is.na(df2$Tmax))&(df2$ques==0)&((df2$year==(yr-1))&(mddf>=1215))|((df2$year==yr)&(mddf<=0215)))
      inddf3=which((!is.na(df3$Tmin))&(df3$ques==0)&((df3$year==(yr-1))&(mddf>=1215))|((df3$year==yr)&(mddf<=0215)))
    }else if(mon<12){
      #ind=which((newt$ques==0)&(newt$year==yr)&((md>=((mon-1)*100+15))&(md<=((mon+1)*100+15))))
      inddf1=which((!is.na(df1$Tmean))&(df1$ques==0)&(df1$year==yr)&((mddf>=((mon-1)*100+15))&(mddf<=((mon+1)*100+15))))
      inddf2=which((!is.na(df2$Tmax))&(df2$ques==0)&(df2$year==yr)&((mddf>=((mon-1)*100+15))&(mddf<=((mon+1)*100+15))))
      inddf3=which((!is.na(df3$Tmin))&(df3$ques==0)&(df3$year==yr)&((mddf>=((mon-1)*100+15))&(mddf<=((mon+1)*100+15))))
    }else{
      #ind=which((newt$ques==0)&((newt$year==yr)&(md>=1115))|((newt$year==yr+1)&(md<=115)))
      inddf1=which((!is.na(df1$Tmean))&(df1$ques==0)&((df1$year==yr)&(mddf>=1115))|((df1$year==yr+1)&(mddf<=115)))
      inddf2=which((!is.na(df2$Tmax))&(df2$ques==0)&((df2$year==yr)&(mddf>=1115))|((df2$year==yr+1)&(mddf<=115)))
      inddf3=which((!is.na(df3$Tmin))&(df3$ques==0)&((df3$year==yr)&(mddf>=1115))|((df3$year==yr+1)&(mddf<=115)))
    }
    #ind1=which((newt$year==yr)&(newt$month==mon))
    ind1df1=which((df1$year==yr)&(df1$month==mon))
    ind1df2=which((df2$year==yr)&(df2$month==mon))
    ind1df3=which((df3$year==yr)&(df3$month==mon))
    
    sdmean[ind1df1]=sd(df1[inddf1,5])
    sdmax[ind1df2]=sd(df2[inddf2,5])
    sdmin[ind1df3]=sd(df3[inddf3,5])    
    meanTmean[ind1df1]=mean(df1[inddf1,5])
    meanTmax[ind1df2]=mean(df2[inddf2,5])
    meanTmin[ind1df3]=mean(df3[inddf3,5])
    countmean[ind1df1]=length(inddf1)
    countmax[ind1df2]=length(inddf2)
    countmin[ind1df3]=length(inddf3)
  }
  df1$sd=sdmean;df2$sd=sdmax;df3$sd=sdmin;
  df1$mean=meanTmean;df2$mean=meanTmax;df3$mean=meanTmin;
  df1$count=countmean;df2$count=countmax;df3$count=countmin;
  df1$ques[which((abs(df1$Tmean-df1$mean)>df1$sd*3)&(df1$count>=30))]=1;
  df2$ques[which((abs(df2$Tmax-df2$mean)>df2$sd*3)&(df2$count>=30))]=1;
  df3$ques[which((abs(df3$Tmin-df3$mean)>df3$sd*3)&(df3$count>=30))]=1;
  datena=c(df1$date[which(df1$ques==1)], df2$date[which(df2$ques==1)], df3$date[which(df3$ques==1)])
  ind=which(newt$date%in%datena)
  newt$ques[ind]=1
  ##i.	A final step, we inspected the daily records for each station and year 
  #and truncated the SWE time series at the first occurrence of a 
  #flagged value. Similarly for PRE time series. This is because 
  #both variables are cumulative, a single bad value may contaminate 
  #the remainder of the record. 
  indswe=which(swe1$ques==1);indp=which(p$ques==1);mdswe=swe1$month*100+swe1$day;mdp=p$month*100+p$day
  if(length(indswe)>0){
    for(y in 1:length(indswe)){
      mdtmp=swe1$month[indswe[y]]*100+swe1$day[indswe[y]]
      yrtmp=swe1$year[indswe[y]]
      if(mdtmp>=1001){
        ind=which((swe1$year==yrtmp)&(mdswe>=mdtmp))
        ind1=which((swe1$year==(yrtmp+1))&(mdswe<1001))
        swe1[c(ind,ind1),2]=NaN
      }else{
        ind=which((swe1$year==yrtmp)&(mdswe>=mdtmp)&(mdswe<1001))
        swe1[ind,2]=NaN
      }
    }
  }
  if(length(indp)>0){
    for(y in 1:length(indp)){
      mdtmp=p$month[indp[y]]*100+p$day[indp[y]]
      yrtmp=p$year[indp[y]]
      if(mdtmp>=1001){
        ind=which((p$year==yrtmp)&(mdp>=mdtmp))
        ind1=which((p$year==(yrtmp+1))&(mdp<1001))
        p[c(ind,ind1),2]=NaN
      }else{
        ind=which((p$year==yrtmp)&(mdp>=mdtmp)&(mdp<1001))
        p[ind,2]=NaN
      }
    } 
  }
  ##Added steps from Durre et al., 2010
  #Internal consistency check from Durre et al., 2010 that should be applied as
  #subsequent screening after Serreze et al., 1999.
  #The iterative internal consistency check evaluates each temperature in a 
  #station's record for violations of expected relationships with other
  #temperatures on the same and adjacent days. It then uses complex logic to 
  #decide which values are in error and repeats the test until no additional 
  #violations are found. The procedure consists of four steps:
  #1.	Each running pair of consecutive days is tested for the following seven conditions, 
  #counting the number of violations found for each element and day:
  # TMIN(0) > TMAX(1) + 1 
  # TMIN(1) > TMAX(0) + 1
  vio=rep(0,nrow(df1))
  cnt1=1
  while(cnt1>0){
    cnt=0
    for(j in 2:nrow(df1)){
      ##check consecutive days
      if((as.Date(df1[j,1])-as.Date(df1[j-1,1]))==1){
        if((df3[j,5]>(df2[j-1,5]+1))&(df3$ques[j]==0)&(df2$ques[j-1]==0)){
          vio[(j-1):j]=vio[(j-1):j]+1;df3$ques[j]=1;df2$ques[j-1]=1
          cnt=cnt+1
        } #Tmin(1)>Tmax(0)+1
        if(df3[j-1,5]>(df2[j,5]+1)&(df3$ques[j-1]==0)&(df2$ques[j]==0)){
          vio[(j-1):j]=vio[(j-1):j]+1;df3$ques[j-1]=1;df2$ques[j]=1
          cnt=cnt+1
        }#Tmin(0)>Tmax(1)+1
      }
    }
    cnt1=cnt;
  }
  datena=df1$date[which(vio==max(vio))]
  newt$ques[which(newt$date%in%datena)]=1
  #5.	Should any days remain on which TMAX < TMIN, both TMAX and TMIN are 
  #flagged on those days. In other words, all cases with TMAX < TMIN on the 
  #same day are flagged regardless of the magnitude of the difference.
  newt$ques[which(newt$date%in%df1$date[which((df2$Tmax<df3$Tmin)&(df2$ques==0)&(df3$ques==0))])]=1
  #6.	Two other checks are included to test for unrealistically large swings in temperature, to the extent that this is possible without excessively flagging actual rapid changes of this kind. 
  #The first check, termed the spike/dip test, identifies temperatures that are
  #at least 25?C warmer or colder than the previous and following days. 
  check=rep(0,nrow(df1))
  for(j in 2:(nrow(df1)-1)){
    if((as.Date(df1[j,1])-as.Date(df1[j-1,1]))==1){#check consecutive days
      if(abs(df1$Tmean[j]-df1$Tmean[j-1])>25){
        check[j]=1
      }
    }
    if(as.Date(df1[j+1,1])-as.Date(df1[j,1])==1){#check consecutive days
      if(abs(df1$Tmean[j]-df1$Tmean[j+1])>25){
        check[j]=1
      }    
    }
    #The second check, termed the lagged range test, looks for differences in 
    #excess of 40?C (i) between TMAX and the warmest TMIN reported on the 
    #previous, same, and following days and (ii) between TMIN and the coldest 
    #TMAX in the 3-day window centered on the day of the TMIN. 
    if(((as.Date(df1[j,1])-as.Date(df1[j-1,1]))==1)&((as.Date(df1[j+1,1])-as.Date(df1[j,1]))==1)){
      if(abs(df2$Tmax[j]-max(df3$Tmin[(j-1):(j+1)]))>40){check[j]=1}
      if(abs(df3$Tmin[j]-min(df2$Tmax[(j-1):(j+1)]))>40){check[j]=1}
    }
  }
  newt$ques[which(newt$date%in%df1$date[which(check==1)])]=1
  ##Description of the Iterative Internal Consistency Check on Precipitation 
  #(dealing with issues e.g. SWE increase without precipitation)
  #1.	a nonzero snowfall amount or increasing snow depth is flagged when TMIN 
  #exceeds 7?C on the same, previous, and subsequent day.
  #Find Tmin>7 on the three days
  check=rep(0,nrow(df1))
  for(j in 2:(nrow(df1)-1)){
    if(((as.Date(df1[j,1])-as.Date(df1[j-1,1]))==1)&((as.Date(df1[j+1,1])-as.Date(df1[j,1]))==1)){
      if(min(df3$Tmin[(j-1):(j+1)])>7){
        check[j]=1
      }
    }
  }
  #find corresponding swe1
  dateck=df1$date[which(check==1)]
  ind=which(swe1$dates%in%dateck)
  swe1$ques[ind[which(swe1$SWE_cm[ind]>0)]]=1
  #2.	Clear days (zero snowfall/precipitation with <7C TMIN on 3 consecutive
  #days, or with precipitation but >7C TMIN on 3 consecutive days *This has been modeled in 1, skip
  #) should not 
  #have much SWE accumulation, if not, flag the snow.
  ##find clear days that meet condition 1.
  check=rep(0,nrow(df1))
  for(j in 2:(nrow(df1)-1)){
    if(((as.Date(df1[j,1])-as.Date(df1[j-1,1]))==1)&((as.Date(df1[j+1,1])-as.Date(df1[j,1]))==1)){
      if(max(df3$Tmin[(j-1):(j+1)])<7){
        check[j]=1
      }
    }
  }
  ind=which(p$dates%in%df1$date[which(check==1)])
  ind1=which(p$pcp_cm[ind]==0)
  ind=ind[ind1]
  ind1=which(swe1$dates%in%p$dates[ind])##the date with clear day and no p
  swe1$ques[ind1[which(swe1$SWE_cm[ind1]>0)]]=1 ##if it has >0 accumulation, questionable.
  ##3.	an increase in snow depth is considered excessively large when compared 
  #with snowfall only when it exceeds the snowfall (here we used precipitation)
  #sums for the previous plus current and current plus subsequent days.
  ##get the snowfall for three continuous days
  dat3p=p$dates[2:(nrow(p)-1)]
  p3p=rep(NaN,nrow(p))
  for(j in 2:(nrow(p)-1)){
    if(((as.Date(p$dates[j])-as.Date(p$dates[j-1]))==1)&
       ((as.Date(p$dates[j+1])-as.Date(p$dates[j]))==1)){
      if(is.na(p$pcp_cm[j])){
        p3p[j]=NaN
      }else{
          if(is.na(p$pcp_cm[j-1])&(!is.na(p$pcp_cm[j+1]))){
            p3p[j]=sum(p$pcp_cm[j:(j+1)])
          }else if(is.na(p$pcp_cm[j+1])&(!is.na(p$pcp_cm[j-1]))){
            p3p[j]=sum(p$pcp_cm[(j-1):j])
          }else{##both nan or both not nan
            p3p[j]=max(sum(p$pcp_cm[(j-1):(j)]),sum(p$pcp_cm[(j):(j+1)])) ##nan if both nan, not nan if all not nan
          }
        }
    }
  }
  df3p=as.data.frame(dat3p)
  df3p$pcp_cm=p3p[2:(nrow(p)-1)]
  colnames(df3p)=c('date','pcp_cm')
  ind=which(!is.na(df3p$pcp_cm))
  date3p=df3p$date[ind]
  ##get swe on the same date
  ind1=which(swe1$dates%in%date3p)##ind for swe1
  ind2=which(date3p%in%swe$dates[ind1])
  ind=ind[ind2]##ind for precipitation, follows the same date order of ind1 for swe1
  ind3=which(!is.na(swe1$SWE_cm[ind1]))
  ind4=which(swe1$SWE_cm[ind1[ind3]]>df3p$pcp_cm[ind[ind3]])
  swe1$ques[ind1[ind3[ind4]]]=1
  ##4.	In general, as in Reek et al. (1992), the consistency checks are set to
  #flag all values involved in an inconsistency because the evaluation process 
  #revealed no justification for systematically incriminating only one of the 
  #elements. The one exception is the snow-temperature consistency check; it does
  #not flag temperature since in 90% of the cases evaluated, it was the snowfall 
  #or snow depth that was found to be in error.
  ##less collateral damage is done if one value can first be flagged by another
  #test (e.g., an outlier test, which might determine that TMIN was erroneous, I
  #used this by including ques in the if function).
  ####
  ###Megaconsistency checks after the internal consistency check.
  #a.	the snow-temperature megaconsistency check flags nonzero values of SNOW 
  #and SNWD when even the lowest TMIN ever recorded for the station and calendar
  #month is greater than or equal to 7?C, provided that at least 140 TMINs are
  #available for determining the lowest TMIN. It thus helps to identify reports 
  #of snow at locations and times of year when temperatures have never been cold 
  #enough to support such reports.
  if((min(df3$Tmin[which(df3$ques==0)])>=7)&(length(which(!is.na(df3$Tmin)))>=140)){
    swe1$ques[which(swe1$SWE_cm>0)]=1
  }
  #b.	the snow season reality check tests for nonzero reports of SNOW during the
  #warm half of the year at locations where the cold half of the year has always 
  #been snow-free. As such, it facilitates the detection of some obviously 
  #erroneous nonzero snowfall and snow depth values that are not detected by any 
  #of the tests involving snow and temperature. This is the case, for example, 
  #in areas where only precipitation-related measurements are available.
  ##get cold half of the years' maximum swe, if it is <0/=0, then the warm half 
  #>0 ones are questionable
  coldmon=c(10,11,12,1,2,3)
  warmmon=c(4,5,6,7,8,9)
  swe1test=swe1[which(!is.na(swe1$SWE_cm)),]
  if(max(swe1test$SWE_cm[which(swe1test$month%in%coldmon)])==0){
    swe1$ques[which((swe1$month%in%warmmon)&(swe1$SWE_cm>0))]=1
  }
  
  ##Final test ##added by zban: after initial output, here I read in the data again and redo the following steps
  #but next time directly run this loop is ok.
  ##i.	A final step, we inspected the daily records for each station and year 
  #and truncated the SWE time series at the first occurrence of a 
  #flagged value. Similarly for PRE time series. This is because 
  #both variables are cumulative, a single bad value may contaminate 
  #the remainder of the record. 
  indswe=which(swe1$ques==1);indp=which(p$ques==1);mdswe=swe1$month*100+swe1$day;mdp=p$month*100+p$day
  if(length(indswe)>0){
    for(y in 1:length(indswe)){
      mdtmp=swe1$month[indswe[y]]*100+swe1$day[indswe[y]]
      yrtmp=swe1$year[indswe[y]]
      if(mdtmp>=1001){
        ind=which((swe1$year==yrtmp)&(mdswe>=mdtmp))
        ind1=which((swe1$year==(yrtmp+1))&(mdswe<1001))
        swe1[c(ind,ind1),2]=NaN
      }else{
        ind=which((swe1$year==yrtmp)&(mdswe>=mdtmp)&(mdswe<1001))
        swe1[ind,2]=NaN
      }
    }
  }
  if(length(indp)>0){
    for(y in 1:length(indp)){
      mdtmp=p$month[indp[y]]*100+p$day[indp[y]]
      yrtmp=p$year[indp[y]]
      if(mdtmp>=1001){
        ind=which((p$year==yrtmp)&(mdp>=mdtmp))
        ind1=which((p$year==(yrtmp+1))&(mdp<1001))
        p[c(ind,ind1),2]=NaN
      }else{
        ind=which((p$year==yrtmp)&(mdp>=mdtmp)&(mdp<1001))
        p[ind,2]=NaN
      }
    } 
  }
  ##Finished test, writing swe, precip, and T into new csvs.
  write.csv(newt[c(1,5,7,8)],paste0('newT/',nam,'.csv'),row.names=F)
  write.csv(p[,c(1,2,6)],paste0('newP/',nam,'.csv'),row.names=F)
  write.csv(swe1[,c(1,2,6)],paste0('newSWE/',nam,'.csv'),row.names=F)
  write.csv(newt,paste0('newTfull/',nam,'.csv'),row.names=F)
  write.csv(p,paste0('newPfull/',nam,'.csv'),row.names=F)
  write.csv(swe1,paste0('newSWEfull/',nam,'.csv'),row.names=F)
}
#stop cluster
stopCluster(cl)
