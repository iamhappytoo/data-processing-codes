library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
setwd("D:/Users/BANZH/Downloads/Huilin paper")
##Fig.1b FGM type##Also for Figs2 plot for POT
#========================================================================================
styp=read.csv("Fig1b/FGM.csv",header=TRUE,sep=',')
styp=read.csv("Figs2/FGM.csv",header=TRUE,sep=',')
ID1=read.csv("../USB/plot_geog256_paper/Floodtype_mean/ID1.csv",header=FALSE)
styp=cbind(ID1[,1:3],styp[8])
colnames(styp)=c("ID","lat","lon","group")
styp$group <- factor(styp$group,level=c("ROS","RS","MS","CAPE",
                                        "AR","FRONT",
                                        "AR/CAPE","MS/CAPE"))
###Use this below to plot dominant mechanisms from 1960 to 2012###
state <- map_data("state")
west_coast <- subset(state, region %in% c("california", "oregon", "washington","montana","idaho","wyoming","nevada","utah","colorado","arizona","new mexico"))
# compute the bounding box
colnames(west_coast)=c("lon","lat","group","order","region","subregion")
bc_bbox <- make_bbox(lat = lat, lon = lon, data = west_coast)
# grab the maps from google
bc_west_coast<- get_map(location = bc_bbox, source = "google", maptype = "terrain")
pdf("fig1b/fig1withbk.pdf",width=6.65,height=6.83,paper='special')
pdf("figs2/fgm.pdf",width=6.65,height=6.83,paper='special')
#ggmap(bc_west_coast) + 
ggplot(data=west_coast)+
  geom_polygon(data = west_coast, aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = styp, mapping = aes(x = lon, y = lat, color = group), size=4, shape=16)+
#  ggtitle("Dominant flood event type 1960-2012")+
  theme(
    plot.title=element_blank() #text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  guides(fill=guide_legend(ncol=2))+
  theme(legend.position = c(0.843,0.8),legend.title=element_blank(), legend.text=element_text(size=14))+
  #theme(legend.position = "bottom",legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0, linetype="solid"))+
  theme(legend.title = element_blank())+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("#990033","orange","red", "blue", "green","purple","magenta","yellow"))+
  scale_shape_manual(values  = c(16,16,16,16,16,16,16,16))+
  scale_size_manual(values = c(4,4,4,4,4,4,4,4))
dev.off()
###FigS1###
bigroup=rep(0,length(styp$ID))
bigroup[styp$group=="Convective_MS"]="Precip dominant"
bigroup[styp$group=="Convective_NonMS"]="Precip dominant"
bigroup[styp$group=="Frontal_AR"]="Precip dominant"
bigroup[styp$group=="Frontal_Non-AR"]="Precip dominant"
bigroup[styp$group=="Rain_Mixing"]="Precip dominant"
bigroup[bigroup==0]="Snow dominant"
styp=cbind(styp,bigroup)
ggplot(data = west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = styp, mapping = aes(x = lon, y = lat, color = bigroup), size=4, shape=16)+
  ggtitle("Dominant flood event type 1960-2012")+
  theme(
    plot.title=element_blank() #text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  guides(fill=guide_legend(ncol=2))+
  theme(legend.position = c(0.862,0.951),legend.title=element_blank(), legend.text=element_text(size=14))+
  #theme(legend.position = "bottom",legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0, linetype="solid"))+
  theme(legend.title = element_blank())+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("red", "green"))+
  scale_shape_manual(values  = c(16,16))+
  scale_size_manual(values = c(4,4))

##Fig.1c-d The temporal trend of annual maximum flood magnitude and flood timing in 1960-2012
trds=read.csv("fig1c/trds_anm.csv",header=TRUE)
trdps=read.csv("fig1c/trdps_anm.csv",header=TRUE)
trdst=read.csv("fig1d/trds_date.csv",header=TRUE)
trdpst=read.csv("fig1d/trdps_date.csv",header=TRUE)
ID=read.csv("fig1c/ID2.csv",header=FALSE)
alldata=cbind(ID[,1:3],trds[,2],trdps[,2])
alldata=cbind(ID1[,1:3],trdst[,2],trdpst[,2])
colnames(alldata)=c("ID","lat","lon","anm_trend","anm_p")
##ANM Flood magnitude trend##
#=============================================================================
anmclass1 <- alldata %>%
  filter(anm_trend>0) %>%
  filter(anm_p<=0.05)
anmclass1$alpha=0.9
anmclass1$group="+ (p<=0.05)"
anmclass2 <- alldata %>%
  filter(anm_trend>0)%>%
  filter(anm_p>0.05)
anmclass2$alpha=0.88
anmclass2$group="+ (p>0.05)"
anmclass3 <- alldata %>%
  filter(anm_trend<0) %>%
  filter(anm_p<=0.05)
anmclass3$alpha=0.9
anmclass3$group="- (p<=0.05)"
anmclass4 <- alldata %>%
  filter(anm_trend<0)%>%
  filter(anm_p>0.05)
anmclass4$alpha=0.88
anmclass4$group="- (p>0.05)"
anmplot=rbind(anmclass1,anmclass2,anmclass3,anmclass4)

##Plot with map
# plot the points and color them by sector
##Plot without background
pdf("fig1c/fig1c.pdf",width=6.65,height=6.83,paper='special')
pdf("fig1d/fig1d.pdf",width=6.65,height=6.83,paper='special')
ggplot(data = west_coast) + 
geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = anmplot, mapping = aes(x = lon, y = lat, fill=group, color = group, shape=group, size=group))+
  scale_alpha(guide = 'none')+
  #ggtitle("ANM flood magnitude trend")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.13,0.097),legend.title=element_blank(), legend.text=element_text(size=15))+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("red", "red", "blue", "blue"))+
  scale_shape_manual(values  = c(16,1,16,1))+
  scale_size_manual(values = c(4,4,4,4))
dev.off()
##timing##
#=============================================================================
alldata=cbind(IDt[,1:3],trdst[,2],trdpst[,2])
colnames(alldata)=c("ID","lat","lon","rft_trend","rft_p")
rftclass1 <- alldata %>%
  filter(rft_trend>0) %>%
  filter(rft_p<=0.05)
rftclass1$group="+ (p<=0.05)"
rftclass2 <- alldata %>%
  filter(rft_trend>0)%>%
  filter(rft_p>0.05)
rftclass2$group="+ (p>0.05)"
rftclass3 <- alldata %>%
  filter(rft_trend<0) %>%
  filter(rft_p<=0.05)
rftclass3$group="- (p<=0.05)"
rftclass4 <- alldata %>%
  filter(rft_trend<0)%>%
  filter(rft_p>0.05)
rftclass4$group="- (p>0.05)"
rftplot=rbind(rftclass1,rftclass2,rftclass3,rftclass4)
##Plot without map
pdf("fig2/fig2RAINDT.pdf",width=6.65,height=6.83,paper='special')
ggplot(data = west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = rftplot, mapping = aes(x = lon, y = lat, fill=group, color = group, shape=group, size=group))+
  scale_alpha(guide = 'none')+
  #ggtitle("Rainfall timing trend")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.127,0.097),legend.title=element_blank(), legend.text=element_text(size=15))+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("red", "red", "blue", "blue"))+
  scale_shape_manual(values  = c(16,8,16,8))+
  scale_size_manual(values = c(4,4,4,4))
dev.off()
###Fig.S2===plot POT fraction of mechanism for each station across time===###Also for new FigS1, which is ANM
styf=read.csv("figS2/count_stat.csv",header=TRUE)
styf=read.csv("figS1/count_stat.csv",header=TRUE)
colnames(ID1)=c("ID","lat","lon")
styf=cbind(ID1[,1:3],styf[,2:7]/rowSums(styf[,2:7]))
colnames(styf)=c("ID","lat","lon","RS","ROS","AR","FRONT","MS","CAPE")
##Snowmelt Radiation mechanism
pdf("figs1/b-RS.pdf",width=6.65,height=6.83,paper='special') ##can also be figs2/b-RS.pdf under pot case.
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group=group), fill = NA, color = "black") +
  geom_point(data = styf, mapping = aes(x = lon, y = lat, color = RS), size=4, shape=16)+
  #ggtitle("Snowmelt_Radiation Mechanism Fraction")+
  theme(
    plot.title=element_blank() #text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.position = c(0.917,0.856),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  theme(axis.text=element_text(size=20),axis.title = element_text(size=20))+
  scale_colour_gradient(low="#FFFFCC",high="#FF6600",limits=c(0,1))
dev.off()

##Snowmelt Rain mechanism
pdf("figs1/a-ROS.pdf",width=6.65,height=6.83,paper='special')
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group=group), fill = NA, color = "black") +
  geom_point(data = styf, mapping = aes(x = lon, y = lat, color = `ROS`), size=4, shape=16)+
  #ggtitle("Snowmelt_Rain Mechanism Fraction")+
  theme(
    plot.title=element_blank()  #text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.position = c(0.93,0.856),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  theme(axis.text=element_text(size=20),axis.title = element_text(size=20))+
  scale_colour_gradient(low="#FFCCCC",high="#CC0000",limits=c(0,0.8))
dev.off()
##FRONT
pdf("figs2/f-FRONT.pdf",width=6.65,height=6.83,paper='special')
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = styf, mapping = aes(x = lon, y = lat, color = `FRONT`), size=4, shape=16)+
  #ggtitle("Frontal_Non.AR Mechanism Fraction")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.position = c(0.915,0.856),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  theme(axis.text=element_text(size=20),axis.title = element_text(size=20))+
  scale_colour_gradient(low="#FFCCFF",high="#9933FF",limits=c(0,1))
dev.off()
##AR
pdf("figs1/e-AR.pdf",width=6.65,height=6.83,paper='special')
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = styf, mapping = aes(x = lon, y = lat, color = `AR`), size=4, shape=16)+
  #ggtitle("Radiation Mechanism Fraction")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.position = c(0.93,0.856),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  theme(axis.text=element_text(size=20),axis.title = element_text(size=20))+
  scale_colour_gradient(low="#CCFFFF",high="#00FF00")
dev.off()
##Convective_MS
pdf("figs1/c-MS.pdf",width=6.65,height=6.83,paper='special')
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = styf, mapping = aes(x = lon, y = lat, color = MS), size=4, shape=16)+
  #ggtitle("Convective_MS Mechanism Fraction")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.position = c(0.928,0.856),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  theme(axis.text=element_text(size=20),axis.title = element_text(size=20))+
  scale_colour_gradient(low="#FFCCCC",high="#FF0000")
dev.off()
##Convective_Non_MS
pdf("figs1/d-CAPE.pdf",width=6.65,height=6.83,paper='special') ##can also be figs2/d-CAPE.pdf
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = styf, mapping = aes(x = lon, y = lat, color = `CAPE`), size=4, shape=16)+
  #ggtitle("Convective_NonMS Mechanism Fraction")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.position = c(0.929,0.856),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  theme(axis.text=element_text(size=20),axis.title = element_text(size=20))+
  scale_colour_gradient(low="#99CCFF",high="#0000CC")

#gridExtra::grid.arrange(p1,p3,p4,p2,p6,p5,ncol=3)
dev.off()
###Fig.S3 Month of most frequent AM flood in 1960-2013
setwd("/home/zban/Downloads/plot_geog256_paper/fignew0227/")
month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
datas3=read.csv("figs3/flood_month.csv",header=TRUE)
ID2=read.table("figs3/ID1.csv",header=TRUE,sep=",")
datas3=cbind(ID2[,1:3],datas3[2:13])
savem=rep(c("0"),120)
savein=rep(0,120)
for(i in 1:nrow(datas3)){
  savem[i]=month[which(datas3[i,4:15]==max(datas3[i,4:15]))]
  savein[i]=which(datas3[i,4:15]==max(datas3[i,4:15]))
}
colnames(datas3)=c("ID","lat","lon",month)
datas3$maxmonth=as.factor(savem)
datas3$maxmonth=factor(datas3$maxmonth,levels=levels(datas3$maxmonth)[c(5,4,7,1,8,6,2,3)])
datas3$monthin=savein
pdf("figs3/figs3.pdf",width=6.65,height=6.83,paper='special')
ggplot(data=west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = datas3, mapping = aes(x = lon, y = lat, color = maxmonth, fill=maxmonth), size=4, shape=21)+
  theme(legend.position = c(0.927,0.824),legend.title=element_text(size=18), legend.text=element_text(size=15))+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.title = element_blank())+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_fill_brewer(palette="Spectral")+
  scale_colour_manual(values=rep("black",8))+
  scale_shape_manual(values  = c(16,16,16,16,16,16))+
  scale_size_manual(values = c(4,4,4,4,4,4))
dev.off()
###Fig.S4a  The temporal trend in a) rainfall within the 3 days of the flood event and 
ds4=read.table("Fig_S4a/trd_anm.csv",header=TRUE,sep=",")
dps4=read.table("Fig_S4a/trdp_anm.csv",header=TRUE,sep=",")
datas4a=cbind(ID2[,1:3],ds4[,3],dps4[,3])
colnames(datas4a)=c("ID","lat","lon","trend","p")
s4aclass1 <- datas4a %>%
  filter(trend>0) %>%
  filter(p<=0.05)
s4aclass1$group="+ (p<=0.05)"
s4aclass2 <- datas4a %>%
  filter(trend>0)%>%
  filter(p>0.05)
s4aclass2$group="+ (p>0.05)"
s4aclass3 <- datas4a %>%
  filter(trend<0) %>%
  filter(p<=0.05)
s4aclass3$group="- (p<=0.05)"
s4aclass4 <- datas4a %>%
  filter(trend<0)%>%
  filter(p>0.05)
s4aclass4$group="- (p>0.05)"
s4aplot=rbind(s4aclass1,s4aclass2,s4aclass3,s4aclass4)
ggplot(data = west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = s4aplot, mapping = aes(x = lon, y = lat, fill=group, color = group, shape=group, size=group))+
  scale_alpha(guide = 'none')+
  #ggtitle("ANM flood timing trend")+
  #theme(
  #  plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  #)+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.112,0.103),legend.title=element_blank(), legend.text=element_text(size=15))+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("red", "red", "blue", "blue"))+
  scale_shape_manual(values  = c(16,8,16,8))+
  scale_size_manual(values = c(4,4,4,4))
###Fig.S4b  b) maximum 7-day rainfall in a year in 1960-2012 
ds4b=read.table("figs4/trds_rain.csv",header=TRUE,sep=",")
ds4b=ds4b[order(ds4b$X),]
dps4b=read.table("figs4/trdps_rain.csv",header=TRUE,sep=",")
dps4b=dps4b[order(dps4b$X),]
datas4b=cbind(ID1[,1:3],ds4b[,2],dps4b[,2])
colnames(datas4b)=c("ID","lat","lon","trend","p")
s4bclass1 <- datas4b %>%
  filter(trend>0) %>%
  filter(p<=0.05)
s4bclass1$group="+ (p<=0.05)"
s4bclass2 <- datas4b %>%
  filter(trend>0)%>%
  filter(p>0.05)
s4bclass2$group="+ (p>0.05)"
s4bclass3 <- datas4b %>%
  filter(trend<0) %>%
  filter(p<=0.05)
s4bclass3$group="- (p<=0.05)"
s4bclass4 <- datas4b %>%
  filter(trend<0)%>%
  filter(p>0.05)
s4bclass4$group="- (p>0.05)"
s4bplot=rbind(s4bclass1,s4bclass2,s4bclass3,s4bclass4)
pdf("figs4/figs4a.pdf",width=6.65,height=6.83,paper='special')
ggplot(data = west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = s4bplot, mapping = aes(x = lon, y = lat, fill=group, color = group, shape=group, size=group))+
  scale_alpha(guide = 'none')+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.127,0.097),legend.title=element_blank(), legend.text=element_text(size=15))+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("red", "red", "blue", "blue"))+
  scale_shape_manual(values  = c(16,1,16,1))+
  scale_size_manual(values = c(4,4,4,4))
dev.off()

###Fig.S4 b) melting season air temperature in a year in 1960-2012 
ds5=read.csv("figs4/trds_tair.csv",header=TRUE)
dps5=read.csv("figs4/trdps_tair.csv",header=TRUE)
datas4b=cbind(ID1[,1:3],ds5[,2],dps5[,2])
colnames(datas4b)=c("ID","lat","lon","trend","p")
s4bclass1 <- datas4b %>%
  filter(trend>0) %>%
  filter(p<=0.05)
s4bclass1$group="+ (p<=0.05)"
s4bclass2 <- datas4b %>%
  filter(trend>0)%>%
  filter(p>0.05)
s4bclass2$group="+ (p>0.05)"
s4bclass3 <- datas4b %>%
  filter(trend<0) %>%
  filter(p<=0.05)
s4bclass3$group="- (p<=0.05)"
s4bclass4 <- datas4b %>%
  filter(trend<0)%>%
  filter(p>0.05)
s4bclass4$group="- (p>0.05)"
s4bplot=rbind(s4bclass1,s4bclass2,s4bclass3,s4bclass4)
pdf("figs4/figs4b.pdf",width=6.65,height=6.83,paper='special')
ggplot(data = west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "bl ack") +
  geom_point(data = s4bplot, mapping = aes(x = lon, y = lat, fill=group, color = group, shape=group, size=group))+
  scale_alpha(guide = 'none')+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.127,0.078),legend.title=element_blank(), legend.text=element_text(size=15))+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("blue", "red", "red"))+
  scale_shape_manual(values  = c(1,16,1))+
  scale_size_manual(values = c(4,4,4))
dev.off()
##Figure S5a magnitude trend for pot
trds=read.csv("figs5/trds_pot.csv",header=TRUE)
trdps=read.csv("figs5/trdps_pot.csv",header=TRUE)
#trdst=read.csv("figs5/trds_flooddate.csv",header=FALSE)
#trdpst=read.csv("figs5/trdps_flooddate.csv",header=FALSE)
alldata=cbind(ID[,1:3],trds[,2],trdps[,2])
colnames(alldata)=c("ID","lat","lon","pot_trend","pot_p")
##ANM Flood magnitude trend##
#=============================================================================
potclass1 <- alldata %>%
  filter(pot_trend>0) %>%
  filter(pot_p<=0.05)
potclass1$alpha=0.9
potclass1$group="+ (p<=0.05)"
potclass2 <- alldata %>%
  filter(pot_trend>0)%>%
  filter(pot_p>0.05)
potclass2$alpha=0.88
potclass2$group="+ (p>0.05)"
potclass3 <- alldata %>%
  filter(pot_trend<0) %>%
  filter(pot_p<=0.05)
potclass3$alpha=0.9
potclass3$group="- (p<=0.05)"
potclass4 <- alldata %>%
  filter(pot_trend<0)%>%
  filter(pot_p>0.05)
potclass4$alpha=0.88
potclass4$group="- (p>0.05)"
potplot=rbind(potclass1,potclass2,potclass3,potclass4)

##Plot with map
# plot the points and color them by sector
##Plot without background
pdf("figs5/figs5a.pdf",width=6.65,height=6.83,paper='special')
ggplot(data = west_coast) + 
  geom_polygon(aes(x=lon, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = potplot, mapping = aes(x = lon, y = lat, fill=group, color = group, shape=group, size=group))+
  scale_alpha(guide = 'none')+
  #ggtitle("ANM flood magnitude trend")+
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,family='', face='bold',size=20)
  )+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.13,0.097),legend.title=element_blank(), legend.text=element_text(size=15))+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18))+
  scale_colour_manual(values = c("red", "red", "blue", "blue"))+
  scale_shape_manual(values  = c(16,1,16,1))+
  scale_size_manual(values = c(4,4,4,4))
dev.off()
###Fig3 #############################
#install.packages(c("modifiedmk"))
library(mblm)
library(modifiedmk)
pdf("fig3/Fig3.pdf",width=7,height=4.5)
magni=read.csv("fig3/fltype_mean.csv",header=TRUE)
par(oma=c(2,2,0,0),mfrow=c(2,3),mgp=c(2,0.5,0),mar=c(2,2,2,0.2))  ##Width=769, height=549
colnames(magni)=c("year",colnames(magni)[2:7])
plot(x=magni$year,y=magni$ROS,col="#990033",xlab="",ylab="",ylim=c(0,4.5),cex.lab=1.5,cex.axis=1.5,pch=19,axes=FALSE)
axis(1, c(1950,1960,1980,2000,2020), tck=-0.02,cex.axis=1.5,col.axis="white")
axis(2, c(0,1,2,3,4,5), tck=-0.02,cex.axis=1.5)
ind=which(!is.na(magni$ROS))
a=magni$ROS[ind]
b=magni$year[ind]
m1=mblm(a~b,repeated = FALSE)
lines(x=magni$year,y=m1$coefficients[1]+m1$coefficients[2]*magni$year,col="black",lwd=2)
title("ROS snowmelt",font.main = 1,cex.main=1.5)
text(1987,4.25,paste0("s=",round(m1$coefficients[2],5)),cex=1.5)

plot(x=magni$year,y=magni$RS,col="#FF6600",xlab="",ylab="",ylim=c(0,4.5),cex.lab=1.5,pch=19,axes=FALSE)
axis(1, c(1950,1960,1980,2000,2020), tck=-0.02,cex.axis=1.5,col.axis="white")
axis(2, c(0,1,2,3,4,5), tck=-0.02,cex.axis=1.5,col.axis="white")
a=magni$RS
b=magni$year
m1=mblm(a~b,repeated = FALSE)
lines(x=magni$year,y=predict(m1),col="black",lwd=2)
title("RAD snowmelt",font.main = 1,cex.main=1.5)
text(1987,4.25,paste0("s=",round(m1$coefficients[2],5)),cex=1.5)

plot(x=magni$year,y=magni$MS,col="#FF0000",xlab="",ylab="",ylim=c(0,4.5),cex.lab=1.5,cex.axis=1.5,pch=19,axes=FALSE)
ind=which(!is.na(magni$MS))
axis(1, c(1950,1960,1980,2000,2020), tck=-0.02,cex.axis=1.5,col.axis="white")
axis(2, c(0,1,2,3,4,5), tck=-0.02,cex.axis=1.5,col.axis="white")
a=magni$MS[ind]
b=magni$year[ind]
m1=mblm(a~b,repeated = FALSE)
lines(x=magni$year,y=m1$coefficients[1]+m1$coefficients[2]*magni$year,col="black",lwd=2)
title("MS",font.main = 1,cex.main=1.5)
text(1987,4.25,paste0("s=",round(m1$coefficients[2],5)),cex=1.5)

plot(x=magni$year,y=magni$CAPE,col="#0000CC",xlab="",ylab="",ylim=c(0,4.5),cex.lab=1.5,cex.axis=1.5,pch=19,axes=FALSE)
axis(1, c(1950,1960,1980,2000,2020), tck=-0.02,cex.axis=1.5)
axis(2, c(0,1,2,3,4,5), tck=-0.02,cex.axis=1.5)
ind=which(!is.na(magni$CAPE))
a=magni$CAPE[ind]
b=magni$year[ind]
m1=mblm(a~b,repeated=FALSE)
lines(x=magni$year,y=m1$coefficients[1]+m1$coefficients[2]*magni$year,col="black",lwd=2)
title("Convective non-monsoon",font.main = 1,cex.main=1.5)
text(1987.5,4.25,paste0("s=",round(m1$coefficients[2],5)),cex=1.5)
#text(1968.7,3.8,"p=0.71",cex=1.5)

plot(x=magni$year,y=magni$AR,col="#006600",xlab="",ylab="",ylim=c(0,4.5),cex.lab=1.5,cex.axis=1.5,pch=19,axes=FALSE)
axis(1, c(1950,1960,1980,2000,2020), tck=-0.02,cex.axis=1.5)
axis(2, c(0,1,2,3,4,5), tck=-0.02,cex.axis=1.5,col.axis="white")
a=magni$AR
b=magni$year
m1=mblm(a~b,repeated = FALSE)
lines(x=magni$year,y=predict(m1),col="black",lwd=2)
title("AR",font.main = 1,cex.main=1.5)
text(1987,4.25,paste0("s=",round(m1$coefficients[2],5)),cex=1.5)
#text(1968.7,3.8,"p=0.93",cex=1.5)

plot(x=magni$year,y=magni$FRONT,col="#9933FF",xlab="",ylab="",ylim=c(0,4.5),cex.lab=1.5,cex.axis=1.5,pch=19,axes=FALSE)
axis(1, c(1950,1960,1980,2000,2020), tck=-0.02,cex.axis=1.5)
axis(2, c(0,1,2,3,4,5), tck=-0.02,cex.axis=1.5,col.axis="white")
a=magni$FRONT
b=magni$year
m1=mblm(a~b,repeated = FALSE)
lines(x=magni$year,y=predict(m1),col="black",lwd=2)
title("FRONT",font.main = 1,cex.main=1.5)
text(1987.7,4.25,paste0("s=",round(m1$coefficients[2],5)),cex=1.5)
#text(1968.7,3.8,"p=0.85",cex=1.5)
# print the overall labels
title(xlab="Year",ylab = "Normalized Magnitude",outer=TRUE,cex.lab=1.5,line=0)
dev.off()
####Fig 4########
frac=read.csv("fig4/fltype_count.csv",header=TRUE)
year=frac$year
frac=cbind(year,frac[,2:7]/rowSums(frac[,2:7]))
colnames(frac)=c("year",colnames(frac)[2:7])
Fraction=c(frac$RS,frac$FRONT,frac$AR,frac$CAPE,frac$ROS,frac$MS)
Year=rep(as.numeric(frac$year),times=6)
Type=rep(c("RAD snowmelt","Frontal Non-AR","AR","Convective NonMS","ROS snowmelt","MS"),each=54)
data=data.frame(Type,Year,Fraction)
data$Type=factor(data$Type,levels=rev(c("MS","Convective NonMS","ROS snowmelt","AR","Frontal Non-AR","RAD snowmelt"))) ##From top to bottom, list the type you want to plot in this order.
library(ggplot2)
library(grid)
pdf("fig4/fig4.pdf",width=7,height=4.5,paper='special')
ggplot(data, aes(x=Year, y=Fraction, fill=Type)) + 
  geom_area(alpha=0.9)+
  #ylab("Mechanism Fraction")+
  ylab("Mechanism Fraction")+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.5,0.87),legend.text=element_text(size=15))+
#  scale_fill_manual(values = c("orange","purple","darkgreen","dark red","blue", "red"))+
  scale_fill_manual(values = c("sandybrown","mediumpurple1","green3","#9F2A00","dodgerblue", "brown1"))+
  guides(fill=guide_legend(ncol=2),byrow=FALSE)+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+#
  geom_hline(yintercept=0.5,col='white')+
  geom_hline(yintercept=0.25,col='white')+
  geom_hline(yintercept=0.0,col='white')+
  geom_hline(yintercept=0.75,col='white')+
  geom_vline(xintercept=1960,col='white')+
  geom_vline(xintercept=1970.0,col='white')+
  geom_vline(xintercept=1980.0,col='white')+
  geom_vline(xintercept=1990.0,col='white')+
  geom_vline(xintercept=2000.0,col='white')+
  geom_vline(xintercept=2010.0,col='white')
dev.off()
#####new Fig 2 curves plot######
frac=read.csv("fig2/count_year.csv",header=TRUE)
year=frac$Date
colnames(frac)=c("year",colnames(frac)[2:7])
pdf("fig2/fig2a.pdf",width=7,height=5,paper='special')
par(mgp=c(3,1.5,0),tck=-0.02,mar=c(5,5,2,2))
plot(x=frac$year,y=frac$RS,ylim=c(0,0.6),type='l',col="sandybrown",xlab="Year",ylab="FGM Frequency",cex.lab=1.5,cex.axis=1.5,lwd=1.5)
a=frac$RS
b=frac$year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$year,y=predict(m1),col="sandybrown",lwd=2,lty=2)

lines(x=frac$year,y=frac$ROS,col="#9F2A00",lwd=1.5)
a=frac$ROS
b=frac$year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$year,y=predict(m1),col="grey",lwd=3,lty=1)
lines(x=frac$year,y=predict(m1),col="#9F2A00",lwd=3,lty=2)

lines(x=frac$year,y=frac$MS,col="brown1",lwd=1.5)
a=frac$MS
b=frac$year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$year,y=predict(m1),col="brown1",lwd=2,lty=2)

lines(x=frac$year,y=frac$CAPE,col="dodgerblue1",lwd=1.5)
a=frac$CAPE
b=frac$year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$year,y=predict(m1),col="grey",lwd=3,lty=1)
lines(x=frac$year,y=predict(m1),col="dodgerblue1",lwd=3,lty=2)

lines(x=frac$year,y=frac$AR,col="green3",lwd=1.5)
a=frac$AR
b=frac$year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$year,y=predict(m1),col="green3",lwd=2,lty=2)

lines(x=frac$year,y=frac$FRONT,col="mediumpurple1",lwd=1.5)
a=frac$FRONT
b=frac$year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$year,y=predict(m1),col="mediumpurple1",lwd=2,lty=2)
legend("top",col=c("sandybrown","#9F2A00","brown1","dodgerblue1","green3","mediumpurple1"),lwd=1.5,c("RS","ROS","MS","CAPE","AR","FRONT"),cex=1,ncol=3)
dev.off()

frac=read.csv("fig2/volumn_year.csv",header=TRUE)
year=frac$Date
colnames(frac)=c("Year",colnames(frac)[2:7])
pdf("fig2/fig2b.pdf",width=7,height=5,paper='special')
par(mgp=c(3,1.5,0),tck=-0.02,mar=c(5,5,2,2))
plot(x=frac$Year,y=frac$RS,ylim=c(0,0.6),type='l',col="sandybrown",xlab="Year",ylab="FGM Volumetric Contribution",cex.lab=1.5,cex.axis=1.5,lwd=1.5)
a=frac$RS
b=frac$Year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$Year,y=predict(m1),col="sandybrown",lwd=2,lty=2)

lines(x=frac$Year,y=frac$ROS,col="#9F2A00",lwd=1.5)
a=frac$ROS
b=frac$Year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$Year,y=predict(m1),col="#9F2A00",lwd=2,lty=2)

lines(x=frac$Year,y=frac$MS,col="brown1",lwd=1.5)
a=frac$MS
b=frac$Year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$Year,y=predict(m1),col="brown1",lwd=2,lty=2)

lines(x=frac$Year,y=frac$CAPE,col="dodgerblue1",lwd=1.5)
a=frac$CAPE
b=frac$Year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$Year,y=predict(m1),col="grey",lwd=3,lty=1)
lines(x=frac$Year,y=predict(m1),col="dodgerblue1",lwd=3,lty=2)

lines(x=frac$Year,y=frac$AR,col="green3",lwd=1.5)
a=frac$AR
b=frac$Year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$Year,y=predict(m1),col="green3",lwd=2,lty=2)

lines(x=frac$Year,y=frac$FRONT,col="mediumpurple1",lwd=1.5)
a=frac$FRONT
b=frac$Year
m1=mblm(a~b,repeated=FALSE)
lines(x=frac$Year,y=predict(m1),col="mediumpurple1",lwd=2,lty=2)
#legend("top",col=c("sandybrown","#9F2A00","brown1","dodgerblue1","green3","mediumpurple1"),lwd=1.5,c("RAD Snowmelt","Rain-On-Snow","MS","Convective Storm","Frontal AR","Frontal Non-AR"),cex=1,ncol=3)
dev.off()
####Fig S7(S6) 
frac=read.csv("fig2/volumn_year.csv",header=TRUE)
year=frac$year
Fraction=c(frac$RS,frac$FRONT,frac$AR,frac$CAPE,frac$MS,frac$ROS)
Year=rep(as.numeric(frac$Date),times=6)
Type=rep(c("RS","FRONT","AR","CAPE","MS","ROS"),each=59)
data=data.frame(Type,Year,Fraction)
data$Type=factor(data$Type,levels=rev(c("MS","CAPE","ROS","AR","FRONT","RS")))##From top to bottom, list the type you want to plot in this order.
library(ggplot2)
library(grid)
pdf("figs6/figs6.pdf",width=7,height=4.5,paper='special')
ggplot(data, aes(x=Year, y=Fraction, fill=Type)) + 
  geom_line()+
  #ylab("Mechanism Fraction")+
  ylab("Volumetric Fraction")+
  theme(axis.text=element_text(size=12),axis.title = element_text(size=15))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="lightgrey", 
                                         size=0.5, linetype="solid"))+
  theme(legend.position = c(0.5,1),legend.text=element_text(size=15))+
#  scale_fill_manual(values = c("orange","purple","green","dark red","blue", "red"))+
  scale_fill_manual(values = c("sandybrown","mediumpurple1","green3","#9F2A00","dodgerblue", "brown1"))+
  guides(fill=guide_legend(ncol=2),byrow=FALSE)+
  theme(plot.margin = unit(c(2,1,1,1), "lines"))+ # This widens the right margin
  geom_hline(yintercept=0.5,col='white')+
  geom_hline(yintercept=0.25,col='white')+
  geom_hline(yintercept=0.0,col='white')+
  geom_hline(yintercept=0.75,col='white')+
  geom_vline(xintercept=1960,col='white')+
  geom_vline(xintercept=1970.0,col='white')+
  geom_vline(xintercept=1980.0,col='white')+
  geom_vline(xintercept=1990.0,col='white')+
  geom_vline(xintercept=2000.0,col='white')+
  geom_vline(xintercept=2010.0,col='white')
dev.off()
#==================This is the end for Fig S7