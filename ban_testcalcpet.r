setwd("/home/zban/Downloads/Master_thesis/multimodel/sac/pet.generating.for.sac")
coor=read.table("yourcoorlist")
for(i in 1:1010){
  lat=coor[i,1]  ## for a specific lat lon from yourcoorlist
  lon=coor[i,2]
  data=read.table(paste0("/mnt/nfs/home/zban/Master_thesis/Livnehthings/multimodel/results/livsac/pet.generating/results/N/origin/fluxes_",lat,"_",lon))
  surf_temp = data[5]
  air_temp = data[6]
  r_net = data[7]
  vpd = data[8]
  wind = data[9]
  ##Check which veg type
  ETf=rep(0,length(surf_temp[,]))
  lambda=2.501-0.002361*(surf_temp)  ##[MJ/kg] You can also take a single value of 2.45 MJ/Kg in FAO chapter 3.
  delta=4098*(0.6108*exp((17.27*air_temp)/(air_temp+237.3)))/((air_temp+237.3)^2) #[kPa/C] ##Eq.13 in FAO chapter 3
  gamma=0.0016286*101.325/lambda ##[kPa/K] You can also use 0.665*0.001*P (Eq.8 in FAO chapter 3).
  gamma_star = gamma * (1 + 0.34 * wind)
  G = surf_temp
  indseq=seq(1,length(data[,9]),by=1)
  ind=which((indseq %% 8 >=3) & (indseq %% 8 <=6)) ##Day
  G[ind,1] = 0.1*r_net[ind,1]
  newind=which((indseq %% 8 < 3) | (indseq %% 8 >6)) ##Night, note that monthly need another approximation, see FAO website (chapter 4).
  G[newind,1] = 0.5*r_net[newind,1]
  r_net_conv = r_net * (10^(-6)) / 24*60*60 #[MJ/(m2.day)]
  G_conv = G * (10^(-6)) / 24*60*60 #[MJ/(m2.day)]
  Ref_ET = 0.408*((delta/(delta + gamma_star)) * (r_net_conv - G_conv)) + ((gamma/(delta + gamma_star)) * (900/(air_temp + 273)) * wind*vpd) #(mm/day)
  Ref_ET = as.matrix(Ref_ET)
  write.table(Ref_ET,paste0("/home2/zban/Master_thesis/Livnehthings/multimodel/newrun/sac/pet.generating.for.sac/results/N/newpm/origin/testpet_fluxes_",lat,"_",lon),row.names=FALSE, col.names=FALSE)
}
