#################################################################################################################
#################################################################################################################
#####################  Calculation of Environmental Indicators for WCPFC SC17-EB-IP-09
#######  Requires: BRAN2020 pre-processed NetCDF files for SST, SSS and MLD
#######  Chamberlain et al. (2021) https://research.csiro.au/bluelink/bran2020-data-released/
####### Authors: Joe Scutt Phillips, Annie Portal June 2021
####### Oceanic Fisheries Programme, FAME, The Pacific Community
#################################################################################################################
#################################################################################################################

library(dplyr)

# Need to define the directories which will hold the appropriate environmental netcdfs and we will save our indicators:
#data_dir
#save_dir

# And here's a function to handle the netcdf loading
loadField = function(fname, var_name=NULL, coords=c("longitude", "latitude", "time")){
  require(ncdf4)
  if(!file.exists(fname)){
    print("File doesn't exist!")
    return(NULL)
  }
  data = nc_open(fname)
  if(is.null(var_name)){
    sizes = vector()
    vars = names(data$var)
    for(v in vars){
      var_temp = ncvar_get(data,v)
      sizes = c(sizes, length(unlist(var_temp)))
    }
    var_name = vars[which.max(sizes)]
  }
  var = ncvar_get(data, var_name)
  lon = ncvar_get(data, coords[1])
  if(length(dim(lon)) > 1)
    lon = lon[,1]
  lat = ncvar_get(data, coords[2])
  if(length(dim(lat)) > 1)
    lat = lat[1,]
  time = ncvar_get(data, coords[3])
  nc_close(data)
  
  return(list(var=var,lon=lon,lat=lat, time=time))
}

# Here are our main spatial filter definitions and other limits
wcpo_lons = c(130,210)
wcpo_lats = c(-50, 50)

wp_temp = 29


###### SST-based indicators ######

#First we should load our netcdfs that we'll need
mon_sst = loadField(paste0(data_dir,'BRAN_WCPO_monthly_SST_2000-2019.nc'), coords=c('xt_ocean', 'yt_ocean', 'Time'))

#Let's make the timestamps a bit more understandable!
mon_sst$Time = as.Date('1979-01-01') + mon_sst$time

#Now let's filter it spatially so that's just the WCPO, and save as a new smaller array
wcpo_lon_filter = between(mon_sst$lon, wcpo_lons[1], wcpo_lons[2])
wcpo_lat_filter = between(mon_sst$lat, wcpo_lats[1], wcpo_lats[2])

#Lets reduce our sst data just to the wcpo then
wcpo_mon_sst = mon_sst$var[wcpo_lon_filter,wcpo_lat_filter,]

### WCPO-wide SST anomaly ###

#First up yo, if we talking anomalies...we gotta calculate the mean temp from which determine the anomaly
wcpo_av_mon_sst = apply(wcpo_mon_sst, c(1,2),mean)

#Now we gonna calculate the anomaly for all cells in the whole WCPO

mon_anom = array(,dim(wcpo_mon_sst)) #The anomaly data will have the same shape (dimensions) as our raw sst data
for(t in 1:dim(mon_anom)[3]){
  mon_anom[,,t] = wcpo_mon_sst[,,t] - wcpo_av_mon_sst
}

#We are going to make a nice empty table with dates and an empty column for the index we're gonna calculate
# (we're going to do this for each index so when we save it to an RDS file, it's clair, net, et bien formee)
Yr_WCPO_Mean_Anom = data.frame(Date = mon_sst$Time[-6 + cumsum(rep(12,dim(mon_anom)[3]/12))], # This gives a nice middle date of mid-june each year
                              Ind=NA) 

for(t in 1:nrow(Yr_WCPO_Mean_Anom)){ #So for each year we're going to calculate the mean WCPO anomaly
  start = ((t-1)*12+1) #Jan of that year
  end = start+11 #to dec
  Yr_WCPO_Mean_Anom$Ind[t] = mean(mon_anom[,,start:end], na.rm=TRUE)
}
plot(Yr_WCPO_Mean_Anom$Date, Yr_WCPO_Mean_Anom$Ind, type='l')
lines(c(0,1e+10), c(0,0), lty=3, lw=2, col='gray90')

#Save that index!
saveRDS(Yr_WCPO_Mean_Anom, file=paste0(save_dir,'WCPOMeanAnomaly.RDS'))


### Equatorial WCPO only anomaly ###
#So this is exactly the same thing as above, except we will add another latitude filter to cut to equatorial zone only
wcpo_eq_filter = between(mon_sst$lat[between(mon_sst$lat, wcpo_lats[1], wcpo_lats[2])], -5,5)

Yr_EqWCPO_Mean_Anom = data.frame(Date = mon_sst$Time[-6 + cumsum(rep(12,dim(mon_anom)[3]/12))], # This gives a nice middle date of mid-june each year
                                 Ind=NA) 

for(t in 1:nrow(Yr_EqWCPO_Mean_Anom)){ #So for each year we're going to calculate the mean WCPO anomaly
  start = ((t-1)*12+1) #Jan of that year
  end = start+11 #to dec
  Yr_EqWCPO_Mean_Anom$Ind[t] = mean(mon_anom[,wcpo_eq_filter,start:end], na.rm=TRUE)
}
plot(Yr_EqWCPO_Mean_Anom$Date, Yr_EqWCPO_Mean_Anom$Ind, type='l')
lines(c(0,1e+10), c(0,0), lty=3, lw=2, col='gray90')

#Save 
saveRDS(Yr_EqWCPO_Mean_Anom, file=paste0(save_dir,'EqWCPOMeanAnomaly.RDS'))


### Warm pool SST anomaly ###
Yr_WP_Mean_Anom = data.frame(Date = mon_sst$Time[1 + cumsum(rep(12,length(mon_sst$Time)/12 - 1))], #For warmpool stuff, let's make the date jan during the WP period
                             Ind=NA) # Minus 1 again because we are crossing the yearly boundary for the warmpool period

#First we gonna calculate the mean 2000-2020 temp for the whole WCPO  during the warmpool period each year
wp_mean_sst = array(0,dim(wcpo_mon_sst[,,1]))
for(t in 1:nrow(Yr_WP_Mean_Anom)){ 
  start = (t-1)*12 + 11 #The index for Nov each year
  end = start+5 #The index for Avr
  wp_mean_sst = wp_mean_sst + apply(wcpo_mon_sst[,,start:end],c(1,2), mean) # Add the mean for each year to the overall mean
}
wp_mean_sst = wp_mean_sst/nrow(Yr_WP_Mean_Anom) #Then divide by the number of years

#Now we gonna work out which cells are in the warm pool each year (nov-avril), and calculate just their anomalies
for(t in 1:nrow(Yr_WP_Mean_Anom)){
  start = (t-1)*12 + 11
  end = start+5
  warmpool_filter = wcpo_mon_sst[,,start:end] > wp_temp #Our warmpool filter
  #Work out all anomalies from all cells, but then filter so we get on cells that were in the warmpool
  all_anoms = (apply(wcpo_mon_sst[,,start:end],c(1,2),mean) - wp_mean_sst)[warmpool_filter]
  Yr_WP_Mean_Anom$Ind[t] = mean(all_anoms, na.rm=TRUE) #Save the mean anomaly for that year
}

plot(Yr_WP_Mean_Anom$Date, Yr_WP_Mean_Anom$Ind, type='l')
lines(c(0,1e+10), c(0,0), lty=3, lw=2, col='gray90')

#Save 
saveRDS(Yr_WP_Mean_Anom, file=paste0(save_dir,'WarmPoolMeanAnomaly.RDS'))


### Size of the warm-pool ###
#So now we're going to count up the number of the cells that constitute the warm-pool each year
Yr_WP_Size = data.frame(Date = mon_sst$Time[1 + cumsum(rep(12,length(mon_sst$Time)/12 - 1))], 
                        Ind=NA) # Minus 1 again because we are crossing the yearly boundary for the warmpool period

#Now we gonna work out which cells are in the warm pool each year (nov-avril), and calculate just their anomalies
for(t in 1:nrow(Yr_WP_Size)){
  start = (t-1)*12 + 11
  end = start+5
  warmpool_filter = wcpo_mon_sst[,,start:end] > wp_temp #Our warmpool filter
  Yr_WP_Size$Ind[t] = sum(warmpool_filter, na.rm=TRUE) #Save the number of cells consituting the warm pool this year (cell area varies slightly near the equator, but this gives approximate size)
}

plot(Yr_WP_Size$Date, Yr_WP_Size$Ind, type='l')
lines(c(0,1e+10), c(mean_WP_size,mean_WP_size), lty=3, lw=2, col='gray90')

#Save comme d'hab
saveRDS(Yr_WP_Size, file=paste0(save_dir,'WarmPoolSize.RDS'))



##### Sea surface salinity-based indicators #####
#Let's load some netcdfs
sss = loadField(paste0(data_dir,'BRAN_EqPacific_monthly_SSS_2000-2019.nc'), var='salt', coords=c('xt_ocean', 'yt_ocean', 'Time'))

#We need to filter our SSS data to very near the equator, 
#and not too west (where there is a permanent salinity barrier)
wcpo_sss_lon_filter = between(sss$lon, 130,240)
wcpo_sss_lat_filter = between(sss$lat, -2,2)

wcpo_sss = sss$var[wcpo_sss_lon_filter,wcpo_sss_lat_filter,]

Yr_BLayer_East = data.frame(Date = mon_sst$Time[1 + cumsum(rep(12,length(mon_sst$Time)/12 - 1))], #Let's use the nice time format from our SST data
                            Ind=NA)

par(mfrow=c(5,4))
for(t in 1:nrow(Yr_BLayer_East)){
  start = (t-1)*12 + 11
  end = start+3 #This time, the boundary layer is determined by Nov-Feb
  mean_SSS = apply(wcpo_sss[,,start:end], 1, mean, na.rm=TRUE) #We're going to calculate the mean for this period
  #We are going to look for the longitude value,
  #where there is the strongest change in sea surface salinity, over a 10degree window.
  #This is the index (i.e. where is there a salinty boundary between warm and cold water)
  Yr_BLayer_East$Ind[t] = sss$lon[wcpo_sss_lon_filter][which.max(diff(mean_SSS,100))+50]
  plot(sss$lon[wcpo_sss_lon_filter],mean_SSS,type='l',main=2000+t)
  lines(rep(Yr_BLayer_East$Ind[t],2), c(0,100), col='red')
}
par(mfrow=c(1,1))
plot(Yr_BLayer_East$Date, Yr_BLayer_East$Ind, type='l')

saveRDS(Yr_BLayer_East, file=paste0(save_dir,'WarmPoolBoundary.RDS'))


##### Mixed-layer depth stuff #####

## MLD within the warm pool
#Load the netcdfs
mon_mld = loadField(paste0(data_dir,'BRAN_WCPO_monthly_MLD_2000-2019.nc'), 
                var='mld', coords=c('xt_ocean', 'yt_ocean', 'Time'))
wcpo_mld_lon_filter = between(mon_mld$lon, wcpo_lons[1], wcpo_lons[2])
wcpo_mld_lat_filter = between(mon_mld$lat, wcpo_lats[1], wcpo_lats[2])


wcpo_mon_mld = mon_mld$var[wcpo_mld_lon_filter,wcpo_mld_lat_filter,mld_time_filter]

#Check that SST and MLD data have the same dimensions (MLD file had one more year)
dim(wcpo_mon_mld)
dim(wcpo_mon_sst)

Yr_WarmPool_MLD = data.frame(Date = mon_sst$Time[1 + cumsum(rep(12,length(mon_sst$Time)/12 - 1))], #Let's use the nice time format from our SST data
                             Ind=NA)

for(t in 1:nrow(Yr_WarmPool_MLD)){
  start = (t-1)*12 + 11
  end = start+5
  warmpool_filter = wcpo_mon_sst[,,start:end] > 29 #Calculate the warmpool extent, just like always
  #Now calculate the mean mld, for only those cells in the warmpool filter
  Yr_WarmPool_MLD$Ind[t] = mean(wcpo_mon_mld[,,start:end][warmpool_filter], na.rm=TRUE)
}

plot(Yr_WarmPool_MLD$Date, Yr_WarmPool_MLD$Ind, type='l')
saveRDS(Yr_WarmPool_MLD, file=paste0(save_dir,'WarmPool_MixedLayerDepth.RDS'))



##### Climate Indices #####
### Climate Indices
#ONI index during NDJ, taken from NOAA report
# and doneby hand!
temp = "2000\t-1.7\t-1.4\t-1.1\t-0.8\t-0.7\t-0.6\t-0.6\t-0.5\t-0.5\t-0.6\t-0.7\t-0.7\n2001\t-0.7\t-0.5\t-0.4\t-0.3\t-0.3\t-0.1\t-0.1\t-0.1\t-0.2\t-0.3\t-0.3\t-0.3\n2002\t-0.1\t0.0\t0.1\t0.2\t0.4\t0.7\t0.8\t0.9\t1.0\t1.2\t1.3\t1.1\n2003\t0.9\t0.6\t0.4\t0.0\t-0.3\t-0.2\t0.1\t0.2\t0.3\t0.3\t0.4\t0.4\n2004\t0.4\t0.3\t0.2\t0.2\t0.2\t0.3\t0.5\t0.6\t0.7\t0.7\t0.7\t0.7\n2005\t0.6\t0.6\t0.4\t0.4\t0.3\t0.1\t-0.1\t-0.1\t-0.1\t-0.3\t-0.6\t-0.8\n2006\t-0.9\t-0.8\t-0.6\t-0.4\t-0.1\t0.0\t0.1\t0.3\t0.5\t0.8\t0.9\t0.9\n2007\t0.7\t0.2\t-0.1\t-0.3\t-0.4\t-0.5\t-0.6\t-0.8\t-1.1\t-1.3\t-1.5\t-1.6\n2008\t-1.6\t-1.5\t-1.3\t-1.0\t-0.8\t-0.6\t-0.4\t-0.2\t-0.2\t-0.4\t-0.6\t-0.7\n2009\t-0.8\t-0.8\t-0.6\t-0.3\t0.0\t0.3\t0.5\t0.6\t0.7\t1.0\t1.4\t1.6\t2010\t1.5\t1.2\t0.8\t0.4\t-0.2\t-0.7\t-1.0\t-1.3\t-1.6\t-1.6\t-1.6\t-1.6\n2011\t-1.4\t-1.2\t-0.9\t-0.7\t-0.6\t-0.4\t-0.5\t-0.6\t-0.8\t-1.0\t-1.1\t-1.0\n2012\t-0.9\t-0.7\t-0.6\t-0.5\t-0.3\t0.0\t0.2\t0.4\t0.4\t0.3\t0.1\t-0.2\n2013\t-0.4\t-0.4\t-0.3\t-0.3\t-0.4\t-0.4\t-0.4\t-0.3\t-0.3\t-0.2\t-0.2\t-0.3\n2014\t-0.4\t-0.5\t-0.3\t0.0\t0.2\t0.2\t0.0\t0.1\t0.2\t0.5\t0.6\t0.7\n2015\t0.5\t0.5\t0.5\t0.7\t0.9\t1.2\t1.5\t1.9\t2.2\t2.4\t2.6\t2.6\n2016\t2.5\t2.1\t1.6\t0.9\t0.4\t-0.1\t-0.4\t-0.5\t-0.6\t-0.7\t-0.7\t-0.6\n2017\t-0.3\t-0.2\t0.1\t0.2\t0.3\t0.3\t0.1\t-0.1\t-0.4\t-0.7\t-0.8\t-1.0\n2018\t-0.9\t-0.9\t-0.7\t-0.5\t-0.2\t0.0\t0.1\t0.2\t0.5\t0.8\t0.9\t0.8\n2019\t0.7\t0.7\t0.7\t0.7\t0.5\t0.5\t0.3\t0.1\t0.2\t0.3\t0.5\t0.5\n2020\t0.5\t0.5\t0.4\t0.2\t-0.1\t-0.3\t-0.4\t-0.6\t-0.9\t-1.2\t-1.3\t-1.2"
temp2 = strsplit(temp, '\t')
temp3 = strsplit(unlist(temp2), '\n')
ONI = matrix(as.numeric(unlist(temp3)),21,13,byrow=TRUE)
row.names(ONI) = ONI[,1]
ONI = ONI[,-1]
plot(seq(-11,20,length=length(c(ONI))),c(ONI), type='l', xaxt='n',yaxt='n',xlab='',ylab='', bty='n', xlim=c(1,nrow(ONI)))
for(yr in c(seq(1,length(ONI)-1,5), length(Yr_Eq_Mean_Anom)))
  lines(c(yr,yr), c(-1e14,1e14), lty=3, col='gray70')
lines(c(-1,length(ONI)), c(0,0), col='gray70')
lines(ONI[,12], lwd=3)
points(length(ONI), last(ONI), col='red', pch=19, cex=2)
Yr_ONI = data.frame(Date = as.Date(paste(row.names(ONI),'12','15',sep='-')), #Let's use the nice time format from our SST data
                             Ind=ONI[,12])
saveRDS(Yr_ONI, file=paste0(save_dir,'ONI_NDJ_Index.RDS'))


#Annual IPO index
#also done by hand for now!
IPO = data.frame(Date = as.Date(paste(row.names(ONI)[1:17],'1','1',sep='-')), #Let's use the nice time format from our SST data
                 Ind=c(-0.415,-0.5375,-0.575,-0.5875,-0.6425,-0.7625,-0.9175,-1.09,
                       -1.2175,-1.275,-1.2375,-1.0725,-0.755,-0.3275,0.1475,0.5775,0.8975))

plot(IPO$Date, IPO$Ind, type='l',lwd=3)
lines(c(-1e14,1e14), c(0,0), col='gray70') 

saveRDS(IPO, file=paste0(save_dir,'IPO_Index.RDS'))

