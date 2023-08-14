############################################################
# Data bundling for Ecosystem Indicators SC19 paper 2023
############################################################

# Created on: 11/07/23
# Latest update: 12/07/23
# Created by: Jed Macdonald $ Joe Scutt Phillips

# Code for compiling, bundling and plotting data on selected indicators for inclusion in the 2023 SC19 paper.

# The indicators fall within the following Report Card headings:

# 1) Environment and climate indicators (1993:2021 inclusive) 
# 2) Fishing Effort indicators (1990:2022 inclusive)
# 3) Annual Tuna Catch indicators (1990:2022 inclusive)
# 4) Fish Condition indicators (1990:2022 inclusive)
# 5) Bycatch indicators (2003:2018 or 2020 inclusive) 

library(RColorBrewer)
library(dplyr)

############################################################
# Functions
############################################################

# Creates a two-column dataframe with 'Date' and 'Ind' fields
createInd = function(data, year, YearRange=range(year)){
  Ind = data.frame(Date = as.Date(paste(YearRange[1]:YearRange[2],'07-01',sep='-')),
                   Ind = NA)
  Ind$Ind[YearRange[1]:YearRange[2] %in% year] = data
  return(Ind[order(Ind$Date),])
}

# Sparkline plotting function (modified from Joe Scutt Phillips' code)
plotEnvInd = function(Index, DateRange=as.Date(c('1990-07-01', '2024-07-01')), finalInd=TRUE, # alter DateRange as needed to encompass max. range of years available and plotting margins
                      rounder=if(is.data.frame(Index)){2}else{rep(2,length(Index))}, 
                      final_paste=if(is.data.frame(Index)){''}else{rep('',length(Index))},
                      final_pos=if(is.data.frame(Index)){4}else{rep(4,length(Index))},
                      final_adj=if(is.data.frame(Index)){0}else{rep(0,length(Index))},
                      final_xadj=if(is.data.frame(Index)){0}else{rep(0,length(Index))},
                      lcols=if(is.data.frame(Index)){'skyblue'}else{1:length(Index)},
                      finalcols=if(is.data.frame(Index)){'blue3'}else{1:length(Index)}, 
                      lty=if(is.data.frame(Index)){1}else{rep(1,length(Index))},
                      lwd=if(is.data.frame(Index)){2.2}else{rep(2.2,length(Index))},
                      ylim=if(!is.data.frame(Index)){range(as.vector(unlist(mapply(get,'Ind',Index))), na.rm=TRUE)}else{range(Index$Ind, na.rm=TRUE)},
                      baseline='mean',baseline_label=NULL, baseline_paste = '', drawaxis=NULL,  yax_labels=yaxis, yax_paste='', yax_adj=rep(NA,2),
                      yaxis=round(quantile(if(!is.data.frame(Index)){as.vector(unlist(mapply(get,'Ind',Index)))}else{Index$Ind}, na.rm=TRUE)[c(2,4)],rounder),
                      caption=NULL, caption_pos='topright', ax.lab.col='black',
                      add=FALSE, multiplot=FALSE, pdf=NULL, drawLegend=FALSE, legend.lty=1,legend.lwd=1, legend,x, ...){
  if(multiplot){
    cex_mult = 2
    font_mult = 1.8
  }else{cex_mult = font_mult = 1}
  if(!is.null(pdf)){
    par(mfrow=c(1,1),mar=c(0,0,0,0),omi=c(0,0,0,0),omd=c(0,0,0,0))
    pdf(paste0(pdf))
    cex_mult = 2
    font_mult = 1.8
  }
  
  if(is.data.frame(Index)){
    Index = list(Index)
  }
  
  if(!add)
    plot(Index[[1]]$Date, Index[[1]]$Ind, xlim=DateRange, type='n',bty='n', xaxs='i',
         xaxt='n', yaxt='n', xlab='',ylab='',ylim=ylim)
  if(!is.na(baseline)){
    if(baseline=='mean')
      baseline = mean(as.vector(unlist(mapply(get,'Ind',Index))), na.rm=TRUE)
    lines(c(-1e14,1e14), rep(baseline,2), lty=1, col='gray90', lwd=1*cex_mult)
    axis(2, at=baseline, 
         labels=paste0(ifelse(is.null(baseline_label),round(baseline,rounder),baseline_label), baseline_paste),
         las=2, col.ticks='gray90', col=NA, col.axis=ax.lab.col, cex.axis=0.7*font_mult)
  }
  if(!is.null(yaxis)){
    axis(2, at=yaxis, padj=-yax_adj,
         labels=paste0(yax_labels,yax_paste),
         las=2, col.ticks='gray90', col=NA, col.axis=ax.lab.col, cex.axis=0.7*font_mult)
    lines(c(-1e14,1e14), rep(yaxis[1],2), lty=2*cex_mult, col='gray90', lwd=1*cex_mult)
    lines(c(-1e14,1e14), rep(yaxis[2],2), lty=2*cex_mult, col='gray90', lwd=1*cex_mult)
  }
  if(!is.null(drawaxis))
    axis(ifelse(drawaxis == 'bottom',1,3), at=seq(DateRange[1], DateRange[2], 365.2*5), 
         labels=format(round(as.POSIXlt(seq(DateRange[1], DateRange[2], 365.2*5)),'years'),format="%Y"),
         col.ticks='gray70', col=NA, col.axis=ax.lab.col, cex.axis=0.9)
  
  for(yr in seq(DateRange[1],DateRange[2],365*5))
    lines(c(yr,yr), c(-1e14,1e14), lty=3, col='gray80', xpd=ifelse(is.null(drawaxis),NA,FALSE), lwd=1*font_mult)
  
  for(ind in 1:length(Index)){
    Ind = Index[[ind]]
    lcol = lcols[ind]
    finalcol = finalcols[ind]
    ltype=lty[ind]
    fpos = final_pos[ind]
    lines(Ind$Date, Ind$Ind, col=lcol, lwd=lwd*cex_mult, lend=2, lty=ltype)
    
    points(last(Ind$Date), last(Ind$Ind), pch=19, cex=(lwd/2+0.1)*cex_mult, col=finalcol, xpd=NA)
    if(finalInd)
      text(last(Ind$Date)+final_xadj, last(Ind$Ind)+final_adj[ind], paste0(round(last(Ind$Ind),rounder),final_paste), pos=fpos, col=finalcol, cex=0.7*font_mult, offset=1, xpd=NA)
  }
  if(drawLegend)
    legend(x=x,legend=legend, lwd = legend.lwd, lty=legend.lty, bty='n', ...)
  if(!is.null(caption)){
    message(caption)
    legend(x=caption_pos, legend=caption, lty=0, col=NA, bty='n', cex=1.5)
  }
  if(!is.null(pdf) & !multiplot)
    dev.off()
}


############################################################
# Read in all data
############################################################

# Environment and Climate
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Environment and climate")
dir<-getwd()
myfiles = list.files(path=dir, pattern=".RDS", full.names=F, recursive = T)
env_list<-lapply(myfiles, readRDS)
names(env_list)<- gsub("\\.RDS","", myfiles)

# Fishing Effort
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Effort")
load("all_effort_indicators_2023.Rdata") # 'all.ind'

# Target Species Catch
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Catch and distribution")
load("Annual_Tuna_Catch.Rdata") # 'catch_list'

# Target Species Condition
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Fish condition")
dir<-getwd()
myfiles = list.files(path=dir, pattern="means", full.names=F, recursive = T)
cond_list<-lapply(myfiles, read.csv)
names(cond_list)<- gsub("\\.csv*","", myfiles)

# Fat content
# Data sourced from Pauline M's extraction from BioDaSys in 2022
# Time series 2007 to 2021, with several missing years
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Fatmeter")
Fat_BET <- read.csv('fatcontent_bet.csv') 
Fat_YFT <- read.csv('fatcontent_yft.csv')
Fat_SKJ <- read.csv('fatcontent_skj.csv')

Fat_BET<-Fat_BET[Fat_BET$length_cm%in%40:60,] # restrict to fish between 40 and 60 cm UF
Fat_YFT<-Fat_YFT[Fat_YFT$length_cm%in%40:60,]
Fat_SKJ<-Fat_SKJ[Fat_SKJ$length_cm%in%40:60,]

Mean_Fat_BET<-aggregate(fat_content ~ Year, Fat_BET, mean) # Calculate mean fat content by species
Mean_Fat_YFT<-aggregate(fat_content ~ Year, Fat_YFT, mean)
Mean_Fat_SKJ<-aggregate(fat_content ~ Year, Fat_SKJ, mean)

# Bycatch
# Data sourced from Tom Peatman's bycatch estimation models
# Tiem series 2003 to 2018 (or 2020).
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Bycatch")
bc_data = read.csv('PS_bycatch_2003-2020_revised.csv') 
llbc_data = read.csv('LL_bycatch_2003-2018.csv')
llbc_data$Quantity = as.numeric(gsub(',','',sapply(llbc_data$Number_individuals_..000s, toString)))

PS_fin = bc_data %>% filter(Species=='Finfish' & Gear=='Purse_seine' & Unit=='Metric_tonne')
Yr_Bycatch_PS_Fish = createInd(PS_fin$Quantity/1000, PS_fin$ï..Year)
PS_shark = bc_data %>% filter(Species=='Shark' & Gear=='Purse_seine' & Unit=='Number_individuals')
Yr_Bycatch_PS_Shark = createInd(PS_shark$Quantity/1000, PS_shark$ï..Year)
PS_bill = bc_data %>% filter(Species=='Billfish' & Gear=='Purse_seine' & Unit=='Number_individuals')
Yr_Bycatch_PS_Bill = createInd(PS_bill$Quantity/1000, PS_bill$ï..Year)

Free_fin = bc_data %>% filter(Species=='Finfish' & Gear=='Purse_seine_UNA' & Unit=='Metric_tonne')
Yr_Bycatch_Free_Fish = createInd(Free_fin$Quantity/1000, Free_fin$ï..Year)
Free_shark = bc_data %>% filter(Species=='Shark' & Gear=='Purse_seine_UNA' & Unit=='Number_individuals')
Yr_Bycatch_Free_Shark = createInd(Free_shark$Quantity/1000, Free_shark$ï..Year)
Free_bill = bc_data %>% filter(Species=='Billfish' & Gear=='Purse_seine_UNA' & Unit=='Number_individuals')
Yr_Bycatch_Free_Bill = createInd(Free_bill$Quantity/1000, Free_bill$ï..Year)

FAD_fin = bc_data %>% filter(Species=='Finfish' & Gear=='Purse_seine_ASS' & Unit=='Metric_tonne')
Yr_Bycatch_FAD_Fish = createInd(FAD_fin$Quantity/1000, FAD_fin$ï..Year)
FAD_shark = bc_data %>% filter(Species=='Shark' & Gear=='Purse_seine_ASS' & Unit=='Number_individuals')
Yr_Bycatch_FAD_Shark = createInd(FAD_shark$Quantity/1000, FAD_shark$ï..Year)
FAD_bill = bc_data %>% filter(Species=='Billfish' & Gear=='Purse_seine_ASS' & Unit=='Number_individuals')
Yr_Bycatch_FAD_Bill = createInd(FAD_bill$Quantity/1000, FAD_bill$ï..Year)

LL_fin = llbc_data[llbc_data$Species=='Finfish',]
Yr_Bycatch_LL_Fish = createInd(LL_fin$Quantity/1000, LL_fin$ï..Year)
LL_shark = llbc_data[llbc_data$Species=='Sharks',]
Yr_Bycatch_LL_Shark = createInd(LL_shark$Quantity/1000, LL_shark$ï..Year)
LL_bill = llbc_data[llbc_data$Species=='Billfish',]
Yr_Bycatch_LL_Bill = createInd(LL_bill$Quantity/1000, LL_bill$ï..Year)


# Build new list to store all indicators
indicators_list_2023_raw<-list(Yr_WCPO_Mean_Anom = env_list$`WCPOMeanAnomaly_1993-2021`, 
                      Yr_EqWCPO_Mean_Anom = env_list$`EqWCPOMeanAnomaly_1993-2021`,
                      Yr_WP_Mean_Anom = env_list$`WarmPoolMeanAnomaly_1993-2021`,
                      Yr_WP_Size = env_list$`WarmPoolSize_1993-2021`,
                      Yr_BLayer_East = env_list$`WarmPoolBoundary_1993-2021`,
                      Yr_WarmPool_MLD = env_list$`WarmPool_MixedLayerDepth_1993-2021`,
                      Yr_ONI = env_list$ONI_NDJ_Index_New,
                      IPO = env_list$IPO_Index_New,
                            cg_lon_DFAD = all.ind[['cg']]$fad[,1:2], 
                            cg_lon_free = all.ind[['cg']]$free[,1:2],
                            ao_DFAD = data.frame(all.ind[['ao']]$fad[,c(1,3)]),
                            ao_free = data.frame(all.ind[['ao']]$free[,c(1,3)]),
                            ao_LL = data.frame(all.ind[['ao']]$ll[,c(1,3)]),
                            HS_prop_DFAD = data.frame(all.ind[['HS.effort']]$eff.props[,c(1,4)]),
                            HS_prop_free = data.frame(all.ind[['HS.effort']]$eff.props[,c(1,7)]),
                                    ann_catch_ALB = catch_list$ALB[,c(1,3)],
                                    ann_catch_BET = catch_list$BET[,c(1,3)],
                                    ann_catch_SKJ = catch_list$SKJ[,c(1,3)],
                                    ann_catch_YFT = catch_list$YFT[,c(1,3)],
                                          mean_length_PS_SKJ = cond_list$L.means_SKJ_PS,
                                          mean_length_LL_SKJ = cond_list$L.means_SKJ_LL,
                                          mean_length_BET = cond_list$L.means_BET,
                                          mean_length_YFT = cond_list$L.means_YFT,
                                          K_rel_SKJ = cond_list$K_rel.means_SKJ,
                                          K_rel_BET = cond_list$K_rel.means_BET,
                                          K_rel_YFT = cond_list$K_rel.means_YFT,
                                                Mean_Fat_BET = Mean_Fat_BET,
                                                Mean_Fat_YFT = Mean_Fat_YFT,
                                                Mean_Fat_SKJ = Mean_Fat_SKJ,
                                                      Yr_Bycatch_PS_Fish = Yr_Bycatch_PS_Fish, 
                                                      Yr_Bycatch_PS_Shark = Yr_Bycatch_PS_Shark,
                                                      Yr_Bycatch_PS_Bill = Yr_Bycatch_PS_Bill,
                                                      Yr_Bycatch_Free_Fish = Yr_Bycatch_Free_Fish,
                                                      Yr_Bycatch_Free_Shark = Yr_Bycatch_Free_Shark,
                                                      Yr_Bycatch_Free_Bill = Yr_Bycatch_Free_Bill,
                                                      Yr_Bycatch_FAD_Fish = Yr_Bycatch_FAD_Fish,
                                                      Yr_Bycatch_FAD_Shark = Yr_Bycatch_FAD_Shark,
                                                      Yr_Bycatch_FAD_Bill = Yr_Bycatch_FAD_Bill,
                                                      Yr_Bycatch_LL_Fish = Yr_Bycatch_LL_Fish,
                                                      Yr_Bycatch_LL_Shark = Yr_Bycatch_LL_Shark,
                                                      Yr_Bycatch_LL_Bill = Yr_Bycatch_LL_Bill)

# Save to .Rdata file
setwd("..")
save(indicators_list_2023_raw, file="indicators_list_2023_raw.Rdata")


############################################################
# Notes for interpretation
############################################################

# 'indicators_list_2023_raw' is a list of 41 elements
#
#  1. Yr_WCPO_Mean_Anom = mean annual SST anomaly (°C) across WCPO area
#     contains 2 list elements: Date=year, Ind=anomaly value
#  2. Yr_EqWCPO_Mean_Anom = Mean annual SST anomaly (°C) across WCPO equatorial zone 
#     contains 2 list elements: Date=year, Ind=anomaly value
#  3. Yr_WP_Mean_Anom = Mean annual SST anomaly (°C) within warm-pool extent
#     contains 2 list elements: Date=year, Ind=anomaly value
#  4. Yr_WP_Size = Approximate size of warm-pool in km^2
#     contains 2 list elements: Date=year, Ind=area in km^2
#  5. Yr_BLayer_East = Longitude of strongest sea surface salinity boundary     
#     contains 2 list elements: Date=year, Ind=longitude
#  6. Yr_WarmPool_MLD = Mean depth (m) of the mixed layer within warm-pool 
#     contains 2 list elements: Date=year, Ind=depth in m
#  7. Yr_ONI = ONI indicates SST anomalies in the Niño 3.4 region during Nov-Jan each year
#     contains 2 list elements: Date=year, Ind=ONI index
#  8. IPO = IPO represents long-term oscillation between El Niño favourable and La Niña favourable phases 
#     contains 2 list elements: Date=year, Ind=IPO index
#  9. cg_lon_DFAD = annual centre of gravity (longitude) of purse seine effort (sets) on drifting FADs
#     contains 2 list elements: yy=year, lon=longitude
#  10. cg_lon_free = annual centre of gravity (longitude) of purse seine effort (sets) on free schools 
#      contains 2 list elements: yy=year, lon=longitude
#  11. ao_DFAD = annual area of fishing effort by purse seine fleet on drifting FADs
#      contains 2 list elements: YY=year, area=area occupied (in km^2)
#  12. ao_free = annual area of fishing effort by purse seine fleet on free schools
#      contains 2 list elements: YY=year, area=area occupied (in km^2)
#  13. ao_LL = annual area of fishing effort by longline fleet
#      contains 2 list elements: YY=year, area=area occupied (in km^2)
#  14. HS_prop_DFAD = annual proportion of purse seine sets on drifting FADs made in high seas areas
#      contains 2 list elements: YY=year, prop.HS_fad=proportion of effort
#  15. HS_prop_free = annual proportion of purse seine sets on free school made in high seas areas
#      contains 2 list elements: YY=year, prop.HS_free=proportion of effort
#  16. ann_catch_ALB = Total albacore catch (both North and South Pacific stocks combined) for entire WCPFC-CA, in 100,000s of metric tonnes
#      contains 2 list elements: yy=year, onee5_t=catch in 100,000s of metric tonnes
#  17. ann_catch_BET = Total bigeye catch for entire WCPFC-CA, in 100,000s of metric tonnes
#      contains 2 list elements: yy=year, onee5_t=catch in 100,000s of metric tonnes
#  18. ann_catch_SKJ = Total skipjack catch for entire WCPFC-CA, in millions of metric tonnes
#      contains 2 list elements: yy=year, onee6_t=catch in millions of metric tonnes
#  19. ann_catch_YFT = Total yellowfin catch for entire WCPFC-CA, in 100,000s of metric tonnes
#      contains 2 list elements: yy=year, onee5_t=catch in 100,000s of metric tonnes
#  20. mean_length_PS_SKJ = Annual mean fork length (cm) of skipjack captured in the purse seine fishery
#      contains 2 list elements: YR=year, LENGTH=mean fork length (cm)
#  21. mean_length_LL_SKJ = Annual mean fork length (cm) of skipjack captured in the longline fishery
#      contains 2 list elements: YR=year, LENGTH=mean fork length (cm)
#  22. mean_length_BET = Annual mean fork length (cm) of bigeye captured in the longline fishery
#      contains 2 list elements: YR=year, LENGTH=mean fork length (cm)
#  23. mean_length_YFT = Annual mean fork length (cm) of yellowfin captured in the longline fishery
#      contains 2 list elements: YR=year, LENGTH=mean fork length (cm)
#  24. K_rel_SKJ = Annual mean relative condition factor (K_rel) for skipjack captured in the longline fishery
#      contains 2 list elements: yy=year, K_rel=annual average of observed individual tuna weight divided by predicted weight at length 
#  25. K_rel_BET = Annual mean relative condition factor (K_rel) for bigeye captured in the longline fishery
#      contains 2 list elements: yy=year, K_rel=annual average of observed individual tuna weight divided by predicted weight at length 
#  26. K_rel_YFT = Annual mean relative condition factor (K_rel) for yellowfin captured in the longline fishery
#      contains 2 list elements: yy=year, K_rel=annual average of observed individual tuna weight divided by predicted weight at length 
#  27. Mean_Fat_BET = Mean fat content (%) as measured by fatmeter during annual PTTP research cruises, on 40 to 60 cm UF BET
#      contains 2 list elements: Year=year, fat_content=% fat content
#  28. Mean_Fat_YFT = Mean fat content (%) as measured by fatmeter during annual PTTP research cruises, on 40 to 60 cm UF YFT
#      contains 2 list elements: Year=year, fat_content=% fat content   
#  29. Mean_Fat_SKJ = Mean fat content (%) as measured by fatmeter during annual PTTP research cruises, on 40 to 60 cm UF SKJ
#      contains 2 list elements: Year=year, fat_content=% fat content
#  30. Yr_Bycatch_PS_Fish = Estimated purse seine catch of finfish bycatch species in 1000s of metric tonnes
#      contains 2 list elements: Date=year, Ind=catch in 1000s metric tonnes
#  31. Yr_Bycatch_PS_Shark = Estimated purse seine catch of sharks in 1000s of individuals 
#      contains 2 list elements: Date=year, Ind=catch in 1000s of individuals
#  32. Yr_Bycatch_PS_Bill = Estimated purse seine catch of billfish in 1000s of individuals 
#      contains 2 list elements: Date=year, Ind=catch in 1000s of individuals
#  33. Yr_Bycatch_Free_Fish = Estimated unassociated purse seine catch of finfish bycatch species in 1000s of metric tonnes
#      contains 2 list elements: Date=year, Ind=catch in 1000s metric tonnes
#  34. Yr_Bycatch_Free_Shark = Estimated unassociated purse seine catch of sharks in 1000s of individuals 
#      contains 2 list elements: Date=year, Ind=catch in 1000s of individuals
#  35. Yr_Bycatch_Free_Bill = Estimated unassociated purse seine catch of billfish in 1000s of individuals
#      contains 2 list elements: Date=year, Ind=catch in 1000s of individuals
#  36. Yr_Bycatch_FAD_Fish = Estimated associated purse seine catch of finfish bycatch species in 1000s of metric tonnes
#      contains 2 list elements: Date=year, Ind=catch in 1000s metric tonnes
#  37. Yr_Bycatch_FAD_Shark = Estimated associated purse seine catch of sharks in 1000s of individuals 
#      contains 2 list elements: Date=year, Ind=catch in 1000s of individuals
#  38. Yr_Bycatch_FAD_Bill = Estimated associated purse seine catch of billfish in 1000s of individuals
#      contains 2 list elements: Date=year, Ind=catch in 1000s of individuals
#  39. Yr_Bycatch_LL_Fish = Estimated longline catch of finfish bycatch species in millions of individuals
#      contains 2 list elements: Date=year, Ind=catch in millions of individuals
#  40. Yr_Bycatch_LL_Shark = Estimated longline catch of sharks in millions of individuals
#      contains 2 list elements: Date=year, Ind=catch in millions of individuals
#  41. Yr_Bycatch_LL_Bill = Estimated Longline catch of billfish in millions of individuals
#      contains 2 list elements: Date=year, Ind=catch in millions of individuals


############################################################
# Plotting
############################################################

# Reload indicators list for plotting (if need be)
load("indicators_list_2023_raw.Rdata")

# Duplicate list for plotting purposes
indicators_list_2023_plots <- indicators_list_2023_raw

# Format list elements (that need it) for plotting
indicators_list_2023_plots$Yr_WP_Size$Ind<-indicators_list_2023_plots$Yr_WP_Size$Ind*85.5625/1000000
indicators_list_2023_plots$cg_lon_DFAD<-createInd(indicators_list_2023_plots$cg_lon_DFAD$lon, year=indicators_list_2023_plots$cg_lon_DFAD$yy)
indicators_list_2023_plots$cg_lon_free<-createInd(indicators_list_2023_plots$cg_lon_free$lon, year=indicators_list_2023_plots$cg_lon_free$yy)
indicators_list_2023_plots$ao_DFAD<-createInd(indicators_list_2023_plots$ao_DFAD$area, year=indicators_list_2023_plots$ao_DFAD$YY)
indicators_list_2023_plots$ao_free<-createInd(indicators_list_2023_plots$ao_free$area, year=indicators_list_2023_plots$ao_free$YY)
indicators_list_2023_plots$ao_LL<-createInd(indicators_list_2023_plots$ao_LL$area, year=indicators_list_2023_plots$ao_LL$YY)
indicators_list_2023_plots$ao_free$Ind<-indicators_list_2023_plots$ao_free$Ind/1000000
indicators_list_2023_plots$ao_DFAD$Ind<-indicators_list_2023_plots$ao_DFAD$Ind/1000000
indicators_list_2023_plots$ao_LL$Ind<-indicators_list_2023_plots$ao_LL$Ind/1000000
indicators_list_2023_plots$HS_prop_DFAD<-createInd(indicators_list_2023_plots$HS_prop_DFAD$prop.HS_fad, year=indicators_list_2023_plots$HS_prop_DFAD$YY)
indicators_list_2023_plots$HS_prop_free<-createInd(indicators_list_2023_plots$HS_prop_free$prop.HS_free, year=indicators_list_2023_plots$HS_prop_free$YY)
indicators_list_2023_plots$ann_catch_ALB<-createInd(indicators_list_2023_plots$ann_catch_ALB$onee5_t, year=indicators_list_2023_plots$ann_catch_ALB$yy)
indicators_list_2023_plots$ann_catch_BET<-createInd(indicators_list_2023_plots$ann_catch_BET$onee5_t, year=indicators_list_2023_plots$ann_catch_BET$yy)
indicators_list_2023_plots$ann_catch_SKJ<-createInd(indicators_list_2023_plots$ann_catch_SKJ$onee6_t, year=indicators_list_2023_plots$ann_catch_SKJ$yy)
indicators_list_2023_plots$ann_catch_YFT<-createInd(indicators_list_2023_plots$ann_catch_YFT$onee5_t, year=indicators_list_2023_plots$ann_catch_YFT$yy)
indicators_list_2023_plots$mean_length_PS_SKJ<-createInd(indicators_list_2023_plots$mean_length_PS_SKJ$LENGTH, year=indicators_list_2023_plots$mean_length_PS_SKJ$YR)
indicators_list_2023_plots$mean_length_LL_SKJ<-createInd(indicators_list_2023_plots$mean_length_LL_SKJ$LENGTH, year=indicators_list_2023_plots$mean_length_LL_SKJ$YR)
indicators_list_2023_plots$mean_length_BET<-createInd(indicators_list_2023_plots$mean_length_BET$LENGTH, year=indicators_list_2023_plots$mean_length_BET$YR)
indicators_list_2023_plots$mean_length_YFT<-createInd(indicators_list_2023_plots$mean_length_YFT$LENGTH, year=indicators_list_2023_plots$mean_length_YFT$YR)
indicators_list_2023_plots$K_rel_SKJ<-createInd(indicators_list_2023_plots$K_rel_SKJ$K_rel, year=indicators_list_2023_plots$K_rel_SKJ$yy)
indicators_list_2023_plots$K_rel_BET<-createInd(indicators_list_2023_plots$K_rel_BET$K_rel, year=indicators_list_2023_plots$K_rel_BET$yy)
indicators_list_2023_plots$K_rel_YFT<-createInd(indicators_list_2023_plots$K_rel_YFT$K_rel, year=indicators_list_2023_plots$K_rel_YFT$yy)
indicators_list_2023_plots$Mean_Fat_BET<-createInd(indicators_list_2023_plots$Mean_Fat_BET$fat_content, year=indicators_list_2023_plots$Mean_Fat_BET$Year)
indicators_list_2023_plots$Mean_Fat_YFT<-createInd(indicators_list_2023_plots$Mean_Fat_YFT$fat_content, year=indicators_list_2023_plots$Mean_Fat_YFT$Year)
indicators_list_2023_plots$Mean_Fat_SKJ<-createInd(indicators_list_2023_plots$Mean_Fat_SKJ$fat_content, year=indicators_list_2023_plots$Mean_Fat_SKJ$Year)

# Save to .Rdata file
save(indicators_list_2023_plots, file="indicators_list_2023_plots.Rdata")

# Set up colour palettes
WP_Pal = tail(brewer.pal(9,'Reds'),6)[c(1,4)]
SST_Pal = c('darkgoldenrod1','gold3')
LL_Pal = c('slateblue2', 'slateblue4')
FAD_Pal = c('indianred2', 'indianred4')
Free_Pal = c('skyblue2', 'skyblue4')
PS_Pal = c('orchid2', 'orchid4')
SKJ_Pal = c('skyblue1', 'skyblue3')
YFT_Pal = c('gold2','gold4')
BET_Pal = c('firebrick2', 'firebrick4')
ALB_Pal = c('seagreen2', 'seagreen4')
Fin_Pal = c('plum2', 'plum4')
Bill_Pal = c('lightsteelblue2','lightsteelblue4')
Shark_Pal = c('gray60', 'gray25')
ax.col="black"


# Plot Environment and Climate Indicators
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Sparklines/Environment and climate")

par(mfrow=c(3,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
pdf.options(width=9,height=4)
plotEnvInd(indicators_list_2023_plots$Yr_WCPO_Mean_Anom, baseline=mean(indicators_list_2023_plots$Yr_EqWCPO_Mean_Anom$Ind), lcol=SST_Pal[1], finalcol=SST_Pal[2], rounder=2,
           pdf='AnomWCPO.pdf', ax.lab.col = ax.col, drawaxis = "top")
plotEnvInd(indicators_list_2023_plots$Yr_EqWCPO_Mean_Anom, baseline=mean(indicators_list_2023_plots$Yr_EqWCPO_Mean_Anom$Ind), lcol=SST_Pal[1], finalcol=SST_Pal[2], rounder=2,
           pdf="AnomEq.pdf", ax.lab.col = ax.col, drawaxis = NULL)
plotEnvInd(indicators_list_2023_plots$Yr_WP_Mean_Anom, baseline=0, lcol=SST_Pal[1], finalcol=SST_Pal[2], rounder=2, 
           pdf='AnomWP.pdf', ax.lab.col = ax.col)
plotEnvInd(indicators_list_2023_plots$Yr_WP_Size, lcol=WP_Pal[1], finalcol=WP_Pal[2], rounder=0,
           pdf='WPsize.pdf', ax.lab.col = ax.col)
plotEnvInd(indicators_list_2023_plots$Yr_BLayer_East, baseline_label = paste0(round(mean(indicators_list_2023_plots$Yr_BLayer_East$Ind)),'E'), rounder=0, final_paste='E',
           yax_paste=c('E','E'), lcol=WP_Pal[1], finalcol=WP_Pal[2], 
           pdf='WPeast.pdf', ax.lab.col = ax.col)
plotEnvInd(indicators_list_2023_plots$Yr_WarmPool_MLD, lcol=WP_Pal[1],finalcol=WP_Pal[2], rounder=1,  
           pdf='WP_MLD.pdf', ax.lab.col = ax.col)
plotEnvInd(list(indicators_list_2023_plots$IPO,indicators_list_2023_plots$Yr_ONI), baseline=0, rounder=1, yaxis=c(-0.5,0.5), lty=2:1, lcols=c('deepskyblue4','skyblue2'), finalcols=c('dodgerblue4','blue3'),
           drawLegend=TRUE, x='top', cex=1.1, legend=c('IPO', 'ONI'), legend.lty=2:1,legend.lwd=2, col=c('deepskyblue4','skyblue2'),
           pdf='Climate.pdf', ax.lab.col = ax.col, drawaxis = "bottom")

pdf("Environment and Climate.pdf", width=4,height=7) # multiplot for SC19 paper
par(mfrow=c(7,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
plotEnvInd(indicators_list_2023_plots$Yr_WCPO_Mean_Anom, baseline=mean(indicators_list_2023_plots$Yr_EqWCPO_Mean_Anom$Ind), lcol=SST_Pal[1], finalcol=SST_Pal[2], rounder=2,
           pdf=NULL, ax.lab.col = ax.col, drawaxis = "top")
plotEnvInd(indicators_list_2023_plots$Yr_EqWCPO_Mean_Anom, baseline=mean(indicators_list_2023_plots$Yr_EqWCPO_Mean_Anom$Ind), lcol=SST_Pal[1], finalcol=SST_Pal[2], rounder=2,
           pdf=NULL, ax.lab.col = ax.col, drawaxis = NULL)
plotEnvInd(indicators_list_2023_plots$Yr_WP_Mean_Anom, baseline=0, lcol=SST_Pal[1], finalcol=SST_Pal[2], rounder=2, 
           pdf=NULL, ax.lab.col = ax.col)
plotEnvInd(indicators_list_2023_plots$Yr_WP_Size, lcol=WP_Pal[1], finalcol=WP_Pal[2], rounder=0,
           pdf=NULL, ax.lab.col = ax.col)
plotEnvInd(indicators_list_2023_plots$Yr_BLayer_East, baseline_label = paste0(round(mean(indicators_list_2023_plots$Yr_BLayer_East$Ind)),'E'), rounder=0, final_paste='E',
           yax_paste=c('E','E'), lcol=WP_Pal[1], finalcol=WP_Pal[2], 
           pdf=NULL, ax.lab.col = ax.col)
plotEnvInd(indicators_list_2023_plots$Yr_WarmPool_MLD, lcol=WP_Pal[1],finalcol=WP_Pal[2], rounder=1,  
           pdf=NULL, ax.lab.col = ax.col)
plotEnvInd(list(indicators_list_2023_plots$IPO,indicators_list_2023_plots$Yr_ONI), baseline=0, rounder=1, yaxis=c(-0.5,0.5), lty=2:1, lcols=c('deepskyblue4','skyblue2'), finalcols=c('dodgerblue4','blue3'),
           drawLegend=TRUE, x='top', cex=1.1, legend=c('IPO', 'ONI'), legend.lty=2:1,legend.lwd=2, col=c('deepskyblue4','skyblue2'),
           pdf=NULL, ax.lab.col = ax.col, drawaxis = "bottom")
dev.off()


# Plot Fishing Effort Indicators
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Sparklines/Effort")

par(mfrow=c(3,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
pdf.options(width=9,height=4)
plotEnvInd(list(indicators_list_2023_plots$cg_lon_free,indicators_list_2023_plots$cg_lon_DFAD),
           lcols=c(Free_Pal[1],FAD_Pal[1]), finalcols=c(Free_Pal[2],FAD_Pal[2]), lty=1:2, rounder=0, final_paste=rep('E',2),
           baseline = mean(indicators_list_2023_plots$cg_lon_free$Ind), baseline_paste = 'E', yax_paste = 'E',
           drawLegend=TRUE, x='topleft', cex=1.1, legend=c('UNA', 'ASS'), legend.lty=1:2,legend.lwd=2, col=c(Free_Pal[1],FAD_Pal[1],LL_Pal[1]),
           pdf='PSEffortLon.pdf')
plotEnvInd(list(indicators_list_2023_plots$ao_free,indicators_list_2023_plots$ao_DFAD),
           lcols=c(Free_Pal[1],FAD_Pal[1]), finalcols=c(Free_Pal[2],FAD_Pal[2]), lty=1:2, rounder=1,
           baseline = mean(indicators_list_2023_plots$ao_free$Ind), 
           pdf='AreaOcc_PS.pdf')
plotEnvInd(indicators_list_2023_plots$ao_LL, lcol=LL_Pal[1], finalcols=LL_Pal[2], rounder=1,
           baseline = mean(indicators_list_2023_plots$ao_LL$Ind), 
           pdf='AreaOcc_LL.pdf')
plotEnvInd(list(indicators_list_2023_plots$HS_prop_free,indicators_list_2023_plots$HS_prop_DFAD),
           lcols=c(Free_Pal[1],FAD_Pal[1]), finalcols=c(Free_Pal[2],FAD_Pal[2]), lty=1:2, rounder=2,
           baseline = mean(indicators_list_2023_plots$HS_prop_DFAD$Ind),
           pdf='HSprop.pdf')

pdf("Fishing Effort.pdf", width=4,height=7) # multiplot for SC19 paper
par(mfrow=c(7,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
plotEnvInd(list(indicators_list_2023_plots$cg_lon_free,indicators_list_2023_plots$cg_lon_DFAD),
           lcols=c(Free_Pal[1],FAD_Pal[1]), finalcols=c(Free_Pal[2],FAD_Pal[2]), lty=1:2, rounder=0, final_paste=rep('E',2),
           baseline = mean(indicators_list_2023_plots$cg_lon_free$Ind), baseline_paste = 'E', yax_paste = 'E',
           drawLegend=TRUE, x='topleft', cex=1.1, legend=c('UNA', 'ASS'), legend.lty=1:2,legend.lwd=3, col=c(Free_Pal[1],FAD_Pal[1],LL_Pal[1]),
           pdf=NULL)
plotEnvInd(list(indicators_list_2023_plots$ao_free,indicators_list_2023_plots$ao_DFAD),
           lcols=c(Free_Pal[1],FAD_Pal[1]), finalcols=c(Free_Pal[2],FAD_Pal[2]), lty=1:2, rounder=1,
           baseline = mean(indicators_list_2023_plots$ao_free$Ind), 
           pdf=NULL)
plotEnvInd(indicators_list_2023_plots$ao_LL, lcol=LL_Pal[1], finalcols=LL_Pal[2], rounder=1,
           baseline = mean(indicators_list_2023_plots$ao_LL$Ind), 
           pdf=NULL)
plotEnvInd(list(indicators_list_2023_plots$HS_prop_free,indicators_list_2023_plots$HS_prop_DFAD),
           lcols=c(Free_Pal[1],FAD_Pal[1]), finalcols=c(Free_Pal[2],FAD_Pal[2]), lty=1:2, rounder=2,
           baseline = mean(indicators_list_2023_plots$HS_prop_DFAD$Ind),
           pdf=NULL, drawaxis = "bottom")
dev.off()



# Plot Annual Tuna Catch indicators
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Sparklines/Catch and distribution")

par(mfrow=c(3,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
pdf.options(width=9,height=4)
plotEnvInd(indicators_list_2023_plots$ann_catch_SKJ, lcol=SKJ_Pal[1], finalcol=SKJ_Pal[2], rounder=1, drawaxis='top',
           baseline = round(mean(indicators_list_2023_plots$ann_catch_SKJ$Ind),1), 
           lwd=1.4,
           pdf='SKJcatch.pdf',
           drawLegend=TRUE, cex=1.2, legend.lwd=2, col=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1],ALB_Pal[1]),legend=c('Skipjack','Yellowfin', 'Bigeye', 'Albacore'), x='topleft')
plotEnvInd(indicators_list_2023_plots$ann_catch_YFT, lcol=YFT_Pal[1], finalcol=YFT_Pal[2], rounder=1,
           lwd=1.4,
           pdf='YFTcatch.pdf')
plotEnvInd(list(indicators_list_2023_plots$ann_catch_ALB, indicators_list_2023_plots$ann_catch_BET), lcol=c(ALB_Pal[1],BET_Pal[1]), finalcol=c(ALB_Pal[2],BET_Pal[2]), rounder=1,
           lwd=1.4, 
           pdf='ALBBETcatch.pdf')

pdf("Annual Tuna Catch.pdf", width=4,height=7) # multiplot for SC19 paper
par(mfrow=c(7,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
plotEnvInd(indicators_list_2023_plots$ann_catch_SKJ, lcol=SKJ_Pal[1], finalcol=SKJ_Pal[2], rounder=1, drawaxis='top',
           baseline = round(mean(indicators_list_2023_plots$ann_catch_SKJ$Ind),1), 
           lwd=1.4,
           pdf=NULL,
           drawLegend=TRUE, cex=1.2, legend.lwd=2, col=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1],ALB_Pal[1]),legend=c('Skipjack','Yellowfin', 'Bigeye', 'Albacore'), x='topleft')
plotEnvInd(indicators_list_2023_plots$ann_catch_YFT, lcol=YFT_Pal[1], finalcol=YFT_Pal[2], rounder=1,
           lwd=1.4,
           pdf=NULL)
plotEnvInd(list(indicators_list_2023_plots$ann_catch_ALB, indicators_list_2023_plots$ann_catch_BET), lcol=c(ALB_Pal[1],BET_Pal[1]), finalcol=c(ALB_Pal[2],BET_Pal[2]), rounder=1,
           lwd=1.4, 
           pdf=NULL)
dev.off()


# Plot Fish Condition indicators
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Sparklines/Fish condition")

par(mfrow=c(3,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
pdf.options(width=9,height=4)
plotEnvInd(indicators_list_2023_plots$mean_length_PS_SKJ, lcol=SKJ_Pal[1], finalcol=SKJ_Pal[2], drawaxis='top',rounder=0, final_paste='cm',
           baseline = mean(indicators_list_2023_plots$mean_length_PS_SKJ$Ind), lwd=1.4, ylim=c(40,55), lty=1,
           pdf='SKJ_PSMeanLength.pdf', 
           drawLegend=TRUE, x='bottomleft', cex=1.2,legend='', legend.lty=0, legend.lwd=2, col=NA,
           caption='Purse seine', caption_pos ='bottomleft')
plotEnvInd(indicators_list_2023_plots$mean_length_LL_SKJ, lcol=SKJ_Pal[1], finalcol=SKJ_Pal[2], rounder=0, final_paste='cm',
           baseline = mean(indicators_list_2023_plots$mean_length_LL_SKJ$Ind), lwd=1.4, lty=1, yax_adj = c(-1,0),
           pdf='SKJ_LLMeanLength.pdf',
           drawLegend=TRUE, x='topright', cex=1.2,legend=c('Skipjack','Yellowfin', 'Bigeye'), legend.lty=c(1,1,1), legend.lwd=2, col=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1]),
           caption='Longline', caption_pos ='topleft')
plotEnvInd(list(indicators_list_2023_plots$mean_length_YFT, indicators_list_2023_plots$mean_length_BET), ylim=c(105,128), lcol=c(YFT_Pal[1],BET_Pal[1]), finalcol=c(YFT_Pal[2],BET_Pal[2]),rounder=0, final_paste='cm',
           baseline = mean(c(indicators_list_2023_plots$mean_length_YFT$Ind, indicators_list_2023_plots$mean_length_BET$Ind)), lwd=1.4,
           pdf='YFTBET_LLMeanLength.pdf')
plotEnvInd(list(indicators_list_2023_plots$K_rel_SKJ[20:32,], indicators_list_2023_plots$K_rel_YFT, indicators_list_2023_plots$K_rel_BET), lcol=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1]), rounder=2,
           finalcol=c(SKJ_Pal[2],YFT_Pal[2],BET_Pal[2]), final_adj = c(0.03,0,-0.02),
           baseline = 1, lwd=1.4, yax_adj = c(-1.5,0),
           pdf='LLMeanCondition.pdf')
plotEnvInd(list(indicators_list_2023_plots$Mean_Fat_SKJ, indicators_list_2023_plots$Mean_Fat_YFT, indicators_list_2023_plots$Mean_Fat_BET), lcol=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1]), rounder=2,
           finalcol=c(SKJ_Pal[2],YFT_Pal[2],BET_Pal[2]), final_adj = c(0.03,0,-0.02), 
           baseline = mean(indicators_list_2023_plots$Mean_Fat_YFT$Ind, na.rm=TRUE), lwd=1.4, 
           pdf='MeanFat.pdf')

pdf("Fish Condition.pdf", width=4,height=7) # multiplot for SC19 paper
par(mfrow=c(7,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
plotEnvInd(indicators_list_2023_plots$mean_length_PS_SKJ, lcol=SKJ_Pal[1], finalcol=SKJ_Pal[2], drawaxis='top',rounder=0, final_paste='cm',
           baseline = mean(indicators_list_2023_plots$mean_length_PS_SKJ$Ind), lwd=1.4, ylim=c(40,55), lty=1,
           pdf=NULL, 
           drawLegend=TRUE, x='bottomleft', cex=1.2,legend='', legend.lty=0, legend.lwd=2, col=NA,
           caption='Purse seine', caption_pos ='bottomleft')
plotEnvInd(indicators_list_2023_plots$mean_length_LL_SKJ, lcol=SKJ_Pal[1], finalcol=SKJ_Pal[2], rounder=0, final_paste='cm',
           baseline = mean(indicators_list_2023_plots$mean_length_LL_SKJ$Ind), lwd=1.4, lty=1, yax_adj = c(-1,0),
           pdf=NULL,
           drawLegend=TRUE, x='topright', cex=1.2,legend=c('Skipjack','Yellowfin', 'Bigeye'), legend.lty=c(1,1,1), legend.lwd=2, col=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1]),
           caption='Longline', caption_pos ='topleft')
plotEnvInd(list(indicators_list_2023_plots$mean_length_YFT, indicators_list_2023_plots$mean_length_BET), ylim=c(105,128), lcol=c(YFT_Pal[1],BET_Pal[1]), finalcol=c(YFT_Pal[2],BET_Pal[2]),rounder=0, final_paste='cm',
           baseline = mean(c(indicators_list_2023_plots$mean_length_YFT$Ind, indicators_list_2023_plots$mean_length_BET$Ind)), lwd=1.4,
           pdf=NULL)
plotEnvInd(list(indicators_list_2023_plots$K_rel_SKJ[20:32,], indicators_list_2023_plots$K_rel_YFT, indicators_list_2023_plots$K_rel_BET), lcol=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1]), rounder=2,
           finalcol=c(SKJ_Pal[2],YFT_Pal[2],BET_Pal[2]), final_adj = c(0.03,0,-0.02),
           baseline = 1, lwd=1.4, yax_adj = c(-1.5,0),
           pdf=NULL)
plotEnvInd(list(indicators_list_2023_plots$Mean_Fat_SKJ, indicators_list_2023_plots$Mean_Fat_YFT, indicators_list_2023_plots$Mean_Fat_BET), lcol=c(SKJ_Pal[1],YFT_Pal[1],BET_Pal[1]), rounder=2,
           finalcol=c(SKJ_Pal[2],YFT_Pal[2],BET_Pal[2]), final_adj = c(0.03,0,-0.02), 
           baseline = mean(indicators_list_2023_plots$Mean_Fat_YFT$Ind, na.rm=TRUE), lwd=1.4, 
           pdf=NULL)
dev.off()

# Plot Bycatch indicators
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Sparklines/Bycatch")

par(mfrow=c(3,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
pdf.options(width=9,height=4)
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_Free_Fish, lcol=Fin_Pal[1], finalcol=Fin_Pal[2], rounder=1,
           lwd=1.4,
           pdf='FreeFinfishBycatch.pdf',
           drawLegend=TRUE, cex=1.2, legend.lty=c(1,2), legend.lwd=2, col=Fin_Pal[1],
           legend=c('UNA','ASS'), x='bottomleft')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_FAD_Fish, lcol=Fin_Pal[1], finalcol=Fin_Pal[2], rounder=1,
           lwd=1.4, lty=2,
           pdf='FADFinfishBycatch.pdf')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_LL_Fish, lcol=Fin_Pal[1], finalcol=Fin_Pal[2], rounder=1,
           lwd=1.4, ylim=c(11,15), 
           pdf='FinfishLLBycatch.pdf')
plotEnvInd(list(indicators_list_2023_plots$Yr_Bycatch_Free_Bill, indicators_list_2023_plots$Yr_Bycatch_FAD_Bill), lcol=rep(Bill_Pal[1],2), finalcol=rep(Bill_Pal[2],2), rounder=1,
           lwd=1.4, lty=1:2, final_adj = c(-0.1,0.2),
           pdf='FreeFADBillBycatch.pdf',
           drawLegend=TRUE, cex=1.2, legend.lty=c(1,2), legend.lwd=2, col=Bill_Pal[2],
           legend=c('UNA','ASS'), x='bottomleft')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_LL_Bill, lcol=Bill_Pal[1], finalcol=Bill_Pal[1], rounder=1,
           lwd=1.4, ylim=c(0.5,1), 
           pdf='LLBillBycatch.pdf')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_Free_Shark, lcol=Shark_Pal[1], finalcol=Shark_Pal[2], rounder=1,
           lwd=1.4,
           pdf='FreeSharkBycatch.pdf',
           drawLegend=TRUE, cex=1.2, legend.lty=c(1,2), legend.lwd=2, col=Shark_Pal[1],
           legend=c('UNA','ASS'), x='bottomleft')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_FAD_Shark, lcol=Shark_Pal[1], finalcol=Shark_Pal[2], rounder=1,
           lwd=1.4, lty=2,
           pdf='FADSharkBycatch.pdf')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_LL_Shark, lcol=Shark_Pal[1], finalcol=Shark_Pal[2], rounder=1,
           lwd=1.4, ylim=c(1.3,2.2), drawaxis = 'bottom', 
           pdf='LLSharkBycatch.pdf')

pdf("Bycatch.pdf", width=4,height=7) # multiplot for SC19 paper
par(mfrow=c(7,1),mar=c(0,1,2,1),oma=c(3,3,1,1))
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_Free_Fish, lcol=Fin_Pal[1], finalcol=Fin_Pal[2], rounder=1,
           lwd=1.4,
           pdf=NULL,
           drawLegend=TRUE, cex=1.2, legend.lty=c(1,2), legend.lwd=2, col=Fin_Pal[1],
           legend=c('UNA','ASS'), x='bottomleft')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_FAD_Fish, lcol=Fin_Pal[1], finalcol=Fin_Pal[2], rounder=1,
           lwd=1.4, lty=2,
           pdf=NULL)
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_LL_Fish, lcol=Fin_Pal[1], finalcol=Fin_Pal[2], rounder=1,
           lwd=1.4, ylim=c(11,15), 
           pdf=NULL)
plotEnvInd(list(indicators_list_2023_plots$Yr_Bycatch_Free_Bill, indicators_list_2023_plots$Yr_Bycatch_FAD_Bill), lcol=rep(Bill_Pal[1],2), finalcol=rep(Bill_Pal[2],2), rounder=1,
           lwd=1.4, lty=1:2, final_adj = c(-0.1,0.2),
           pdf=NULL,
           drawLegend=TRUE, cex=1.2, legend.lty=c(1,2), legend.lwd=2, col=Bill_Pal[1],
           legend=c('UNA','ASS'), x='bottomleft')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_LL_Bill, lcol=Bill_Pal[1], finalcol=Bill_Pal[2], rounder=1,
           lwd=1.4, ylim=c(0.5,1), 
           pdf=NULL)
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_Free_Shark, lcol=Shark_Pal[1], finalcol=Shark_Pal[2], rounder=1,
           lwd=1.4,
           pdf=NULL,
           drawLegend=TRUE, cex=1.2, legend.lty=c(1,2), legend.lwd=2, col=Shark_Pal[1],
           legend=c('UNA','ASS'), x='bottomleft')
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_FAD_Shark, lcol=Shark_Pal[1], finalcol=Shark_Pal[2], rounder=1,
           lwd=1.4, lty=2,
           pdf=NULL)
plotEnvInd(indicators_list_2023_plots$Yr_Bycatch_LL_Shark, lcol=Shark_Pal[1], finalcol=Shark_Pal[2], rounder=1,
           lwd=1.4, ylim=c(1.3,2.2), drawaxis = 'bottom', 
           pdf=NULL)
dev.off()


# Plot labels and keys
setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem indicators/Sparklines")
par(mfrow=c(1,1),mar=c(0,0,0,0),omi=c(0,0,0,0),omd=c(0,0,0,0))

pdf('Key.pdf')
plot(c(1,2),c(1,1),type='n', xaxt='n', yaxt='n', bty='n')
legend('center', c("Mean/Reference Value", "Central 50% of data range"), lty=c(1,2), lwd=4, col='gray60',bty='n', ncol=2,seg.len = 4)
dev.off()

## END ##